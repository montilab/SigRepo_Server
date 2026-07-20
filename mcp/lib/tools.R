# Defines the MCP tool list served by mcp/run_sigrepo_mcp.R. Each tool
# function takes a plain api_key (like every /read/* Plumber route) and
# authenticates via require_api_key() from api/lib/auth.R -- an auth failure
# throws a classed condition that mcptools/ellmer report back to the calling
# agent as a tool error. No admin-only or write-capable tools are exposed
# here by design.

mcp_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null")
}

mcp_list_vocabulary <- function(api_key) {
  require_api_key(api_key)
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  mcp_json(list_vocabulary(conn))
}

mcp_search_signatures <- function(api_key, organism = NULL, phenotype = NULL,
                                   assay_type = NULL, keyword = NULL, limit = 20) {
  auth <- require_api_key(api_key)
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  mcp_json(search_signatures(
    conn,
    organism = organism,
    phenotype = phenotype,
    assay_type = assay_type,
    keyword = keyword,
    limit = limit,
    is_admin = identical(auth$user_role, "admin")
  ))
}

mcp_get_signature_context <- function(api_key, signature_hashkey,
                                       include_features = TRUE, max_features = 50) {
  auth <- require_api_key(api_key)
  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = include_features,
    max_features = max_features,
    auth = auth
  )
  if (is.null(context)) {
    stop(base::sprintf(
      "No signature found for signature_hashkey = '%s', or you do not have access to it.",
      signature_hashkey
    ))
  }
  mcp_json(context)
}

mcp_compare_signatures <- function(api_key, signature_hashkey_1, signature_hashkey_2,
                                    max_features = 200) {
  auth <- require_api_key(api_key)
  mcp_json(compare_two_signatures(signature_hashkey_1, signature_hashkey_2, auth = auth, max_features = max_features))
}

build_mcp_tools <- function() {
  list(
    ellmer::tool(
      mcp_list_vocabulary,
      "List the distinct organism, phenotype, sample_type, platform, and assay_type values currently used by signatures in SigRepo. Call this before search_signatures to use valid filter values.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key")
      ),
      name = "list_vocabulary"
    ),
    ellmer::tool(
      mcp_search_signatures,
      "Search SigRepo signatures by organism, phenotype, assay type, and/or a free-text keyword (matched against name, description, and keywords). Returns a compact list of candidate matches -- use get_signature_context on a specific signature_hashkey for full detail.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        organism = ellmer::type_string("Exact organism name, e.g. 'Homo sapiens'", required = FALSE),
        phenotype = ellmer::type_string("Exact phenotype name", required = FALSE),
        assay_type = ellmer::type_enum(
          c("transcriptomics", "proteomics", "metabolomics", "methylomics", "snps"),
          "Assay type",
          required = FALSE
        ),
        keyword = ellmer::type_string("Free-text match against signature name, description, and keywords", required = FALSE),
        limit = ellmer::type_integer("Maximum number of results (default 20, max 100)", required = FALSE)
      ),
      name = "search_signatures"
    ),
    ellmer::tool(
      mcp_get_signature_context,
      "Fetch full metadata and (optionally) ranked features for one signature by its signature_hashkey.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        signature_hashkey = ellmer::type_string("The signature's hashkey, e.g. from search_signatures"),
        include_features = ellmer::type_boolean("Whether to include the ranked feature list", required = FALSE),
        max_features = ellmer::type_integer("Maximum number of features to return (default 50)", required = FALSE)
      ),
      name = "get_signature_context"
    ),
    ellmer::tool(
      mcp_compare_signatures,
      "Compare two signatures by shared/unique features and Jaccard similarity.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        signature_hashkey_1 = ellmer::type_string("First signature's hashkey"),
        signature_hashkey_2 = ellmer::type_string("Second signature's hashkey"),
        max_features = ellmer::type_integer("Maximum features considered per signature (default 200)", required = FALSE)
      ),
      name = "compare_signatures"
    )
  )
}
