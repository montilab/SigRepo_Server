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

mcp_search_collections <- function(api_key, collection_name = NULL, user_name = NULL,
                                    keyword = NULL, limit = 20) {
  auth <- require_api_key(api_key)
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  mcp_json(search_collections(
    conn,
    collection_name = collection_name,
    user_name = user_name,
    keyword = keyword,
    limit = limit,
    is_admin = identical(auth$user_role, "admin")
  ))
}

mcp_search_geneset_resources <- function(api_key, source = NULL, species = NULL, collection = NULL,
                                          subcollection = NULL, current_only = TRUE, limit = 50) {
  require_api_key(api_key)
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  mcp_json(search_geneset_resources(
    conn,
    source = source,
    species = species,
    collection = collection,
    subcollection = subcollection,
    current_only = current_only,
    limit = limit
  ))
}

mcp_search_geneset_entries <- function(api_key, geneset_resource_id = NULL, source = NULL,
                                        species = NULL, collection = NULL, subcollection = NULL,
                                        keyword = NULL, limit = 20) {
  require_api_key(api_key)
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  mcp_json(search_geneset_entries(
    conn,
    geneset_resource_id = geneset_resource_id,
    source = source,
    species = species,
    collection = collection,
    subcollection = subcollection,
    keyword = keyword,
    limit = limit
  ))
}

mcp_search_features <- function(api_key, assay_type, feature_name = NULL, organism = NULL,
                                 feature_database = NULL, limit = 50) {
  require_api_key(api_key)
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  mcp_json(search_features(
    conn,
    assay_type = assay_type,
    feature_name = feature_name,
    organism = organism,
    feature_database = feature_database,
    limit = limit
  ))
}

mcp_run_enrichment <- function(api_key, signature_hashkey, geneset_resource_id = NULL,
                                msigdb_species = NULL, msigdb_collection = NULL, msigdb_subcollection = NULL,
                                method = "hypergeo", background = NULL, fdr = 0.05,
                                split_by_group = FALSE, split_by_direction = FALSE, limit = 20) {
  auth <- require_api_key(api_key)

  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = FALSE,
    auth = auth
  )
  if (is.null(context)) {
    stop(base::sprintf(
      "No signature found for signature_hashkey = '%s', or you do not have access to it.",
      signature_hashkey
    ))
  }

  mcp_json(run_enrichment(
    signature_id = context$signature$signature_id,
    geneset_resource_id = geneset_resource_id,
    msigdb_species = msigdb_species,
    msigdb_collection = msigdb_collection,
    msigdb_subcollection = msigdb_subcollection,
    method = method,
    background = background,
    fdr = fdr,
    split_by_group = split_by_group,
    split_by_direction = split_by_direction,
    limit = limit
  ))
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
    ),
    ellmer::tool(
      mcp_search_collections,
      "Search signature collections by name, owning user, and/or a free-text keyword (matched against collection name and description). Use search_signatures with signature_hashkey values found here to see which signatures a collection groups together.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        collection_name = ellmer::type_string("Exact collection name", required = FALSE),
        user_name = ellmer::type_string("Owning user's account name", required = FALSE),
        keyword = ellmer::type_string("Free-text match against collection name and description", required = FALSE),
        limit = ellmer::type_integer("Maximum number of results (default 20, max 100)", required = FALSE)
      ),
      name = "search_collections"
    ),
    ellmer::tool(
      mcp_search_geneset_resources,
      "Browse the gene-set catalog (e.g. MSigDB) by source, species, collection, and/or subcollection. Call this before search_geneset_entries to see which resources exist, or before running enrichment to pick a valid collection/subcollection.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        source = ellmer::type_string("Gene-set source, e.g. 'MSigDB'", required = FALSE),
        species = ellmer::type_string("Species name", required = FALSE),
        collection = ellmer::type_string("Gene-set collection, e.g. 'H' or 'C2'", required = FALSE),
        subcollection = ellmer::type_string("Gene-set subcollection", required = FALSE),
        current_only = ellmer::type_boolean("Only return the current version of each resource (default TRUE)", required = FALSE),
        limit = ellmer::type_integer("Maximum number of results (default 50, max 200)", required = FALSE)
      ),
      name = "search_geneset_resources"
    ),
    ellmer::tool(
      mcp_search_geneset_entries,
      "Search individual gene sets (e.g. 'HALLMARK_APOPTOSIS') within the gene-set catalog, by resource, source/species/collection/subcollection, and/or a free-text keyword.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        geneset_resource_id = ellmer::type_integer("Restrict to one geneset_resource_id from search_geneset_resources", required = FALSE),
        source = ellmer::type_string("Gene-set source, e.g. 'MSigDB'", required = FALSE),
        species = ellmer::type_string("Species name", required = FALSE),
        collection = ellmer::type_string("Gene-set collection, e.g. 'H' or 'C2'", required = FALSE),
        subcollection = ellmer::type_string("Gene-set subcollection", required = FALSE),
        keyword = ellmer::type_string("Free-text match against gene-set name and description", required = FALSE),
        limit = ellmer::type_integer("Maximum number of results (default 20, max 100)", required = FALSE)
      ),
      name = "search_geneset_entries"
    ),
    ellmer::tool(
      mcp_search_features,
      "Search the feature reference tables (genes, proteins, metabolites, or genetic variants) by name and/or organism, to find which biological features SigRepo knows about independent of any one signature. For metabolomics, feature_database selects which identifier namespace feature_name is matched against.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        assay_type = ellmer::type_enum(
          c("transcriptomics", "proteomics", "metabolomics", "snps"),
          "Which feature reference table to search"
        ),
        feature_name = ellmer::type_string("Exact feature name/identifier to look up", required = FALSE),
        organism = ellmer::type_string("Exact organism name, e.g. 'Homo sapiens' (ignored for metabolomics)", required = FALSE),
        feature_database = ellmer::type_enum(
          c("refmet_id", "refmet", "hmdb", "smiles", "inchikey"),
          "Identifier namespace to search against -- required when assay_type is 'metabolomics', ignored otherwise",
          required = FALSE
        ),
        limit = ellmer::type_integer("Maximum number of results (default 50, max 200)", required = FALSE)
      ),
      name = "search_features"
    ),
    ellmer::tool(
      mcp_run_enrichment,
      "Run gene-set enrichment (hypeR) against one SigRepo signature. Provide exactly one of: geneset_resource_id (from search_geneset_resources -- an already-registered collection), or msigdb_collection (+ optional msigdb_species/msigdb_subcollection) to use one by name instead -- if that collection isn't registered yet, it's fetched from MSigDB and added to the shared catalog automatically on this call (a one-time cost; every later call for the same collection, from anyone, hits the cache). Returns the top results ranked by FDR.",
      arguments = list(
        api_key = ellmer::type_string("SigRepo API key"),
        signature_hashkey = ellmer::type_string("The signature's hashkey to test for enrichment"),
        geneset_resource_id = ellmer::type_integer("A geneset_resource_id from search_geneset_resources (already registered)", required = FALSE),
        msigdb_species = ellmer::type_string("Species, e.g. 'Homo sapiens' (default). Only used with msigdb_collection.", required = FALSE),
        msigdb_collection = ellmer::type_string("MSigDB collection by name, e.g. 'H' or 'C2' -- registered on demand if not already cached. Ignored if geneset_resource_id is supplied.", required = FALSE),
        msigdb_subcollection = ellmer::type_string("Optional MSigDB subcollection, e.g. 'CP:KEGG_LEGACY'", required = FALSE),
        method = ellmer::type_enum(c("hypergeo", "kstest", "gsea"), "Enrichment test to run (default hypergeo)", required = FALSE),
        background = ellmer::type_integer("Background gene universe size passed to hypeR", required = FALSE),
        fdr = ellmer::type_number("FDR threshold passed to hypeR (default 0.05)", required = FALSE),
        split_by_group = ellmer::type_boolean("Run enrichment separately per signature group_label (default FALSE)", required = FALSE),
        split_by_direction = ellmer::type_boolean("For method=hypergeo, split each group into up/down feature sets by score sign (default FALSE)", required = FALSE),
        limit = ellmer::type_integer("Maximum ranked results returned per query (default 20, max 100)", required = FALSE)
      ),
      name = "run_enrichment"
    )
  )
}
