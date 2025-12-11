# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all("/home/snu3/Connect/SigRepo")

# Loading OmicSignature package
devtools::load_all("/home/snu3/Connect/OmicSignature")

conn_handler <- SigRepo::newConnHandler(
  dbname = base::Sys.getenv("DB_NAME"),
  host = base::Sys.getenv("DB_HOST"),
  port = base::as.integer(Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"),
  password = base::Sys.getenv("DB_PASSWORD"),
  api_host = base::Sys.getenv("API_HOST"),
  api_port = base::Sys.getenv("API_PORT")
)

transcriptomics_tbl <- SigRepo::searchTranscriptomicsFeatureSet(conn_handler = conn_handler)

proteomics_tbl <- SigRepo::searchProteomicsFeatureSet(conn_handler = conn_handler)

organism_tbl <- SigRepo::searchOrganism(conn_handler = conn_handler)


SigRepo::updateTranscriptomicsFeatureSet(
  conn_handler = conn_handler,
  organism = "homo sapiens"
)

SigRepo::updateProteomicsFeatureSet(
  conn_handler = conn_handler,
  organism = "homo sapiens"
)

feature_tbl_colnames <- c("ensembl_gene_id", 'hgnc_symbol')
feature_tbl <- base::matrix(base::vector(), nrow = 0, ncol = base::length(feature_tbl_colnames), byrow = TRUE, dimnames = base::list(c(), feature_tbl_colnames)) %>% 
  base::as.data.frame() |> 
  dplyr::transmute(
    feature_name = base::trimws(base::tolower(.data$ensembl_gene_id)),
    gene_symbol = base::trimws(base::tolower(.data$hgnc_symbol)),
    new_feature_name = base::trimws(.data$ensembl_gene_id),
    new_gene_symbol = base::trimws(.data$hgnc_symbol),
    new_organism_id = 1,
    new_is_current = 1,
    new_version = 115,
  ) |> 
  dplyr::distinct(.data$feature_name, .keep_all = TRUE) |> 
  dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })


transcriptomics_tbl <- SigRepo::lookup_table_sql(
  conn = conn, 
  db_table_name = "transcriptomics_features", 
  return_var = "*", 
  filter_coln_var = "organism_id", 
  filter_coln_val = base::list("organism_id" = 333),
  check_db_table = TRUE
) |> 
  dplyr::transmute(
    feature_name = base::trimws(base::tolower(.data$feature_name)),
    gene_symbol = base::trimws(base::tolower(.data$gene_symbol)),
    orig_feature_name = feature_name,
    orig_gene_symbol = gene_symbol,
    organism_id = .data$organism_id,
    is_current = .data$is_current,
    version = .data$version
  ) |>
  dplyr::mutate_all(function(x){ base::replace(x, base::is.na(x), "") })



