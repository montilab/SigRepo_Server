# script to update the references within the database

# using biomart? probably the easiest way


library(biomaRt)


## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = 'sigrepo', 
  host = 'sigrepo.org', 
  port = 3306, 
  user = 'root', 
  password = 'sigrepo'
)

## Establish database connection for sql queries
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = 'sigrepo', 
  host = 'sigrepo.org', 
  port = 3306, 
  user = 'root', 
  password = 'sigrepo'
)

# Transcriptomics Human refs #### 


ensembl <- useEnsembl(biomart = "genes", dataset = 'hsapiens_gene_ensembl')

# grabbing ensembl ids and hgnc symbols 
ensembl_ids_human_latest <- getBM(
  attributes = c("ensembl_gene_id", 'hgnc_symbol'), mart = ensembl)



# Adding organism,version, and is_current cols

ensembl_ids_human_latest <- ensembl_ids_human_latest %>%
  distinct(ensembl_gene_id, .keep_all = TRUE) %>%
  mutate(version = 114,
         is_current = 1,
         organism = "Homo sapiens") %>%
  rename(
    'feature_name' = 'ensembl_gene_id',
    'gene_symbol' = 'hgnc_symbol'
  )


# dropping refs from the our database and importing the current ids

# DBI::dbGetQuery(conn = conn, statement = "
#   DELETE FROM transcriptomics_features
#   WHERE organism_id = 1
# ")

# add in the new data 

SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = ensembl_ids_human_latest)

# Transcriptomics Mouse Refs ####

ensembl <- useEnsembl(biomart = "genes", dataset = "mmusculus_gene_ensembl")

ensembl_ids_mouse_latest <- getBM(
  attributes = c("ensembl_gene_id", "hgnc_symbol"), mart = ensembl)

ensembl_ids_mouse_latest <- ensembl_ids_mouse_latest %>%
  distinct(ensembl_gene_id, .keep_all = TRUE) %>%
  mutate(version = 114,
         is_current = 1,
         organism = "Mus musculus") %>%
  rename(
    'feature_name' = "ensembl_gene_id",
    'gene_symbol' = 'hgnc_symbol'
  )

# # dropping reference data in our database 
# DBI::dbGetQuery(conn = conn, statement = "
#                DELETE FROM transcriptomics_features
#                WHERE organism_id = 2
#                ")

# add in the new data

SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = ensembl_ids_mouse_latest)


# Proteomics Human Refs ####

#use SigRepo::protTransform to grab organism uniprot ids into SigRepo database dictionary


human_prot_latest <- SigRepo::protTransform(
  organism_code = "HUMAN",
  tax_id = "9606",
)


# dropping reference data in our database 