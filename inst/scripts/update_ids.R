library(httr)
library(jsonlite)
library(xml2)
library(SigRepo)
# example code




# Query the database and grab all of the signature names (ids)

current_ids <- SigRepo::lookup_table_sql(
  conn = conn,
  db_table_name = 'signatures',
  return_var =  '*',
  filter_coln_var = NULL,
  filter_coln_val = NULL,
  filter_var_by = NULL,
  check_db_table = TRUE
)

# using the ensembl rest API to find the
server <- "https://rest.ensembl.org"
ext <- "/archive/id"
r <- POST(paste(server, ext, sep = ""), content_type("application/json"), accept("application/json"), body = '{ "id" : ["ENSG00000157764", "ENSG00000248378"] }')

stop_for_status(r)

# use this if you get a simple nested list back, otherwise inspect its structure
ids <- data.frame(t(sapply(content(r),c)))
head(fromJSON(toJSON(content(r))))



