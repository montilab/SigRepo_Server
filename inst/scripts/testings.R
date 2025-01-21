# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)
load_all()

## Establish database connection
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)





# Get data path
data_path <- system.file("data", package="SigRepo")


SigRepo::searchSignature(
  conn_handler,
  user_name = "all",
  signature_name = "_Aging_Gene_2023",
  organism = NULL,
  phenotype = NULL,
  sample_type = NULL
)



# Searching signature 

SigRepo::searchSignature(conn_init, signature_name = '_Aging_Gene_2023')

# 7. Add signatures ####
LLFS_Transcriptomic_AGS_OmS <- base::readRDS(file.path(data_path, "signatures/LLFS_Transcriptomic_AGS_OmS.rds"))
SigRepo::addSignature(conn = conn, omic_signature = LLFS_Transcriptomic_AGS_OmS)

# Check the signatures table ####
statement <- "select * FROM signatures"
signature_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 8. Update signatures ####
SigRepo::updateSignature(conn = conn, signature_id = signature_db_tbl$signature_id, omic_signature = LLFS_Transcriptomic_AGS_OmS)

# 9. delete signatures ####
SigRepo::deleteSignature(conn = conn, signature_id = signature_db_tbl$signature_id)


# Check the signatures table ####
statement <- "show databases;"
tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))


DBI::dbGetQuery(conn = conn, statement = "DROP USER 'andrewdr'@'montilab.bu.edu';")
DBI::dbGetQuery(conn = conn, statement = "DROP USER 'lkroeh'@'montilab.bu.edu';")
DBI::dbGetQuery(conn = conn, statement = "DROP USER 'smonti'@'montilab.bu.edu';")
DBI::dbGetQuery(conn = conn, statement = "DROP USER 'rchau88'@'montilab.bu.edu';")
DBI::dbGetQuery(conn = conn, statement = "DROP USER 'vmli'@'montilab.bu.edu';")
DBI::dbGetQuery(conn = conn, statement = "DROP USER 'zihuang'@'montilab.bu.edu';")


DBI::dbGetQuery(conn = conn, statement = "CREATE USER 'guest'@'%' IDENTIFIED WITH mysql_native_password BY 'guest';")
DBI::dbGetQuery(conn = conn, statement = "GRANT SELECT ON *.* TO 'guest'@'%';")
DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;")


DBI::dbGetQuery(conn = conn, statement = "SHOW GRANTS FOR 'root'@'%';") %>% 
  dplyr::slice(1) %>% 
  purrr::flatten_chr() %>% 
  gsub("GRANT(.*)ON(.*)TO(.*)", "\\1", ., perl = TRUE) %>% 
  stringr::str_split(., ",") %>% 
  purrr::flatten_chr() %>% 
  trimws()

#CREATE USER 'guest'@'%' IDENTIFIED WITH mysql_native_password BY 'guest';
#CREATE USER 'guest'@'%' IDENTIFIED WITH mysql_native_password BY 'guest';
#ALTER USER 'guest'@'%' IDENTIFIED WITH mysql_native_password BY 'guest';
#ALTER USER 'root'@'%' IDENTIFIED WITH mysql_native_password BY 'root';


############# 
#
#  USERS ####
#
############# 

# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load OmicSignature package
devtools::load_all("/home/rstudio/SigRepoR")

## Establish database connection
conn <- SigRepo::newConnHandler(
  driver = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# Set foreign key checks to false when dropping tables
DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;")

table_name <- "users"
drop_table_sql <- sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
DBI::dbGetQuery(conn = conn, statement = drop_table_sql)

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `user_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `user_name` VARCHAR(255) NOT NULL,
  `user_password_hashkey` VARCHAR(255) NOT NULL,
  `user_email` VARCHAR(255) NOT NULL,
  `user_first` VARCHAR(255) DEFAULT NULL,
  `user_last` VARCHAR(255) DEFAULT NULL,
  `user_affiliation` TEXT DEFAULT NULL,
  `user_role` SET("admin", "user", "guest") NOT NULL,
  PRIMARY KEY (`user_id`),
  CHECK (`user_email` REGEXP "^[a-zA-Z0-9][+a-zA-Z0-9._-]*@[a-zA-Z0-9][a-zA-Z0-9._-]*[a-zA-Z0-9]*\\.[a-zA-Z]{2,4}$")
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

# Create a hash for looking up multiple columns quickly
hash_var <- "user_hashkey"
hash_colns <- paste0("`", c("user_name", "user_email", "user_role"), "`" ) %>% paste0(., collapse = ", ")

create_hash_sql <- sprintf(
'
ALTER TABLE %s ADD COLUMN %s VARCHAR(32) GENERATED ALWAYS AS (
  UNHEX(MD5(
    CONCAT_WS("|", %s)
  ))
)
', table_name, hash_var, hash_colns)

suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_hash_sql))

# Create an index for the hash variable to prevent duplicates from entering the table
create_hash_index_sql <- sprintf(
'
ALTER table %s ADD UNIQUE INDEX (%s);
', table_name, hash_var)

DBI::dbGetQuery(conn = conn, statement = create_hash_index_sql)

# Check the imported values
statement <- "select * FROM users"
user_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))



## Create an user df
table <- data.frame(
  user_name = c("rchau88", "smonti", "vmli", "lkroeh", "andrewdr", "zihuang"), 
  user_password = "password",
  user_email = c("rchau88@bu.edu", "smonti@bu.edu", "vmli@bu.edu", "lkroeh@bu.edu", "andrewdr@bu.edu", "zihuang@bu.edu"), 
  user_first = c("Reina", "Stefano", "Vanessa", "Lina", "Andrew", "Ziwei"), 
  user_last = c("Chau", "Monti", "Li", "Kroehling", "Chen", "Huang"), 
  user_affiliation = "Boston University",
  user_role = "admin",
  stringsAsFactors = FALSE
) %>% 
  dplyr::mutate(
    user_password = sodium::password_store(as.character(user_password))
  )

# Get SQL statement
statement <- SigRepo::insert_table_sql(
  conn = conn, 
  database = database, 
  db_table_name = db_table_name, 
  table = table
)

# Insert table into database
tryCatch({
  DBI::dbGetQuery(conn = conn, statement = statement)
}, error = function(e){
  stop(e, "\n")
}, warning = function(w){
  message(w, "\n")
})

# Check the imported values
statement <- "select * FROM users"
user_db_tbl <- DBI::dbGetQuery(conn = conn, statement = statement)

statement <- sprintf("SELECT * FROM %s LIMIT 1", "users")
db_table <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# GRANT USER PERMISSION TO DATABASE
# purrr::walk(
#   seq_len(nrow(table)),
#   function(u){
#     #u=1;      
#     check_user_tbl <- DBI::dbGetQuery(conn = conn, statement = sprintf("SELECT host, user FROM mysql. user WHERE user IN ('%s') AND host IN ('168.122.76.140');", table$user_id[u]))
#     if(nrow(check_user_tbl) == 0 && table$user_role[u] == "admin"){
#       DBI::dbGetQuery(conn = conn, statement = sprintf("CREATE USER '%s'@'168.122.76.140' IDENTIFIED BY '%s';", table$user_id[u], table$user_password[u]))
#       DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT ALL PRIVILEGES ON * . * TO '%s'@'168.122.76.140';", table$user_id[u]))
#       DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;")
#     }else if(nrow(check_user_tbl) == 0 && table$user_role[u] == "user"){
#       DBI::dbGetQuery(conn = conn, statement = sprintf("CREATE USER '%s'@'168.122.76.140' IDENTIFIED BY '%s';", table$user_id[u], table$user_password[u]))
#       DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT ON * . * TO '%s'@'168.122.76.140';", table$user_id[u]))
#       DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;")        
#     }
#   }
# )

SigRepo::addUser(conn = conn, user_tbl = user_tbl)





database = "sigrepo"
db_table_name = "users"

# Get table primary key
primary_key <- suppressWarnings(
  DBI::dbGetQuery(
    conn = conn,
    statement = sprintf(
      "
      SHOW INDEX
      FROM %s.%s
      WHERE Key_name != 'PRIMARY'
      ", database, db_table_name
    )
  )
) %>% 
  dplyr::select("Column_name") %>% 
  purrr::flatten_chr()



## Create an user df
table <- data.frame(
  user_id = c("rchau88", "smonti", "vmli", "lkroeh", "andrewdr", "zihuang"), 
  user_password = "password",
  user_email = c("rchau88@bu.edu", "smonti@bu.edu", "vmli@bu.edu", "lkroeh@bu.edu", "andrewdr@bu.edu", "zihuang@bu.edu"), 
  user_first = c("Reina", "Stefano", "Vanessa", "Lina", "Andrew", "Ziwei"), 
  user_last = c("Chau", "Monti", "Li", "Kroehling", "Chen", "Huang"), 
  user_affiliation = "Boston University",
  user_role = "admin",
  stringsAsFactors = FALSE
) %>% 
  dplyr::mutate(
    user_password = sodium::password_store(as.character(user_password))
  )

coln_var <- "user_id"
coln_var_id <- "user_password"

return_var <- c(coln_var_id, coln_var)
filter_coln_var <- coln_var
filter_coln_val <- table %>% dplyr::distinct(!!!syms(coln_var)) %>% as.list()

statement <- SigRepo::lookup_table_sql(
  db_table_name = db_table_name, 
  return_var = return_var, 
  filter_coln_var = filter_coln_var, 
  filter_coln_val = filter_coln_val
)

existing_tbl <- tryCatch({
  suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
}, error = function(e){
  stop(e, "\n")
}, warning = function(w){
  message(w, "\n")
})


existing_tbl <- tryCatch({
  suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
}, error = function(e){
  stop(e, "\n")
}, warning = function(w){
  message(w, "\n")
})

organism_tbl <- data.frame(
  organism = c(
    "Homo sapiens",
    "Mus musculus",
    "Rattus norvegicus",
    "Danio rerio",
    "Heterocephalus glaber"
  )
)

SigRepo::addOrganism(conn=conn, organism_tbl = organism_tbl)

# Check the imported values
statement <- "select * FROM organisms"
organism_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))


table <- data.frame(
  organism = c(
    "homo sapiens",
    "mus musculus",
    "Arabidopsis thaliana",
    "Drosophila melanogaster"
  )
)

SigRepo::addOrganism(conn=conn, organism_tbl = table)





