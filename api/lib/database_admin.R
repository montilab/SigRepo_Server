# Schema (re)creation and reference-table seeding, backing /init_db, /reset_db,
# /init_db_schema, /init_db_tables.

reset_db_tables <- function(conn_handler) {

  ## Establish database connection
  conn <- DBI::dbConnect(
    drv = RMySQL::MySQL(),
    dbname = base::Sys.getenv("DB_NAME"),
    host = base::Sys.getenv("DB_LOCAL_HOST"),
    port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"),
    password = base::Sys.getenv("DB_PASSWORD")
  )

  # Set foreign key checks to false when dropping tables
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

  # Show all tables in DB
  table_result <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SHOW TABLES;"))

  ###################
  #
  # DROP ALL TABLES
  #
  ##################
  purrr::walk(
    base::seq_len(base::nrow(table_result)),
    function(t) {
      table_name <- table_result[[1]][t]
      drop_table_sql <- base::sprintf("DROP TABLE IF EXISTS `%s`;", table_name)
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))
    }
  )

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
}

# Function to generate schema for the database ####
generate_db_schema <- function(sigrepo_server_path = base::Sys.getenv("SIGREPO_SERVER_DIR")) {

  ## Establish database connection
  conn <- DBI::dbConnect(
    drv = RMySQL::MySQL(),
    dbname = base::Sys.getenv("DB_NAME"),
    host = base::Sys.getenv("DB_LOCAL_HOST"),
    port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"),
    password = base::Sys.getenv("DB_PASSWORD")
  )

  # Set foreign key checks to false when dropping tables
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

  # Show all tables in DB
  table_result <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SHOW TABLES;"))

  ###################
  #
  # DROP ALL TABLES
  #
  ##################
  purrr::walk(
    base::seq_len(base::nrow(table_result)),
    function(t) {
      table_name <- table_result[[1]][t]
      drop_table_sql <- base::sprintf("DROP TABLE IF EXISTS `%s`;", table_name)
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))
    }
  )

  run_schema_file <- function(relative_path, label) {
    print(base::sprintf("Create schema for '%s' table in the database...", label))
    sql_file <- base::file.path(sigrepo_server_path, relative_path)
    sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  }

  run_schema_file("mysql/schema/collection_access.sql", "collection_access")
  run_schema_file("mysql/schema/collection.sql", "collection")
  run_schema_file("mysql/schema/keywords.sql", "keywords")
  run_schema_file("mysql/schema/geneset_resources.sql", "geneset_resources")
  run_schema_file("mysql/schema/geneset_entries.sql", "geneset_entries")
  run_schema_file("mysql/schema/organisms.sql", "organisms")
  run_schema_file("mysql/schema/phenotypes.sql", "phenotypes")
  run_schema_file("mysql/schema/platforms.sql", "platforms")
  run_schema_file("mysql/schema/proteomics_features.sql", "proteomics_features")
  run_schema_file("mysql/schema/sample_types.sql", "sample_types")
  run_schema_file("mysql/schema/signature_access.sql", "signature_access")
  run_schema_file("mysql/schema/signature_collection_access.sql", "signature_collection_access")
  run_schema_file("mysql/schema/signature_feature_set.sql", "signature_feature_set")
  run_schema_file("mysql/schema/signatures.sql", "signatures")
  run_schema_file("mysql/schema/transcriptomics_features.sql", "transcriptomics_features")
  run_schema_file("mysql/schema/users.sql", "users")
  run_schema_file("mysql/schema/metabolite_reference.sql", "metabolite_reference")
  run_schema_file("mysql/schema/metabolite_xref.sql", "metabolite_xref")
  run_schema_file("mysql/schema/signature_feature_set_ambiguity.sql", "signature_feature_set_ambiguity")
  run_schema_file("mysql/schema/genetic_variants_features.sql", "genetic_variants_features")

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))

  # Print message
  print("Finished creating table schema for the database.")
}

# Function to generate a list of reference tables for the database ####
generate_db_tables <- function(conn_handler, sigrepo_server_path = base::Sys.getenv("SIGREPO_SERVER_DIR")) {

  #############
  #
  #  ORGANISMS ####
  #
  #############
  print("Upload organisms to the database...")
  organism_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/organisms.csv"))
  SigRepo::addOrganism(conn_handler = conn_handler, organism_tbl = organism_tbl)

  #############
  #
  #  PLATFORMS ####
  #
  #############
  print("Upload platforms to the database...")
  platform_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/platforms.csv"))
  SigRepo::addPlatform(conn_handler = conn_handler, platform_tbl = platform_tbl)

  #############
  #
  #  PHENOTYPES ####
  #
  #############
  print("Upload phenotypes to the database...")
  phenotype_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/phenotypes.csv"), header = TRUE)
  SigRepo::addPhenotype(conn_handler = conn_handler, phenotype_tbl = phenotype_tbl)

  #############
  #
  #  SAMPLE TYPES ####
  #
  #############
  print("Upload sample types to the database...")
  sample_type_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/sample_types.csv"), header = TRUE)
  SigRepo::addSampleType(conn_handler = conn_handler, sample_type_tbl = sample_type_tbl)

  #############
  #
  #  TRANSCRIPTOMICS ####
  #
  #############
  print("Upload human transcriptomics features to the database...")
  transcriptomics_human_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Transcriptomics_Homo_Sapiens.csv"), header = TRUE)
  SigRepo::addTranscriptomicsFeatureSet(conn_handler = conn_handler, feature_set = transcriptomics_human_gene_tbl)

  print("Upload mouse transcriptomics features to the database...")
  transcriptomics_mouse_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Transcriptomics_Mus_Musculus.csv"), header = TRUE)
  SigRepo::addTranscriptomicsFeatureSet(conn_handler = conn_handler, feature_set = transcriptomics_mouse_gene_tbl)

  #############
  #
  #  PROTEOMICS ####
  #
  #############
  print("Upload human proteomics features to the database...")
  proteomics_human_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Proteomics_Homo_Sapiens.csv"), header = TRUE)
  SigRepo::addProteomicsFeatureSet(conn_handler = conn_handler, feature_set = proteomics_human_gene_tbl)

  #############
  #
  #  USERS ####
  #
  #############
  print("Upload users to the database...")
  user_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/users.csv"), header = TRUE)
  SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)

  # Print message
  print("Finished uploading tables to the database.")
}
