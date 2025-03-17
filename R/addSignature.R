#' @title addSignature
#' @description Add signature to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param omic_signature An R6 class object from OmicSignature package
#' @param return_signature_id a logical value indicates whether or not to return
#' the ID of the uploaded signature. Default is \code{FALSE}.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}. 
#' @export
addSignature <- function(
    conn_handler,
    omic_signature,
    return_signature_id = FALSE,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]  

  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Create signature metadata table ####
  metadata_tbl <- SigRepo::createSignatureMetadata(
    conn_handler = conn_handler, 
    omic_signature = omic_signature
  )
  
  # Reset the options message
  SigRepo::print_messages(verbose = verbose)
  
  # Add additional variables in signature metadata table ####
  metadata_tbl <- metadata_tbl %>% dplyr::mutate(user_name = user_name)
  
  # Create a hash key to look up whether signature is already existed in the database ####
  metadata_tbl <- SigRepo::createHashKey(
    table = metadata_tbl,
    hash_var = "signature_hashkey",
    hash_columns = c("signature_name", "user_name"),
    hash_method = "md5"
  )

  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    return_var = "*", 
    filter_coln_var = "signature_hashkey",
    filter_coln_val = list("signature_hashkey" = metadata_tbl$signature_hashkey[1]),
    check_db_table = TRUE
  ) 
  
  # If the signature exists, throw an error message ####
  if(nrow(signature_tbl) > 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Show message
    SigRepo::verbose(
      base::sprintf("\tYou already uploaded a signature with the name = '%s' to the SigRepo Database.\n", signature_tbl$signature_name[1]),
      base::sprintf("\tID of the uploaded signature: %s\n", signature_tbl$signature_id[1])
    )
    
    # Return signature id
    if(return_signature_id == TRUE){
      return(signature_tbl$signature_id[1])
    }
    
  }else{
    
    # 1. Uploading signature metadata into database
    SigRepo::verbose("Uploading signature metadata into the database...\n")
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn, 
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = c("signature_id", "date_created"),
      check_db_table = FALSE
    )
    
    # If signature has difexp, save a copy with its signature hash key ####
    # This action must be performed before a signature is imported into the database.
    # This helps to make sure data is stored properly if there are interruptions in-between.
    if(metadata_tbl$has_difexp == TRUE){
      # 1.1 Saving signature difexp into database
      SigRepo::verbose("Saving difexp to the database...\n")
      # Extract difexp from omic_signature ####
      difexp <- omic_signature$difexp
      # Save difexp to local storage ####
      data_path <- base::system.file("inst/data/difexp", package = "SigRepo")
      if(!base::dir.exists(data_path)){
        base::dir.create(path = base::file.path(base::system.file("inst", package = "SigRepo"), "data/difexp"), showWarnings = FALSE, recursive = TRUE, mode = "0777")
      }
      base::saveRDS(difexp, file = base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
      # Get API URL
      api_url <- base::sprintf("http://%s:%s/store_difexp?api_key=%s&signature_hashkey=%s", conn_handler$host[1], conn_handler$api_port[1], conn_info$api_key[1], metadata_tbl$signature_hashkey[1])
      # Store difexp in database
      res <- 
        httr::POST(
          url = api_url,
          body = list(
            difexp = httr::upload_file(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")), "application/rds")
          )
        )
      # Check status code
      if(res$status_code != 200){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))
        # Show message
        base::stop(
          base::sprintf("\tAPI Link: %s\n", api_url),
          base::sprintf("\tSomething went wrong with API. Cannot upload the difexp table to SigRepo Database. Please contact admin for more details.\n")
        )
      }else{
        # Remove files from file system 
        base::unlink(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
      }
    }
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      table = metadata_tbl,
      check_db_table = FALSE
    ) 
    
    # Look up signature id for the next step ####
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      return_var = "*", 
      filter_coln_var = "signature_hashkey",
      filter_coln_val = list("signature_hashkey" = metadata_tbl$signature_hashkey[1]),
      check_db_table = FALSE
    ) 
    
    # 2. Adding user to signature access table after signature
    # was imported successfully in step (1)
    SigRepo::verbose("Adding user to the signature access table of the database...\n")
    
    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      SigRepo::addUserToSignature(
        conn_handler = conn_handler,
        signature_id = signature_tbl$signature_id[1],
        user_name = user_name,
        access_type = "owner",
        verbose = verbose
      )
    }, error = function(e){
      # Delete signature
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    }) 
    
    # 3. Importing signature set into database after signature
    # was imported successfully in step (1)
    SigRepo::verbose("Adding signature feature set to the database...\n")
    
    # Get the signature assay type
    assay_type <- signature_tbl$assay_type[1]
    
    # Add signature set to database based on its assay type
    if(assay_type == "transcriptomics"){
      
      # If there is a error during the process, remove the signature and output the message
      base::tryCatch({
        SigRepo::addTranscriptomicsSignatureSet(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id[1],
          organism_id = signature_tbl$organism_id[1],
          signature_set = omic_signature$signature,
          verbose = verbose
        )
      }, error = function(e){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id[1], verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(e, "\n")
      }) 
      
    }else if(assay_type == "proteomics"){
      
    }else if(assay_type == "metabolomics"){
      
    }else if(assay_type == "methylomics"){
      
    }else if(assay_type == "genetic_variations"){
      
    }else if(assay_type == "dna_binding_sites"){
      
    }
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))    
    
    # Return message
    SigRepo::verbose("Finished uploading.\n")
    SigRepo::verbose(base::sprintf("ID of the uploaded signature: %s\n", signature_tbl$signature_id[1]))
    
    # Return signature id
    if(return_signature_id == TRUE){
      return(signature_tbl$signature_id[1])
    }
    
  } 
}  



