#' @title phenotypeCheckOrAdd
#' @description checks if phenotype exists in database already and, if not,
#' inserts into phenotypes table of database
#'
#' @param phenotype character vector
#' @param thisHandle connection handle to use
#' @param verbose Boolean: whether to show more detailed outputs
#' @export
phenotypeCheckOrAdd <-
  function(phenotype=NULL,
           thisHandle=newConnHandle(),
           verbose=T) {
    # tracking upload date for DB insert
    # tracking and adding phenotype if not present in db currently
    phenotypeIdInsert <- NA
    if (!is.null(phenotype) && typeof(phenotype) == "character") {
      # check if the phenotype exists already in DB
      phenotypeIdInsert <- sqlFindingQuery(
        "phenotypes",
        c("phenotype_id"),
        ins=list("phenotype"=phenotype))$phenotype_id[1]
      if (is.na(phenotypeIdInsert)) {
        if (verbose == T) {
          print(paste(
            "This phenotype [",
            phenotype,
            "] does not exist in our DB. adding now."
          ))
        }
        insertPhenotypeQuery <-
          paste("INSERT INTO phenotypes(phenotype) values(",
                singleQuote(phenotype),
                ");")
        dbSendQuery(thisHandle, insertPhenotypeQuery)
        # dbDisconnect(insertPhenotypeConn)
        # now that the phenotype is in the database, need its ID
        phenotypeIdInsert <- sqlFindingQuery(
          "phenotypes",
          c("phenotype_id"),
          ins=list("phenotype"=phenotype))$phenotype_id[1]
      }
    }
    else if (!is.null(phenotype) &&
             typeof(phenotype) != "character") {
      if (verbose == T) {
        print("phenotype parameter must be a string, if not null")
      }
      return(F)
    }
    return(phenotypeIdInsert)
  }
