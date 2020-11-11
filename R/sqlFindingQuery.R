#' @title sqlFindingQuery
#' @description Constructs sql query based on where clause, if one is needed,
#'   and executes final query as output#'
#' @importFrom plyr compact
#' @param fields character vector, the fields to select from the target table
#' @param jsonFields named list. name is the field with the json. values are the
#'   names of the keys in that field you're trying to get values of.
#' @param dbTable string; the table to select from
#' @param ins named list where the names are the fields to narrow search by
#'   and the values are vectors of the values to look for in those fields
#' @param betweens named list where the names are the fields to narrow search by
#'   and the values are vectors of length 2 containing the endpoints of the
#'   range to consider in those fields
#' @param collapse_by how to collapse the input queries, default to be "AND"
#' @param orderFields a vector of fields to order the query results by
#' @param distinct a boolean indicating whether to return unique results
#' @param verbose default to be FALSE. set TRUE to print messages
#' @param handle DB connection handle
#' @param disconnectAfter whether disconnect the DB after finding. default to be TRUE
#' @export
sqlFindingQuery <-
  function(dbTable,
           fields = c("*"),
           jsonFields = NULL,
           ins = NULL,
           betweens = NULL,
           orderFields = NULL,
           distinct = TRUE,
           verbose = FALSE,
           collapse_by = " AND ",
           handle = newConnHandle(),
           disconnectAfter = TRUE) {
    ## Query construction
    orderSub <- ""
    if (!is.null(orderFields)) {
      orderSub <- paste("ORDER BY", paste(orderFields, sep = ","))
    }
    if (!is.null(jsonFields)) {
      jf <- paste(sapply(names(jsonFields), function(x) {
        paste0("JSON_VALUE(", x, ", ", singleQuote(paste0("$.", jsonFields[[x]])), ")")
      }), collapse = ", ")
      if (fields != "*") {
        fields <- c(fields, jf)
      }
      else {
        fields <- c(jf)
      }
    }
    sql <- paste0(
      "SELECT ",
      ifelse(distinct, "DISTINCT ", ""),
      paste(fields, collapse = ","),
      " FROM ",
      dbTable
    )
    ## There's a very subtle but important point to be made when dealing with
    ## multiple possible values you want to query the DB with.
    ## Here, I could pass a vector of values, but the query constructed would
    ## asking for everything in one go.
    ## If you lapply instead, using the list of where values, you'll get
    ## separate queries/executions.
    ## lapply approach is advised if you're doing granular checking of values
    ## in a submitted list.
    ## bulk approach technically works as well, but you won't know which
    ## values resulted in zero records from the DB.
    whereClauses <- ""
    ## Removes elements where the value is null
    ins <- compact(ins)
    betweens <- compact(betweens)
    if (!is.null(ins) && length(ins) > 0) {
      ## Assemble the part of each in clause of the form
      ##   "<field> IN (<field_values>)"
      ins <-
        lapply(names(ins), constructInClause, myList = ins)
    }
    if (!is.null(betweens) && length(betweens) > 0) {
      ## Assemble the part of each between clause of the form
      ##   "<field> BETWEEN <value1> AND <value2>"
      betweens <-
        lapply(names(betweens), constructBetweenClause,
          myList = betweens
        )
    }
    if (!is.null(c(ins, betweens)) &&
      length(c(ins, betweens)) > 0) {
      ## Add "WHERE" to the beginning of the clauses and separate
      ##   each clause by " AND "
      whereClauses <-
        paste("WHERE", paste(c(ins, betweens), collapse = collapse_by))
    }
    ## Add where clauses to query
    sql <- paste(sql, whereClauses, orderSub, ";", sep = " ")
    ## Debugging block
    if (verbose == TRUE) {
      print(sql)
    }
    ## Execute
    return(sqlGeneric(sql, handle, disconnectAfter))
  }
