## General functions used by search page

#' General function for generating html output showing selected search terms
#'
#' @param searchID the dropdown menu id whose selected values should be
#'   displayed
#' @param displayName the word or phrase to display in the html output
#'   representing the dropdown menu field
selectedHTML <- function(searchID, displayName) {
    ## Display "no <displayName> selected" if there is no input to this dropdown
    if (length(input[[searchID]]) < 1) {
        paste0("no ", displayName, " selected</br>")
    } else {
        ## Otherwise display "<displayName>: <value1>, <value2>, ..."
        paste0(displayName,
            ": <b>",
            paste(input[[searchID]], collapse=", "),
            "</b></br>")
    }
}

#' General function for getting the values of a field using sqlFindingQuery()
#' 
#' @param field the name of the search field whose dropdown menu should be
#'   updated
#' @param dbTable the database table to query
#' @param ins list of all of the possible in clauses for sqlFindingQuery()
#' @param betweens list of all of the possible between clauses for
#'   sqlFindingQuery()
getFieldValues <- function(field, dbTable, ins, betweens) {
    ## Query the database
    sqlObj <-
        sqlFindingQuery(
            fields=field,
            dbTable=dbTable,
            ins=ins[names(ins)!=field],
            betweens=betweens[names(betweens)!=field]
        )
    ## Return the list of possible values for the given field
    return(sqlObj[[field]])
}

#' General function for updating a dropdown menu
#'
#' @param dropdown the id of the dropdown menu to update
#' @param field the name of the search field whose dropdown menu should be
#'   updated
#' @param dbTable the database table to query
#' @param ins list of all of the possible in clauses for sqlFindingQuery()
#' @param betweens list of all of the possible between clauses for
#'   sqlFindingQuery()
updateDropdown <- function(dropdown, field, dbTable, ins, betweens) {
    ## Query the database to find values of field that match selected values of
    ##   other fields
    fieldValues <- getFieldValues(field, dbTable, ins, betweens)
    ## Update dropdown menu
    updateSelectizeInput(session,
        dropdown,
        choices=sort(c(fieldValues, input[[dropdown]])),
        selected=input[[dropdown]])
}

#' General function for clearing selections of a dropdown menu
#'
#' @param dropdown the id of the dropdown menu to update
#' @param field the name of the search field whose dropdown menu should be
#'   cleared
#' @param dbTable the database table to query
clearDropdown <- function(dropdown, field, dbTable) {
    ## Query the database to find all values of field
    fieldValues <- getFieldValues(field, dbTable, NULL, NULL)
    ## Update dropdown menu
    updateSelectizeInput(session,
        dropdown,
        choices=c(fieldValues),
        selected=NULL)
}

#' General function for updating the values of a field in a list of in clauses
#'   based on the intersection of two lists of possible values for that field
#'   
#' @param field the field to update in the list of in clauses
#' @param dbTable the database table to query to get a list of possible values
#'   for field
#' @param queryIns the list of in clauses to use in the sql query to get a list
#'   of possible values for field
#' @param insList the list of in clauses containing the original list of
#'   possible values for field
getIntersection <- function(field, dbTable, queryIns, insList) {
    ## Get the list of field values corresponding to query_ins
    correspondingValues <-
        getFieldValues(
            field,
            dbTable,
            ins = queryIns,
            betweens = NULL
        )
    ## Get the list of selected field values
    selectedValues <- insList[[field]]
    ## Get the intersection of the two lists of field values
    intersectValues <-
        intersect(correspondingValues, selectedValues)
    if (length(intersectValues) < 1 & length(selectedValues) >= 1) {
        ## If the intersection is empty and field values are selected
        ##   then set the field value to an empty string for sqlFindingQuery()
        insList[[field]] <- ""
    } else if (length(intersectValues) < 1 & length(selectedValues) < 1) {
        ## If the intersection is empty and no field values are selected
        ##   then set the field value to the values corresponding to the
        ##   selected ins_query for sqlFindingQuery()
        insList[[field]] <- correspondingValues
    } else {
        ## Otherwise set field values to the intersection for sqlFindingQuery()
        insList[[field]] <- intersectValues
    }
    return(insList)
}