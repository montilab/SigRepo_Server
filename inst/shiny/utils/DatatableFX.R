# utils/DatatableFX.R 

library(DT)

DatatableFX <- function(df, 
                        hidden_columns = c(0,6,7,8,11,14,15,16,19,24,25,26),
                        scrollY = "500px",
                        paging  = FALSE,
                        row_selection = "single"
                        ) {
  datatable(
    df,
    extensions = "Buttons",
    filter = "top",
    options = list(
      pageLength = -1,
      scrollY = scrollY,
      paging = paging,
      scrollX = TRUE,
      ordering = FALSE,
      fixedHeader = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      columnDefs = list(
        list(targets = hidden_columns, visible = FALSE)
      )
    ),
    class = "compact stripe hover nowrap",
    selection = row_selection,
    rownames = FALSE
  )
}

  