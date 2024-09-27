## Downloaded and adapted from 
## https://www.metabolomicsworkbench.org/databases/refmet/refmet_convert.zip

library(data.table)
library(curl)
library(stringi)

refmet_convert <- function( 
    DF # data.frame w/ 1st column containing metabolite names
)
{
  ## check input
  stopifnot( methods::is(DF,"data.frame") )
  
  ## Note: DF[,1] can be any data frame column containing metabolite names
  mets <- stringi::stri_join_list(list(DF |> dplyr::slice(1) |> dplyr::pull(1)), sep = "\n")
  h <- curl::new_handle()
  curl::handle_setform(h, metabolite_name = mets)

  ## run the RefMet request on the Metabolomics Workbench server
  req <- curl::curl_fetch_memory(
    "https://www.metabolomicsworkbench.org/databases/refmet/name_to_refmet_new_min.php", 
    handle = h
  )
  
  ## Parse the output
  x <- rawToChar(req$content)
  y <- strsplit(x, "\n")
  refmet <- data.frame(ncol = 7)

  for (i in 1:length(y[[1]])) {
    if (nchar(y[[1]][i]) > 1) {
      z <- strsplit(y[[1]][i], "\t")
      for (j in 1:length(z[[1]])) {
        refmet[i, j] <- z[[1]][j]
      }
    }
  }
  
  refmet <- refmet[rowSums(is.na(refmet)) != ncol(refmet), , drop = FALSE ]
  colnames(refmet) <- refmet[1, ]
  refmet <- refmet[-c(1), ]
  refmet[is.na(refmet)] <- ""

  return(
    refmet |> 
      dplyr::rename_with(~ trimws(.x)) |>
      dplyr::rename_with(~ tolower(stringr::str_replace(.x,"[ ]+","_")), everything()) |>
      dplyr::rename(metabolite = "input_name", refmet_name = "standardized_name") |>
      dplyr::mutate(refmet_name = ifelse(refmet_name == "-", NA, refmet_name)) |>
      dplyr::mutate(across(-c(metabolite, refmet_name), ~ifelse(.x=="", NA, .x)))
  )
  
}

if (FALSE) {
  ## simple example
  DF <- data.frame(metabolite = c(
    "deoxycholate",
    "dummy",
    "lithocholate",
    "lithocholate sulfate (1)",
    "glycochenodeoxycholate",
    "glycodeoxycholate")
  )
  refmet_convert(DF)
}
  