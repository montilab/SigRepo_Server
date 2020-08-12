#' @title Compare two lists of signatures
#' @description Compare two lists of signatures and return Venn diagram and hypergeometric test p-value for intersect.
#' updated 06/2020
#' @param sig1 a character vector for signature 1
#' @param sig2 a character vector for signature 2
#' @param sig1_name string. the name of signature 1. used for display in Venn diagram.
#' @param sig2_name string. the name of signature 2. used for display in Venn diagram.
#' @param background_number a numeric value indicating the total number of features. For example, in microarray, there are usually 22000 genes on a chip.
#' @return a list of the comparison result of the two signature.
#' including: "sig1_name", "sig2_name", "sig1_symbol", "sig2_symbol",
#' "only_sig1", "only_sig2", "sig_both", "Venn", "hyper_p.value"
#'
#' @examples
#' sig1 <- c("TNMD", "DPM1", "CFH", "FUCA2", "HECW1", "LASP1", "MAD1L1", "KLHL13", "FUCA2")
#' sig2 <- c("TNMD", "FUCA2", "C1orf112", "FGR", "MAD1L1", "NFYA", "STPG1", "CFTR", "LAS1L", "ENPP4", "SEMA3F", "ANKIB1")
#' compare_result <- twoSigCompare(sig1, sig2, sig1_name = "sig1", sig2_name = "sig2", is.lv2 = FALSE, background_number = 22000)
#' compare_result$Venn
#' compare_result$sig_both
twoSigCompare <- function(sig1, sig2, sig1_name = "sig1", sig2_name = "sig2", background_number = 22000) {
  # check: should be dataframe with symbol, (score), direction; if it's lv3, can be character:
  if (!is(sig1, "character") | !is(sig2, "character")) {
    stop("Error: input signatures should be character vectors.")
  }

  # comparison 1: compare symbol (feature) + output Venn diagram
  sig_both <- intersect(sig1, sig2)
  Venn <- hypeR::ggvenn(sig1, sig2, ga = sig1_name, gb = sig2_name, title = paste(sig1_name, sig2_name, sep = " vs "))

  # comparison 2: hypergeometric test of the intercept
  # note: if p-val is very small, it means the the overlap of the two signatures is significantly "not random",
  #       which means the two signatures are similar
  # background: N = m+n (total number of balls in the urn) 22k gene
  # m: (total number of marked balls) number of signature 1
  # n: (total number of unmarked balls) number of (background - signature 1)
  # k: (a random selection of balls) number of signature 2
  # x: (number of marked balls within the selection) number in both signatures
  q <- length(sig_both)
  m <- length(sig1)
  n <- background_number - length(sig1)
  k <- length(sig2)
  hyper_pval <- phyper(q = q, m = m, n = n, k = k, lower.tail = FALSE)

  # output:
  output <- list()
  output[[1]] <- sig1_name # signature name, e.g. MDA_AhR_KO
  output[[2]] <- sig2_name
  output[[3]] <- sig1 # all symbols (features) in sig1
  output[[4]] <- sig2
  output[[5]] <- setdiff(sig1, sig2)
  output[[6]] <- setdiff(sig2, sig1)
  output[[7]] <- sig_both
  output[[8]] <- Venn
  output[[9]] <- hyper_pval
  names(output) <- c(
    "sig1_name", "sig2_name", "sig1_symbol", "sig2_symbol",
    "only_sig1", "only_sig2", "sig_both", "Venn",
    "hyper_p.value"
  )
  # note: the sequence of the output things does not matter, so long as the names specified are corresponded.
  #       because in Shiny, the results are called using the names, e.g. result$only_sig1, not index, e.g. result[[5]].

  return(output)
}

## take care of directions
twoSigCompareServer <- function(sig1_df, sig2_df,
                                sig1_name = "sig1", sig2_name = "sig2", background_number = 22000) {

  ## check column names ##
  sig1_df <- replaceSigCol(sig1_df, ObjtoGeneral = TRUE)
  sig2_df <- replaceSigCol(sig2_df, ObjtoGeneral = TRUE)
  if (!"symbol" %in% colnames(sig1_df) | !"symbol" %in% colnames(sig2_df)) {
    stop("Error: input signature dataframes should have symbol information.")
  }

  ## determine compare type ##
  if (!"direction" %in% colnames(sig1_df) | length(summary(as.factor(sig1_df$direction))) == 1) {
    type1 <- "Uni"
  } else if (length(summary(as.factor(sig1_df$direction))) == 2) {
    type1 <- "Bi"
  } else if (length(summary(as.factor(sig1_df$direction))) > 2) {
    type1 <- "Multi"
  } else {
    stop("Error: signature 1 direction info invalid.")
  }

  if (!"direction" %in% colnames(sig2_df) | length(summary(as.factor(sig2_df$direction))) == 1) {
    type2 <- "Uni"
  } else if (length(summary(as.factor(sig2_df$direction))) == 2) {
    type2 <- "Bi"
  } else if (length(summary(as.factor(sig2_df$direction))) > 2) {
    type2 <- "Multi"
  } else {
    stop("Error: signature 2 direction info invalid.")
  }

  compareType <- paste(type1, "vs", type2, collapse = " ")

  ## Uni vs Uni ##
  if (compareType == "Uni vs Uni") {
    sig1 <- sig1_df$symbol
    sig2 <- sig2_df$symbol
    result <- list(
      "compareType" = "Uni vs Uni",
      "UnivsUni" = twoSigCompare(sig1, sig2,
        sig1_name = sig1_name, sig2_name = sig2_name,
        background_number = background_number
      )
    )
  }

  ## Uni vs Bi ##
  if (compareType == "Uni vs Bi") {
    sig1 <- sig1_df$symbol
    sig2_up <- sig2_df$symbol[which(sig2_df$direction == "+")]
    sig2_dn <- sig2_df$symbol[which(sig2_df$direction == "-")]
    result <- list(
      "compareType" = "Uni vs Bi",
      "UnivsUp" = twoSigCompare(sig1, sig2_up,
        sig1_name = sig1_name, sig2_name = paste(sig2_name, "up"),
        background_number = background_number
      ),
      "UnivsDn" = twoSigCompare(sig1, sig2_dn,
        sig1_name = sig1_name, sig2_name = paste(sig2_name, "dn"),
        background_number = background_number
      )
    )
  }

  ## Bi vs Uni ##
  if (compareType == "Bi vs Uni") {
    sig1_up <- sig1_df$symbol[which(sig1_df$direction == "+")]
    sig1_dn <- sig1_df$symbol[which(sig1_df$direction == "-")]
    sig2 <- sig2_df$symbol
    result <- list(
      "compareType" = "Bi vs Uni",
      "UnivsUp" = twoSigCompare(sig1_up, sig2,
        sig1_name = paste(sig1_name, "up"), sig2_name = sig2_name,
        background_number = background_number
      ),
      "UnivsDn" = twoSigCompare(sig1_dn, sig2,
        sig1_name = paste(sig1_name, "dn"), sig2_name = sig2_name,
        background_number = background_number
      )
    )
  }

  ## Bi vs Bi ##
  if (compareType == "Bi vs Bi") {
    sig1_up <- sig1_df$symbol[which(sig1_df$direction == "+")]
    sig1_dn <- sig1_df$symbol[which(sig1_df$direction == "-")]
    sig2_up <- sig2_df$symbol[which(sig2_df$direction == "+")]
    sig2_dn <- sig2_df$symbol[which(sig2_df$direction == "-")]
    result <- list(
      "compareType" = "Bi vs Bi",
      "UpvsUp" = twoSigCompare(sig1_up, sig2_up,
        sig1_name = paste(sig1_name, "up"), sig2_name = paste(sig2_name, "up"),
        background_number = background_number
      ),
      "UpvsDn" = twoSigCompare(sig1_up, sig2_dn,
        sig1_name = paste(sig1_name, "up"), sig2_name = paste(sig2_name, "dn"),
        background_number = background_number
      ),
      "DnvsUp" = twoSigCompare(sig1_dn, sig2_up,
        sig1_name = paste(sig1_name, "dn"), sig2_name = paste(sig2_name, "up"),
        background_number = background_number
      ),
      "DnvsDn" = twoSigCompare(sig1_dn, sig2_dn,
        sig1_name = paste(sig1_name, "dn"), sig2_name = paste(sig2_name, "dn"),
        background_number = background_number
      )
    )
  }

  if (result$compareType != "Uni vs Uni") {
    result$General <- twoSigCompare(
      sig1_df$symbol, sig2_df$symbol, sig1_name, sig2_name,
      background_number = background_number
    )
  }

  return(result)
}
