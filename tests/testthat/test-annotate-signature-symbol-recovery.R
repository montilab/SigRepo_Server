source(testthat::test_path("../../legacy_app/modules/annotate_module.R"), local = FALSE)

test_that("hypergeometric enrichment recovers gene symbols from difexp when signature stores feature ids", {
  sig_objs <- list(list(
    metadata = list(direction_type = "categorical"),
    signature = data.frame(
      probe_id = c("p1", "p2", "p3"),
      feature_name = c("ENSG1", "ENSG2", "ENSG3"),
      score = c(2.1, -1.8, 1.2),
      group_label = c("white", "white", "red"),
      stringsAsFactors = FALSE
    ),
    difexp = data.frame(
      probe_id = c("p1", "p2", "p3"),
      feature_name = c("ENSG1", "ENSG2", "ENSG3"),
      gene_symbol = c("GENE1", "GENE2", "GENE3"),
      score = c(2.1, -1.8, 1.2),
      group_label = c("white", "white", "red"),
      stringsAsFactors = FALSE
    )
  ))
  sig_list <- list(list(signature_name = "Recovered_Signature"))

  result <- build_enrichment_signatures(sig_objs, sig_list)

  expect_equal(result$vectors[["Recovered_Signature | white_up"]], "GENE1")
  expect_equal(result$vectors[["Recovered_Signature | white_dn"]], "GENE2")
  expect_equal(result$vectors[["Recovered_Signature | red_up"]], "GENE3")
})
