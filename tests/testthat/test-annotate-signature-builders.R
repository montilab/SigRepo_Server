source(testthat::test_path("../../shiny/modules/annotate_module.R"), local = FALSE)

test_that("hypergeometric enrichment uses signature table and splits categorical scores", {
  sig_objs <- list(list(
    metadata = list(direction_type = "categorical"),
    signature = data.frame(
      probe_id = c(1, 3, 3, 5),
      gene = c("gene1", "gene3", "gene3", "gene5"),
      group_label = c("white", "white", "yellow", "red"),
      score = c(1.1, 0.8, 0.3, -0.4),
      stringsAsFactors = FALSE
    ),
    difexp = data.frame()
  ))
  sig_list <- list(list(signature_name = "My_Tulip_Experiments"))

  result <- build_enrichment_signatures(sig_objs, sig_list)

  expect_named(
    result$vectors,
    c(
      "My_Tulip_Experiments | white_up",
      "My_Tulip_Experiments | yellow_up",
      "My_Tulip_Experiments | red_dn"
    )
  )
  expect_equal(result$vectors[["My_Tulip_Experiments | white_up"]], c("gene1", "gene3"))
  expect_equal(result$vectors[["My_Tulip_Experiments | yellow_up"]], "gene3")
  expect_equal(result$vectors[["My_Tulip_Experiments | red_dn"]], "gene5")
  expect_equal(result$metadata$group_label, c("white_up", "yellow_up", "red_dn"))
})

test_that("hypergeometric enrichment keeps categorical groups intact when scores are absent", {
  sig_objs <- list(list(
    metadata = list(direction_type = "categorical"),
    signature = data.frame(
      probe_id = c(1, 2, 3),
      gene = c("gene1", "gene2", "gene3"),
      group_label = c("white", "red", "yellow"),
      stringsAsFactors = FALSE
    ),
    difexp = data.frame()
  ))
  sig_list <- list(list(signature_name = "My_Tulip_Experiments"))

  result <- build_enrichment_signatures(sig_objs, sig_list)

  expect_named(
    result$vectors,
    c(
      "My_Tulip_Experiments | white",
      "My_Tulip_Experiments | red",
      "My_Tulip_Experiments | yellow"
    )
  )
  expect_equal(result$metadata$group_label, c("white", "red", "yellow"))
})

test_that("ks enrichment ranks each categorical group in both directions", {
  sig_objs <- list(list(
    metadata = list(direction_type = "categorical"),
    difexp = data.frame(
      probe_id = c(1, 2, 3, 4, 5, 6),
      gene = c("gene1", "gene2", "gene3", "gene4", "gene5", "gene6"),
      group_label = c("white", "white", "white", "red", "red", "red"),
      score = c(1.1, -0.2, 0.8, -0.7, 0.4, -0.1),
      stringsAsFactors = FALSE
    )
  ))
  sig_list <- list(list(signature_name = "My_Tulip_Experiments"))

  result <- build_ranked_enrichment_signatures(sig_objs, sig_list, mode = "ks")

  expect_named(
    result$vectors,
    c(
      "My_Tulip_Experiments | white_up",
      "My_Tulip_Experiments | white_dn",
      "My_Tulip_Experiments | red_up",
      "My_Tulip_Experiments | red_dn"
    )
  )
  expect_equal(
    names(result$vectors[["My_Tulip_Experiments | white_up"]]),
    c("gene1", "gene3", "gene2")
  )
  expect_equal(
    names(result$vectors[["My_Tulip_Experiments | white_dn"]]),
    c("gene2", "gene3", "gene1")
  )
  expect_equal(
    names(result$vectors[["My_Tulip_Experiments | red_up"]]),
    c("gene5", "gene6", "gene4")
  )
  expect_equal(
    names(result$vectors[["My_Tulip_Experiments | red_dn"]]),
    c("gene4", "gene6", "gene5")
  )
  expect_equal(result$metadata$group_label, c("white_up", "white_dn", "red_up", "red_dn"))
})
