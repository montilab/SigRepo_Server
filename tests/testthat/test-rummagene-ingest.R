# Qualifying a Rummagene gene set for ingest as an OmicSignature.
#
# The rule these tests encode: SigRepo accepts a Rummagene set ONLY when every
# mandatory OmicSignature field is explicitly attested by an authoritative
# source -- never inferred, never defaulted to something plausible. Rummagene
# itself carries none of them (its GeneSet type is term/geneIds/nGeneIds/
# created/description/hash/genes, and its PmcInfo is pmcid/title/yr/doi), so
# organism and assay_type come from PubMed's MeSH indexing, reached through the
# PMC id Rummagene does supply.
#
# No network here: the payloads below are trimmed copies of real responses
# (idconv + efetch), same convention as test-rummagene.R.
source(testthat::test_path("../../api/lib/rummagene_ingest.R"), local = FALSE)

# ---------------------------------------------------------------- organism ---

test_that("rummagene_mesh_organism resolves a single human descriptor", {
  expect_equal(rummagene_mesh_organism(c("Humans", "Adipose Tissue")), "Homo sapiens")
})

test_that("rummagene_mesh_organism resolves mouse alongside the generic Animals descriptor", {
  # PubMed indexes animal studies with "Animals" PLUS the species. "Animals" on
  # its own is not a species and must not qualify (covered below).
  expect_equal(rummagene_mesh_organism(c("Animals", "Mice", "Liver")), "Mus musculus")
})

test_that("rummagene_mesh_organism refuses a paper indexed as both human and mouse", {
  # Two species means we cannot say which one THIS supplementary table came
  # from. Picking one would be exactly the invention this filter exists to
  # prevent. 2 of the 65 sampled papers looked like this.
  expect_null(rummagene_mesh_organism(c("Humans", "Mice")))
})

test_that("rummagene_mesh_organism refuses a species SigRepo does not hold", {
  # The real case that motivated this: PMC9334849, chicken adipose tissue,
  # whose genes Rummagene serves as ordinary human-style symbols.
  expect_null(rummagene_mesh_organism(c("Animals", "Chickens")))
})

test_that("rummagene_mesh_organism refuses human co-indexed with another species", {
  expect_null(rummagene_mesh_organism(c("Humans", "Animals", "Rats")))
})

test_that("rummagene_mesh_organism refuses a paper with no organism descriptor", {
  expect_null(rummagene_mesh_organism(c("Neoplasms", "Biomarkers, Tumor")))
})

test_that("rummagene_mesh_organism refuses a paper with no MeSH indexing at all", {
  # 13 of 65 sampled papers. Common for recent articles.
  expect_null(rummagene_mesh_organism(character(0)))
  expect_null(rummagene_mesh_organism(NULL))
})

test_that("rummagene_mesh_organism does not treat the bare Animals descriptor as a species", {
  expect_null(rummagene_mesh_organism(c("Animals", "Liver")))
})

# -------------------------------------------------------------- assay type ---

test_that("rummagene_mesh_assay_type maps transcriptomic MeSH descriptors", {
  expect_equal(rummagene_mesh_assay_type(c("Humans", "Transcriptome")), "transcriptomics")
  expect_equal(rummagene_mesh_assay_type(c("Gene Expression Profiling")), "transcriptomics")
})

test_that("rummagene_mesh_assay_type maps the non-transcriptomic assays SigRepo holds", {
  expect_equal(rummagene_mesh_assay_type(c("Proteomics")), "proteomics")
  expect_equal(rummagene_mesh_assay_type(c("DNA Methylation")), "methylomics")
  expect_equal(rummagene_mesh_assay_type(c("Metabolome")), "metabolomics")
  expect_equal(rummagene_mesh_assay_type(c("Polymorphism, Single Nucleotide")), "genetic_variants")
})

test_that("rummagene_mesh_assay_type collapses several descriptors that mean one assay", {
  # A paper routinely carries both. They agree, so this is not ambiguity.
  expect_equal(
    rummagene_mesh_assay_type(c("Transcriptome", "Gene Expression Profiling", "Sequence Analysis, RNA")),
    "transcriptomics"
  )
})

test_that("rummagene_mesh_assay_type refuses a paper spanning two different assays", {
  # Multi-omic papers are real, and the supplementary table could be from
  # either. Rejecting is the only honest answer.
  expect_null(rummagene_mesh_assay_type(c("Transcriptome", "Proteomics")))
})

test_that("rummagene_mesh_assay_type refuses a paper with no assay descriptor", {
  expect_null(rummagene_mesh_assay_type(c("Humans", "Adipose Tissue")))
  expect_null(rummagene_mesh_assay_type(character(0)))
})

# --------------------------------------------------------------- qualifying ---

# A trimmed Rummagene geneSetTermSearch node, exactly as the GraphQL returns it.
gene_set_fixture <- function(term = "PMC7202592-Table_1.xlsx-liver-tumor_vs_normal",
                             genes = c("TP53", "MYC", "EGFR", "BRCA1")) {
  base::list(
    term = term,
    description = "DEGs, FDR < 0.05",
    nGeneIds = base::length(genes),
    genes = base::list(nodes = base::lapply(genes, function(g) base::list(symbol = g))),
    geneSetPmcsById = base::list(nodes = base::list(base::list(
      pmcInfoByPmcid = base::list(
        pmcid = "PMC7202592", title = "A liver paper", yr = 2020, doi = "10.1/abc"
      )
    )))
  )
}

test_that("rummagene_qualify accepts a set whose paper attests both organism and assay", {
  q <- rummagene_qualify(gene_set_fixture(), mesh = c("Humans", "Transcriptome", "Liver"))

  expect_true(q$ok)
  expect_equal(q$organism, "Homo sapiens")
  expect_equal(q$assay_type, "transcriptomics")
  expect_equal(q$pmcid, "PMC7202592")
  expect_equal(q$genes, c("TP53", "MYC", "EGFR", "BRCA1"))
})

test_that("rummagene_qualify reports WHY a set was refused, per field", {
  # A rejection has to be inspectable -- "23% passed" is only trustworthy if the
  # other 77% can each be accounted for.
  chicken <- rummagene_qualify(gene_set_fixture(), mesh = c("Animals", "Chickens", "Transcriptome"))
  expect_false(chicken$ok)
  expect_equal(chicken$reason, "organism")

  no_assay <- rummagene_qualify(gene_set_fixture(), mesh = c("Humans", "Liver"))
  expect_false(no_assay$ok)
  expect_equal(no_assay$reason, "assay_type")

  unindexed <- rummagene_qualify(gene_set_fixture(), mesh = character(0))
  expect_false(unindexed$ok)
  expect_equal(unindexed$reason, "no_mesh")
})

test_that("rummagene_qualify refuses a set with too few genes to be a signature", {
  q <- rummagene_qualify(gene_set_fixture(genes = c("TP53")), mesh = c("Humans", "Transcriptome"))
  expect_false(q$ok)
  expect_equal(q$reason, "too_few_genes")
})

test_that("rummagene_qualify refuses a set Rummagene could not tie to a paper", {
  # No PMC record means no MeSH lookup is even possible.
  gs <- gene_set_fixture()
  gs$geneSetPmcsById <- base::list(nodes = base::list())
  q <- rummagene_qualify(gs, mesh = c("Humans", "Transcriptome"))
  expect_false(q$ok)
  expect_equal(q$reason, "no_pmc")
})

# ------------------------------------------------------- building the object ---

test_that("rummagene_build_signature produces a valid OmicSignature", {
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  q <- rummagene_qualify(gene_set_fixture(), mesh = c("Humans", "Transcriptome"))
  os <- rummagene_build_signature(q)

  expect_s3_class(os, "OmicSignature")
  expect_equal(os$metadata$organism, "Homo sapiens")
  expect_equal(os$metadata$assay_type, "transcriptomics")
  expect_equal(base::nrow(os$signature), 4)
})

test_that("rummagene_build_signature declares phenotype unknown rather than inventing one", {
  # Rummagene has no phenotype field and MeSH disease descriptors would have to
  # be CHOSEN by us. "unknown" is a statement that we do not know -- the one
  # thing here that is honest to record.
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  os <- rummagene_build_signature(rummagene_qualify(gene_set_fixture(), mesh = c("Humans", "Transcriptome")))
  expect_equal(os$metadata$phenotype, "unknown")
})

test_that("rummagene_build_signature declares uni-directional, so no group_label is fabricated", {
  # A Rummagene set is an unordered gene list. uni-directional is the accurate
  # description of that shape, and it is also the only direction_type whose
  # checkSignature() does NOT require a group_label column -- so honesty and
  # the schema agree here.
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  os <- rummagene_build_signature(rummagene_qualify(gene_set_fixture(), mesh = c("Humans", "Transcriptome")))
  expect_equal(os$metadata$direction_type, "uni-directional")
  expect_false("group_label" %in% base::colnames(os$signature))
})

test_that("rummagene_build_signature records where every asserted field came from", {
  # Provenance is the whole basis for trusting an ingested signature: a reader
  # must be able to check the organism claim against the PubMed record.
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  os <- rummagene_build_signature(rummagene_qualify(gene_set_fixture(), mesh = c("Humans", "Transcriptome")))
  expect_match(os$metadata$others, "PMC7202592")
  expect_match(os$metadata$others, "MeSH")
})

test_that("rummagene_build_signature refuses to build from a rejected candidate", {
  q <- rummagene_qualify(gene_set_fixture(), mesh = c("Animals", "Chickens"))
  expect_error(rummagene_build_signature(q), "did not qualify")
})

# ---------------------------------------------------- parsing NCBI responses ---

test_that("rummagene_parse_idconv maps PMC ids to PMIDs, skipping records without one", {
  # Trimmed from the live endpoint. Note the PMID arrives as a JSON number.
  json <- '{"records":[
    {"pmcid":"PMC7202592","pmid":32341563},
    {"pmcid":"PMC9334849","pmid":35911745},
    {"pmcid":"PMC0000000","errmsg":"invalid article id"}
  ]}'
  out <- rummagene_parse_idconv(json)

  expect_equal(out[["PMC7202592"]], "32341563")
  expect_equal(out[["PMC9334849"]], "35911745")
  expect_false("PMC0000000" %in% base::names(out))
})

test_that("rummagene_parse_mesh_xml extracts descriptors per PMID", {
  xml <- '<PubmedArticleSet>
    <PubmedArticle><MedlineCitation><PMID>35911745</PMID>
      <MeshHeadingList>
        <MeshHeading><DescriptorName>Animals</DescriptorName></MeshHeading>
        <MeshHeading><DescriptorName>Chickens</DescriptorName></MeshHeading>
      </MeshHeadingList>
    </MedlineCitation></PubmedArticle>
    <PubmedArticle><MedlineCitation><PMID>32341563</PMID>
      <MeshHeadingList>
        <MeshHeading><DescriptorName>Humans</DescriptorName></MeshHeading>
      </MeshHeadingList>
    </MedlineCitation></PubmedArticle>
  </PubmedArticleSet>'
  out <- rummagene_parse_mesh_xml(xml)

  expect_equal(out[["35911745"]], c("Animals", "Chickens"))
  expect_equal(out[["32341563"]], "Humans")
})

test_that("rummagene_parse_mesh_xml returns an empty vector for an unindexed article", {
  # Distinct from "article absent" -- an unindexed article still comes back,
  # just with no MeshHeadingList, and must qualify as no_mesh rather than error.
  xml <- '<PubmedArticleSet>
    <PubmedArticle><MedlineCitation><PMID>37223537</PMID></MedlineCitation></PubmedArticle>
  </PubmedArticleSet>'
  out <- rummagene_parse_mesh_xml(xml)

  expect_equal(out[["37223537"]], character(0))
})

# ------------------------------------------------- qualifying a whole batch ---

test_that("rummagene_qualify_all splits a batch and tallies every rejection reason", {
  # The yield number is only meaningful if the discards are accounted for, so
  # the batch result carries a per-reason tally rather than silently dropping.
  gene_sets <- base::list(
    gene_set_fixture(term = "keep-human"),
    gene_set_fixture(term = "drop-chicken"),
    gene_set_fixture(term = "drop-no-assay"),
    gene_set_fixture(term = "drop-unindexed")
  )
  # All four fixtures share one pmcid, so vary the MeSH per set instead by
  # giving each its own pmcid first.
  for (i in base::seq_along(gene_sets)) {
    gene_sets[[i]]$geneSetPmcsById$nodes[[1]]$pmcInfoByPmcid$pmcid <- base::paste0("PMC", i)
  }
  mesh_by_pmcid <- base::list(
    PMC1 = c("Humans", "Transcriptome"),
    PMC2 = c("Animals", "Chickens", "Transcriptome"),
    PMC3 = c("Humans", "Liver"),
    PMC4 = base::character(0)
  )

  out <- rummagene_qualify_all(gene_sets, mesh_by_pmcid)

  expect_equal(base::length(out$qualified), 1)
  expect_equal(out$qualified[[1]]$term, "keep-human")
  expect_equal(out$rejected[["organism"]], 1)
  expect_equal(out$rejected[["assay_type"]], 1)
  expect_equal(out$rejected[["no_mesh"]], 1)
  expect_equal(out$n_examined, 4)
})

test_that("rummagene_qualify_all treats a paper absent from the MeSH lookup as unindexed", {
  # efetch simply omits an article it has no record of. That must land as
  # no_mesh, not as an error mid-batch.
  gene_sets <- base::list(gene_set_fixture())
  out <- rummagene_qualify_all(gene_sets, mesh_by_pmcid = base::list())

  expect_equal(base::length(out$qualified), 0)
  expect_equal(out$rejected[["no_mesh"]], 1)
})

# -------------------------------------------------------------- signature name ---

test_that("rummagene_signature_name leaves a short term untouched", {
  expect_equal(rummagene_signature_name("PMC123-t1.xlsx-up"), "PMC123-t1.xlsx-up")
})

test_that("rummagene_signature_name caps a long term at 255 characters", {
  long <- base::paste0("PMC7202592-", base::paste(base::rep("x", 400), collapse = ""))
  out <- rummagene_signature_name(long)
  expect_lte(base::nchar(out), 255)
  # The length check alone would also let a broken truncation through -- e.g.
  # one that discarded the term and returned only a short hash suffix.
  # Confirm the term's own leading characters actually survive into the
  # output, checked well short of where truncation happens so this does not
  # hardcode the exact cutoff arithmetic.
  expect_equal(base::substr(out, 1, 20), base::substr(long, 1, 20))
})

test_that("rummagene_signature_name keeps two long terms sharing a prefix distinct", {
  # The collision that UNIQUE(signature_name, user_name) would otherwise reject:
  # sibling tables from one paper differ only in a suffix past character 255.
  prefix <- base::paste0("PMC7202592-", base::paste(base::rep("x", 300), collapse = ""))
  a <- rummagene_signature_name(base::paste0(prefix, "-cluster_1"))
  b <- rummagene_signature_name(base::paste0(prefix, "-cluster_2"))
  expect_false(base::identical(a, b))
  expect_lte(base::nchar(a), 255)
  expect_lte(base::nchar(b), 255)
})

test_that("rummagene_signature_name is deterministic", {
  long <- base::paste0("PMC1-", base::paste(base::rep("y", 400), collapse = ""))
  expect_equal(rummagene_signature_name(long), rummagene_signature_name(long))
})

test_that("rummagene_parse_article_xml returns MeSH and citation metadata together", {
  # One efetch response carries both. Parsing it once avoids a second round trip
  # per paper across ~188k papers.
  xml <- '<PubmedArticleSet>
    <PubmedArticle><MedlineCitation><PMID>32341563</PMID>
      <Article>
        <Journal><JournalIssue><PubDate><Year>2020</Year></PubDate></JournalIssue></Journal>
        <ArticleTitle>A liver paper</ArticleTitle>
        <ELocationID EIdType="doi">10.1/abc</ELocationID>
      </Article>
      <MeshHeadingList>
        <MeshHeading><DescriptorName>Humans</DescriptorName></MeshHeading>
        <MeshHeading><DescriptorName>Transcriptome</DescriptorName></MeshHeading>
      </MeshHeadingList>
    </MedlineCitation></PubmedArticle>
  </PubmedArticleSet>'
  out <- rummagene_parse_article_xml(xml)

  expect_equal(out[["32341563"]]$mesh, c("Humans", "Transcriptome"))
  expect_equal(out[["32341563"]]$title, "A liver paper")
  expect_equal(out[["32341563"]]$year, 2020L)
  expect_equal(out[["32341563"]]$doi, "10.1/abc")
})

test_that("rummagene_parse_article_xml tolerates an article missing every optional field", {
  # Unindexed articles, and articles with no DOI or no structured year, are
  # common. They must come back with empty fields rather than erroring, so the
  # build treats them as no_mesh instead of dying mid-pass.
  xml <- '<PubmedArticleSet>
    <PubmedArticle><MedlineCitation><PMID>37223537</PMID></MedlineCitation></PubmedArticle>
  </PubmedArticleSet>'
  out <- rummagene_parse_article_xml(xml)

  expect_equal(out[["37223537"]]$mesh, character(0))
  expect_true(base::is.na(out[["37223537"]]$title))
  expect_true(base::is.na(out[["37223537"]]$year))
  expect_true(base::is.na(out[["37223537"]]$doi))
})
