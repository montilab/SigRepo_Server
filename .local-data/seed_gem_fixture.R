# Seeds one synthetic metabolomics signature on the LOCAL dev stack so the
# hypeR-GEM annotate path can be tested end to end. The repository has only 3
# metabolomics signatures in production and none locally, so without this the
# GEM code path is unreachable.
#
# The 40 metabolites were chosen (see the selection step in the session log)
# because 87.8% of the genes Human-GEM maps them onto fall inside
# HALLMARK_FATTY_ACID_METABOLISM -- a planted signal, so a correct pipeline has
# to surface that pathway at the top and a broken one visibly cannot.
# Wrapped in a function so it can be source()d from rebuild_local_db.R without
# either script's `con` or on.exit() clobbering the other's -- sourcing at top
# level registers on.exit on the CALLER's frame, which closed the caller's
# connection mid-run and produced "corrupt connection handle".
suppressMessages(library(DBI))
local({
con <- dbConnect(RMySQL::MySQL(),
                 host = Sys.getenv("DB_LOCAL_HOST"), port = as.integer(Sys.getenv("DB_PORT")),
                 user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASSWORD"), dbname = "sigrepo")
on.exit(suppressWarnings(dbDisconnect(con)), add = TRUE)

mets <- readLines("/SigRepo_Server/.local-data/gem_fixture_metabolites.txt")
mets <- unique(mets[nzchar(mets)])
sq <- function(x) paste0("'", gsub("'", "''", x), "'")
hash <- function(x) substr(digest::digest(x, algo = "md5"), 1, 32)

# Idempotent: drop any previous run of this fixture first.
old <- dbGetQuery(con, "SELECT signature_id FROM signatures WHERE signature_name = 'DEV_GEM_FattyAcid_Fixture'")
if (nrow(old) > 0) {
  dbExecute(con, sprintf("DELETE FROM signature_feature_set WHERE signature_id IN (%s)", paste(old$signature_id, collapse = ",")))
  dbExecute(con, sprintf("DELETE FROM signature_access WHERE signature_id IN (%s)", paste(old$signature_id, collapse = ",")))
  dbExecute(con, sprintf("DELETE FROM signatures WHERE signature_id IN (%s)", paste(old$signature_id, collapse = ",")))
}
dbExecute(con, sprintf("DELETE FROM metabolite_reference WHERE refmet_name IN (%s)", paste(sq(mets), collapse = ",")))

# 1. metabolite_reference
for (m in mets) {
  dbExecute(con, sprintf(
    "INSERT INTO metabolite_reference (refmet_id, refmet_name, is_current, version, metabolite_hashkey) VALUES (%s, %s, 1, 1, %s)",
    sq(paste0("RM_", substr(hash(m), 1, 8))), sq(m), sq(hash(m))))
}
ref <- dbGetQuery(con, sprintf("SELECT metabolite_id, refmet_name FROM metabolite_reference WHERE refmet_name IN (%s)", paste(sq(mets), collapse = ",")))
cat("metabolite_reference rows:", nrow(ref), "\n")

# 2. the signature itself. 'others' uses the "key: value" form
#    parseRetrievedOthers() expects, and metabolomics_nomenclature is required
#    -- createOmicSignature() stops without it.
# Look the reference ids up rather than hardcoding them: the rebuild creates
# exactly one row in each of these tables, so a literal 2 for platform_id
# fails the foreign key.
ref_id <- function(tbl, col) dbGetQuery(con, sprintf("SELECT %s AS id FROM %s LIMIT 1", col, tbl))$id[1]
org_id <- ref_id("organisms", "organism_id")
phe_id <- ref_id("phenotypes", "phenotype_id")
plt_id <- ref_id("platforms", "platform_id")
smp_id <- ref_id("sample_types", "sample_type_id")

sig_hash <- hash("DEV_GEM_FattyAcid_Fixture")
dbExecute(con, sprintf(
  "INSERT INTO signatures (signature_name, organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id,
     description, others, year, has_difexp, user_name, date_created, visibility, signature_hashkey)
   VALUES ('DEV_GEM_FattyAcid_Fixture', %d, 'bi-directional', 'metabolomics', %d, %d, %d,
     'Synthetic local fixture for hypeR-GEM. Metabolites chosen so Human-GEM maps them onto HALLMARK_FATTY_ACID_METABOLISM.',
     'metabolomics_nomenclature: refmet', 2026, 0, 'devadmin', NOW(), 1, %s)",
  org_id, phe_id, plt_id, smp_id, sq(sig_hash)))
sig_id <- dbGetQuery(con, "SELECT LAST_INSERT_ID() id")$id[1]
cat("signature_id:", sig_id, " hashkey:", sig_hash, "\n")

# 3. feature set. Alternating sign so split_by_direction has both arms to work
#    with -- a single-direction fixture would leave that branch untested.
for (i in seq_len(nrow(ref))) {
  score <- if (i %% 3 == 0) -1 * (1 + i / 100) else (1 + i / 100)
  dbExecute(con, sprintf(
    "INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
     VALUES (%d, %d, %s, %.4f, 'All Features', 'metabolomics', %s)",
    sig_id, ref$metabolite_id[i], sq(ref$refmet_name[i]), score,
    sq(hash(paste0(sig_hash, ref$metabolite_id[i])))))
}
cat("signature_feature_set rows:", dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM signature_feature_set WHERE signature_id=%d", sig_id))$n, "\n")
cat("DONE hashkey=", sig_hash, "\n", sep = "")
})
