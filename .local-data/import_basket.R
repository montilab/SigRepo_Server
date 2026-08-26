# Load a downloaded SigRepo signature basket into the local development
# repository, replacing the synthetic fixtures this stack used to carry.
#
#   docker exec sigrepo-local-api Rscript /SigRepo_Server/.local-data/import_basket.R /tmp/basket
#
# Each file is the /signatures/export shape: list(metadata, signature, difexp).
#
# Loaded with direct SQL rather than through POST /signatures/upload, because
# that route only accepts transcriptomics and proteomics
# (api/lib/create_signature.R: "Signature upload supports assay_type in
# {transcriptomics, proteomics}"), and this basket also contains a
# metabolomics and a genetic_variants signature. Going straight to SQL keeps
# one code path for all four assay types.
#
# The export carries the feature_ids of the repository it came from, so the
# reference rows are recreated under those same ids -- that is what lets the
# signature_feature_set rows point somewhere real.
suppressMessages({ library(DBI); library(digest) })

args <- commandArgs(trailingOnly = TRUE)
basket_dir <- if (length(args) > 0) args[1] else "/tmp/basket"
difexp_dir <- Sys.getenv("DIFEXP_DIR", unset = "/difexp")

con <- dbConnect(RMySQL::MySQL(), host = Sys.getenv("DB_LOCAL_HOST"), port = as.integer(Sys.getenv("DB_PORT")),
                 user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASSWORD"), dbname = "sigrepo")
on.exit(suppressWarnings(dbDisconnect(con)), add = TRUE)

ex   <- function(sql) invisible(dbExecute(con, sql))
sq   <- function(x) if (is.na(x)) "NULL" else paste0("'", gsub("'", "''", as.character(x)), "'")
hash <- function(...) substr(digest(paste0(...), algo = "md5"), 1, 32)
meta <- function(m, k, default = NA) {
  v <- m[[k]]
  if (is.null(v) || length(v) == 0 || is.na(v[1]) || !nzchar(as.character(v[1]))) default else as.character(v[1])
}

files <- list.files(basket_dir, pattern = "[.]rds$", full.names = TRUE)
if (length(files) == 0) stop("No .rds files found in ", basket_dir)
cat("found", length(files), "signature files in", basket_dir, "\n\n")
sigs <- lapply(files, readRDS)

# --- vocabulary -------------------------------------------------------------
# Insert only what these signatures actually reference, so the local vocabulary
# mirrors the data rather than a guess at it.
upsert <- function(table, col, value, extra_cols = NULL, extra_vals = NULL) {
  if (is.na(value)) return(invisible(NULL))
  hit <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s WHERE %s = %s", table, col, sq(value)))$n
  if (hit > 0) return(invisible(NULL))
  cols <- c(col, extra_cols); vals <- c(sq(value), extra_vals)
  ex(sprintf("INSERT INTO %s (%s) VALUES (%s)", table, paste(cols, collapse = ", "), paste(vals, collapse = ", ")))
}
for (s in sigs) {
  m <- s$metadata
  upsert("organisms",    "organism",      meta(m, "organism"))
  upsert("phenotypes",   "phenotype",     meta(m, "phenotype"))
  upsert("platforms",    "platform_name", meta(m, "platform_name"))
  upsert("sample_types", "sample_type",   meta(m, "sample_type"))
}
lookup_id <- function(table, id_col, col, value) {
  if (is.na(value)) return(NA_integer_)
  r <- dbGetQuery(con, sprintf("SELECT %s AS id FROM %s WHERE %s = %s LIMIT 1", id_col, table, col, sq(value)))
  if (nrow(r) == 0) NA_integer_ else as.integer(r$id[1])
}
cat("vocabulary: ",
    dbGetQuery(con, "SELECT COUNT(*) n FROM organisms")$n, "organisms, ",
    dbGetQuery(con, "SELECT COUNT(*) n FROM phenotypes")$n, "phenotypes, ",
    dbGetQuery(con, "SELECT COUNT(*) n FROM platforms")$n, "platforms, ",
    dbGetQuery(con, "SELECT COUNT(*) n FROM sample_types")$n, "sample types\n", sep = "")

# --- reference features -----------------------------------------------------
# The export's `signature` table gives (feature_id, probe_id). For
# transcriptomics the difexp additionally carries feature_name (an Ensembl id)
# and symbol (the real gene symbol), joined on probe_id -- so the local
# reference table gets genuine gene symbols instead of the anonymized
# probe ids, which is what the annotate and gene-search paths need.
insert_features <- function(assay_type, rows, organism_id) {
  if (nrow(rows) == 0) return(0L)
  n <- 0L
  for (i in seq_len(nrow(rows))) {
    fid <- rows$feature_id[i]
    if (is.na(fid)) next
    ok <- tryCatch({
      if (assay_type == "metabolomics") {
        ex(sprintf(
          "INSERT IGNORE INTO metabolite_reference (metabolite_id, refmet_id, refmet_name, is_current, version, metabolite_hashkey)
           VALUES (%d, %s, %s, 1, 1, %s)",
          fid, sq(paste0("RM_", substr(hash(rows$name[i]), 1, 8))), sq(rows$name[i]), sq(hash("met", fid))))
      } else if (assay_type == "proteomics") {
        ex(sprintf(
          "INSERT IGNORE INTO proteomics_features (feature_id, feature_name, organism_id, version, feature_hashkey)
           VALUES (%d, %s, %d, 1, %s)",
          fid, sq(rows$name[i]), organism_id, sq(hash(tolower(paste0(rows$name[i], organism_id))))))
      } else if (assay_type == "genetic_variants") {
        ex(sprintf(
          "INSERT IGNORE INTO genetic_variants_features (feature_id, feature_name, organism_id, version, feature_hashkey)
           VALUES (%d, %s, %d, 1, %s)",
          fid, sq(rows$name[i]), organism_id, sq(hash(tolower(paste0(rows$name[i], organism_id))))))
      } else {
        ex(sprintf(
          "INSERT IGNORE INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
           VALUES (%d, %s, %d, %s, 1, %s)",
          fid, sq(rows$name[i]), organism_id, sq(rows$symbol[i]), sq(hash(tolower(paste0(rows$name[i], organism_id))))))
      }
      TRUE
    }, error = function(e) { cat("    feature warn:", substr(conditionMessage(e), 1, 70), "\n"); FALSE })
    if (ok) n <- n + 1L
  }
  n
}

# The (feature_id, probe_id, name, symbol) frame for one signature, joining the
# difexp in for the real feature_name and gene symbol where it has them.
feature_frame <- function(s) {
  sig_tbl <- s$signature
  fr <- data.frame(
    feature_id = suppressWarnings(as.integer(sig_tbl$feature_id)),
    probe_id = as.character(sig_tbl$probe_id),
    stringsAsFactors = FALSE
  )
  fr$name <- fr$probe_id
  fr$symbol <- NA_character_
  de <- s$difexp
  if (is.data.frame(de) && "probe_id" %in% colnames(de)) {
    if ("feature_name" %in% colnames(de)) {
      hit <- match(fr$probe_id, de$probe_id)
      fr$name <- ifelse(is.na(hit), fr$probe_id, as.character(de$feature_name)[hit])
    }
    sym_col <- intersect(c("gene_symbol", "symbol", "geneSymbol", "gene"), colnames(de))
    if (length(sym_col) > 0) fr$symbol <- as.character(de[[sym_col[1]]])[match(fr$probe_id, de$probe_id)]
  }
  blank <- is.na(fr$name) | !nzchar(fr$name)
  fr$name[blank] <- fr$probe_id[blank]
  fr
}

# Pass 1: every reference row that carries an explicit feature_id from the
# source repository, for ALL signatures, before any AUTO_INCREMENT row is
# allocated below.
#
# Interleaving the two per-signature silently corrupted the import: the first
# signature's 14,563 difexp rows consumed the id range that later signatures'
# explicit ids fell in, so their INSERT IGNOREs were dropped as duplicates and
# those signatures ended up referencing features that did not exist. It only
# surfaced when uploading the same files through POST /signatures/upload
# reported "882 uploaded feature(s) do not exist in transcriptomics_features".
cat("\nregistering reference features\n")
for (k in seq_along(sigs)) {
  s <- sigs[[k]]; m <- s$metadata
  if (!is.data.frame(s$signature) || nrow(s$signature) == 0) next
  org <- lookup_id("organisms", "organism_id", "organism", meta(m, "organism"))
  n <- insert_features(meta(m, "assay_type", "transcriptomics"), feature_frame(s), org)
  cat(sprintf("  %-58s %d explicit ids\n", substr(meta(m, "signature_name", ""), 1, 56), n))
}

# --- signatures -------------------------------------------------------------
owner <- "devadmin"
imported <- 0L
for (k in seq_along(sigs)) {
  s <- sigs[[k]]; m <- s$metadata
  name <- meta(m, "signature_name", basename(files[k]))
  assay <- meta(m, "assay_type", "transcriptomics")
  sig_tbl <- s$signature
  if (!is.data.frame(sig_tbl) || nrow(sig_tbl) == 0) {
    cat("skip  ", name, "-- no features in the export\n"); next
  }

  org_id <- lookup_id("organisms", "organism_id", "organism", meta(m, "organism"))
  phe_id <- lookup_id("phenotypes", "phenotype_id", "phenotype", meta(m, "phenotype"))
  plt_id <- lookup_id("platforms", "platform_id", "platform_name", meta(m, "platform_name"))
  smp_id <- lookup_id("sample_types", "sample_type_id", "sample_type", meta(m, "sample_type"))

  fr <- feature_frame(s)
  de <- s$difexp
  n_feat <- 0L

  # Also register every gene the difexp mentions, not just the ones that made
  # the signature's cutoff. Rank-based enrichment (KS and GSEA) ranks the whole
  # difexp, and resolves each row's symbol through the reference table -- so
  # with only the signature's own features registered, a 14,563-row difexp
  # resolved 831 genes and GSEA found nothing. These get AUTO_INCREMENT ids
  # (they have no feature_id in the export) and are inserted after the
  # explicitly-keyed rows above so the counter starts past them.
  if (assay == "transcriptomics" && is.data.frame(de) && "feature_name" %in% colnames(de) && !is.na(org_id)) {
    sym_col <- intersect(c("gene_symbol", "symbol", "geneSymbol", "gene"), colnames(de))
    extra <- data.frame(
      name = as.character(de$feature_name),
      symbol = if (length(sym_col) > 0) as.character(de[[sym_col[1]]]) else NA_character_,
      stringsAsFactors = FALSE
    )
    extra <- extra[!is.na(extra$name) & nzchar(extra$name) & !(extra$name %in% fr$name), , drop = FALSE]
    extra <- extra[!duplicated(extra$name), , drop = FALSE]
    if (nrow(extra) > 0) {
      values <- sprintf("(%s, %d, %s, 1, %s)",
                        vapply(extra$name, sq, character(1)), org_id,
                        vapply(extra$symbol, sq, character(1)),
                        vapply(vapply(tolower(paste0(extra$name, org_id)), hash, character(1)), sq, character(1)))
      # Chunked: a single statement with 14k tuples exceeds max_allowed_packet.
      before <- dbGetQuery(con, "SELECT COUNT(*) n FROM transcriptomics_features")$n
      for (chunk in split(values, ceiling(seq_along(values) / 1000))) {
        tryCatch(ex(sprintf(
          "INSERT IGNORE INTO transcriptomics_features (feature_name, organism_id, gene_symbol, version, feature_hashkey) VALUES %s",
          paste(chunk, collapse = ", "))),
          error = function(e) cat("    difexp feature warn:", substr(conditionMessage(e), 1, 90), "\n"))
      }
      # Count what actually landed. Adding nrow(extra) unconditionally reported
      # 13,596 registered while a broken statement had inserted none.
      n_feat <- n_feat + (dbGetQuery(con, "SELECT COUNT(*) n FROM transcriptomics_features")$n - before)
    }
  }

  hk <- meta(m, "signature_hashkey", hash(name))
  ex(sprintf("DELETE FROM signature_feature_set WHERE signature_id IN (SELECT signature_id FROM signatures WHERE signature_hashkey = %s)", sq(hk)))
  ex(sprintf("DELETE FROM signatures WHERE signature_hashkey = %s", sq(hk)))

  has_difexp <- is.data.frame(de) && nrow(de) > 0
  ex(sprintf(
    "INSERT INTO signatures (signature_name, organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id,
       description, keywords, others, year, has_difexp, num_of_difexp, num_up_regulated, num_down_regulated,
       user_name, date_created, visibility, signature_hashkey)
     VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %d, %s, %s, %s, %s, NOW(), 1, %s)",
    sq(name), if (is.na(org_id)) "NULL" else org_id, sq(meta(m, "direction_type", "bi-directional")), sq(assay),
    if (is.na(phe_id)) "NULL" else phe_id, if (is.na(plt_id)) "NULL" else plt_id, if (is.na(smp_id)) "NULL" else smp_id,
    sq(meta(m, "description")), sq(meta(m, "keywords")),
    # createOmicSignature() needs metabolomics_nomenclature to know which
    # metabolite dictionary a metabolomics signature was deposited under; the
    # export records it per-feature as nomenclature_type instead.
    if (assay == "metabolomics") {
      nom <- if ("nomenclature_type" %in% colnames(sig_tbl)) as.character(sig_tbl$nomenclature_type)[1] else "refmet"
      sq(paste0("metabolomics_nomenclature: ", nom))
    } else sq(meta(m, "others")),
    if (is.na(meta(m, "year"))) "NULL" else meta(m, "year"),
    as.integer(has_difexp),
    if (has_difexp) nrow(de) else "NULL",
    if (is.na(meta(m, "num_up_regulated"))) "NULL" else meta(m, "num_up_regulated"),
    if (is.na(meta(m, "num_down_regulated"))) "NULL" else meta(m, "num_down_regulated"),
    sq(owner), sq(hk)))
  sid <- dbGetQuery(con, "SELECT LAST_INSERT_ID() id")$id[1]

  ok_rows <- 0L
  for (i in seq_len(nrow(sig_tbl))) {
    fid <- fr$feature_id[i]
    if (is.na(fid)) next
    score <- suppressWarnings(as.numeric(sig_tbl$score[i]))
    grp <- if ("group_label" %in% colnames(sig_tbl)) as.character(sig_tbl$group_label[i]) else "All Features"
    if (is.na(grp) || !nzchar(grp)) grp <- "All Features"
    ok <- tryCatch({
      ex(sprintf(
        "INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
         VALUES (%d, %d, %s, %s, %s, %s, %s)",
        sid, fid, sq(fr$probe_id[i]), if (is.na(score)) "NULL" else sprintf("%.8f", score),
        sq(grp), sq(assay), sq(hash(sid, fid, assay, fr$probe_id[i]))))
      TRUE
    }, error = function(e) FALSE)
    if (ok) ok_rows <- ok_rows + 1L
  }

  if (has_difexp) saveRDS(de, file.path(difexp_dir, paste0(hk, ".rds")))
  cat(sprintf("import %-58s %-16s features=%-4d refs=%-4d difexp=%s\n",
              substr(name, 1, 56), assay, ok_rows, n_feat, if (has_difexp) nrow(de) else "-"))
  imported <- imported + 1L
}

cat("\nimported", imported, "signatures\n")
print(dbGetQuery(con, "SELECT assay_type, COUNT(*) n FROM signatures GROUP BY assay_type"))
