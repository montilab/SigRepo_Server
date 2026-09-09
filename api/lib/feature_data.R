# Organism-filter parsing shared by /update_transcriptomics and /update_proteomics.

parse_organism_filter <- function(organism) {
  if (base::length(organism) > 0 && base::all(!organism %in% c("", NA))) {
    organism <- base::sapply(base::seq_along(organism), function(i) {
      base::strsplit(organism[i], ",", fixed = TRUE)[[1]] |> base::trimws() |> base::as.character()
    }) |> base::as.vector() |> base::sort() |> base::unique()
  }

  organism
}

# The SigRepo client signals "nothing to do" by stopping with one of these
# messages (see updateTranscriptomicsFeatureSet / updateProteomicsFeatureSet).
# They are not failures, so a route should report them as skips.
FEATURE_UPDATE_NOOP_PATTERNS <- c("No updates needed", "No further updates needed", "Updates canceled")

# Run `updater(organism)` for every row of organism_tbl and report what
# happened to each one. The routes used to wrap the loop in a tryCatch whose
# handler built a response and then threw it away, so a failed organism still
# produced "Finish updating ..." -- which is how the mouse transcriptomics
# symbols stayed empty for months without anyone seeing an error.
#
# Returns list(ok = <all organisms updated or skipped>, messages = one line
# per organism: "<organism>: updated" | "skipped: <why>" | "failed: <error>").
run_feature_updates <- function(organism_tbl, updater) {
  messages <- character(0)
  ok <- TRUE
  for (organism in organism_tbl$organism) {
    outcome <- base::tryCatch({ updater(organism); "updated" }, error = function(e) e)
    if (base::inherits(outcome, "error")) {
      msg <- base::trimws(base::conditionMessage(outcome))
      is_noop <- base::any(base::vapply(FEATURE_UPDATE_NOOP_PATTERNS, function(p) base::grepl(p, msg, fixed = TRUE), logical(1)))
      if (is_noop) {
        messages <- c(messages, base::sprintf("%s: skipped: %s", organism, msg))
      } else {
        ok <- FALSE
        messages <- c(messages, base::sprintf("%s: failed: %s", organism, msg))
      }
    } else {
      messages <- c(messages, base::sprintf("%s: updated", organism))
    }
  }
  base::list(ok = ok, messages = messages)
}
