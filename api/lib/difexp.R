# RDS file lifecycle for differential-expression payloads, backing
# /store_difexp, /get_difexp, /delete_difexp.

difexp_file_path <- function(difexp_dir, signature_hashkey) {
  base::file.path(difexp_dir, base::paste0(signature_hashkey, ".RDS"))
}

save_difexp_rds <- function(difexp_dir, signature_hashkey, difexp_obj) {
  if (!base::is.data.frame(difexp_obj)) {
    return(FALSE)
  }

  base::saveRDS(difexp_obj, file = difexp_file_path(difexp_dir, signature_hashkey))
  TRUE
}

load_difexp_rds <- function(difexp_dir, signature_hashkey) {
  difexp_file <- difexp_file_path(difexp_dir, signature_hashkey)

  if (!base::file.exists(difexp_file)) {
    return(NULL)
  }

  base::readRDS(difexp_file)
}

delete_difexp_rds <- function(difexp_dir, signature_hashkey) {
  difexp_file <- difexp_file_path(difexp_dir, signature_hashkey)

  if (base::file.exists(difexp_file)) {
    base::unlink(difexp_file)
  }

  invisible(TRUE)
}
