repos <- "https://cloud.r-project.org"
options(repos = c(CRAN = repos))

install_if_missing <- function(pkgs) {
  installed <- rownames(utils::installed.packages())
  to_install <- setdiff(pkgs, installed)
  if (length(to_install) > 0) {
    utils::install.packages(
      to_install,
      dependencies = c("Depends", "Imports", "LinkingTo"),
      repos = repos
    )
  }
}

# Minimal bootstrap only.
bootstrap_pkgs <- c("BiocManager", "remotes", "yaml")
install_if_missing(bootstrap_pkgs)

# Parse DESCRIPTION (DCF format)
desc <- read.dcf("DESCRIPTION")
imports_field <- if ("Imports" %in% colnames(desc)) desc[1, "Imports"] else ""
remotes_field <- if ("Remotes" %in% colnames(desc)) desc[1, "Remotes"] else ""

parse_pkg_names <- function(field) {
  if (!nzchar(field)) return(character(0))
  x <- unlist(strsplit(field, ",", fixed = TRUE), use.names = FALSE)
  x <- trimws(gsub("\\s*\\(.*\\)", "", x))
  x[x != ""]
}

# Anything listed in Remotes comes from GitHub, not CRAN, and is installed
# separately (see the remotes::install_github calls in Dockerfile and
# .github/workflows/test.yml). Attempting them here resolves to a different
# package or none at all: hypeR, for instance, pulls kableExtra -> svglite ->
# textshaping from CRAN, which needs system libraries the runners do not all
# have, and fails the whole install before the correct GitHub version is ever
# fetched. Strip the "owner/" prefix and any "@ref" suffix to get the package
# name to exclude.
remote_pkgs <- parse_pkg_names(remotes_field)
remote_pkgs <- sub("^.*/", "", remote_pkgs)
remote_pkgs <- sub("@.*$", "", remote_pkgs)

required_pkgs <- setdiff(unique(parse_pkg_names(imports_field)), remote_pkgs)

bioc_available <- tryCatch(BiocManager::available(), error = function(e) character(0))
bioc_pkgs <- intersect(required_pkgs, bioc_available)
cran_pkgs <- setdiff(required_pkgs, bioc_pkgs)

if (length(cran_pkgs) > 0) {
  install_if_missing(cran_pkgs)
}

if (length(bioc_pkgs) > 0) {
  installed <- rownames(utils::installed.packages())
  bioc_to_install <- setdiff(bioc_pkgs, installed)
  if (length(bioc_to_install) > 0) {
    BiocManager::install(bioc_to_install, ask = FALSE, update = FALSE)
  }
}

installed <- rownames(utils::installed.packages())
missing <- setdiff(required_pkgs, installed)
if (length(missing) > 0) {
  stop(sprintf("Failed to install required packages: %s", paste(missing, collapse = ", ")))
}

cat("Dependency install complete.\n")
