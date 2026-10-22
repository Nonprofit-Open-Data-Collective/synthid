# Shared settings for the full Part VII panel build (dev/partvii-panel/).
#
# Reuses the development dataset's settings (efile v3.1 cache, TY2010-2024,
# the one-filing-per-org-year frame) from dev/devdata/_config.R, and adds the
# panel's own folders. Run every script from the package root:
#
#   Rscript dev/partvii-panel/01-classify.R

source("dev/devdata/_config.R")

pv_root  <- file.path(sd_root, paste0("partvii-", EFILE_VERSION))
pv_cls   <- file.path(pv_root, "classified")   # titleclassifier output, one file a year
pv_ppl   <- file.path(pv_root, "people")       # linker input, one file a year
pv_repo  <- "dev/partvii-panel"                # small committed summaries
for (d in c(pv_root, pv_cls, pv_ppl)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

S3_DEST  <- sprintf("s3://nccs-efile/dev/synthid/partvii-%s", EFILE_VERSION)

cls_file <- function(y) file.path(pv_cls, sprintf("classified-%d.parquet", y))
ppl_file <- function(y) file.path(pv_ppl, sprintf("people-%d.parquet", y))

# Installed version and source repo state of each package the build uses.
pkg_info <- function(pkgs = c("titleclassifier", "peopleparser", "panel990", "synthid", "arrow", "data.table")) {
  gh <- normalizePath("..", winslash = "/")
  rbindlist(lapply(pkgs, function(p) {
    d <- packageDescription(p)
    repo <- file.path(gh, p)
    git <- function(...) tryCatch(system2("git", c("-C", shQuote(repo), ...), stdout = TRUE, stderr = FALSE),
                                  error = function(e) NA_character_, warning = function(w) NA_character_)
    is_repo <- dir.exists(file.path(repo, ".git"))
    data.table(package = p, version = d$Version,
               installed = if (!is.null(d$Packaged)) d$Packaged else NA_character_,
               remote_sha = if (!is.null(d$RemoteSha)) d$RemoteSha else NA_character_,
               repo_head = if (is_repo) git("rev-parse", "--short", "HEAD")[1] else NA_character_,
               repo_dirty_files = if (is_repo) length(git("status", "--porcelain", "--untracked-files=no")) else NA_integer_)
  }))
}
