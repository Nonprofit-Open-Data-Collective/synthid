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
               repo_dirty_files = if (is_repo) length(git("status", "--porcelain", "--untracked-files=no")) else NA_integer_,
               # only the files that go into the installed package, not dev/ scripts
               repo_dirty_pkg_files = if (is_repo) length(git("status", "--porcelain", "--untracked-files=no", "--",
                                                              "R", "src", "inst", "data", "DESCRIPTION", "NAMESPACE")) else NA_integer_)
  }))
}

# ---- restart safety for the bucket runners (05-link.R, 05b-profiles.R) -------------
# A runner skips a bucket whose output file exists, so a crash must never leave a
# partial file at the final path. Each bucket's output is written to a temporary
# name in the same folder and renamed into place only once complete; a rename
# within one folder is a single filesystem step, so the final path is either
# absent or whole.
#
# Every start, finish and failure is also appended to run-ledger.csv (full date,
# PID), next to progress.log. When a runner starts it reads the ledger, and any
# bucket whose last event is "started" by a process that no longer exists is
# marked "interrupted" -- the record of what a shutdown killed -- and goes back
# in the queue. Leftover temporary files are removed at the same time.
#
# Both helpers are self-contained (namespaced calls, no globals) so they can be
# passed to a callr child.
write_parquet_atomic <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  arrow::write_parquet(x, tmp, ...)
  if (file.exists(path)) file.remove(path)
  if (!file.rename(tmp, path)) stop("could not rename ", tmp, " to ", path)
  invisible(path)
}

ledger_append <- function(run_dir, step, bucket, event, pid = Sys.getpid(), detail = "") {
  f <- file.path(run_dir, "run-ledger.csv")
  row <- data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), step = step,
                    bucket = bucket, event = event, pid = pid, detail = detail)
  utils::write.table(row, f, sep = ",", append = file.exists(f), col.names = !file.exists(f),
                     row.names = FALSE, qmethod = "double")
}

# Mark buckets killed mid-run as interrupted, clear stray temp files, and
# return the interrupted bucket ids.
ledger_recover <- function(run_dir, step, out_root) {
  tmp <- list.files(out_root, pattern = "[.]tmp-[0-9]+$", recursive = TRUE, full.names = TRUE)
  if (length(tmp)) { file.remove(tmp); stamp("removed %d partial file(s) left by a crash", length(tmp)) }
  f <- file.path(run_dir, "run-ledger.csv")
  if (!file.exists(f)) return(character(0))
  want <- step   # inside lg[...], `step` would mean the column
  lg <- fread(f, colClasses = list(character = "bucket"))
  lg <- lg[lg$step == want]
  if (!nrow(lg)) return(character(0))
  last <- lg[, .SD[.N], by = bucket]
  # ps_handle() errors for a PID that no longer exists; treat that as not running.
  hung <- last[event == "started"][!vapply(pid, function(p)
    tryCatch(ps::ps_is_running(ps::ps_handle(as.integer(p))), error = function(e) FALSE), logical(1))]
  for (i in seq_len(nrow(hung))) {
    ledger_append(run_dir, step, hung$bucket[i], "interrupted", hung$pid[i],
                  paste("started", hung$time[i], "and never finished"))
  }
  if (nrow(hung)) stamp("%d bucket(s) were interrupted by an earlier shutdown and will be re-run: %s",
                        nrow(hung), paste(hung$bucket, collapse = " "))
  hung$bucket
}

# ---- build log: one row per step start / finish, across the whole build cycle -------
# SYNTHID_DIR/partvii-v3_1/build-log.csv records which steps ran, when, on what
# data, and with which titleclassifier / peopleparser / synthid. A step calls
# build_step("02-people") once, after sourcing this file; that writes a
# "started" row now, and a "done" row when the script ends (including an early
# quit()) or a "failed" row with the error message if it stops on an error. A
# step with a "started" row and nothing after it was killed (shutdown, Ctrl-C).
#
# A build cycle is named by build_id. 01-classify.R starts a new one (the date
# and time it started); every later step joins the latest. Set SYNTHID_BUILD to
# name or join a cycle by hand.
#
# Package columns hold version@sha: the GitHub commit an install came from, or
# for a local install (synthid) the repo's HEAD when the step ran, with
# "+N dirty" for uncommitted package files (R/, src/, DESCRIPTION, ...; edits
# under dev/ do not count). For a local install that is the repo state, not
# proof of what was installed: reinstall after committing package changes.
BUILD_LOG <- file.path(pv_root, "build-log.csv")
BUILD_PKGS <- c("titleclassifier", "peopleparser", "synthid")

build_pkg_ids <- function(pkgs = BUILD_PKGS) {
  info <- pkg_info(pkgs)
  id <- ifelse(!is.na(info$remote_sha), substr(info$remote_sha, 1, 7),
               ifelse(is.na(info$repo_head), "unknown",
                      paste0(info$repo_head, ifelse(info$repo_dirty_pkg_files > 0,
                                                     paste0("+", info$repo_dirty_pkg_files, " dirty"), ""))))
  setNames(paste0(info$version, "@", id), info$package)
}

build_current_id <- function(new_cycle = FALSE) {
  env <- Sys.getenv("SYNTHID_BUILD")
  if (nzchar(env)) return(env)
  if (!new_cycle && file.exists(BUILD_LOG)) {
    ids <- fread(BUILD_LOG, select = "build_id", colClasses = "character")$build_id
    if (length(ids)) return(ids[length(ids)])
  }
  format(Sys.time(), "%Y%m%d-%H%M")
}

build_log_row <- function(step, event, build_id, pkgs, secs = NA_real_, detail = "") {
  efile_man <- file.path(sd_efile, "download-manifest.rds")
  row <- data.table(
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), build_id = build_id, step = step,
    event = event, pid = Sys.getpid(), host = Sys.info()[["nodename"]],
    data_version = EFILE_VERSION, years = paste(range(YEARS), collapse = "-"),
    efile_cache = if (file.exists(efile_man)) format(file.mtime(efile_man), "%Y-%m-%d") else NA_character_,
    titleclassifier = pkgs[["titleclassifier"]], peopleparser = pkgs[["peopleparser"]],
    synthid = pkgs[["synthid"]], secs = round(secs), detail = detail)
  fwrite(row, BUILD_LOG, append = file.exists(BUILD_LOG))
}

build_step <- function(step, new_cycle = FALSE) {
  st <- new.env()
  st$step <- step; st$t0 <- Sys.time(); st$closed <- FALSE
  st$id <- build_current_id(new_cycle)
  st$pkgs <- build_pkg_ids()
  build_log_row(step, "started", st$id, st$pkgs, detail = paste(commandArgs(trailingOnly = TRUE), collapse = " "))
  close <- function(event, detail = "") {
    if (st$closed) return(invisible())
    st$closed <- TRUE
    build_log_row(st$step, event, st$id, st$pkgs,
                  as.numeric(Sys.time() - st$t0, units = "secs"), detail)
  }
  # On an uncaught error, a non-interactive R calls options("error") and then
  # stops; .Last runs on a normal end or quit(), and not after q(runLast = FALSE).
  options(error = function() {
    close("failed", gsub("[\r\n]+", " ", geterrmessage()))
    q(save = "no", status = 1, runLast = FALSE)
  })
  assign(".Last", function() close("done"), envir = globalenv())
  stamp("build %s, step %s: %s", st$id, step, paste(names(st$pkgs), st$pkgs, collapse = ", "))
  invisible(st$id)
}
