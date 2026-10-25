# 05-status.R
# Where did 05-link.R get to? Reads only what the linkage already wrote, so it
# is safe to run while a link is in flight (or after one was interrupted).
#
# For each of the 100 EIN buckets it reports whether emp-map, profiles and the
# link-run record exist, and checks that each emp-map file opens and holds as
# many rows as its people-by-ein input. 05-link.R decides what to re-run from
# the presence of the emp-map file alone, so a file that was truncated when the
# process died would be skipped on a re-run: those are listed as BAD and the
# fix is to delete them before re-running.
#
#   Rscript dev/partvii-panel/05-status.R

source("dev/partvii-panel/_panel.R")

pv_bkt  <- file.path(pv_root, "people-by-ein")
pv_map  <- file.path(pv_root, "emp-map")
pv_prof <- file.path(pv_root, "profiles")
pv_run  <- file.path(pv_root, "link-runs")
BKTS    <- sprintf("%02d", 0:99)
part    <- function(root, b) file.path(root, paste0("bucket=", b), "part-0.parquet")

n_rows <- function(f) tryCatch(nrow(open_dataset(f)), error = function(e) NA_integer_)

st <- rbindlist(lapply(BKTS, function(b) {
  f_map  <- part(pv_map, b)
  f_prof <- part(pv_prof, b)
  f_run  <- file.path(pv_run, paste0("bucket=", b, ".rds"))
  run    <- if (file.exists(f_run)) readRDS(f_run) else NULL
  data.table(
    bucket      = b,
    in_rows     = n_rows(part(pv_bkt, b)),
    map         = file.exists(f_map),
    map_rows    = if (file.exists(f_map)) n_rows(f_map) else NA_integer_,
    map_mtime   = if (file.exists(f_map)) file.mtime(f_map) else as.POSIXct(NA),
    prof        = file.exists(f_prof),
    prof_rows   = if (file.exists(f_prof)) n_rows(f_prof) else NA_integer_,
    run         = !is.null(run),
    persons     = if (!is.null(run)) run$persons else NA_integer_,
    link_min    = if (!is.null(run)) run$link_secs / 60 else NA_real_,
    profile_min = if (!is.null(run) && !is.na(run$profile_secs)) run$profile_secs / 60 else NA_real_
  )
}))

st[, status := fifelse(!map, "todo",
              fifelse(is.na(map_rows) | map_rows != in_rows, "BAD", "done"))]

stamp("%d of 100 buckets done, %d to link, %d BAD (truncated or unreadable)",
      sum(st$status == "done"), sum(st$status == "todo"), sum(st$status == "BAD"))
stamp("%s of %s input rows linked; %s persons so far",
      format(sum(st[status == "done", in_rows]), big.mark = ","),
      format(sum(st$in_rows, na.rm = TRUE), big.mark = ","),
      format(sum(st$persons, na.rm = TRUE), big.mark = ","))
stamp("profiles written for %d of the %d done buckets", sum(st$prof & st$status == "done"),
      sum(st$status == "done"))
if (any(st$status == "done")) {
  stamp("link %.1f min a bucket (median), last emp-map written %s",
        median(st[status == "done", link_min], na.rm = TRUE),
        format(max(st$map_mtime, na.rm = TRUE), "%Y-%m-%d %H:%M"))
}

if (any(st$status == "BAD")) {
  cat("\nBAD buckets (delete these emp-map files, then re-run 05-link.R):\n")
  print(st[status == "BAD", .(bucket, in_rows, map_rows, map_mtime)])
  cat("\n  file.remove(c(", paste0('"', st[status == "BAD", part(pv_map, bucket)], '"', collapse = ",\n    "),
      "))\n", sep = "")
}

if (any(st$status != "done")) {
  cat("\nStill to link:", paste(st[status != "done", bucket], collapse = " "), "\n")
}

fwrite(st, file.path(pv_repo, "link-status.csv"))
stamp("wrote %s", file.path(pv_repo, "link-status.csv"))

log <- file.path(pv_run, "progress.log")
if (file.exists(log)) {
  cat("\nLast 25 lines of", log, ":\n")
  writeLines(tail(readLines(log, warn = FALSE), 25))
}
