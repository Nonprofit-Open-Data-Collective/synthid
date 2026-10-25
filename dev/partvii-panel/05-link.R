# 05-link.R
# Within-organization linkage for the whole panel: synthid::link_panel() with
# default settings (threshold 7, weighted; the core_dev baseline in
# dev/devdata FINDINGS.md F-007), one EIN bucket (04-bucket.R) per task.
#
# link_panel() links within an organization, and a bucket holds every year of
# its organizations, so buckets are independent. A bucket's cost grows with
# the sum over its organizations of records^2 (all year pairs), so the
# costliest buckets are sent first and the cluster load-balances the rest.
#
# Writes, per bucket, SYNTHID_DIR/partvii-v3_1/:
#   emp-map/bucket=NN/part-0.parquet   PERSON_YEAR_ID, ein, taxyr, EMP_ID,
#                                      EMP_ANCHOR, EMP_N_RECORDS, EMP_N_YEARS
#   profiles/bucket=NN/part-0.parquet  build_person_profile(): one row per
#                                      EMP_ID, the cross-org input (only with
#                                      profiles = TRUE: it took 2-3x as long
#                                      as the linkage, 120-170 min a bucket)
#   link-runs/bucket=NN.rds            timings, counts, package versions
# A bucket already written is skipped, so the script can be stopped and re-run.
#
#   Rscript dev/partvii-panel/05-link.R [workers] [buckets, e.g. 'c("00","37")' or all] [profiles: TRUE/FALSE]

source("dev/partvii-panel/_panel.R")
args <- commandArgs(trailingOnly = TRUE)
NW   <- if (length(args) >= 1) as.integer(args[1]) else max(1L, parallel::detectCores(logical = FALSE) - 2L)
BKTS <- if (length(args) >= 2 && args[2] != "all") eval(parse(text = args[2])) else sprintf("%02d", 0:99)
PROFILES <- length(args) >= 3 && as.logical(args[3])

pv_bkt  <- file.path(pv_root, "people-by-ein")
pv_map  <- file.path(pv_root, "emp-map")
pv_prof <- file.path(pv_root, "profiles")
pv_run  <- file.path(pv_root, "link-runs")
for (d in c(pv_map, pv_prof, pv_run)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
bkt_in  <- function(b) file.path(pv_bkt, paste0("bucket=", b), "part-0.parquet")
bkt_out <- function(root, b) file.path(root, paste0("bucket=", b), "part-0.parquet")

info <- pkg_info()
fwrite(info, file.path(pv_root, "build-info-link.csv"))
print(info)

todo <- BKTS[!file.exists(bkt_out(pv_map, BKTS))]
stamp("%d of %d buckets to link, %d workers", length(todo), length(BKTS), NW)
if (!length(todo)) quit(save = "no")

# ---- order buckets by estimated cost --------------------------------------------------
cost <- rbindlist(lapply(todo, function(b) {
  e <- read_parquet(bkt_in(b), col_select = "ein")$ein
  n <- as.numeric(table(e))
  data.table(bucket = b, records = length(e), orgs = length(n), max_org = max(n), cost = sum(n^2))
}))[order(-cost)]
stamp("cost per bucket max/mean = %.2f; largest org %s records (bucket %s)",
      max(cost$cost) / mean(cost$cost), format(max(cost$max_org), big.mark = ","),
      cost[which.max(max_org), bucket])

link_bucket <- function(b, f_in, f_map, f_prof, f_run, versions, profiles) {
  suppressPackageStartupMessages({ library(data.table); library(arrow); library(synthid) })
  t0 <- Sys.time()
  people <- as.data.frame(read_parquet(f_in))
  lk <- as.data.table(synthid::link_panel(people))
  t_link <- as.numeric(Sys.time() - t0, units = "secs")
  stopifnot(nrow(lk) == nrow(people), !anyDuplicated(lk$PERSON_YEAR_ID))
  map <- lk[, .(PERSON_YEAR_ID, ein, taxyr, EMP_ID, EMP_ANCHOR, EMP_N_RECORDS, EMP_N_YEARS)]
  setorder(map, ein, taxyr, EMP_ID)
  t_prof <- NA_real_
  if (profiles) {
    t1 <- Sys.time()
    prof <- as.data.table(synthid::build_person_profile(as.data.frame(lk)))
    t_prof <- as.numeric(Sys.time() - t1, units = "secs")
    dir.create(dirname(f_prof), recursive = TRUE, showWarnings = FALSE)
    # profiles first, so an emp-map file always means the bucket is complete
    write_parquet(prof, f_prof)
  }
  dir.create(dirname(f_map), recursive = TRUE, showWarnings = FALSE)
  write_parquet(map, f_map, chunk_size = 100000L)
  run <- list(bucket = b, records = nrow(lk), orgs = uniqueN(lk$ein), persons = uniqueN(lk$EMP_ID),
              link_secs = t_link, profile_secs = t_prof, versions = versions, date = Sys.time())
  saveRDS(run, f_run)
  run[c("bucket", "records", "orgs", "persons", "link_secs", "profile_secs")]
}

# ---- one fresh R process per bucket ------------------------------------------------------
# A long-lived worker kept its memory from bucket to bucket (6-7 GB each after a
# few buckets), the machine paged, and per-bucket time rose from ~45 to ~200 min.
# So each bucket runs in its own callr process, and a new one starts only while
# MIN_FREE_GB of RAM is available. Note that stopping this script stops the
# running buckets too.
MIN_FREE_GB <- 12
free_gb <- function() ps::ps_system_memory()$avail / 1e9
log_line <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", sprintf(...), "\n",
                              file = file.path(pv_run, "progress.log"), append = TRUE)
run_one <- function(b, pv_bkt, pv_map, pv_prof, pv_run, versions, link_bucket, profiles) {
  Sys.setenv(OMP_NUM_THREADS = "1"); data.table::setDTthreads(1L)
  f <- function(root) file.path(root, paste0("bucket=", b), "part-0.parquet")
  link_bucket(b, f(pv_bkt), f(pv_map), f(pv_prof), file.path(pv_run, paste0("bucket=", b, ".rds")), versions, profiles)
}

t0 <- Sys.time()
queue <- cost$bucket
running <- list()
res <- list()
while (length(queue) || length(running)) {
  for (b in names(running)) {
    p <- running[[b]]
    if (p$is_alive()) next
    r <- tryCatch(p$get_result(), error = function(e) list(bucket = b, error = conditionMessage(e)))
    if (is.null(r$error)) log_line("bucket %s done: %d records -> %d persons, link %.1f min", b, r$records, r$persons, r$link_secs / 60)
    else log_line("bucket %s FAILED: %s", b, r$error)
    res[[b]] <- r
    running[[b]] <- NULL
  }
  while (length(queue) && length(running) < NW && free_gb() > MIN_FREE_GB) {
    b <- queue[1]; queue <- queue[-1]
    running[[b]] <- callr::r_bg(run_one, args = list(b = b, pv_bkt = pv_bkt, pv_map = pv_map, pv_prof = pv_prof,
                                pv_run = pv_run, versions = info, link_bucket = link_bucket, profiles = PROFILES),
                                supervise = TRUE)
    log_line("bucket %s started (%d running, %.0f GB free)", b, length(running), free_gb())
    Sys.sleep(20)   # let the new process load its bucket before checking memory again
  }
  Sys.sleep(15)
}

res <- rbindlist(lapply(res, as.data.table), fill = TRUE)
if ("error" %in% names(res) && any(!is.na(res$error))) {
  print(res[!is.na(error), .(bucket, error)])
}
ok <- if ("error" %in% names(res)) res[is.na(error)] else res
stamp("linked %d buckets in %.1f min wall (%.1f min of worker time): %s records -> %s persons",
      nrow(ok), as.numeric(Sys.time() - t0, units = "mins"), sum(ok$link_secs, ok$profile_secs, na.rm = TRUE) / 60,
      format(sum(ok$records), big.mark = ","), format(sum(ok$persons), big.mark = ","))
