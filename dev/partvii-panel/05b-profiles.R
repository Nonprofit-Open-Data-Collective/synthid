# 05b-profiles.R
# Person profiles for the whole panel, without re-running the linkage.
#
# build_person_profile() turns the linked records of one EMP_ID into one row --
# the variant sets (surnames and their blocking keys, given names, titles,
# organizations, years, states, metro areas, NTEE codes) that cross-organization
# linkage compares. It is the input to link_cross_org().
#
# 05-link.R can write profiles itself, but only inside link_bucket() and only
# for a bucket it is linking: a bucket whose emp-map already exists is skipped
# whole. Since profiles took 2-3x the linkage (120-170 min a bucket), getting
# them for an already-linked bucket that way means paying for the linkage again.
# This script skips it: emp-map holds the EMP_ID of every record, and
# people-by-ein holds the record, so the profile needs only a join.
#
# That join is exact because PERSON_YEAR_ID is a pure function of OBJECTID and
# TABLE_ID (synthid::person_year_id()), so rebuilding people/ and
# people-by-ein/ -- as adding org.state, org.cbsa and org.ntee requires -- does
# not renumber a row, and an emp-map from before that rebuild still lines up.
# The script asserts that alignment rather than trusting it.
#
# Reads, per bucket:  emp-map/bucket=NN/part-0.parquet       (05-link.R)
#                     people-by-ein/bucket=NN/part-0.parquet (04-bucket.R)
# Writes:             profiles/bucket=NN/part-0.parquet
#                     profile-runs/bucket=NN.rds   timings, counts, versions
#
# A bucket whose profile already carries the blocking columns is skipped, so the
# script can be stopped and re-run. A profile written before those columns
# existed is NOT treated as done: it would silently leave every pass but
# `surname` unable to fire, so it is rebuilt. Profiles are written under a
# temporary name and renamed into place, and starts / finishes / failures go to
# profile-runs/run-ledger.csv, so a forced restart is recorded and re-queued
# (write_parquet_atomic() and ledger_recover() in _panel.R).
#
#   Rscript dev/partvii-panel/05b-profiles.R [workers] [buckets, e.g. 'c("00","37")' or all]

source("dev/partvii-panel/_panel.R")
build_step("05b-profiles")   # build-log.csv
args <- commandArgs(trailingOnly = TRUE)
NW   <- if (length(args) >= 1) as.integer(args[1]) else max(1L, parallel::detectCores(logical = FALSE) - 2L)
BKTS <- if (length(args) >= 2 && args[2] != "all") eval(parse(text = args[2])) else sprintf("%02d", 0:99)

pv_bkt  <- file.path(pv_root, "people-by-ein")
pv_map  <- file.path(pv_root, "emp-map")
pv_prof <- file.path(pv_root, "profiles")
pv_run  <- file.path(pv_root, "profile-runs")
for (d in c(pv_prof, pv_run)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
part <- function(root, b) file.path(root, paste0("bucket=", b), "part-0.parquet")

# The columns build_person_profile() reads: synthid_cols()'s org_id / year /
# name, the parsed name parts, the title, and the three blocking dimensions.
# ein and taxyr come from the bucket, not the emp-map, which carries its own
# copies; the emp-map is read only for the ids.
PROF_COLS <- c("PERSON_YEAR_ID", "ein", "taxyr", "name", "first_name", "middle_name",
               "last_name", "suffix", "gender", "title.standard",
               "org.state", "org.cbsa", "org.ntee")
BLOCK_SETS <- c("states", "cbsa", "ntee")

info <- pkg_info()
fwrite(info, file.path(pv_root, "build-info-profiles.csv"))
print(info)

# A profile is current only if it carries the blocking set columns. Reading the
# parquet schema is enough -- no need to open the data.
profile_is_current <- function(b) {
  f <- part(pv_prof, b)
  if (!file.exists(f)) return(FALSE)
  nm <- tryCatch(names(open_dataset(f)), error = function(e) character(0))
  all(BLOCK_SETS %in% nm)
}

have_map <- BKTS[file.exists(part(pv_map, BKTS))]
if (length(have_map) < length(BKTS)) {
  stamp("no emp-map for %d of %d buckets (link them with 05-link.R first): %s",
        length(BKTS) - length(have_map), length(BKTS),
        paste(setdiff(BKTS, have_map), collapse = " "))
}
interrupted <- ledger_recover(pv_run, "profile", pv_root)
current <- vapply(have_map, profile_is_current, logical(1))
stale <- have_map[file.exists(part(pv_prof, have_map)) & !current]
if (length(stale)) {
  stamp("%d profile(s) predate the blocking columns and will be rebuilt: %s",
        length(stale), paste(stale, collapse = " "))
}
todo <- have_map[!current]
stamp("%d of %d buckets to profile, %d workers", length(todo), length(BKTS), NW)
if (!length(todo)) quit(save = "no")

# ---- order by cost ------------------------------------------------------------------
# Profile cost rises with the records to group and the clusters to summarize, and
# the record count is readable from the parquet footer alone.
cost <- rbindlist(lapply(todo, function(b) data.table(
  bucket = b, records = arrow::ParquetFileReader$create(part(pv_bkt, b))$num_rows)))[order(-records)]
stamp("%s records over %d buckets; largest %s, smallest %s",
      format(sum(cost$records), big.mark = ","), nrow(cost),
      format(max(cost$records), big.mark = ","), format(min(cost$records), big.mark = ","))

# Every value this needs is a parameter: callr serializes the function without
# the parent's global environment, so a reference to PROF_COLS or BLOCK_SETS
# here would be unresolved in the child.
profile_bucket <- function(b, f_bkt, f_map, f_prof, f_run, versions, prof_cols, block_sets, write_atomic) {
  suppressPackageStartupMessages({ library(data.table); library(arrow); library(synthid) })
  t0 <- Sys.time()
  ppl <- as.data.table(read_parquet(f_bkt, col_select = prof_cols))
  map <- as.data.table(read_parquet(f_map, col_select = c("PERSON_YEAR_ID", "EMP_ID", "EMP_ANCHOR")))

  # The emp-map must cover this bucket exactly. A mismatch means the two were
  # built from different panels, and the profile would be silently incomplete.
  if (nrow(map) != nrow(ppl) || anyDuplicated(map$PERSON_YEAR_ID)) {
    stop(sprintf("bucket %s: emp-map has %d rows for %d people rows (duplicates: %s)",
                 b, nrow(map), nrow(ppl), anyDuplicated(map$PERSON_YEAR_ID) > 0L))
  }
  lk <- merge(ppl, map, by = "PERSON_YEAR_ID")
  if (nrow(lk) != nrow(ppl)) {
    stop(sprintf("bucket %s: %d of %d people rows have no EMP_ID -- emp-map is from another build",
                 nrow(ppl) - nrow(lk), nrow(ppl)))
  }
  rm(ppl, map); gc(verbose = FALSE)

  n_emp <- uniqueN(lk$EMP_ID)
  prof <- as.data.table(synthid::build_person_profile(
    as.data.frame(lk), state = "org.state", cbsa = "org.cbsa", ntee = "org.ntee"))
  t_prof <- as.numeric(Sys.time() - t0, units = "secs")

  stopifnot(nrow(prof) == n_emp, !anyDuplicated(prof$EMP_ID),
            "EMP_ANCHOR" %in% names(prof), all(block_sets %in% names(prof)))
  # How many profiles can actually reach the tighter passes: a pass whose
  # component is an empty set emits no key at all.
  cover <- vapply(block_sets, function(k) mean(lengths(prof[[k]]) > 0L), 0)

  write_atomic(prof, f_prof)
  run <- list(bucket = b, records = nrow(lk), persons = nrow(prof),
              profile_secs = t_prof, coverage = cover, versions = versions, date = Sys.time())
  saveRDS(run, f_run)
  c(list(bucket = b, records = nrow(lk), persons = nrow(prof), profile_secs = t_prof),
    as.list(setNames(cover, paste0("has_", block_sets))))
}

# ---- one fresh R process per bucket -------------------------------------------------
# As in 05-link.R: a long-lived worker kept its memory from bucket to bucket and
# the machine paged, so each bucket runs in its own callr process and a new one
# starts only while MIN_FREE_GB is available. Stopping this script stops the
# running buckets too.
MIN_FREE_GB <- as.numeric(Sys.getenv("MIN_FREE_GB", "12"))
free_gb <- function() ps::ps_system_memory()$avail / 1e9
log_line <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", sprintf(...), "\n",
                              file = file.path(pv_run, "progress.log"), append = TRUE)
run_one <- function(b, pv_bkt, pv_map, pv_prof, pv_run, versions, profile_bucket,
                    prof_cols, block_sets, write_atomic) {
  Sys.setenv(OMP_NUM_THREADS = "1"); data.table::setDTthreads(1L)
  f <- function(root) file.path(root, paste0("bucket=", b), "part-0.parquet")
  profile_bucket(b, f(pv_bkt), f(pv_map), f(pv_prof),
                 file.path(pv_run, paste0("bucket=", b, ".rds")), versions,
                 prof_cols, block_sets, write_atomic)
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
    if (is.null(r$error)) {
      log_line("bucket %s done: %d records -> %d persons in %.1f min (state %.3f, cbsa %.3f, ntee %.3f)",
               b, r$records, r$persons, r$profile_secs / 60, r$has_states, r$has_cbsa, r$has_ntee)
      ledger_append(pv_run, "profile", b, "done", p$get_pid(), sprintf("%.1f min", r$profile_secs / 60))
    } else {
      log_line("bucket %s FAILED: %s", b, r$error)
      ledger_append(pv_run, "profile", b, "failed", p$get_pid(), r$error)
    }
    res[[b]] <- r
    running[[b]] <- NULL
  }
  while (length(queue) && length(running) < NW && free_gb() > MIN_FREE_GB) {
    b <- queue[1]; queue <- queue[-1]
    running[[b]] <- callr::r_bg(run_one, args = list(b = b, pv_bkt = pv_bkt, pv_map = pv_map,
                                pv_prof = pv_prof, pv_run = pv_run, versions = info,
                                profile_bucket = profile_bucket, prof_cols = PROF_COLS,
                                block_sets = BLOCK_SETS, write_atomic = write_parquet_atomic),
                                supervise = TRUE)
    log_line("bucket %s started (%d running, %.0f GB free)", b, length(running), free_gb())
    ledger_append(pv_run, "profile", b, "started", running[[b]]$get_pid())
    Sys.sleep(20)   # let the new process load its bucket before checking memory again
  }
  Sys.sleep(15)
}

res <- rbindlist(lapply(res, as.data.table), fill = TRUE)
if ("error" %in% names(res) && any(!is.na(res$error))) {
  print(res[!is.na(error), .(bucket, error)])
}
ok <- if ("error" %in% names(res)) res[is.na(error)] else res
if (nrow(ok)) {
  stamp("profiled %d buckets in %.1f min wall (%.1f min of worker time): %s records -> %s persons",
        nrow(ok), as.numeric(Sys.time() - t0, units = "mins"), sum(ok$profile_secs) / 60,
        format(sum(ok$records), big.mark = ","), format(sum(ok$persons), big.mark = ","))
  # Weighted by persons: the share of profiles that can reach each tighter pass.
  # A low number here means that pass will mostly not fire, whatever the ladder
  # says, and the candidate measurement (see PLAN-cross-org.md) will show it.
  w <- ok$persons / sum(ok$persons)
  stamp("blocking components present: state %.3f, cbsa %.3f, ntee %.3f",
        sum(w * ok$has_states), sum(w * ok$has_cbsa), sum(w * ok$has_ntee))
}
