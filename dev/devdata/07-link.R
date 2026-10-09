# 07-link.R
# Baseline linkage: run synthid::link_panel() with default settings on one
# cohort of the development dataset (default core_dev).
#
# link_panel() links within an organization, so organizations are independent
# and are spread over worker processes. The cost of an organization grows with
# the square of its records (all year pairs), so chunks are balanced on
# sum(records^2) with a greedy longest-first assignment.
#
# Writes SYNTHID_DIR/linked/<cohort>.parquet: people.parquet's rows for the
# cohort, plus EMP_ID, EMP_ANCHOR, EMP_N_RECORDS, EMP_N_YEARS. Also writes
# SYNTHID_DIR/linked/<cohort>-run.rds with timings and the package versions.
#
#   Rscript dev/devdata/07-link.R [cohort] [workers] [n_orgs]
# n_orgs (optional) links only the first n organizations, for a pilot.

source("dev/devdata/_config.R")
args    <- commandArgs(trailingOnly = TRUE)
COHORT  <- if (length(args) >= 1) args[1] else "core_dev"
NW      <- if (length(args) >= 2) as.integer(args[2]) else max(1L, parallel::detectCores(logical = FALSE) - 2L)
N_ORGS  <- if (length(args) >= 3) as.integer(args[3]) else NA_integer_
out_dir <- file.path(sd_root, "linked"); dir.create(out_dir, showWarnings = FALSE)
tag     <- if (is.na(N_ORGS)) COHORT else sprintf("%s-pilot%d", COHORT, N_ORGS)

people <- as.data.table(read_parquet(file.path(sd_data, "people.parquet")))
people <- people[cohort == COHORT]
if (!is.na(N_ORGS)) people <- people[ein %in% head(sort(unique(ein)), N_ORGS)]
stamp("%s: %s records, %d orgs, %d workers", tag, format(nrow(people), big.mark = ","),
      uniqueN(people$ein), NW)

# ---- balance orgs over chunks on sum(n^2) --------------------------------------------
load <- people[, .(n = .N), by = ein][, cost := as.numeric(n)^2][order(-cost)]
n_chunks <- NW * 4L
bins <- numeric(n_chunks); load[, chunk := 0L]
for (i in seq_len(nrow(load))) { b <- which.min(bins); bins[b] <- bins[b] + load$cost[i]; set(load, i, "chunk", b) }
chunks <- split(load$ein, load$chunk)
stamp("largest org: %d records; chunk cost max/mean = %.2f", max(load$n), max(bins) / mean(bins))

cl <- parallel::makeCluster(NW)
on.exit(parallel::stopCluster(cl), add = TRUE)
parallel::clusterEvalQ(cl, {
  Sys.setenv(OMP_NUM_THREADS = "1")
  suppressPackageStartupMessages({ library(data.table); library(synthid) })
  data.table::setDTthreads(1L)
  NULL
})
link_chunk <- function(sub) {
  t0 <- Sys.time()
  lk <- synthid::link_panel(as.data.frame(sub))
  out <- data.table::as.data.table(lk)[, c("PERSON_YEAR_ID", "EMP_ID", "EMP_ANCHOR", "EMP_N_RECORDS", "EMP_N_YEARS"), with = FALSE]
  attr(out, "secs") <- as.numeric(Sys.time() - t0, units = "secs")
  out
}
pieces <- lapply(chunks, function(e) people[ein %in% e])

t0 <- Sys.time()
res <- parallel::parLapplyLB(cl, pieces, link_chunk)
wall <- as.numeric(Sys.time() - t0, units = "secs")
chunk_secs <- vapply(res, function(x) attr(x, "secs"), numeric(1))

ids <- rbindlist(res)
stopifnot(nrow(ids) == nrow(people), !anyDuplicated(ids$PERSON_YEAR_ID))
linked <- merge(people, ids, by = "PERSON_YEAR_ID")
setorder(linked, ein, taxyr, OBJECTID, TABLE_ID)
write_parquet(linked, file.path(out_dir, paste0(tag, ".parquet")))

run <- list(cohort = COHORT, tag = tag, n_records = nrow(linked), n_orgs = uniqueN(linked$ein),
            n_persons = uniqueN(linked$EMP_ID), workers = NW, wall_secs = wall,
            cpu_secs = sum(chunk_secs), chunk_secs = chunk_secs,
            synthid = as.character(packageVersion("synthid")),
            synthid_commit = tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
                                      error = function(e) NA_character_),
            titleclassifier = packageDescription("titleclassifier")$RemoteSha,
            date = Sys.time())
saveRDS(run, file.path(out_dir, paste0(tag, "-run.rds")))
stamp("linked in %.1f min wall (%.1f min of worker time): %s records -> %s persons (%.1f%% compression)",
      wall / 60, sum(chunk_secs) / 60, format(run$n_records, big.mark = ","),
      format(run$n_persons, big.mark = ","), 100 * (1 - run$n_persons / run$n_records))
