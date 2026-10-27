# 07-xorg-csa.R
# Step 8 of PLAN-cross-org.md, blocked by geography: score every pair of
# people (within-org EMP_IDs in different organizations) who share a surname
# key and a geography unit.
#
#   unit = the CSA, for organizations in a CBSA that belongs to a CSA
#          the CBSA, for a CBSA outside any CSA
#          the state, for organizations outside any CBSA (rural)
#
# No NTEE in the block: the CSA 450 test (06b) found 74% of matches cross NTEE
# groups. Pairs across two units (Raleigh vs Charlotte) are not compared.
# Blocks over MAX_BLOCK people are skipped and reported (common surnames in the
# largest CSAs).
#
# Part 1  prep, once: regroup the 98 profile files into N_BATCHES surname-key
#         batches (keys assigned greedily by step 6's comparison cost), one
#         file per profile bucket per batch, plus population surname weights.
# Part 2  per batch, one callr process each, behind a free-memory gate: build
#         the (unit, key) blocks, form pairs, and score them in chunks of CHUNK
#         pairs with synthid::score_candidate_pairs(). Keeps pairs scoring
#         >= SCORE_FLOOR with flags same_cbsa / same_ntee / same_state, so the
#         CBSA, NTEE and threshold variants can be taken later without
#         rescoring. A finished chunk is never redone: a crash loses at most
#         one chunk per worker. Ledger: runs/run-ledger.csv.
# Part 3  when every batch is done: merge, keep each pair once (best score),
#         and summarize by unit.
#
# Writes SYNTHID_DIR/partvii-v3_1/xorg-csa/{prep,scores,runs}/, pairs.parquet,
#        and dev/partvii-panel/xorg-csa-summary.csv
#
#   Rscript dev/partvii-panel/07-xorg-csa.R [workers=20] [n_batches=256]
#
# Test on a few units without touching the full run:
#   ONLY_UNITS=csa450 XORG_OUT=<dir> Rscript dev/partvii-panel/07-xorg-csa.R

source("dev/partvii-panel/_panel.R")
build_step("07-xorg-csa")
args <- commandArgs(trailingOnly = TRUE)
NW <- if (length(args) >= 1) as.integer(args[1]) else 20L
NB <- if (length(args) >= 2) as.integer(args[2]) else 256L
MAX_BLOCK <- 5000L; SCORE_FLOOR <- 3; CHUNK <- 500000L
ONLY <- strsplit(Sys.getenv("ONLY_UNITS"), ",")[[1]]
MIN_FREE_GB <- as.numeric(Sys.getenv("MIN_FREE_GB", "12"))

ROOT <- file.path(pv_root, "xorg-csa")
PREP <- file.path(ROOT, "prep")                                   # shared by test and full runs
OUT  <- Sys.getenv("XORG_OUT", ROOT)
SCORES <- file.path(OUT, "scores"); RUNS <- file.path(OUT, "runs")
for (dd in c(PREP, SCORES, RUNS, file.path(PREP, "counts"))) dir.create(dd, recursive = TRUE, showWarnings = FALSE)
pv_prof <- file.path(pv_root, "profiles")
BKTS <- sort(sub("^bucket=", "", basename(dirname(Sys.glob(file.path(pv_prof, "bucket=*", "part-0.parquet"))))))
bname <- function(b) sprintf("%03d", as.integer(b))
log_line <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", sprintf(...), "\n",
                              file = file.path(RUNS, "progress.log"), append = TRUE)
free_gb <- function() ps::ps_system_memory()$avail / 1e9
stamp("%d workers, %d batches, blocks of up to %d people, keeping scores >= %g%s", NW, NB, MAX_BLOCK, SCORE_FLOOR,
      if (length(ONLY)) paste0("; test units: ", paste(ONLY, collapse = ",")) else "")

# ---- geography map and surname-key batches --------------------------------------------
map_file <- file.path(PREP, "prep-map.rds")
if (!file.exists(map_file)) {
  dl <- as.data.table(readxl::read_excel(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/cbsa/list1_2023.xlsx"), skip = 2))
  xw <- unique(dl[!is.na(`CBSA Code`) & !is.na(`CSA Code`), .(cbsa = `CBSA Code`, csa = `CSA Code`)])
  cost <- fread(file.path(pv_repo, "blocks-surname-cost.csv"))[order(-comparisons)]
  load <- numeric(NB); kb <- integer(nrow(cost))
  for (i in seq_len(nrow(cost))) { j <- which.min(load); kb[i] <- j - 1L; load[j] <- load[j] + cost$comparisons[i] }
  saveRDS(list(csa_of = setNames(xw$csa, xw$cbsa), key_batch = setNames(kb, cost$surname_key),
               batch_cost = setNames(load, bname(seq_len(NB) - 1L)), NB = NB), map_file)
}
MAP <- readRDS(map_file)
stopifnot(MAP$NB == NB)

# ---- part 1: prep ------------------------------------------------------------------------
prep_bucket <- function(b, f_prof, prep_dir, map_file, write_atomic) {
  suppressPackageStartupMessages({ library(arrow); library(data.table) })
  t0 <- Sys.time(); m <- readRDS(map_file)
  plain <- function(v) { v <- unclass(v); attr(v, "ptype") <- NULL; v }
  p <- as.data.table(read_parquet(f_prof, col_select = c("EMP_ID", "orgs", "last_name", "last_name_keys",
        "last_name_variants", "first_name_variants", "middle_initials", "suffixes", "gender", "states", "cbsa", "ntee")))
  for (cn in names(p)) if (is.list(p[[cn]])) set(p, j = cn, value = plain(p[[cn]]))
  s <- toupper(trimws(as.character(p$last_name))); s <- s[!is.na(s) & nzchar(s)]
  counts <- table(s)
  org1 <- vapply(p$orgs, function(o) if (length(o)) o[[1]] else NA_character_, "")
  units <- lapply(seq_len(nrow(p)), function(i) {
    cb <- p$cbsa[[i]]; cb <- cb[!is.na(cb) & nzchar(cb)]
    if (length(cb)) unique(ifelse(cb %in% names(m$csa_of), paste0("csa", m$csa_of[cb]), paste0("cbsa", cb)))
    else { st <- p$states[[i]]; st <- st[!is.na(st) & nzchar(st)]; if (length(st)) paste0("st", unique(st)) else character(0) }
  })
  nt <- lapply(p$ntee, function(v) { v <- v[!is.na(v) & nzchar(v) & !startsWith(v, "UND")]; unique(substr(v, 1, 1)) })
  keep <- lengths(units) > 0 & lengths(p$last_name_keys) > 0 & !is.na(org1)
  idx <- rep(which(keep), lengths(p$last_name_keys)[keep])
  key <- unlist(p$last_name_keys[keep], use.names = FALSE)
  bt <- unname(m$key_batch[key])
  miss <- is.na(bt)
  if (any(miss)) bt[miss] <- vapply(key[miss], function(k) strtoi(substr(rlang::hash(k), 1, 7), 16L) %% m$NB, 0)
  x <- data.table(skey = key, batch = sprintf("%03d", as.integer(bt)), EMP_ID = p$EMP_ID[idx], org = org1[idx],
                  last_name = p$last_name[idx], gender = p$gender[idx])
  for (cn in c("last_name_variants", "first_name_variants", "middle_initials", "suffixes", "states", "cbsa"))
    set(x, j = cn, value = p[[cn]][idx])
  x[, `:=`(units = units[idx], ntee_l = nt[idx])]
  for (bt_i in unique(x$batch))
    write_atomic(x[batch == bt_i, !"batch"], file.path(prep_dir, paste0("batch=", bt_i), paste0("bucket=", b, ".parquet")))
  saveRDS(counts, file.path(prep_dir, "counts", paste0("bucket=", b, ".rds")))   # written last: the done marker
  list(bucket = b, persons = nrow(p), rows = nrow(x), secs = as.numeric(Sys.time() - t0, units = "secs"))
}

run_pool <- function(todo, step, launch, describe) {
  queue <- todo; running <- list(); failed <- character(0)
  while (length(queue) || length(running)) {
    for (k in names(running)) {
      pr <- running[[k]]
      if (pr$is_alive()) next
      r <- tryCatch(pr$get_result(), error = function(e) list(error = conditionMessage(e)))
      if (is.null(r$error)) { log_line("%s %s done: %s", step, k, describe(r)); ledger_append(RUNS, step, k, "done", pr$get_pid(), describe(r)) }
      else { log_line("%s %s FAILED: %s", step, k, r$error); ledger_append(RUNS, step, k, "failed", pr$get_pid(), substr(r$error, 1, 300)); failed <- c(failed, k) }
      running[[k]] <- NULL
    }
    while (length(queue) && length(running) < NW && free_gb() > MIN_FREE_GB) {
      k <- queue[1]; queue <- queue[-1]
      running[[k]] <- launch(k)
      ledger_append(RUNS, step, k, "started", running[[k]]$get_pid())
      log_line("%s %s started (%d running, %.0f GB free, %d queued)", step, k, length(running), free_gb(), length(queue))
      Sys.sleep(1)
    }
    Sys.sleep(5)
  }
  failed
}

ledger_recover(RUNS, "prep", PREP)
todo <- BKTS[!file.exists(file.path(PREP, "counts", paste0("bucket=", BKTS, ".rds")))]
stamp("prep: %d of %d profile buckets to regroup", length(todo), length(BKTS))
if (length(todo)) {
  failed <- run_pool(todo, "prep", function(b) callr::r_bg(prep_bucket,
    args = list(b = b, f_prof = file.path(pv_prof, paste0("bucket=", b), "part-0.parquet"), prep_dir = PREP,
                map_file = map_file, write_atomic = write_parquet_atomic), supervise = TRUE),
    function(r) sprintf("%d persons -> %d rows in %.1f min", r$persons, r$rows, r$secs / 60))
  if (length(failed)) stop("prep failed for bucket(s) ", paste(failed, collapse = " "), "; re-run to retry")
}
sw_file <- file.path(PREP, "surname-weight.rds")
if (!file.exists(sw_file)) {
  cnt <- rbindlist(lapply(Sys.glob(file.path(PREP, "counts", "*.rds")), function(f) {
    t <- readRDS(f); data.table(s = names(t), n = as.integer(t)) }))[, .(n = sum(n)), by = s]
  N <- sum(cnt$n)   # population_surname_weight() over all profiles
  saveRDS(setNames(pmin(pmax(log(N / cnt$n) / log(N), 0), 1), cnt$s), sw_file)
  stamp("surname weights for %s surnames over %s people", format(nrow(cnt), big.mark = ","), format(N, big.mark = ","))
}

# ---- part 2: score each batch ------------------------------------------------------------
score_batch <- function(bt, prep_dir, out_dir, sw_file, max_block, floor, chunk, only, log_file, write_atomic) {
  suppressPackageStartupMessages({ library(arrow); library(data.table); library(synthid) })
  t0 <- Sys.time()
  say <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", sprintf(...), "\n", file = log_file, append = TRUE)
  plain <- function(v) { v <- unclass(v); attr(v, "ptype") <- NULL; v }
  x <- rbindlist(lapply(Sys.glob(file.path(prep_dir, paste0("batch=", bt), "*.parquet")), function(f) {
    d <- as.data.table(read_parquet(f)); for (cn in names(d)) if (is.list(d[[cn]])) set(d, j = cn, value = plain(d[[cn]])); d }))
  P <- x[!duplicated(EMP_ID)]
  K <- x[, .(EMP_ID, skey, units)][, .(unit = unlist(units)), by = .(EMP_ID, skey)]
  if (length(only)) K <- K[unit %in% only]
  K <- unique(K)[, blk := paste(unit, skey)]
  sz <- K[, .N, by = .(blk, unit)]
  dropped <- sz[N > max_block]
  K <- K[blk %in% sz[N >= 2 & N <= max_block, blk], .(EMP_ID, blk, unit)]
  pr <- K[K, on = "blk", allow.cartesian = TRUE, nomatch = 0][EMP_ID < i.EMP_ID, .(emp_a = EMP_ID, emp_b = i.EMP_ID, unit)]
  rm(K); invisible(gc())
  pr <- pr[!duplicated(pr, by = c("emp_a", "emp_b"))]
  org <- setNames(P$org, P$EMP_ID)
  pr <- pr[org[emp_a] != org[emp_b]]
  setorder(pr, emp_a, emp_b)
  n_chunks <- max(1L, ceiling(nrow(pr) / chunk))
  dir_b <- file.path(out_dir, paste0("batch=", bt))
  dir.create(dir_b, recursive = TRUE, showWarnings = FALSE)
  say("batch %s: %d people, %d pairs in %d chunk(s), %d blocks over %d skipped", bt, nrow(P), nrow(pr), n_chunks, nrow(dropped), max_block)
  prof <- as.data.frame(P[, .(EMP_ID, last_name, last_name_variants, first_name_variants, middle_initials, suffixes, gender)])
  sw <- readRDS(sw_file)
  overlap <- function(L, a, b) mapply(function(u, v) length(intersect(u, v)) > 0, L[a], L[b], USE.NAMES = FALSE)
  cb_of <- setNames(P$cbsa, P$EMP_ID); nt_of <- setNames(P$ntee_l, P$EMP_ID); st_of <- setNames(P$states, P$EMP_ID)
  kept <- 0L
  for (k in seq_len(n_chunks)) {
    f_k <- file.path(dir_b, sprintf("chunk-%04d.parquet", k))
    if (file.exists(f_k)) next
    rows <- if (nrow(pr)) ((k - 1L) * chunk + 1L):min(nrow(pr), k * chunk) else integer(0)
    cc <- pr[rows]
    sc <- if (nrow(cc)) as.data.table(synthid::score_candidate_pairs(as.data.frame(cc[, .(emp_a, emp_b)]), prof, surname_weight = sw))[, .(emp_a, emp_b, score)] else
      data.table(emp_a = character(0), emp_b = character(0), score = numeric(0))
    sc <- sc[!is.na(score) & score >= floor]
    sc[cc, on = c("emp_a", "emp_b"), unit := i.unit]
    sc[, `:=`(same_cbsa = overlap(cb_of, emp_a, emp_b), same_ntee = overlap(nt_of, emp_a, emp_b),
              same_state = overlap(st_of, emp_a, emp_b))]
    write_atomic(as.data.frame(sc), f_k)
    kept <- kept + nrow(sc)
    say("batch %s chunk %d/%d: %d pairs, %d kept", bt, k, n_chunks, nrow(cc), nrow(sc))
  }
  s <- list(batch = bt, people = nrow(P), pairs = nrow(pr), chunks = n_chunks, kept = kept,
            dropped_blocks = nrow(dropped), dropped_people = sum(dropped$N),
            dropped_comparisons = sum(as.numeric(dropped$N) * (dropped$N - 1) / 2),
            secs = as.numeric(Sys.time() - t0, units = "secs"))
  saveRDS(list(summary = s, dropped = dropped), file.path(dir_b, "_done.rds"))   # the batch's done marker
  s
}

ledger_recover(RUNS, "score", SCORES)
B <- bname(seq_len(NB) - 1L)
B <- B[order(-MAP$batch_cost[B])]                               # costliest first
todo <- B[!file.exists(file.path(SCORES, paste0("batch=", B), "_done.rds"))]
stamp("score: %d of %d batches to run", length(todo), NB)
if (length(todo)) {
  failed <- run_pool(todo, "score", function(bt) callr::r_bg(score_batch,
    args = list(bt = bt, prep_dir = PREP, out_dir = SCORES, sw_file = sw_file, max_block = MAX_BLOCK, floor = SCORE_FLOOR,
                chunk = CHUNK, only = ONLY, log_file = file.path(RUNS, "progress.log"), write_atomic = write_parquet_atomic),
    supervise = TRUE),
    function(r) sprintf("%d pairs, %d kept, %d blocks skipped, %.0f min", r$pairs, r$kept, r$dropped_blocks, r$secs / 60))
  if (length(failed)) stop("scoring failed for batch(es) ", paste(failed, collapse = " "), "; re-run to retry (finished chunks are kept)")
}

# ---- part 3: merge -----------------------------------------------------------------------
done <- lapply(Sys.glob(file.path(SCORES, "batch=*", "_done.rds")), readRDS)
summ <- rbindlist(lapply(done, `[[`, "summary"))
drop <- rbindlist(lapply(done, `[[`, "dropped"))
fwrite(summ, file.path(OUT, "batch-summary.csv")); fwrite(drop, file.path(OUT, "dropped-blocks.csv"))
pairs <- rbindlist(lapply(Sys.glob(file.path(SCORES, "batch=*", "chunk-*.parquet")), function(f) as.data.table(read_parquet(f))))
setorder(pairs, emp_a, emp_b, -score)
pairs <- pairs[!duplicated(pairs, by = c("emp_a", "emp_b"))]   # a pair seen under two surname keys
write_parquet_atomic(as.data.frame(pairs), file.path(OUT, "pairs.parquet"))
by_unit <- pairs[, .(pairs_kept = .N, score_7 = sum(score >= 7), score_8 = sum(score >= 8), score_10 = sum(score >= 10),
                     score_7_same_cbsa = sum(score >= 7 & same_cbsa), score_7_same_ntee = sum(score >= 7 & same_ntee)), by = unit][order(-score_7)]
stamp("%s pairs scored over %d batches in %.1f worker-hours; %s kept at score >= %g; %s at >= 7, %s at >= 10; %d blocks skipped (%s comparisons)",
      format(sum(summ$pairs), big.mark = ","), nrow(summ), sum(summ$secs) / 3600, format(nrow(pairs), big.mark = ","),
      SCORE_FLOOR, format(sum(pairs$score >= 7), big.mark = ","), format(sum(pairs$score >= 10), big.mark = ","),
      nrow(drop), format(sum(summ$dropped_comparisons), big.mark = ","))
if (!length(ONLY)) fwrite(by_unit, file.path(pv_repo, "xorg-csa-summary.csv")) else print(by_unit)
