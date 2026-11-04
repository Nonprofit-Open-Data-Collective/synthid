# 07b-xorg-big-blocks.R
# Score the blocks 07-xorg-csa.R skipped for being over MAX_BLOCK people (common
# surname keys such as SMITH, BROWN, LEE in New York, Washington, Los Angeles
# and San Jose), then merge them into the national pairs file.
#
# 07 scores a whole surname-key batch in one process, so a single big block
# would run for many hours on one core. Here each block is cut into chunks of
# about CHUNK pairs (a run of "a" people, each paired with every later person in
# the block), and the chunks are spread over NW workers. Scoring, the score
# floor and the same_cbsa / same_ntee / same_state flags are the same as 07.
#
# A finished chunk is never redone, so a re-run picks up where it stopped. The
# merge keeps the old national file as pairs-before-big-blocks.parquet and
# writes pairs.parquet with each pair once (best score), as 07 does.
#
# Writes SYNTHID_DIR/partvii-v3_1/xorg-csa/scores-big/blk=<unit>_<key>/chunk-*.parquet,
#        xorg-csa/pairs.parquet, and dev/partvii-panel/xorg-csa-summary.csv
#
#   Rscript dev/partvii-panel/07b-xorg-big-blocks.R [workers=20] [blocks=all, e.g. csa488_L000] [merge=1]

source("dev/partvii-panel/_panel.R")
build_step("07b-xorg-big-blocks")
args <- commandArgs(trailingOnly = TRUE)
NW <- if (length(args) >= 1) as.integer(args[1]) else 20L
ONLY <- if (length(args) >= 2 && nzchar(args[2]) && args[2] != "all") strsplit(args[2], ",")[[1]] else character(0)
MERGE <- if (length(args) >= 3) args[3] == "1" else TRUE
SCORE_FLOOR <- 3; CHUNK <- 500000L

ROOT <- file.path(pv_root, "xorg-csa"); PREP <- file.path(ROOT, "prep"); BIG <- file.path(ROOT, "scores-big")
dir.create(BIG, showWarnings = FALSE)
MAP <- readRDS(file.path(PREP, "prep-map.rds"))
sw_file <- file.path(PREP, "surname-weight.rds")
plain <- function(v) { v <- unclass(v); attr(v, "ptype") <- NULL; v }

drop <- fread(file.path(ROOT, "dropped-blocks.csv"))
drop[, `:=`(skey = sub("^\\S+ ", "", blk), id = paste0(unit, "_", sub("^\\S+ ", "", blk)))]
if (length(ONLY)) drop <- drop[id %in% ONLY]
drop[, batch := sprintf("%03d", as.integer(MAP$key_batch[skey]))]
stopifnot(!anyNA(MAP$key_batch[drop$skey]))
stamp("%d skipped blocks, %s people, %s comparisons before removing same-org pairs", nrow(drop),
      format(sum(drop$N), big.mark = ","), format(sum(drop$N * (drop$N - 1) / 2), big.mark = ","))

# ---- people per block, and the chunk plan ------------------------------------------------
tasks <- list()
for (bt in unique(drop$batch)) {
  db <- drop[batch == bt]
  x <- rbindlist(lapply(Sys.glob(file.path(PREP, paste0("batch=", bt), "*.parquet")), function(f) {
    d <- as.data.table(arrow::read_parquet(f))[skey %in% db$skey]
    for (cn in names(d)) if (is.list(d[[cn]])) set(d, j = cn, value = plain(d[[cn]])); d }))
  for (i in seq_len(nrow(db))) {
    bdir <- file.path(BIG, paste0("blk=", db$id[i])); dir.create(bdir, showWarnings = FALSE)
    pf <- file.path(bdir, "people.rds")
    if (!file.exists(pf)) {
      u <- db$unit[i]
      P <- x[skey == db$skey[i] & vapply(units, function(v) u %in% v, TRUE)][!duplicated(EMP_ID)][order(EMP_ID)]
      if (nrow(P) != db$N[i]) stop(sprintf("block %s: %d people found, %d expected", db$id[i], nrow(P), db$N[i]))
      saveRDS(P, pf)
    } else P <- readRDS(pf)
    n <- nrow(P); cum <- cumsum(as.numeric(n - seq_len(n)))   # pairs from a = 1..i
    k <- pmax(1L, ceiling(cum / CHUNK)); k <- match(k, unique(k))
    a <- split(seq_len(n), k)
    tasks[[length(tasks) + 1]] <- data.table(id = db$id[i], unit = db$unit[i], k = seq_along(a),
                                             a_from = vapply(a, min, 0L), a_to = vapply(a, max, 0L))
  }
  rm(x); invisible(gc())
}
tasks <- rbindlist(tasks)
tasks[, out := file.path(BIG, paste0("blk=", id), sprintf("chunk-%04d.parquet", k))]
todo <- tasks[!file.exists(out)]
setorder(todo, id, k)   # in block order, so a worker usually keeps the same block's people loaded
todo <- head(todo, as.integer(Sys.getenv("MAX_CHUNKS", "1000000")))   # MAX_CHUNKS=2 for a quick test
stamp("%d chunks over %d blocks; %d to run on %d workers", nrow(tasks), uniqueN(tasks$id), nrow(todo), NW)

# ---- score ------------------------------------------------------------------------------------
score_chunk <- function(t, BIG, sw_file, floor, write_atomic) {
  suppressPackageStartupMessages({ library(data.table); library(synthid) })
  t0 <- Sys.time()
  pf <- file.path(BIG, paste0("blk=", t$id), "people.rds")
  if (!identical(.GlobalEnv$.cur_blk, t$id)) {
    assign(".P", readRDS(pf), envir = .GlobalEnv); assign(".cur_blk", t$id, envir = .GlobalEnv)
  }
  if (is.null(.GlobalEnv$.sw)) assign(".sw", readRDS(sw_file), envir = .GlobalEnv)
  P <- .GlobalEnv$.P; n <- nrow(P)
  ia <- rep(t$a_from:t$a_to, n - (t$a_from:t$a_to))
  ib <- unlist(lapply(t$a_from:t$a_to, function(a) if (a < n) (a + 1L):n else integer(0)))
  keep <- P$org[ia] != P$org[ib]; ia <- ia[keep]; ib <- ib[keep]
  cc <- data.table(emp_a = P$EMP_ID[ia], emp_b = P$EMP_ID[ib])
  prof <- as.data.frame(P[, .(EMP_ID, last_name, last_name_variants, first_name_variants, middle_initials, suffixes, gender)])
  sc <- if (nrow(cc)) as.data.table(synthid::score_candidate_pairs(as.data.frame(cc), prof, surname_weight = .GlobalEnv$.sw))[, .(emp_a, emp_b, score)] else
    data.table(emp_a = character(0), emp_b = character(0), score = numeric(0))
  sc <- sc[!is.na(score) & score >= floor]
  sc[, unit := t$unit]
  ix <- setNames(seq_len(n), P$EMP_ID)
  ov <- function(L, a, b) mapply(function(u, v) length(intersect(u, v)) > 0, L[a], L[b], USE.NAMES = FALSE)
  a <- ix[sc$emp_a]; b <- ix[sc$emp_b]
  sc[, `:=`(same_cbsa = ov(P$cbsa, a, b), same_ntee = ov(P$ntee_l, a, b), same_state = ov(P$states, a, b))]
  write_atomic(as.data.frame(sc), t$out)
  data.frame(id = t$id, k = t$k, pairs = nrow(cc), kept = nrow(sc), secs = as.numeric(Sys.time() - t0, units = "secs"))
}

log_file <- file.path(BIG, "progress.log")
if (nrow(todo)) {
  cl <- parallel::makePSOCKcluster(min(NW, nrow(todo)))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, "score_chunk")
  tl <- split(todo, seq_len(nrow(todo)))
  res <- parallel::clusterApplyLB(cl, tl, function(t, BIG, sw_file, floor, write_atomic, log_to) {
    r <- tryCatch(score_chunk(t, BIG, sw_file, floor, write_atomic),
                  error = function(e) data.frame(id = t$id, k = t$k, pairs = NA, kept = NA, secs = NA, error = conditionMessage(e)))
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", sprintf("%s chunk %d: %s pairs, %s kept, %.1f min%s", t$id, t$k,
        format(r$pairs), format(r$kept), r$secs / 60, if (!is.null(r$error)) paste(" FAILED:", r$error) else ""), "\n",
        file = log_to, append = TRUE)
    r
  }, BIG = BIG, sw_file = sw_file, floor = SCORE_FLOOR, write_atomic = write_parquet_atomic, log_to = log_file)
  res <- rbindlist(res, fill = TRUE)
  parallel::stopCluster(cl); on.exit()
  fwrite(res, file.path(BIG, sprintf("run-%s.csv", format(Sys.time(), "%Y%m%d-%H%M"))))
  stamp("%s pairs scored, %s kept, %.1f worker-hours", format(sum(res$pairs, na.rm = TRUE), big.mark = ","),
        format(sum(res$kept, na.rm = TRUE), big.mark = ","), sum(res$secs, na.rm = TRUE) / 3600)
  if ("error" %in% names(res) && any(!is.na(res$error))) stop(sum(!is.na(res$error)), " chunk(s) failed; re-run to retry")
}
if (any(!file.exists(tasks$out))) stop("some chunks are not done; re-run to finish them")

# ---- merge --------------------------------------------------------------------------------------
if (!MERGE || length(ONLY)) { stamp("scored; merge skipped"); quit(save = "no") }
all_blk <- fread(file.path(ROOT, "dropped-blocks.csv"))[, paste0(unit, "_", sub("^\\S+ ", "", blk))]
if (!all(file.exists(file.path(BIG, paste0("blk=", all_blk))))) stop("not every skipped block has been scored")
old <- file.path(ROOT, "pairs-before-big-blocks.parquet")
if (!file.exists(old)) { stopifnot(file.rename(file.path(ROOT, "pairs.parquet"), old)) }
pairs <- as.data.table(arrow::read_parquet(old))
n0 <- nrow(pairs)
big <- rbindlist(lapply(Sys.glob(file.path(BIG, "blk=*", "chunk-*.parquet")), function(f) as.data.table(arrow::read_parquet(f))))
pairs <- rbind(pairs, big, use.names = TRUE)
setorder(pairs, emp_a, emp_b, -score)
pairs <- pairs[!duplicated(pairs, by = c("emp_a", "emp_b"))]
write_parquet_atomic(as.data.frame(pairs), file.path(ROOT, "pairs.parquet"))
by_unit <- pairs[, .(pairs_kept = .N, score_7 = sum(score >= 7), score_8 = sum(score >= 8), score_10 = sum(score >= 10),
                     score_7_same_cbsa = sum(score >= 7 & same_cbsa), score_7_same_ntee = sum(score >= 7 & same_ntee)), by = unit][order(-score_7)]
fwrite(by_unit, file.path(pv_repo, "xorg-csa-summary.csv"))
stamp("merged: %s pairs before, %s from the big blocks, %s after (each pair once)", format(n0, big.mark = ","),
      format(nrow(big), big.mark = ","), format(nrow(pairs), big.mark = ","))
