# 06b-csa-test.R
# Step 6b of PLAN-cross-org.md: on one CSA, does widening the metro block from
# CBSA to CSA, or dropping NTEE from it, change the cross-org network?
#
# Four blockings, all within one CSA and all keyed on the surname key:
#   cbsa_ntee  surname + CBSA + NTEE major group   (the current metro pass)
#   cbsa       surname + CBSA
#   csa_ntee   surname + CSA + NTEE major group
#   csa        surname + CSA                       (the widest; holds the other three)
# The csa pairs are scored once; each narrower blocking is a subset of them, so
# every comparison below uses identical scores.
#
# Surname rarity is the population weight over all 15M profiles, as in a full
# run, not a weight from this CSA alone.
#
# Writes: SYNTHID_DIR/partvii-v3_1/blocks/csa-test/csa=<code>/pairs.parquet
#         (every scored pair, with a flag per blocking)
#         dev/partvii-panel/csa-test-<code>.csv   (one row per blocking x threshold)
#
#   Rscript dev/partvii-panel/06b-csa-test.R [csa=450] [workers=20]

source("dev/partvii-panel/_panel.R")
build_step("06b-csa-test")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
NW  <- if (length(args) >= 2) as.integer(args[2]) else 20L
THRESHOLDS <- c(6, 7, 8)
MAX_BLOCK <- 5000L

out_dir <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dl <- as.data.table(readxl::read_excel(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/cbsa/list1_2023.xlsx"), skip = 2))
xw <- unique(dl[`CSA Code` == CSA, .(cbsa = `CBSA Code`, cbsa_title = `CBSA Title`, csa_title = `CSA Title`)])
stopifnot(nrow(xw) > 0)
CBSAS <- xw$cbsa
stamp("CSA %s %s: %d CBSAs (%s)", CSA, xw$csa_title[1], length(CBSAS), paste(xw$cbsa_title, collapse = "; "))

# ---- profiles: this CSA's people, and population surname counts ------------------
pv_prof <- file.path(pv_root, "profiles")
files <- Sys.glob(file.path(pv_prof, "bucket=*", "part-0.parquet"))
cache <- file.path(out_dir, "profiles.rds")   # a re-run skips the full read
t0 <- Sys.time()
if (file.exists(cache)) { load_c <- readRDS(cache); prof <- load_c$prof; sw <- load_c$sw; N <- load_c$N; rm(load_c) } else {
cl <- parallel::makePSOCKcluster(min(NW, 12L))
got <- parallel::parLapply(cl, files, function(f, cbsas) {
  p <- as.data.frame(arrow::read_parquet(f))
  s <- toupper(trimws(as.character(p$last_name)))
  s <- s[!is.na(s) & nzchar(s)]
  keep <- vapply(p$cbsa, function(v) any(v %in% cbsas), logical(1))
  p <- p[keep, , drop = FALSE]
  # arrow's list_of columns will not rbind() across files; make them plain lists
  for (cn in names(p)) if (is.list(p[[cn]])) p[[cn]] <- lapply(p[[cn]], as.vector)
  list(counts = table(s), prof = p)
}, cbsas = CBSAS)
cnt <- rbindlist(lapply(got, function(g) data.table(s = names(g$counts), n = as.integer(g$counts))))[, .(n = sum(n)), by = s]
N <- sum(cnt$n)   # same weight as population_surname_weight() over all profiles
sw <- setNames(pmin(pmax(log(N / cnt$n) / log(N), 0), 1), cnt$s)
prof <- do.call(rbind, lapply(got, `[[`, "prof"))
rm(got); invisible(gc()); parallel::stopCluster(cl)
saveRDS(list(prof = prof, sw = sw, N = N), cache)
}
stamp("%s profiles in CSA %s (of %s people) loaded in %.1f min", format(nrow(prof), big.mark = ","), CSA,
      format(N, big.mark = ","), as.numeric(Sys.time() - t0, units = "mins"))

# ---- blocks and pairs -------------------------------------------------------------
org1 <- vapply(prof$orgs, function(o) if (length(o)) o[[1]] else NA_character_, character(1))
long <- rbindlist(lapply(seq_len(nrow(prof)), function(i) {
  last <- prof$last_name_keys[[i]]; cb <- intersect(prof$cbsa[[i]], CBSAS)
  nt <- unique(substr(prof$ntee[[i]][!is.na(prof$ntee[[i]]) & nzchar(prof$ntee[[i]])], 1, 1))
  if (!length(last) || !length(cb)) return(NULL)
  CJ(EMP_ID = prof$EMP_ID[i], last = last, cbsa = cb, ntee = if (length(nt)) nt else NA_character_)
}))
emp_org <- setNames(org1, prof$EMP_ID)

pairs_for <- function(cols) {
  u <- unique(long[, c("EMP_ID", cols), with = FALSE])
  if ("ntee" %in% cols) u <- u[!is.na(ntee)]
  u[, blk := do.call(paste, c(.SD, sep = "|")), .SDcols = cols]
  sz <- u[, .N, by = blk]
  drop <- sz[N > MAX_BLOCK]
  u <- u[blk %in% sz[N >= 2 & N <= MAX_BLOCK, blk], .(EMP_ID, blk)]
  p <- u[u, on = "blk", allow.cartesian = TRUE, nomatch = 0][EMP_ID < i.EMP_ID, .(emp_a = EMP_ID, emp_b = i.EMP_ID)]
  p <- unique(p)
  p <- p[emp_org[emp_a] != emp_org[emp_b]]
  attr(p, "dropped") <- drop
  p
}
long[, csa := CSA]
BLOCKINGS <- list(cbsa_ntee = c("last", "cbsa", "ntee"), cbsa = c("last", "cbsa"),
                  csa_ntee = c("last", "csa", "ntee"), csa = c("last", "csa"))
P <- lapply(BLOCKINGS, pairs_for)
for (nm in names(P)) stamp("%-9s %s candidate pairs (different orgs), %d blocks over %d dropped",
                           nm, format(nrow(P[[nm]]), big.mark = ","), nrow(attr(P[[nm]], "dropped")), MAX_BLOCK)
cand <- unique(rbindlist(P))
for (nm in names(P)) { cand[P[[nm]], (nm) := TRUE, on = c("emp_a", "emp_b")]; cand[is.na(get(nm)), (nm) := FALSE] }

# ---- score every pair once, in parallel -------------------------------------------
t1 <- Sys.time()
chunks <- split(seq_len(nrow(cand)), cut(seq_len(nrow(cand)), NW, labels = FALSE))
cl <- parallel::makePSOCKcluster(NW)
parallel::clusterExport(cl, c("prof", "sw"), envir = environment())
parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(synthid)))
sc <- parallel::parLapply(cl, lapply(chunks, function(ix) as.data.frame(cand[ix, .(emp_a, emp_b)])), function(cc) {
  r <- synthid::score_candidate_pairs(cc, prof, surname_weight = sw)
  r[, c("emp_a", "emp_b", "score")]
})
sc <- rbindlist(sc)
parallel::stopCluster(cl)
cand[sc, score := i.score, on = c("emp_a", "emp_b")]
secs <- as.numeric(Sys.time() - t1, units = "secs")
stamp("scored %s pairs in %.1f min (%.0f pairs/s over %d workers)", format(nrow(cand), big.mark = ","),
      secs / 60, nrow(cand) / secs, NW)

# What separates the pairs: a CBSA line, an NTEE line.
cb_of <- setNames(lapply(prof$cbsa, intersect, CBSAS), prof$EMP_ID)
nt_of <- setNames(lapply(prof$ntee, function(v) unique(substr(v[!is.na(v) & nzchar(v)], 1, 1))), prof$EMP_ID)
share <- function(L, a, b) mapply(function(x, y) length(intersect(x, y)) > 0, L[a], L[b], USE.NAMES = FALSE)
cand[, same_cbsa := share(cb_of, emp_a, emp_b)]
cand[, same_ntee := share(nt_of, emp_a, emp_b)]
board <- setNames(prof$is_board, prof$EMP_ID)
cand[, both_board := board[emp_a] & board[emp_b]]
write_parquet_atomic(as.data.frame(cand), file.path(out_dir, "pairs.parquet"))

# ---- network per blocking x threshold ---------------------------------------------
# Connected components by label propagation (small graphs; no igraph here).
components <- function(from, to, nodes) {
  lab <- setNames(seq_along(nodes), nodes)
  if (!length(from)) return(lab)
  repeat {
    m <- pmin(lab[from], lab[to])
    new <- lab
    lo <- tapply(c(m, m), c(from, to), min)
    new[names(lo)] <- pmin(new[names(lo)], lo)
    if (identical(new, lab)) break
    lab <- new
  }
  lab
}
all_orgs <- unique(org1[!is.na(org1)])
n_orgs <- length(all_orgs)
summ <- rbindlist(lapply(names(BLOCKINGS), function(nm) rbindlist(lapply(THRESHOLDS, function(th) {
  e <- cand[get(nm) == TRUE & !is.na(score) & score >= th, .(emp_a, emp_b, score)]
  # Only people on an edge: the resolver's per-cluster lookups grow with the
  # square of the people it is given, and singletons add nothing here.
  xo <- synthid:::resolve_cross_org(as.data.frame(e), prof[prof$EMP_ID %in% c(e$emp_a, e$emp_b), , drop = FALSE])
  xo <- as.data.table(xo)[, org := emp_org[EMP_ID]]
  multi <- xo[XORG_N_ORGS > 1]
  ties <- unique(multi[, {o <- sort(unique(org)); if (length(o) > 1) as.data.table(t(combn(o, 2))) else NULL}, by = XORG_ID][, .(V1, V2)])
  comp <- components(ties$V1, ties$V2, all_orgs)
  csize <- sort(table(comp), decreasing = TRUE)
  bo <- prof$EMP_ID[prof$is_board %in% TRUE]
  data.table(blocking = nm, threshold = th, candidate_pairs = sum(cand[[nm]]),
             edges = nrow(e), edges_cross_cbsa = cand[get(nm) == TRUE & score >= th & !same_cbsa, .N],
             edges_cross_ntee = cand[get(nm) == TRUE & score >= th & !same_ntee, .N],
             edges_board_board = cand[get(nm) == TRUE & score >= th & both_board %in% TRUE, .N],
             rejected_by_org_guard = attr(xo, "rejected_edges") %||% NA_integer_,
             people_linked = nrow(multi), interlockers = uniqueN(multi$XORG_ID),
             board_interlockers = uniqueN(multi[EMP_ID %in% bo, XORG_ID]),
             orgs = n_orgs, orgs_tied = uniqueN(c(ties$V1, ties$V2)), org_ties = nrow(ties),
             density_per_1k = 1000 * nrow(ties) / (n_orgs * (n_orgs - 1) / 2),
             mean_degree = 2 * nrow(ties) / n_orgs,
             largest_component_orgs = as.integer(csize[1]))
}))))
summ[, people := nrow(prof)]
fwrite(summ, file.path(pv_repo, paste0("csa-test-", CSA, ".csv")))
options(width = 200)
print(summ[, !c("orgs", "people")])
stamp("CSA %s: %s people, %s orgs; pairs and scores in %s", CSA, format(nrow(prof), big.mark = ","),
      format(n_orgs, big.mark = ","), out_dir)
