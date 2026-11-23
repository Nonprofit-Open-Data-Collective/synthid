# 06n-lexicon-rescore.R
# Rescore one CSA's cross-org candidate pairs with the first-name lexicon.
#
# 06b-csa-test.R scored every candidate pair with the nickname_table() pairs
# only. The match score is additive, and the first name adds
# weight * (2 * sim - 1) with weight 5 (default_weights()), so switching the
# first-name comparator changes a pair's score by exactly 10 * (sim_new - sim_old),
# where sim is the best match over the two people's first-name forms. Only
# pairs with a listed pair (first_name_lexicon() or nickname_table()) among
# their form combinations can change; this finds those, computes both sims,
# and updates the score. A sample of changed pairs is rescored in full with
# score_candidate_pairs() under both settings to check the update is exact.
#
# Writes SYNTHID_DIR/partvii-v3_1/blocks/csa-test/csa=<CSA>/pairs-lexicon.parquet
#          (the pairs file with score = lexicon score, score_legacy = old score)
#        dev/partvii-panel/csa-test-<CSA>/lexicon-rescore.csv (threshold crossings)
#
#   Rscript dev/partvii-panel/06n-lexicon-rescore.R [csa=450]

source("dev/partvii-panel/_panel.R")
build_step("06n-lexicon-rescore")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
fmt <- function(v) trimws(format(v, big.mark = ","))
up <- function(v) toupper(trimws(as.character(v)))
W_FIRST <- synthid::default_weights()[["first_name"]]
# RESCORE_TAG names the output (pairs-<tag>.parquet, <tag>-rescore*.csv).
# Co-filing lexicon pairs are capped at the package default (0.85) unless
# COFILING_SIM is set (NA = no cap). CSA 450 runs: "lexicon-cofiling" is the
# default comparator, "lexicon" was run with COFILING_SIM=NA.
TAG <- Sys.getenv("RESCORE_TAG", "lexicon-cofiling")
if (nzchar(Sys.getenv("COFILING_SIM"))) options(synthid.first_name_cofiling_sim = suppressWarnings(as.numeric(Sys.getenv("COFILING_SIM"))))
stamp("tag %s; co-filing pairs capped at %s", TAG, getOption("synthid.first_name_cofiling_sim", 0.85))

P <- as.data.table(readRDS(file.path(d, "profiles.rds"))$prof)
pairs <- as.data.table(read_parquet(file.path(d, "pairs.parquet")))
stamp("%s candidate pairs, %s profiles", fmt(nrow(pairs)), fmt(nrow(P)))

# first-name forms per person, long
FV <- P[, .(fn = up(unlist(first_name_variants))), by = EMP_ID][!is.na(fn) & nchar(fn) > 1]
FV <- unique(FV)
# every listed pair, both directions
lx <- as.data.table(synthid::first_name_lexicon())[, .(x = up(name_short), y = up(name_long))]
nt <- synthid::nickname_table()
lg <- data.table(x = rep(names(nt), lengths(nt)), y = unlist(nt, use.names = FALSE))
L <- unique(rbind(lx, lg, lx[, .(x = y, y = x)], lg[, .(x = y, y = x)]))

# pairs with a listed form combination: a's form x, partner y, and b carries y
pairs[, pid := .I]
A <- pairs[, .(pid, EMP_ID = emp_a)][FV, on = "EMP_ID", nomatch = 0L, allow.cartesian = TRUE]
A <- A[L, on = c(fn = "x"), nomatch = 0L, allow.cartesian = TRUE]                 # columns: pid, EMP_ID, fn, y
B <- pairs[pid %in% A$pid, .(pid, EMP_ID = emp_b)][FV, on = "EMP_ID", nomatch = 0L, allow.cartesian = TRUE]
hit <- unique(A[B, on = c(pid = "pid", y = "fn"), nomatch = 0L]$pid)
stamp("%s pairs have a listed first-name combination", fmt(length(hit)))

fv_list <- split(FV$fn, FV$EMP_ID)
best <- function(va, vb) {
  if (!length(va) || !length(vb)) return(NA_real_)
  g <- expand.grid(a = va, b = vb, stringsAsFactors = FALSE)
  max(synthid::compare_first_names(g$a, g$b), na.rm = TRUE)
}
H <- pairs[hit, .(pid, emp_a, emp_b, score)]
sim_with <- function(on) { options(synthid.first_name_lexicon = on)
  mapply(function(a, b) best(fv_list[[a]], fv_list[[b]]), H$emp_a, H$emp_b, USE.NAMES = FALSE) }
H[, sim_old := sim_with(FALSE)]
H[, sim_new := sim_with(TRUE)]
H[, delta := 2 * W_FIRST * (sim_new - sim_old)]
stamp("%s pairs change score (median change %+.2f)", fmt(H[delta != 0, .N]), H[delta != 0, median(delta)])

# check: full rescore of a sample under both settings
sw <- readRDS(file.path(pv_root, "xorg-csa", "prep", "surname-weight.rds"))
set.seed(1)
S <- H[delta != 0][sample(.N, min(.N, 300))]
COLS <- c("EMP_ID", "last_name", "first_name", "gender", "last_name_variants", "first_name_variants", "middle_initials", "suffixes")
prof <- as.data.frame(P[EMP_ID %in% c(S$emp_a, S$emp_b), ..COLS])
full <- function(on) { options(synthid.first_name_lexicon = on)
  sc <- as.data.table(synthid::score_candidate_pairs(data.frame(emp_a = S$emp_a, emp_b = S$emp_b), prof, surname_weight = sw))
  sc[S, on = c("emp_a", "emp_b"), x.score] }
S[, `:=`(full_old = full(FALSE), full_new = full(TRUE))]
chk <- S[, .(stored_vs_legacy = max(abs(full_old - score)), update_vs_full = max(abs(score + delta - full_new)))]
cat("\nCheck on", nrow(S), "changed pairs (max absolute difference):\n"); print(chk)
if (chk$update_vs_full > 1e-6) stop("score update does not match a full rescore")

pairs[, score_legacy := score]
pairs[H, on = "pid", score := score + i.delta]
pairs[, pid := NULL]
write_parquet(pairs, file.path(d, paste0("pairs-", TAG, ".parquet")))

cross <- rbindlist(lapply(c(7, 10), function(t) pairs[csa == TRUE, .(
  threshold = t, legacy = sum(score_legacy >= t, na.rm = TRUE), lexicon = sum(score >= t, na.rm = TRUE),
  gained = sum(score >= t & !(score_legacy >= t), na.rm = TRUE), lost = sum(score_legacy >= t & !(score >= t), na.rm = TRUE))]))
cross[, pct_change := round(100 * (lexicon - legacy) / legacy, 2)]
cat("\nCSA pairs at each threshold, before and after the lexicon:\n"); print(cross)
fwrite(cross, file.path(fig, paste0(TAG, "-rescore.csv")))

# what the gained matches rest on: the first-name pair that changed
G <- H[delta != 0][pairs[csa == TRUE & score >= 7 & !(score_legacy >= 7), .(emp_a, emp_b)], on = c("emp_a", "emp_b"), nomatch = 0L]
bestpair <- function(va, vb) { g <- expand.grid(a = va, b = vb, stringsAsFactors = FALSE)
  s <- synthid::compare_first_names(g$a, g$b); k <- which.max(s); paste(sort(c(g$a[k], g$b[k])), collapse = "/") }
options(synthid.first_name_lexicon = TRUE)
if (nrow(G)) {
  G[, pair := mapply(function(a, b) bestpair(fv_list[[a]], fv_list[[b]]), emp_a, emp_b)]
  G[, type := { p <- tstrsplit(pair, "/"); synthid:::first_name_pair_type(p[[1]], p[[2]]) }]
  cat("\nMatches gained at >= 7, by the first-name pair type they rest on:\n"); print(G[, .N, by = type][order(-N)])
  cat("\nMost common first-name pairs among them:\n"); print(G[, .N, by = .(pair, type)][order(-N)][1:25])
  fwrite(G[, .N, by = .(pair, type)][order(-N)], file.path(fig, paste0(TAG, "-rescore-gained-pairs.csv")))
}
