# 06g-name-risk.R
# Pilot on one CSA: how much of the cross-org network rests on matches that
# may join different people, and how much would network measures move if
# those matches were wrong?
#
# 1. Match types at score >= 7: identical full names vs name variants, and
#    whether middle initials back the match (both present and agreeing),
#    conflict, or are missing.
# 2. Name commonness. For each full name (FIRST LAST), lambda = the number of
#    records with that name expected by chance in this CSA if first and last
#    names were independent: n_records * p(first) * p(last). A cross-org person
#    is "ambiguous" when its records share one identical name with lambda >= 1
#    (a repeat of the name is unsurprising) and no middle initial confirms it.
#    Compare boards per person by commonness: rare names show the true
#    distribution; excess boards for common names estimates homonym merges.
# 3. Impact. Split every ambiguous person back into one person per
#    organization, and recompute the organization network (1+ shared board
#    members, meta-orgs collapsed). Compare with splitting the same number of
#    other cross-org persons, matched on number of boards, REPS times.
#
# Input: 06b pairs and profiles; 06c meta-collapsed.rds (person ids at score 7).
# Writes dev/partvii-panel/csa-test-<CSA>/name-risk-*.csv
#
#   Rscript dev/partvii-panel/06g-name-risk.R [csa=450]

source("dev/partvii-panel/_panel.R")
build_step("06g-name-risk")
suppressPackageStartupMessages(library(igraph))
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
LAMBDA <- 1; REPS <- 50L
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
prof <- as.data.table(readRDS(file.path(d, "profiles.rds"))$prof[, c("EMP_ID", "first_name", "last_name", "middle_initials", "orgs")])
x <- readRDS(file.path(d, "meta-collapsed.rds")); persons <- x$persons; mattr <- x$mattr; META <- x$META
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa")))
fmt <- function(v) trimws(format(v, big.mark = ","))

up <- function(v) toupper(trimws(as.character(v)))
prof[, `:=`(fn = up(first_name), ln = up(last_name))]
prof[, full := paste(fn, ln)]
mi <- setNames(lapply(prof$middle_initials, function(v) unique(up(v[!is.na(v) & nzchar(v)]))), prof$EMP_ID)
full <- setNames(prof$full, prof$EMP_ID)

# ---- 1. match types --------------------------------------------------------------------
e <- cand[csa == TRUE & !is.na(score) & score >= 7, .(emp_a, emp_b, score)]
e[, same_full := full[emp_a] == full[emp_b]]
e[, mi_status := mapply(function(a, b) { if (!length(a) || !length(b)) "missing" else if (length(intersect(a, b))) "agree" else "conflict" },
                        mi[emp_a], mi[emp_b])]
types <- e[, .(pairs = .N, median_score = median(score)), by = .(same_full, mi_status)][order(-pairs)]
types[, share := round(pairs / sum(pairs), 3)]
cat("\nMatches at score >= 7 by type:\n"); print(types)

# ---- 2. name commonness ----------------------------------------------------------------
N <- nrow(prof)
pf <- prop.table(table(prof$fn)); pl <- prop.table(table(prof$ln))
nm <- prof[, .(n_records = .N), by = .(full, fn, ln)]
nm[, lambda := N * as.numeric(pf[fn]) * as.numeric(pl[ln])]
lam <- setNames(nm$lambda, nm$full)

seats <- unique(persons[board == TRUE & !is.na(x$META[org]), .(EMP_ID, person, meta = META[org])])
cl <- seats[, .(boards = uniqueN(meta), records = .N,
                names = uniqueN(full[EMP_ID]), name = full[EMP_ID][1],
                mi_confirmed = { m <- mi[EMP_ID]; m <- m[lengths(m) > 0]; length(m) >= 2 && length(Reduce(intersect, m)) > 0 }),
            by = person][boards >= 2]
cl[, lambda := lam[name]]
cl[, ambiguous := names == 1 & !is.na(lambda) & lambda >= LAMBDA & !mi_confirmed]
cl[, commonness := cut(lambda, c(0, 0.01, 0.1, 1, 10, Inf), labels = c("< 0.01 (rare)", "0.01-0.1", "0.1-1", "1-10", ">= 10 (very common)"), include.lowest = TRUE)]
bycom <- cl[, .(cross_org_persons = .N, mean_boards = round(mean(boards), 2), share_5plus_boards = round(mean(boards >= 5), 3),
                mi_confirmed = round(mean(mi_confirmed), 3)), by = commonness][order(commonness)]
cat("\nCross-org board members (2+ boards) by name commonness (lambda = chance repeats of the full name in the CSA):\n"); print(bycom)
fwrite(bycom, file.path(fig, "name-risk-by-commonness.csv"))
cat(sprintf("\nAmbiguous persons (one identical name, lambda >= %g, no middle-initial confirmation): %s of %s cross-org board members (%.1f%%)\n",
            LAMBDA, fmt(sum(cl$ambiguous)), fmt(nrow(cl)), 100 * mean(cl$ambiguous)))

# ---- 3. ties carried by ambiguous persons, and the impact of splitting them -----------
org_ties <- function(S) {
  S <- unique(S[, .(meta, person)])
  ip <- S[, .N, by = person][N >= 2, person]
  z <- S[person %in% ip]
  z[z, on = "person", allow.cartesian = TRUE, nomatch = 0][meta < i.meta, .(u1 = meta, u2 = i.meta, person)]
}
base_pairs <- org_ties(seats)
amb <- cl[ambiguous == TRUE, person]
tie_support <- base_pairs[, .(w = uniqueN(person), w_amb = uniqueN(person[person %in% amb])), by = .(u1, u2)]
share_tab <- data.table(
  ties = c("1+ shared board members", "2+ shared board members"),
  total = c(nrow(tie_support), tie_support[w >= 2, .N]),
  only_ambiguous = c(tie_support[w == w_amb, .N], tie_support[w >= 2 & (w - w_amb) < 2, .N]))
share_tab[, share := round(only_ambiguous / total, 3)]
cat("\nOrganization ties that would disappear if every ambiguous person were several people:\n"); print(share_tab)
fwrite(share_tab, file.path(fig, "name-risk-tie-share.csv"))

cbsa_of <- setNames(mattr$cbsa, mattr$meta)
metrics <- function(S, top_ref = NULL) {
  pp <- org_ties(S)[, .(w = uniqueN(person)), by = .(u1, u2)]
  g <- graph_from_data_frame(pp[, .(u1, u2)], directed = FALSE)
  cm <- components(g)
  same <- cbsa_of[pp$u1] == cbsa_of[pp$u2]
  bt <- betweenness(g, directed = FALSE)
  top <- names(sort(bt, decreasing = TRUE))[1:50]
  c(orgs_tied = vcount(g), ties = ecount(g), ties_2plus = sum(pp$w >= 2), mean_degree = 2 * ecount(g) / vcount(g),
    largest_component = max(cm$csize) / vcount(g), transitivity = transitivity(g, "global"),
    ei_cbsa = mean(!same, na.rm = TRUE) - mean(same, na.rm = TRUE),
    top50_broker_overlap = if (is.null(top_ref)) 1 else length(intersect(top, top_ref)) / 50,
    attr = 0)[1:8]
}
split_persons <- function(S, who) { S <- copy(S); S[person %in% who, person := EMP_ID]; S }
g0 <- org_ties(seats)[, .(w = uniqueN(person)), by = .(u1, u2)]
gb <- graph_from_data_frame(g0[, .(u1, u2)], directed = FALSE)
top_ref <- names(sort(betweenness(gb, directed = FALSE), decreasing = TRUE))[1:50]
m0 <- metrics(seats, top_ref)
m_amb <- metrics(split_persons(seats, amb), top_ref)
# random persons matched on number of boards
pool <- cl[ambiguous == FALSE]
need <- cl[ambiguous == TRUE, .N, by = boards]
set.seed(1)
reps <- t(sapply(seq_len(REPS), function(r) {
  pick <- unlist(lapply(seq_len(nrow(need)), function(i) {
    cand_p <- pool[boards == need$boards[i], person]
    if (length(cand_p) <= need$N[i]) cand_p else sample(cand_p, need$N[i])
  }))
  metrics(split_persons(seats, pick), top_ref)
}))
imp <- data.table(metric = names(m0), baseline = m0, split_ambiguous = m_amb,
                  split_random_mean = colMeans(reps), split_random_sd = apply(reps, 2, sd))
imp[, `:=`(change_ambiguous = split_ambiguous - baseline, change_random = split_random_mean - baseline)]
imp[, ratio := round(change_ambiguous / change_random, 2)]
num <- c("baseline", "split_ambiguous", "split_random_mean", "split_random_sd", "change_ambiguous", "change_random")
imp[, (num) := lapply(.SD, signif, 4), .SDcols = num]
cat(sprintf("\nImpact of splitting the %s ambiguous persons vs %d draws of the same number of other persons with the same board counts:\n", fmt(length(amb)), REPS))
print(imp)
fwrite(imp, file.path(fig, "name-risk-impact.csv"))
