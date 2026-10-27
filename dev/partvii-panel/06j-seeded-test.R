# 06j-seeded-test.R
# Seeded perturbation test for missed matches (false negatives), on one CSA.
#
# Seeds: near-certain cross-org matches -- identical full names that are rare
# in the CSA (lambda < 0.01) scoring >= 11. For each seed, side B is rewritten
# the way names vary between filings, as if B's organization had recorded the
# person differently, and blocking (shared surname key) and scoring are rerun
# against the unchanged side A. Recall = still blocked together and scoring
# >= 7 (the organization networks) or >= 10 (the people analysis).
#
# Perturbations (each applied alone; "n/a" seeds are skipped):
#   control                 unchanged
#   drop_middle             middle initial removed (seeds that have one)
#   middle_differs          a different middle initial
#   nickname                formal first name -> a nickname from nickname_table()
#   first_initial_only      first name reduced to its initial
#   first_typo              one letter changed in the first name (not the first letter)
#   surname_typo            one letter changed in the surname (not the first letter)
#   surname_typo_first      the surname's first letter changed (breaks the phonetic key)
#   surname_hyphenated      another surname added: OTHER-SURNAME (married, hyphenated)
#   surname_replaced        surname replaced by another (married, not hyphenated)
#   order_swapped           first and last name swapped
#
# Weighted miss rate: each perturbation's miss rate times how often that kind
# of variation shows up within organizations (06h "name variants", CSA 450).
#
# Writes dev/partvii-panel/csa-test-<CSA>/seeded-test.csv
#
#   Rscript dev/partvii-panel/06j-seeded-test.R [csa=450] [seeds=2000]

source("dev/partvii-panel/_panel.R")
build_step("06j-seeded-test")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
N_SEEDS <- if (length(args) >= 2) as.integer(args[2]) else 2000L
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
P <- as.data.table(readRDS(file.path(d, "profiles.rds"))$prof)
sw <- readRDS(file.path(pv_root, "xorg-csa", "prep", "surname-weight.rds"))
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa")))
nt <- synthid::nickname_table()
fmt <- function(v) trimws(format(v, big.mark = ","))

up <- function(v) toupper(trimws(as.character(v)))
P[, `:=`(fn = up(first_name), ln = up(last_name))]
P[, full := paste(fn, ln)]
pf <- prop.table(table(P$fn)); pl <- prop.table(table(P$ln))
lam <- setNames(nrow(P) * as.numeric(pf[P$fn]) * as.numeric(pl[P$ln]), P$EMP_ID)
full <- setNames(P$full, P$EMP_ID)

seeds <- cand[csa == TRUE & score >= 11]
seeds <- seeds[full[emp_a] == full[emp_b] & lam[emp_a] < 0.01]
set.seed(450); seeds <- seeds[sample(.N, min(.N, N_SEEDS))]
stamp("%s seeds (rare identical names scoring >= 11)", fmt(nrow(seeds)))

COLS <- c("EMP_ID", "last_name", "first_name", "gender", "last_name_variants", "first_name_variants", "middle_initials", "suffixes")
A <- P[match(seeds$emp_a, EMP_ID), ..COLS]
B0 <- P[match(seeds$emp_b, EMP_ID), ..COLS]
common_surnames <- names(sort(table(P$ln), decreasing = TRUE))[1:200]
set.seed(7)

swap_letter <- function(s, pos_min = 2L) {
  if (is.na(s) || nchar(s) < max(3L, pos_min)) return(NA_character_)
  p <- if (pos_min == 1L) 1L else sample(pos_min:nchar(s), 1)
  old <- substr(s, p, p); new <- sample(setdiff(LETTERS, old), 1)
  substr(s, p, p) <- new; s
}
set_first <- function(B, v) { B[, first_name := v]; B[, first_name_variants := lapply(v, function(x) if (is.na(x)) character(0) else x)]; B }
set_last  <- function(B, v) { B[, last_name := v]; B[, last_name_variants := lapply(v, function(x) if (is.na(x)) character(0) else x)]; B }

perturb <- list(
  control = function(B) list(B = B, ok = rep(TRUE, nrow(B))),
  drop_middle = function(B) { ok <- lengths(B$middle_initials) > 0; B[, middle_initials := list(rep(list(character(0)), .N))]; list(B = B, ok = ok) },
  middle_differs = function(B) { ok <- lengths(B$middle_initials) > 0
    B[, middle_initials := lapply(middle_initials, function(m) if (length(m)) setdiff(LETTERS, m)[1] else m)]; list(B = B, ok = ok) },
  nickname = function(B) { f <- up(B$first_name); ok <- f %in% names(nt)
    v <- ifelse(ok, vapply(f, function(x) if (x %in% names(nt)) sample(nt[[x]], 1) else NA_character_, ""), NA_character_)
    list(B = set_first(B, v), ok = ok) },
  first_initial_only = function(B) { v <- substr(up(B$first_name), 1, 1); list(B = set_first(B, v), ok = !is.na(v) & nzchar(v)) },
  first_typo = function(B) { v <- vapply(up(B$first_name), swap_letter, ""); list(B = set_first(B, v), ok = !is.na(v)) },
  surname_typo = function(B) { v <- vapply(up(B$last_name), swap_letter, ""); list(B = set_last(B, v), ok = !is.na(v)) },
  surname_typo_first = function(B) { v <- vapply(up(B$last_name), swap_letter, "", pos_min = 1L); list(B = set_last(B, v), ok = !is.na(v)) },
  surname_hyphenated = function(B) { o <- sample(common_surnames, nrow(B), TRUE); v <- paste0(o, "-", up(B$last_name)); list(B = set_last(B, v), ok = rep(TRUE, nrow(B))) },
  surname_replaced = function(B) { o <- sample(common_surnames, nrow(B), TRUE); v <- ifelse(o == up(B$last_name), common_surnames[1], o); list(B = set_last(B, v), ok = rep(TRUE, nrow(B))) },
  order_swapped = function(B) { f <- B$first_name; l <- B$last_name; B <- set_first(B, l); B <- set_last(B, f); list(B = B, ok = !is.na(f) & !is.na(l)) }
)

keysA <- lapply(A$last_name_variants, function(v) if (length(v)) unique(unlist(lapply(v, synthid::surname_blocking_keys))) else character(0))
res <- rbindlist(lapply(names(perturb), function(k) {
  pr <- perturb[[k]](copy(B0)); B <- pr$B[pr$ok]; Ak <- A[pr$ok]; kA <- keysA[pr$ok]
  if (!nrow(B)) return(data.table(perturbation = k, seeds = 0L))
  B[, EMP_ID := paste0(EMP_ID, "_p", .I)]
  kB <- lapply(B$last_name_variants, function(v) if (length(v)) unique(unlist(lapply(v, synthid::surname_blocking_keys))) else character(0))
  blocked <- mapply(function(a, b) length(intersect(a, b)) > 0, kA, kB)
  prof <- as.data.frame(rbind(Ak[!duplicated(EMP_ID)], B))
  sc <- as.data.table(synthid::score_candidate_pairs(data.frame(emp_a = Ak$EMP_ID, emp_b = B$EMP_ID), prof, surname_weight = sw))
  s <- sc$score[match(B$EMP_ID, sc$emp_b)]
  data.table(perturbation = k, seeds = nrow(B), blocked = mean(blocked), median_score = round(median(s, na.rm = TRUE), 2),
             score_7 = mean(s >= 7, na.rm = TRUE), score_10 = mean(s >= 10, na.rm = TRUE),
             recall_7 = mean(blocked & s >= 7, na.rm = TRUE), recall_10 = mean(blocked & s >= 10, na.rm = TRUE))
}))
num <- c("blocked", "score_7", "score_10", "recall_7", "recall_10")
res[, (num) := lapply(.SD, round, 3), .SDcols = num]
print(res)

# Weighted miss rate. Rates of each variation, per record, from the
# within-organization name variants in CSA 450 (06h): a middle initial dropped
# 3.9%; first name switching forms 2.7% (counted as nickname); surname varying
# 3.2% (split evenly: typo, hyphenated, replaced); first and last swapped 4.8%.
rate <- c(drop_middle = 0.039, nickname = 0.027, surname_typo = 0.032 / 3, surname_hyphenated = 0.032 / 3,
          surname_replaced = 0.032 / 3, order_swapped = 0.048)
w <- res[perturbation %in% names(rate)]
w[, rate := rate[perturbation]]
miss7 <- sum(w$rate * (1 - w$recall_7)); miss10 <- sum(w$rate * (1 - w$recall_10))
base_miss7 <- 1 - res[perturbation == "control", recall_7]
cat(sprintf("\nWeighted miss rate from name variation (per cross-org pair, rough): %.1f%% at score 7, %.1f%% at score 10 (control misses %.1f%%)\n",
            100 * miss7, 100 * miss10, 100 * base_miss7))
res[, rate_within_org := rate[perturbation]]
fwrite(res, file.path(fig, "seeded-test.csv"))
