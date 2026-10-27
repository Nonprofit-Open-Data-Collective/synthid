# 06k-name-checks.R
# A conservative post-match name check, calibrated before it is used.
#
# Surname relation between two names, in order:
#   same            identical once spaces, hyphens and apostrophes are removed
#   particle        identical once particles are dropped (VAN, DE, O, ST, ...)
#                   and MAC/MC are treated as one (VAN DEUSEN = DEUSEN, MACDONALD = MCDONALD)
#   compound        one name is a component of the other (LEHMAN-HAJEK vs HAJEK)
#   spelling        within OSA edit distance 1 (names up to 6 letters) or 2 (longer)
#   different       two established surnames that are none of the above (HOLLEY vs HILL)
#   unrelated       none of the above, at least one name rare (a garble, a parse error)
# "Established" = carried by at least K people nationally (all 15M profiles).
#
# First-name relation: same / nickname (shared nickname root) / initial /
# spelling / different (both established first names in the CSA) / unrelated.
#
# Calibration (CSA 450):
#   trusted   cross-org matches at score >= 7 corroborated by 2+ shared board
#             members (people matched at score >= 10): the check should almost
#             never call these "different"
#   negative  pairs of distinct surnames that share a phonetic key, drawn by
#             frequency: what the blocker puts side by side, nearly always
#             different families
# Application:
#   within-org  EMP_IDs whose own surname variants include two different
#               established surnames (DAVID HENDERSON / DAVID JOHNSON), plus
#               EMP_IDs holding two records in one filing year
#   cross-org   matches at score >= 7 whose best surname pair is "different"
#
# Writes dev/partvii-panel/csa-test-<CSA>/name-checks-*.csv; examples with names
# to the data folder (csa=<CSA>/review/name-checks-examples.csv).
#
#   Rscript dev/partvii-panel/06k-name-checks.R [csa=450]

source("dev/partvii-panel/_panel.R")
source("dev/partvii-panel/_hard_cases.R")
build_step("06k-name-checks")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
KS <- c(10L, 25L, 100L)         # thresholds for an "established" surname, calibrated below
K_FIRST <- 5L                   # first names: established if carried by 5+ records in the CSA
JW_MIN <- as.numeric(Sys.getenv("JW_MIN", "0.89"))   # Jaro-Winkler at or above this counts as a spelling variant
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
P <- as.data.table(readRDS(file.path(d, "profiles.rds"))$prof)
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa")))
fmt <- function(v) trimws(format(v, big.mark = ","))
up <- function(v) toupper(trimws(as.character(v)))

nat <- rbindlist(lapply(Sys.glob(file.path(pv_root, "xorg-csa", "prep", "counts", "*.rds")), function(f) {
  t <- readRDS(f); data.table(s = names(t), n = as.integer(t)) }))[, .(n = sum(n)), by = s]
# Hashed lookups: indexing a 1.2M-name vector by name rehashes it on every call.
nat_env <- new.env(hash = TRUE, size = nrow(nat) * 1.3)
invisible(Map(function(s, n) assign(s, n, envir = nat_env), nat$s[nzchar(nat$s)], nat$n[nzchar(nat$s)]))
cnt <- function(env, x) vapply(x, function(s) { if (is.na(s) || !nzchar(s)) return(0L); v <- env[[s]]; if (is.null(v)) 0L else as.integer(v) }, 0L, USE.NAMES = FALSE)
P[, fn := up(first_name)]
ft <- table(P$fn); first_env <- new.env(hash = TRUE)
invisible(Map(function(s, n) assign(s, n, envir = first_env), names(ft)[nzchar(names(ft))], as.integer(ft)[nzchar(names(ft))]))

# ---- classifiers ------------------------------------------------------------------------
PARTICLES <- c("AL", "BIN", "DA", "DE", "DEL", "DELA", "DELLA", "DER", "DES", "DI", "DO", "DOS", "DU", "EL", "IBN",
               "LA", "LE", "LO", "MAC", "MC", "SAN", "SANTA", "ST", "STE", "SAINT", "TER", "VAN", "VANDE", "VANDER", "VON", "Y", "O")
nrm <- function(x) gsub("[^A-Z]", "", up(x))
toks <- function(x) { t <- strsplit(up(x), "[^A-Z]+")[[1]]; t[nzchar(t)] }
core <- function(x) {
  t <- toks(x); t2 <- t[!t %in% PARTICLES]; if (!length(t2)) t2 <- t
  s <- paste(t2, collapse = ""); sub("^MAC", "MC", s)
}
ed_ok <- function(a, b) { d <- stringdist::stringdist(a, b, method = "osa"); d <= ifelse(pmax(nchar(a), nchar(b)) <= 6, 1, 2) }
surname_rel <- function(a, b, K) {
  a <- up(a); b <- up(b)
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return("missing")
  if (nrm(a) == nrm(b)) return("same")
  if (core(a) == core(b) || sub("^MAC", "MC", nrm(a)) == sub("^MAC", "MC", nrm(b))) return("particle")
  ta <- setdiff(toks(a), PARTICLES); tb <- setdiff(toks(b), PARTICLES)
  if ((length(ta) > 1 || length(tb) > 1) && length(intersect(ta[nchar(ta) >= 2], tb[nchar(tb) >= 2]))) return("compound")
  if (ed_ok(nrm(a), nrm(b)) || ed_ok(core(a), core(b))) return("spelling")
  # transposed or doubled letters (KLEIN / KLINE, CURRIE / CURRY): Jaro-Winkler
  if (stringdist::stringsim(nrm(a), nrm(b), method = "jw", p = 0.1) >= JW_MIN) return("spelling")
  # a compound whose component is a spelling of the other name (GONZALES-GREEN / GREENE)
  if (length(ta) > 1 || length(tb) > 1) {
    ca <- ta[nchar(ta) >= 4]; cb <- tb[nchar(tb) >= 4]
    if (length(ca) && length(cb) && any(outer(ca, cb, function(x, y) ed_ok(x, y)))) return("compound")
  }
  # a multi-word name counts as established if its whole form or any component is
  est <- function(x) cnt(nat_env, nrm(x)) >= K || any(cnt(nat_env, setdiff(toks(x), PARTICLES)) >= K)
  if (est(a) && est(b)) "different" else "unrelated"
}
first_rel <- function(a, b, roots_a, roots_b) {
  a <- up(a); b <- up(b)
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return("missing")
  if (a == b) return("same")
  if (length(intersect(up(roots_a), up(roots_b)))) return("nickname")
  if (nchar(a) == 1 || nchar(b) == 1) return(if (substr(a, 1, 1) == substr(b, 1, 1)) "initial" else "different")
  if (ed_ok(a, b)) return("spelling")
  if (startsWith(a, b) || startsWith(b, a)) return("nickname")          # KRIS / KRISTINA, CHRIS / CHRISTOPHER
  if (cnt(first_env, a) >= K_FIRST && cnt(first_env, b) >= K_FIRST) "different" else "unrelated"
}
# Generational suffixes: two different ones mean two people (a father and a
# son), except JR and II, which some filers use interchangeably.
GEN <- c("JR", "SR", "I", "II", "III", "IV", "V")
suffix_rel <- function(sa, sb) {
  sa <- intersect(up(sa), GEN); sb <- intersect(up(sb), GEN)
  if (!length(sa) && !length(sb)) return("none")
  if (!length(sa) || !length(sb)) return("one side only")
  if (length(intersect(sa, sb))) return("agree")
  if (all(c(sa, sb) %in% c("JR", "II"))) return("JR vs II (compatible)")
  "conflict"
}
best_pair <- function(va, vb) {   # the surname pair the scorer would use
  va <- va[!is.na(va) & nzchar(va)]; vb <- vb[!is.na(vb) & nzchar(vb)]
  if (!length(va) || !length(vb)) return(c(NA_character_, NA_character_))
  if (length(va) == 1 && length(vb) == 1) return(c(va, vb))
  g <- expand.grid(a = va, b = vb, stringsAsFactors = FALSE)
  s <- synthid::compare_last_names(g$a, g$b); k <- which.max(s); c(g$a[k], g$b[k])
}
bp_first <- function(va, vb) {
  va <- va[!is.na(va) & nzchar(va)]; vb <- vb[!is.na(vb) & nzchar(vb)]
  if (!length(va) || !length(vb)) return(c(NA_character_, NA_character_))
  if (length(va) == 1 && length(vb) == 1) return(c(va, vb))
  g <- expand.grid(a = va, b = vb, stringsAsFactors = FALSE)
  s <- synthid::compare_first_names(g$a, g$b); k <- which.max(s); c(g$a[k], g$b[k])
}
ix <- function(e) match(e, P$EMP_ID)

# ---- calibration sets ---------------------------------------------------------------------
# trusted: matches corroborated by 2+ other shared board members (score >= 10 identity)
e10 <- cand[csa == TRUE & score >= 10, .(emp_a, emp_b, score)]
xo <- as.data.table(synthid:::resolve_cross_org(as.data.frame(e10), as.data.frame(P[EMP_ID %in% c(e10$emp_a, e10$emp_b)])))
p10 <- setNames(P$EMP_ID, P$EMP_ID); p10[xo$EMP_ID] <- xo$XORG_ID
P[, org := vapply(orgs, function(o) if (length(o)) o[[1]] else NA_character_, "")]
seat <- unique(data.table(org = P$org, person = p10[P$EMP_ID]))
by_org <- split(seat$person, seat$org)
M <- cand[csa == TRUE & !is.na(score) & score >= 7, .(emp_a, emp_b, score)]
M[, `:=`(org_a = P$org[ix(emp_a)], org_b = P$org[ix(emp_b)])]
M[, shared := mapply(function(oa, ob, a, b) length(setdiff(intersect(by_org[[oa]], by_org[[ob]]), c(p10[a], p10[b]))), org_a, org_b, emp_a, emp_b)]
BP <- t(mapply(best_pair, P$last_name_variants[ix(M$emp_a)], P$last_name_variants[ix(M$emp_b)]))
M[, `:=`(ln_a = BP[, 1], ln_b = BP[, 2])]
FP <- t(mapply(bp_first, P$first_name_variants[ix(M$emp_a)], P$first_name_variants[ix(M$emp_b)]))
M[, `:=`(fn_a = FP[, 1], fn_b = FP[, 2])]
M[, first_relation := mapply(first_rel, fn_a, fn_b, P$first_name_roots[ix(emp_a)], P$first_name_roots[ix(emp_b)])]
diffM <- M[up(ln_a) != up(ln_b)]
trusted <- diffM[shared >= 2]
stamp("%s matches at >= 7; %s with different best surnames; %s of those corroborated by 2+ shared board members",
      fmt(nrow(M)), fmt(nrow(diffM)), fmt(nrow(trusted)))

# negative: distinct surnames sharing a phonetic key, drawn by record frequency
ln_tab <- P[!is.na(last_name) & nzchar(last_name), .N, by = .(ln = up(last_name))]
ln_tab[, key := vapply(ln, function(x) { k <- synthid::surname_blocking_keys(x); if (length(k)) k[1] else NA_character_ }, "")]
set.seed(1)
neg <- rbindlist(lapply(1:6000, function(i) {
  k <- sample(ln_tab$key, 1, prob = ln_tab$N); s <- ln_tab[key == k]
  if (nrow(s) < 2) return(NULL); pr <- s[sample(.N, 2, prob = N)]
  if (pr$ln[1] == pr$ln[2]) return(NULL); data.table(ln_a = pr$ln[1], ln_b = pr$ln[2])
}))
neg <- unique(neg)

cal <- rbindlist(lapply(KS, function(K) rbind(
  data.table(K = K, set = "trusted matches (2+ shared members)", n = nrow(trusted),
             relation = mapply(surname_rel, trusted$ln_a, trusted$ln_b, MoreArgs = list(K = K))),
  data.table(K = K, set = "negative: different surnames, same phonetic key", n = nrow(neg),
             relation = mapply(surname_rel, neg$ln_a, neg$ln_b, MoreArgs = list(K = K))))))
calt <- cal[, .N, by = .(K, set, relation)][, share := round(N / sum(N), 3), by = .(K, set)]
calt <- dcast(calt, K + set ~ relation, value.var = "share", fill = 0)
cat("\nCalibration: share of each surname relation\n"); print(calt)
fwrite(calt, file.path(fig, "name-checks-calibration.csv"))

K <- 25L
cat(sprintf("\nUsing K = %d (an established surname is carried by %d+ people nationally)\n", K, K))
tr_flag <- trusted[mapply(surname_rel, ln_a, ln_b, MoreArgs = list(K = K)) == "different"]
cat("Trusted matches the check would call 'different' (inspect: are these errors or real?):\n")
print(tr_flag[, .(ln_a, ln_b, fn_a, fn_b, score = round(score, 2), shared)])

# ---- application: within organizations ----------------------------------------------------
W <- P[lengths(last_name_variants) > 1]
wrel <- rbindlist(lapply(seq_len(nrow(W)), function(i) {
  v <- unique(up(W$last_name_variants[[i]])); cb <- t(combn(v, 2))
  data.table(EMP_ID = W$EMP_ID[i], a = cb[, 1], b = cb[, 2])
}))
wrel[, relation := mapply(surname_rel, a, b, MoreArgs = list(K = K))]
wsum <- wrel[, .(relation = c("different", "unrelated", "spelling", "compound", "particle", "same")[
  min(match(relation, c("different", "unrelated", "spelling", "compound", "particle", "same")))]), by = EMP_ID][, .N, by = relation]
wsum[, share_of_multi_surname := round(N / sum(N), 3)]
wsum[, share_of_all_records := round(N / nrow(P), 4)]
cat(sprintf("\nWithin-org: %s of %s records carry 2+ surnames; worst relation per record:\n", fmt(nrow(W)), fmt(nrow(P))))
print(wsum[order(-N)])
# two records in one filing year inside one organization: two people, or a duplicate row
dup_year <- P[n_records > n_years, .N]
cat(sprintf("Records holding more rows than filing years (two rows in one year): %s\n", fmt(dup_year)))

# ---- application: cross-org matches -------------------------------------------------------
diffM[, surname_relation := mapply(surname_rel, ln_a, ln_b, MoreArgs = list(K = K))]
xs <- diffM[, .N, by = surname_relation][, share_of_matches := round(N / nrow(M), 4)][order(-N)]
cat(sprintf("\nCross-org matches at >= 7 whose best surname pair differs (%s of %s):\n", fmt(nrow(diffM)), fmt(nrow(M))))
print(xs)
fr <- M[, .N, by = first_relation][, share_of_matches := round(N / nrow(M), 4)][order(-N)]
cat("\nCross-org matches at >= 7 by first-name relation:\n"); print(fr)
both <- M[up(ln_a) != up(ln_b)][diffM, on = c("emp_a", "emp_b"), surname_relation := i.surname_relation]
fwrite(rbind(wsum[, .(check = "within-org worst surname relation", relation, N)],
             xs[, .(check = "cross-org surname relation (surnames differ)", relation = surname_relation, N)],
             fr[, .(check = "cross-org first-name relation", relation = first_relation, N)]),
       file.path(fig, "name-checks-results.csv"))

# examples, with names: data folder only
nmv <- function(e) vapply(P$name_variants[ix(e)], function(v) paste(unique(v), collapse = " / "), "")
ex <- rbind(
  wrel[relation == "different"][sample(.N, min(25, .N))][, .(check = "within-org: two different surnames", forms_a = nmv(EMP_ID), forms_b = "", score = NA_real_, shared = NA_integer_)],
  diffM[surname_relation == "different"][sample(.N, min(25, .N))][, .(check = "cross-org: different surnames", forms_a = nmv(emp_a), forms_b = nmv(emp_b), score = round(score, 2), shared)],
  M[first_relation == "different"][sample(.N, min(25, .N))][, .(check = "cross-org: different first names", forms_a = nmv(emp_a), forms_b = nmv(emp_b), score = round(score, 2), shared)])
fwrite(ex, file.path(d, "review", "name-checks-examples.csv"))
options(width = 200); print(head(ex, 40))

# ---- gates on generational suffixes and middle initials -----------------------------------
M[, suffix_relation := mapply(suffix_rel, P$suffixes[ix(emp_a)], P$suffixes[ix(emp_b)])]
mi_rel <- function(a, b) { a <- unique(up(a)); b <- unique(up(b))
  if (!length(a) || !length(b)) "missing" else if (length(intersect(a, b))) "agree" else "conflict" }
M[, middle_relation := mapply(mi_rel, P$middle_initials[ix(emp_a)], P$middle_initials[ix(emp_b)])]
gates <- rbind(M[, .N, by = .(relation = suffix_relation)][, gate := "generational suffix"],
               M[, .N, by = .(relation = middle_relation)][, gate := "middle initial"])
gates[, share_of_matches := round(N / nrow(M), 4)]
cat("\nCross-org matches at >= 7 by suffix and middle-initial relation:\n"); print(gates)
fwrite(gates, file.path(fig, "name-checks-gates.csv"))

# ---- hard-case library ----------------------------------------------------------------------
onames <- as.data.table(read_parquet(file.path(pv_root, "orgs", "orgs.parquet"), col_select = c("ein", "org.name")))
onames <- setNames(onames$org.name, onames$ein)
det <- function(...) paste0(...)
# Ground truth: the same full name held by two people in one organization in overlapping years.
P[, full := paste(fn, up(last_name))]
cp <- P[!is.na(org) & nzchar(fn), .(EMP_ID, org, full, years)]
cp <- cp[cp[, .N, by = .(org, full)][N >= 2], on = c("org", "full")]
cp <- cp[cp, on = c("org", "full"), allow.cartesian = TRUE][EMP_ID < i.EMP_ID]
cp <- cp[mapply(function(y1, y2) length(intersect(y1, y2)) > 0, years, i.years)]
write_hard_cases(cp[, .(emp_a = EMP_ID, emp_b = i.EMP_ID, detail = "same organization, overlapping years")],
                 "co_present_homonym", "Two records with the same full name in one organization in the same years: two people", P, onames, CSA)
write_hard_cases(M[suffix_relation == "conflict", .(emp_a, emp_b, score, detail = det("shared board members: ", shared))],
                 "suffix_conflict", "Generational suffixes differ (e.g. JR vs SR, II vs III)", P, onames, CSA)
write_hard_cases(M[suffix_relation == "JR vs II (compatible)", .(emp_a, emp_b, score, detail = det("shared board members: ", shared))],
                 "suffix_jr_vs_ii", "JR on one side, II on the other: some filers use them interchangeably", P, onames, CSA)
tr_ok <- trusted[mapply(surname_rel, ln_a, ln_b, MoreArgs = list(K = K)) != "different"]
write_hard_cases(tr_ok[, .(emp_a, emp_b, score, detail = det(ln_a, " vs ", ln_b, "; shared board members: ", shared))],
                 "corroborated_surname_variant", "Surnames differ but 2+ other board members sit on both boards", P, onames, CSA)
write_hard_cases(wrel[relation == "different", .(emp_a = EMP_ID, detail = det(a, " vs ", b))][!duplicated(emp_a)],
                 "within_org_two_surnames", "One within-org record holds two established surnames", P, onames, CSA)
write_hard_cases(diffM[surname_relation == "different", .(emp_a, emp_b, score, detail = det(ln_a, " vs ", ln_b, "; shared board members: ", shared))],
                 "cross_org_surname_different", "Matched surnames are two different established surnames", P, onames, CSA)
write_hard_cases(M[first_relation == "different", .(emp_a, emp_b, score, detail = det(fn_a, " vs ", fn_b, "; shared board members: ", shared))],
                 "cross_org_first_name_different", "Matched first names are two different established first names", P, onames, CSA)
write_hard_cases(M[middle_relation == "conflict", .(emp_a, emp_b, score, detail = det("shared board members: ", shared))],
                 "middle_initial_conflict", "Middle initials differ", P, onames, CSA)
write_hard_cases(M[first_relation == "initial", .(emp_a, emp_b, score, detail = det(fn_a, " vs ", fn_b, "; shared board members: ", shared))],
                 "first_initial_only", "One side gives only a first initial", P, onames, CSA)
