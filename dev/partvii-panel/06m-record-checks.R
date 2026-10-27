# 06m-record-checks.R
# Record-level validation checks for one CSA: inconsistencies inside a single
# within-org record (EMP_ID) that point to a linkage error, plus two kinds of
# near-certain match for calibration.
#
#   two rows, one year     a record holds 2+ rows in one filing year; split into
#                          duplicate rows (same name and title: a repeated or
#                          amended filing) and differing rows (two people)
#   suffix conflict        a record holds two generational suffixes (JR and SR,
#                          II and III; JR with II is compatible): parent and child
#   two first names        a record holds two established first names that are
#                          not a nickname, initial, prefix or spelling of each other
#   gender mixed           rows recorded female and rows recorded male
#   particle variant       surnames that differ only by a particle or MAC/MC
#                          (VAN DEUSEN / DEUSEN): one person, a parsing difference
#   rare first names       how much a first name's rarity adds: cross-org matches
#                          at score >= 7 by first-name frequency; identical rare
#                          full names with agreeing middle initials go to the
#                          library as near-certain matches
#
# Panel rows are cached for the other checks: csa=<CSA>/panel-rows.rds
# Writes dev/partvii-panel/csa-test-<CSA>/record-checks.csv; cases to the
# hard-case library.
#
#   Rscript dev/partvii-panel/06m-record-checks.R [csa=450]

source("dev/partvii-panel/_panel.R")
source("dev/partvii-panel/_hard_cases.R")
build_step("06m-record-checks")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
P <- as.data.table(readRDS(file.path(d, "profiles.rds"))$prof)
fmt <- function(v) trimws(format(v, big.mark = ","))
up <- function(v) toupper(trimws(as.character(v)))
onames <- as.data.table(read_parquet(file.path(pv_root, "orgs", "orgs.parquet"), col_select = c("ein", "org.name")))
onames <- setNames(onames$org.name, onames$ein)

# ---- panel rows (cached) --------------------------------------------------------------------
rows_file <- file.path(d, "panel-rows.rds")
if (file.exists(rows_file)) R <- readRDS(rows_file) else {
  eins <- unique(unlist(P$orgs))
  cols <- c("PERSON_YEAR_ID", "ein", "taxyr", "name", "first_name", "middle_name", "last_name", "suffix", "gender",
            "title.standard", "tot.comp", "tot.hours")
  R <- rbindlist(lapply(Sys.glob(file.path(pv_root, "people-by-ein", "bucket=*")), function(b) {
    f <- Sys.glob(file.path(b, "*.parquet")); fm <- Sys.glob(file.path(pv_root, "emp-map", basename(b), "*.parquet"))
    if (!length(f) || !length(fm)) return(NULL)
    x <- rbindlist(lapply(f, function(z) as.data.table(read_parquet(z, col_select = cols))[ein %in% eins]))
    if (!nrow(x)) return(NULL)
    m <- rbindlist(lapply(fm, function(z) as.data.table(read_parquet(z, col_select = c("PERSON_YEAR_ID", "EMP_ID")))))
    x[m, on = "PERSON_YEAR_ID", EMP_ID := i.EMP_ID]
  }))
  R <- R[!is.na(EMP_ID)]
  saveRDS(R, rows_file)
}
R[, `:=`(fn = up(first_name), ln = up(last_name), mn = up(middle_name), sx = up(suffix), yr = as.integer(taxyr))]
stamp("%s rows, %s records", fmt(nrow(R)), fmt(uniqueN(R$EMP_ID)))
nrec <- uniqueN(R$EMP_ID)
first_n <- R[, .(n = uniqueN(EMP_ID)), by = fn]; first_n <- setNames(first_n$n, first_n$fn)
out <- list()
add <- function(check, records, note = "") out[[length(out) + 1]] <<- data.table(check = check, records = records,
                                                                              per_10k_records = round(1e4 * records / nrec, 1), note = note)

# ---- two rows in one filing year ---------------------------------------------------------------
dy <- R[, .(rows = .N, names = uniqueN(paste(fn, mn, ln, sx)), titles = uniqueN(title.standard)), by = .(EMP_ID, ein, yr)][rows > 1]
dy[, kind := fifelse(names == 1 & titles == 1, "duplicate rows", "differing rows")]
add("two rows in one year: duplicate rows (same name and title)", dy[kind == "duplicate rows", uniqueN(EMP_ID)], "a repeated or amended filing")
add("two rows in one year: differing names or titles", dy[kind == "differing rows", uniqueN(EMP_ID)], "two people in one record, or one person listed twice")
ex2 <- dy[kind == "differing rows"][!duplicated(EMP_ID)]
ex2[R[, .(forms = paste(unique(paste(fn, ln, sx)), collapse = " / ")), by = .(EMP_ID, yr)], on = c("EMP_ID", "yr"), forms := i.forms]
write_hard_cases(ex2[, .(emp_a = EMP_ID, detail = sprintf("%d: %s", yr, forms))], "within_org_two_rows_one_year",
                 "Two rows in one filing year with different names or titles", P, onames, CSA)

# ---- generational suffix conflict inside a record ---------------------------------------------
GEN <- c("JR", "SR", "I", "II", "III", "IV", "V")
sc <- R[sx %in% GEN, .(sx = list(unique(sx))), by = EMP_ID][lengths(sx) > 1]
sc <- sc[!vapply(sx, function(s) all(s %in% c("JR", "II")), TRUE)]
add("suffix conflict inside a record (JR and SR, II and III)", nrow(sc), "a parent and child merged")
write_hard_cases(sc[, .(emp_a = EMP_ID, detail = vapply(sx, paste, "", collapse = " + "))], "within_org_suffix_conflict",
                 "One record holds two generational suffixes", P, onames, CSA)

# ---- two first names inside a record ----------------------------------------------------------
fv <- R[!is.na(fn) & nchar(fn) > 1, .(fns = list(unique(fn))), by = EMP_ID][lengths(fns) > 1]
roots <- function(x) tryCatch(synthid:::first_name_roots(x), error = function(e) x)
ok_pair <- function(a, b) {
  if (startsWith(a, b) || startsWith(b, a)) return(TRUE)
  if (stringdist::stringdist(a, b, method = "osa") <= ifelse(max(nchar(a), nchar(b)) <= 6, 1, 2)) return(TRUE)
  if (length(intersect(roots(a), roots(b)))) return(TRUE)
  synthid::compare_first_names(a, b) >= 0.8
}
fv[, conflict := vapply(fns, function(f) {
  cb <- combn(f, 2)
  any(apply(cb, 2, function(p) !ok_pair(p[1], p[2]) && first_n[p[1]] >= 5 && first_n[p[2]] >= 5))
}, TRUE)]
add("two established first names inside a record", fv[conflict == TRUE, .N], "e.g. DAVID / DANIEL: two people merged")
write_hard_cases(fv[conflict == TRUE, .(emp_a = EMP_ID, detail = vapply(fns, paste, "", collapse = " / "))],
                 "within_org_two_first_names", "One record holds two different established first names", P, onames, CSA)

# ---- gender mixed inside a record -------------------------------------------------------------
gm <- R[!is.na(gender) & nzchar(gender), .(g = list(unique(up(gender)))), by = EMP_ID][lengths(g) > 1]
add("rows recorded female and male inside a record", nrow(gm), "a merge of two people, or a name whose gender is ambiguous")
gm[R[, .(forms = paste(unique(paste(fn, ln)), collapse = " / ")), by = EMP_ID], on = "EMP_ID", forms := i.forms]
write_hard_cases(gm[, .(emp_a = EMP_ID, detail = forms)], "within_org_gender_mixed",
                 "Rows recorded female and rows recorded male in one record", P, onames, CSA)

# ---- particle variants ------------------------------------------------------------------------
PARTICLES <- c("AL", "BIN", "DA", "DE", "DEL", "DELA", "DELLA", "DER", "DES", "DI", "DO", "DOS", "DU", "EL", "IBN",
               "LA", "LE", "LO", "MAC", "MC", "SAN", "SANTA", "ST", "STE", "SAINT", "TER", "VAN", "VANDE", "VANDER", "VON", "Y", "O")
core <- function(x) { t <- strsplit(x, "[^A-Z]+")[[1]]; t <- t[nzchar(t)]; t2 <- t[!t %in% PARTICLES]; if (!length(t2)) t2 <- t
  sub("^MAC", "MC", paste(t2, collapse = "")) }
nrm <- function(x) gsub("[^A-Z]", "", x)
pv <- R[!is.na(ln) & nzchar(ln), .(lns = list(unique(ln))), by = EMP_ID][lengths(lns) > 1]
pv[, particle := vapply(lns, function(l) { cb <- combn(l, 2)
  any(apply(cb, 2, function(p) nrm(p[1]) != nrm(p[2]) && core(p[1]) == core(p[2]))) }, TRUE)]
add("surname differs only by a particle or MAC/MC inside a record", pv[particle == TRUE, .N], "parsing drops or splits the particle")
write_hard_cases(pv[particle == TRUE, .(emp_a = EMP_ID, detail = vapply(lns, paste, "", collapse = " / "))], "particle_variant",
                 "Surnames differ only by a particle or MAC/MC", P, onames, CSA)

# ---- how much a rare first name adds ----------------------------------------------------------
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa")))
M <- cand[csa == TRUE & !is.na(score) & score >= 7, .(emp_a, emp_b, score)]
ix <- function(e) match(e, P$EMP_ID)
M[, `:=`(fa = up(P$first_name[ix(emp_a)]), la = up(P$last_name[ix(emp_a)]), fb = up(P$first_name[ix(emp_b)]), lb = up(P$last_name[ix(emp_b)]))]
M[, fcount := as.integer(first_n[fa])]
M[, first_name_rarity := cut(fcount, c(0, 5, 25, 200, Inf), labels = c("1-5 records", "6-25", "26-200", "200+"))]
fr <- M[, .(matches = .N, median_score = round(median(score), 2), share_score_10 = round(mean(score >= 10), 3)), by = first_name_rarity][order(first_name_rarity)]
cat("\nCross-org matches at >= 7 by how many CSA records share the first name:\n"); print(fr)
mi <- function(e) lapply(P$middle_initials[ix(e)], function(v) unique(up(v)))
mia <- mi(M$emp_a); mib <- mi(M$emp_b)
M[, mi_agree := mapply(function(a, b) length(a) > 0 && length(b) > 0 && length(intersect(a, b)) > 0, mia, mib)]
rare <- M[fa == fb & la == lb & fcount <= 5 & mi_agree]
add("cross-org matches: identical rare full name, agreeing middle initial", nrow(rare), "near-certain matches for calibration")
write_hard_cases(rare[, .(emp_a, emp_b, score, detail = sprintf("first name carried by %d records in the CSA", fcount))],
                 "rare_name_match", "Identical full name with a rare first name and agreeing middle initials", P, onames, CSA)

res <- rbindlist(out)
cat("\nRecord-level checks:\n"); print(res)
fwrite(res, file.path(fig, "record-checks.csv")); fwrite(fr, file.path(fig, "record-checks-first-name-rarity.csv"))
