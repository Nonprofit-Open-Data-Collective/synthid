# 06l-name-changes.R
# Surname changes (marriage, divorce) inside one organization, for one CSA.
#
# Within an organization, an old record A ends and a new record B begins 1-2
# years later, with
#   - the same first name,
#   - a different surname that is not a spelling or particle variant,
#   - the same title in A's last year and B's first year,
#   - the new name (first + new surname) appearing nowhere in the CSA's panel
#     before B starts, and the old name (first + old surname) nowhere after A ends.
# Evidence that strengthens it:
#   - pay stable: compensation in the two years within 25% (nonzero), or both zero
#   - maiden name kept: B's middle name, or a component of B's surname, is A's surname
#   - middle initial: the same (both present)
# Strength:
#   strong    title stable, plus nonzero stable pay, a kept maiden name, or the same middle initial
#   moderate  title stable, both unpaid
# Men rarely change surnames, so the rate among men estimates coincidence (one
# MARY replaced by another MARY as treasurer); the excess among women
# estimates real name changes.
#
# Also counts records the within-org linker already joined across a surname
# change: one record whose surname switches once, cleanly, at a year boundary.
#
# Writes dev/partvii-panel/csa-test-<CSA>/name-changes.csv; cases (with names)
# to the hard-case library (category married_name_change).
#
#   Rscript dev/partvii-panel/06l-name-changes.R [csa=450]

source("dev/partvii-panel/_panel.R")
source("dev/partvii-panel/_hard_cases.R")
build_step("06l-name-changes")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
P <- as.data.table(readRDS(file.path(d, "profiles.rds"))$prof)
fmt <- function(v) trimws(format(v, big.mark = ","))
up <- function(v) toupper(trimws(as.character(v)))
mode1 <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (!length(v)) NA_character_ else names(which.max(table(v))) }

eins <- unique(unlist(P$orgs))
cols <- c("PERSON_YEAR_ID", "ein", "taxyr", "first_name", "middle_name", "last_name", "suffix", "gender",
          "title.standard", "tot.comp", "tot.hours")
t0 <- Sys.time()
rows_file <- file.path(d, "panel-rows.rds")   # cached by 06m-record-checks.R
if (file.exists(rows_file)) R <- readRDS(rows_file) else {
R <- rbindlist(lapply(Sys.glob(file.path(pv_root, "people-by-ein", "bucket=*")), function(b) {
  f <- Sys.glob(file.path(b, "*.parquet")); if (!length(f)) return(NULL)
  x <- rbindlist(lapply(f, function(z) as.data.table(read_parquet(z, col_select = cols))[ein %in% eins]))
  fm <- Sys.glob(file.path(pv_root, "emp-map", basename(b), "*.parquet"))
  if (!nrow(x) || !length(fm)) return(NULL)
  m <- rbindlist(lapply(fm, function(z) as.data.table(read_parquet(z, col_select = c("PERSON_YEAR_ID", "EMP_ID")))))
  x[m, on = "PERSON_YEAR_ID", EMP_ID := i.EMP_ID]
}))
R <- R[!is.na(EMP_ID)]
}
R[, `:=`(fn = up(first_name), ln = up(last_name), mn = up(middle_name), yr = as.integer(taxyr), comp = suppressWarnings(as.numeric(tot.comp)))]
# Not people: company names filed in the person-name field (NATIONWIDE LIFE-INS, UNITRIN AUTO-HOME).
corp <- paste0("\\b(", paste(c(synthid::corporate_tokens(), "INS", "INSURANCE", "AUTO", "LIFE", "MUTUAL"), collapse = "|"), ")\\b")
R <- R[!grepl(corp, paste(fn, ln))]
stamp("%s panel rows for %s organizations in %.1f min", fmt(nrow(R)), fmt(uniqueN(R$ein)), as.numeric(Sys.time() - t0, units = "mins"))

# full-name timelines across the whole CSA
R[, full := paste(fn, ln)]
span <- R[, .(first_seen = min(yr), last_seen = max(yr)), by = full]

# one row per within-org record: first/last year, title and pay at each end
setorder(R, EMP_ID, yr)
E <- R[, .(ein = ein[1], fn = mode1(fn), ln = mode1(ln), mn = mode1(mn), gender = mode1(gender),
           y0 = min(yr), y1 = max(yr), n_ln = uniqueN(ln[!is.na(ln) & nzchar(ln)]),
           title0 = title.standard[1], title1 = title.standard[.N], comp0 = comp[1], comp1 = comp[.N]), by = EMP_ID]

# ---- candidate changes between two records ------------------------------------------------
X <- E[E, on = c("ein", "fn"), allow.cartesian = TRUE, nomatch = 0][
  EMP_ID != i.EMP_ID & i.y0 - y1 >= 1 & i.y0 - y1 <= 2 & ln != i.ln & !is.na(fn) & nchar(fn) > 1]
setnames(X, c("EMP_ID", "ln", "mn", "gender", "y0", "y1", "title1", "comp1", "i.EMP_ID", "i.ln", "i.mn", "i.gender", "i.y0", "i.title0", "i.comp0"),
            c("emp_a", "ln_a", "mn_a", "gender_a", "y0_a", "y1_a", "title_a", "comp_a", "emp_b", "ln_b", "mn_b", "gender_b", "y0_b", "title_b", "comp_b"))
X <- X[, .(ein, fn, emp_a, ln_a, mn_a, gender_a, y0_a, y1_a, title_a, comp_a, emp_b, ln_b, mn_b, gender_b, y0_b, title_b, comp_b)]
nrm <- function(x) gsub("[^A-Z]", "", x)
close <- function(a, b) stringdist::stringdist(nrm(a), nrm(b), method = "osa") <= ifelse(pmax(nchar(a), nchar(b)) <= 6, 1, 2)
X <- X[!close(ln_a, ln_b)]
X[, `:=`(new_full = paste(fn, ln_b), old_full = paste(fn, ln_a))]
X[span, on = c(new_full = "full"), new_first_seen := i.first_seen]
X[span, on = c(old_full = "full"), old_last_seen := i.last_seen]
X <- X[new_first_seen >= y0_b & old_last_seen <= y1_a]
X[, title_stable := !is.na(title_a) & title_a == title_b]
X[, pay := fcase(is.na(comp_a) | is.na(comp_b), "unknown",
                 comp_a == 0 & comp_b == 0, "both unpaid",
                 comp_a > 0 & comp_b > 0 & comp_b / comp_a >= 0.75 & comp_b / comp_a <= 1.33, "stable",
                 default = "changed")]
X[, maiden_kept := (!is.na(mn_b) & mn_b == ln_a) | grepl(paste0("\\b", ln_a, "\\b"), ln_b)]
X[, hyphenated := grepl("[- ]", ln_b) & grepl(paste0("\\b", ln_a, "\\b"), ln_b)]
X[, same_middle := !is.na(mn_a) & !is.na(mn_b) & substr(mn_a, 1, 1) == substr(mn_b, 1, 1)]
# A generic board title carries no evidence: one BOARD MEMBER MARY is often
# followed by another. Distinctive titles (TREASURER, EXECUTIVE DIRECTOR) do.
GENERIC <- c("BOARD", "MEMBER", "MEMBERS", "DIRECTOR", "DIRECTORS", "TRUSTEE", "TRUSTEES", "AT", "LARGE", "EX",
             "OFFICIO", "VOTING", "GOVERNOR", "GOVERNORS", "COUNCIL", "COMMITTEE", "ADVISORY", "OF", "THE")
X[, generic_title := vapply(strsplit(fifelse(is.na(title_b), "", title_b), "[^A-Z]+"), function(t) { t <- t[nzchar(t)]; !length(t) || all(t %in% GENERIC) }, TRUE)]
X[, strength := fcase(title_stable & (pay == "stable" | maiden_kept | same_middle), "strong",
                      title_stable & !generic_title & pay != "changed", "distinctive title",
                      title_stable & generic_title, "generic title",
                      default = "title changed")]
X[, gender := fifelse(!is.na(gender_b), gender_b, gender_a)]
X[, gender := fcase(gender %in% c("F", "FEMALE", "female", "f"), "female", gender %in% c("M", "MALE", "male", "m"), "male", default = "unknown")]

# records per gender, for rates
base <- E[, .(records = .N), by = .(gender = fcase(gender %in% c("F", "FEMALE", "female", "f"), "female",
                                                   gender %in% c("M", "MALE", "male", "m"), "male", default = "unknown"))]
tab <- X[, .N, by = .(strength, gender)]
tab <- dcast(tab, strength ~ gender, value.var = "N", fill = 0)
for (g in c("female", "male")) if (g %in% names(tab)) set(tab, j = paste0(g, "_per_10k_records"), value = round(1e4 * tab[[g]] / base[gender == g, records], 1))
cat("\nSurname changes between two records in one organization:\n"); print(tab)
cat("\nRecords by gender:\n"); print(base)

# ---- changes already joined inside one record -----------------------------------------------
sw <- R[!is.na(ln) & nzchar(ln), .(lns = list(ln), yrs = list(yr)), by = EMP_ID][lengths(lns) > 1]
sw[, clean_switch := mapply(function(l, y) {
  u <- unique(l); if (length(u) != 2) return(FALSE)
  last_old <- max(y[l == u[1]]); first_new <- min(y[l == u[2]])
  a <- last_old < first_new; b <- max(y[l == u[2]]) < min(y[l == u[1]])
  (a || b) && !close(u[1], u[2])
}, lns, yrs)]
sw <- sw[clean_switch == TRUE]
sw[E, on = "EMP_ID", gender := fcase(i.gender %in% c("F", "FEMALE", "female", "f"), "female", i.gender %in% c("M", "MALE", "male", "m"), "male", default = "unknown")]
joined <- sw[, .N, by = gender]
cat("\nRecords already joined across a clean surname switch:\n"); print(joined)

res <- rbind(X[, .(type = "two records", strength, gender)][, .N, by = .(type, strength, gender)],
             sw[, .(type = "one record (already joined)", strength = "joined", gender)][, .N, by = .(type, strength, gender)])
res[base, on = "gender", per_10k_records := round(1e4 * N / i.records, 1)]
fwrite(res[order(type, strength, gender)], file.path(fig, "name-changes.csv"))

# Estimated surname changes: women's count minus what men's rate predicts for
# that many women (men rarely change surnames, so their rate is coincidence).
fw <- base[gender == "female", records]; mw <- base[gender == "male", records]
est <- dcast(res[gender %in% c("female", "male")], type + strength ~ gender, value.var = "N", fill = 0)
est[, expected_by_chance := round(male * fw / mw, 1)]
est[, estimated_changes := pmax(0, round(female - expected_by_chance))]
cat("\nEstimated surname changes among women (count minus the men's-rate baseline):\n"); print(est)
cat(sprintf("Total: about %s surname changes among %s women's records (%.1f per 1,000); %s already joined inside one record\n",
            fmt(sum(est$estimated_changes)), fmt(fw), 1000 * sum(est$estimated_changes) / fw,
            fmt(est[type == "one record (already joined)", sum(estimated_changes)])))
fwrite(est, file.path(fig, "name-changes-estimate.csv"))

# ---- hard-case library ------------------------------------------------------------------------
onames <- as.data.table(read_parquet(file.path(pv_root, "orgs", "orgs.parquet"), col_select = c("ein", "org.name")))
onames <- setNames(onames$org.name, onames$ein)
X[, detail := sprintf("%s: %s %s -> %s %s (%d -> %d); title %s%s; pay %s%s%s; %s", strength, fn, ln_a, fn, ln_b, y1_a, y0_b,
                      fifelse(title_stable, "same", "changed"), fifelse(is.na(title_b), "", paste0(" (", title_b, ")")), pay,
                      fifelse(maiden_kept, "; maiden name kept", ""), fifelse(same_middle, "; same middle initial", ""), gender)]
lab <- X[strength %in% c("strong", "distinctive title")]
lab[, label := fifelse(gender != "male", "likely_same", "uncertain")]
write_hard_cases(lab[, .(emp_a, emp_b, detail)], "married_name_change",
                 "Same organization, first name and distinctive title (or stable pay, kept maiden name, same middle initial); new surname starts as the old one stops",
                 P, onames, CSA, label = lab$label)
# Hard negatives: same first name succeeding under a generic board title, at the
# same rate for women and men -- two different people.
write_hard_cases(X[strength == "generic title", .(emp_a, emp_b, detail)], "same_first_name_succession",
                 "A board seat passes between two people who share a first name; as common among men as women", P, onames, CSA)
print(head(lab[order(strength), .(detail)], 20))
