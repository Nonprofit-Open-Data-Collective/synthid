# 06h-review-sample.R
# A stratified clerical-review sample of cross-org match pairs for one CSA,
# to estimate how often a match joins two different people (false positives)
# and how often a below-threshold pair is really one person (missed matches).
#
# Universe: 06b's surname + CSA pairs (people in different organizations)
# scoring >= 5. Strata:
#   score band   5-7 (below the threshold of 7), 7-8.5, 8.5-10, 10+
#   name match   identical full name (FIRST LAST) or variant
#   commonness   lambda = records with that full name expected by chance in
#                the CSA (n * p(first) * p(last)), larger of the two names:
#                rare < 0.1, middle 0.1-1, common >= 1
# PER_CELL pairs per cell (all of a smaller cell), with weight = cell size /
# sample size, so reviewed rates scale to all pairs.
#
# The review sheet is blinded: no score, stratum or match flags; rows in
# random order. The key sheet (hide it from reviewers) links pair_id to them.
# Evidence per side: name parts, organization, EIN with a ProPublica link,
# metro, NTEE, years, titles, role, and how many organizations the score-7
# match would place this name on. Per pair: other people (matched at score
# 10+) who sit on both organizations, and whether the two are one meta-org.
#
# Names individual people: written to the data folder, not the repository.
#   SYNTHID_DIR/partvii-v3_1/blocks/csa-test/csa=<CSA>/review/
#
#   Rscript dev/partvii-panel/06h-review-sample.R [csa=450] [per_cell=25]

source("dev/partvii-panel/_panel.R")
build_step("06h-review-sample")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
PER_CELL <- if (length(args) >= 2) as.integer(args[2]) else 25L
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
out <- file.path(d, "review"); dir.create(out, showWarnings = FALSE)
fmt <- function(v) trimws(format(v, big.mark = ","))

prof <- as.data.table(readRDS(file.path(d, "profiles.rds"))$prof)
x <- readRDS(file.path(d, "meta-collapsed.rds")); META <- x$META; persons7 <- x$persons
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa", "same_cbsa", "same_ntee")))
onames <- as.data.table(read_parquet(file.path(pv_root, "orgs", "orgs.parquet"), col_select = c("ein", "org.name")))
onames <- setNames(onames$org.name, onames$ein)
dl <- as.data.table(readxl::read_excel(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/cbsa/list1_2023.xlsx"), skip = 2))
cbsa_title <- unique(dl[, .(cbsa = `CBSA Code`, t = `CBSA Title`)]); cbsa_title <- setNames(cbsa_title$t, cbsa_title$cbsa)

up <- function(v) toupper(trimws(as.character(v)))
tc <- function(s) {   # Title Case with capital initials and Roman-numeral suffixes
  s <- gsub("\\b([a-z])", "\\U\\1", tolower(s), perl = TRUE)
  gsub("\\b(Iii|Ii|Iv)\\b", "\\U\\1", s, perl = TRUE)
}
mode1 <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (!length(v)) NA_character_ else names(which.max(table(v))) }
j <- function(v, n = 3) { v <- unique(v[!is.na(v) & nzchar(v)]); if (!length(v)) "" else paste(head(v, n), collapse = "; ") }
prof[, `:=`(fn = up(first_name), ln = up(last_name), org = vapply(orgs, function(o) if (length(o)) o[[1]] else NA_character_, ""))]
prof[, full := paste(fn, ln)]
N <- nrow(prof)
pf <- prop.table(table(prof$fn)); pl <- prop.table(table(prof$ln))
lam <- setNames(N * as.numeric(pf[prof$fn]) * as.numeric(pl[prof$ln]), prof$EMP_ID)

# ---- per-record flags: name frequency, known homonyms, within-org name variation -------
first_n <- table(prof$fn); last_n <- table(prof$ln); full_n <- table(prof$full)
nat_dir <- file.path(pv_root, "xorg-csa", "prep", "counts")    # national surname counts (07 prep)
last_nat <- if (length(Sys.glob(file.path(nat_dir, "*.rds")))) {
  z <- rbindlist(lapply(Sys.glob(file.path(nat_dir, "*.rds")), function(f) { t <- readRDS(f); data.table(s = names(t), n = as.integer(t)) }))[, .(n = sum(n)), by = s]
  setNames(z$n, z$s) } else integer(0)
# Known homonyms: one full name held by 2+ distinct people in the same organization
# (the within-org linker keeps co-present records apart), anywhere in the CSA.
hom <- prof[, .(k = .N), by = .(org, full)][k >= 2, .(orgs = .N), by = full]
hom <- setNames(hom$orgs, hom$full)
# How one person's name varies inside one organization's filings.
HONOR <- c("DR", "MR", "MRS", "MS", "REV", "HON", "PROF", "MISS")
SUFX <- c("JR", "SR", "II", "III", "IV", "V", "MD", "PHD", "ESQ", "CPA", "DDS", "RN")
toks <- function(s) { t <- strsplit(gsub("\\([^)]*\\)", " ", toupper(s)), "[^A-Z]+")[[1]]; t[nzchar(t) & !t %in% c(HONOR, SUFX)] }
variant_flags <- function(nv, fv, roots, lv, fn) {
  tk <- lapply(nv, toks); nt <- lengths(tk)
  list(drops_middle = any(nt == 2) && any(nt >= 3),
       nickname_switch = length(unique(fv)) > 1,
       uses_nickname = length(fv) > 0 && any(!fv %in% roots),
       surname_varies = length(unique(lv)) > 1,
       order_swapped = any(vapply(tk, function(t) length(t) >= 2 && t[1] != fn && fn %in% t[-1], TRUE)),
       honorific = any(grepl("^(DR|REV|HON|PROF)\\b", toupper(nv))))
}
VF <- rbindlist(Map(variant_flags, prof$name_variants, prof$first_name_variants, prof$first_name_roots,
                    prof$last_name_variants, prof$fn))
VF[, EMP_ID := prof$EMP_ID]

# ---- universe and strata --------------------------------------------------------------
U <- cand[csa == TRUE & !is.na(score) & score >= 5, .(emp_a, emp_b, score, same_cbsa, same_ntee)]
full <- setNames(prof$full, prof$EMP_ID)
U[, `:=`(band = cut(score, c(5, 7, 8.5, 10, Inf), right = FALSE, labels = c("5-7 below threshold", "7-8.5", "8.5-10", "10+")),
         name_match = fifelse(full[emp_a] == full[emp_b], "identical", "variant"),
         lambda = pmax(lam[emp_a], lam[emp_b], na.rm = TRUE))]
U[, commonness := cut(lambda, c(-Inf, 0.1, 1, Inf), right = FALSE, labels = c("rare", "middle", "common"))]
U[is.na(commonness), commonness := "rare"]
cells <- U[, .(pairs = .N), by = .(band, name_match, commonness)][order(band, name_match, commonness)]
set.seed(450)
S <- U[, .SD[sample(.N, min(.N, PER_CELL))], by = .(band, name_match, commonness)]
S <- S[cells, on = c("band", "name_match", "commonness"), cell_pairs := i.pairs]
S[, sampled := .N, by = .(band, name_match, commonness)]
S[, weight := cell_pairs / sampled]
cells[S[, .(sampled = .N), by = .(band, name_match, commonness)], on = c("band", "name_match", "commonness"), sampled := i.sampled]
cells[, weight := round(pairs / sampled, 1)]
stamp("%s pairs at score >= 5 in %d cells; %s sampled", fmt(nrow(U)), nrow(cells), fmt(nrow(S)))
print(cells)

# ---- evidence -------------------------------------------------------------------------
# Person identity at score >= 10 (rare names and middle-initial matches), to
# find other people sitting on both organizations without leaning on homonyms.
e10 <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa")))[csa == TRUE & score >= 10, .(emp_a, emp_b, score)]
xo <- as.data.table(synthid:::resolve_cross_org(as.data.frame(e10), as.data.frame(prof[EMP_ID %in% c(e10$emp_a, e10$emp_b)])))
p10 <- setNames(prof$EMP_ID, prof$EMP_ID); p10[xo$EMP_ID] <- xo$XORG_ID
seat10 <- unique(data.table(org = prof$org, person = p10[prof$EMP_ID], name = prof$full))
by_org <- split(seat10[, .(person, name)], seat10$org)
co_members <- function(oa, ob, pa, pb) {
  a <- by_org[[oa]]; b <- by_org[[ob]]
  if (is.null(a) || is.null(b)) return(list(n = 0L, names = ""))
  sh <- setdiff(intersect(a$person, b$person), c(pa, pb))
  list(n = length(sh), names = j(tc(a$name[match(sh, a$person)]), 5))
}
cl7 <- persons7[, .(orgs7 = uniqueN(org)), by = person]
orgs7 <- setNames(cl7$orgs7[match(persons7$person, cl7$person)], persons7$EMP_ID)

P <- prof[EMP_ID %in% c(S$emp_a, S$emp_b)]
side <- P[, .(EMP_ID,
              name = tc(paste(fn, vapply(middle_initials, j, ""), ln, vapply(suffixes, j, ""))),
              org, org_name = onames[org],
              propublica = paste0("https://projects.propublica.org/nonprofits/organizations/", gsub("\\D", "", org)),
              metro = vapply(cbsa, function(v) j(cbsa_title[v], 2), ""),
              ntee = vapply(ntee, mode1, ""),
              years = vapply(years, function(y) if (length(y)) sprintf("%d-%d (%d yrs)", min(y), max(y), length(unique(y))) else "", ""),
              titles = vapply(titles, j, ""), role = primary_role, board = is_board %in% TRUE)]
side[, name := gsub("\\s+", " ", trimws(name))]
side[, orgs_if_matched_at_7 := orgs7[EMP_ID]]
side[, p10 := p10[EMP_ID]]

R <- copy(S)
for (s in c("a", "b")) {
  sd <- copy(side); setnames(sd, names(sd), paste0(names(sd), "_", s)); setnames(sd, paste0("EMP_ID_", s), paste0("emp_", s))
  R <- merge(R, sd, by = paste0("emp_", s), all.x = TRUE)
}
cm <- R[, co_members(org_a, org_b, p10_a, p10_b), by = .(emp_a, emp_b)]
R[cm, on = c("emp_a", "emp_b"), `:=`(shared_board_members = i.n, shared_member_names = i.names)]
R[, same_meta_org := !is.na(META[org_a]) & META[org_a] == META[org_b]]

# ---- triangulation flags ----------------------------------------------------------------
# The score-7 cluster each record would join: how many organizations, metros
# and sectors it spans, and the most organizations it holds in a single year.
und_na <- function(v) { m <- mode1(v); if (is.na(m) || startsWith(m, "UND")) NA_character_ else substr(m, 1, 1) }
prof[, `:=`(cbsa1 = vapply(cbsa, mode1, ""), ntee1 = vapply(ntee, und_na, ""))]
p7 <- persons7[, .(EMP_ID, person, org, years)]
p7[, n := .N, by = person]
p7 <- p7[n >= 2]
p7[, `:=`(cbsa1 = prof$cbsa1[match(EMP_ID, prof$EMP_ID)], ntee1 = prof$ntee1[match(EMP_ID, prof$EMP_ID)])]
cl7s <- p7[, .(cluster_cbsas = uniqueN(na.omit(cbsa1)), cluster_sectors = uniqueN(na.omit(ntee1)),
               max_orgs_one_year = { yy <- data.table(o = rep(org, lengths(years)), y = unlist(years)); if (nrow(yy)) max(yy[, uniqueN(o), by = y]$V1) else 0L }),
           by = person]
cl_of <- function(emp) { pp <- persons7$person[match(emp, persons7$EMP_ID)]; cl7s[match(pp, cl7s$person)] }

GEN <- c("JR", "SR", "II", "III", "IV")
ix <- function(e) match(e, prof$EMP_ID)
pair_flags <- function(ea, eb, org_name_a, org_name_b) {
  a <- ix(ea); b <- ix(eb)
  ok <- function(v) { v <- up(v); unique(v[!is.na(v) & nzchar(v) & v != "NA"]) }
  fa <- ok(c(prof$fn[a], prof$first_name_variants[[a]])); fb <- ok(c(prof$fn[b], prof$first_name_variants[[b]]))
  ra <- ok(prof$first_name_roots[[a]]); rb <- ok(prof$first_name_roots[[b]])
  first_rel <- if (!length(fa) || !length(fb)) "missing" else if (length(intersect(fa, fb))) "exact" else
    if (length(intersect(ra, rb))) "nickname" else
    if (any(nchar(c(fa, fb)) == 1) && substr(fa[1], 1, 1) == substr(fb[1], 1, 1)) "initial only" else "different"
  mia <- unique(up(prof$middle_initials[[a]])); mib <- unique(up(prof$middle_initials[[b]]))
  mi <- if (!length(mia) && !length(mib)) "both missing" else if (!length(mia) || !length(mib)) "one missing" else
    if (length(intersect(mia, mib))) "agree" else "conflict"
  sa <- intersect(up(prof$suffixes[[a]]), GEN); sb <- intersect(up(prof$suffixes[[b]]), GEN)
  suffix <- if (!length(sa) && !length(sb)) "none" else if (!length(sa) || !length(sb)) "one only" else
    if (length(intersect(sa, sb))) "agree" else "conflict"
  la <- prof$ln[a]; lb <- prof$ln[b]
  if (is.na(la)) la <- ""; if (is.na(lb)) lb <- ""
  if (is.na(org_name_a)) org_name_a <- ""; if (is.na(org_name_b)) org_name_b <- ""
  surname_rel <- if (!nzchar(la) || !nzchar(lb)) "missing" else if (la == lb) "exact" else if (grepl(la, lb, fixed = TRUE) || grepl(lb, la, fixed = TRUE)) "compound or partial" else "spelling"
  ya <- prof$years[[a]]; yb <- prof$years[[b]]
  ov <- length(intersect(ya, yb))
  fam <- nchar(la) >= 3 && (grepl(paste0("\\b", la, "\\b"), toupper(org_name_a)) || grepl(paste0("\\b", lb, "\\b"), toupper(org_name_b)))
  list(first_name_relation = first_rel, middle_initials = mi, generational_suffix = suffix,
       gender_conflict = !is.na(prof$gender[a]) && !is.na(prof$gender[b]) && prof$gender[a] != prof$gender[b],
       surname_relation = surname_rel, years_overlap = ov,
       years_gap = if (ov || !length(ya) || !length(yb)) 0L else as.integer(min(abs(outer(ya, yb, "-")))),
       family_name_in_org = fam)
}
PF <- rbindlist(Map(pair_flags, R$emp_a, R$emp_b, R$org_name_a, R$org_name_b))
R <- cbind(R, PF)
for (s in c("a", "b")) {
  e <- R[[paste0("emp_", s)]]; k <- ix(e); cl <- cl_of(e); vf <- VF[match(e, VF$EMP_ID)]
  set(R, j = paste0("first_name_count_", s), value = as.integer(first_n[prof$fn[k]]))
  set(R, j = paste0("last_name_count_", s), value = as.integer(last_n[prof$ln[k]]))
  set(R, j = paste0("last_name_count_us_", s), value = if (length(last_nat)) as.integer(last_nat[prof$ln[k]]) else NA_integer_)
  set(R, j = paste0("full_name_count_", s), value = as.integer(full_n[prof$full[k]]))
  set(R, j = paste0("full_name_expected_", s), value = round(lam[e], 2))
  set(R, j = paste0("known_homonym_orgs_", s), value = { h <- as.integer(hom[prof$full[k]]); h[is.na(h)] <- 0L; h })
  set(R, j = paste0("forms_in_own_org_", s), value = vapply(prof$name_variants[k], function(v) paste(head(unique(v), 4), collapse = " / "), ""))
  set(R, j = paste0("drops_middle_", s), value = vf$drops_middle)
  set(R, j = paste0("nickname_switch_", s), value = vf$nickname_switch)
  set(R, j = paste0("surname_varies_", s), value = vf$surname_varies)
  set(R, j = paste0("cluster_cbsas_", s), value = cl$cluster_cbsas)
  set(R, j = paste0("max_orgs_one_year_", s), value = cl$max_orgs_one_year)
}
nz <- function(v) { v[is.na(v)] <- 0; v }
tf <- function(v) !is.na(v) & v
R[, `:=`(
  conflict = tf(middle_initials == "conflict") | tf(generational_suffix == "conflict") | tf(gender_conflict) | tf(first_name_relation == "different"),
  corroborated = tf(shared_board_members >= 1) | tf(same_meta_org) | tf(middle_initials == "agree"),
  risk_flags = tf(lambda >= 0.1) + tf(lambda >= 1) + tf(known_homonym_orgs_a + known_homonym_orgs_b > 0) +
    tf(pmax(nz(max_orgs_one_year_a), nz(max_orgs_one_year_b)) >= 5) + tf(pmax(nz(cluster_cbsas_a), nz(cluster_cbsas_b)) >= 3) +
    tf(family_name_in_org) + tf(years_gap >= 10) + tf(middle_initials == "one missing" & (tf(drops_middle_a) | tf(drops_middle_b))))]
R[, risk := risk_flags > 0]
# high: a conflict, or 2+ risk signals with nothing corroborating; medium: one
# uncorroborated risk signal; low: everything else.
R[, review_priority := fifelse(conflict | (risk_flags >= 2 & !corroborated), "high",
                       fifelse(risk_flags == 1 & !corroborated, "medium", "low"))]
R[, flags := apply(cbind(
  fifelse(middle_initials == "conflict", "middle initials differ", ""),
  fifelse(generational_suffix == "conflict", "Jr/Sr/II/III differ", ""),
  fifelse(gender_conflict, "gender differs", ""),
  fifelse(first_name_relation == "different", "first names differ", ""),
  fifelse(first_name_relation == "nickname", "nickname", ""),
  fifelse(lambda >= 1, "common full name", fifelse(lambda >= 0.1, "fairly common full name", "")),
  fifelse(known_homonym_orgs_a + known_homonym_orgs_b > 0, "two people share this name in one org", ""),
  fifelse(pmax(nz(max_orgs_one_year_a), nz(max_orgs_one_year_b)) >= 5, "match puts name on 5+ boards in one year", ""),
  fifelse(pmax(nz(cluster_cbsas_a), nz(cluster_cbsas_b)) >= 3, "match spans 3+ metros", ""),
  fifelse(family_name_in_org, "surname in org name (family)", ""),
  fifelse(years_gap >= 10, "10+ years apart (generations?)", ""),
  fifelse(middle_initials == "one missing" & (drops_middle_a | drops_middle_b), "initial dropped within own org", ""),
  fifelse(shared_board_members >= 1, "shared board members", ""),
  fifelse(same_meta_org, "same meta-org", ""),
  fifelse(middle_initials == "agree", "middle initials agree", "")), 1, function(z) paste(z[nzchar(z)], collapse = "; "))]
print(R[, .N, by = review_priority])
set.seed(4501)
R <- R[sample(.N)]
R[, pair_id := sprintf("P%04d", .I)]

review <- R[, .(pair_id,
  name_a, org_a = org_name_a, ein_a = org_a, link_a = propublica_a, metro_a, ntee_a, years_a, titles_a, role_a,
  name_b, org_b = org_name_b, ein_b = org_b, link_b = propublica_b, metro_b, ntee_b, years_b, titles_b, role_b,
  shared_board_members, shared_member_names, same_meta_org,
  review_priority, flags,
  first_name_relation, middle_initials, generational_suffix, gender_conflict, surname_relation,
  years_overlap, years_gap, family_name_in_org,
  forms_in_own_org_a, forms_in_own_org_b, drops_middle_a, drops_middle_b, nickname_switch_a, nickname_switch_b,
  surname_varies_a, surname_varies_b,
  first_name_count_a, last_name_count_a, last_name_count_us_a, full_name_count_a, full_name_expected_a, known_homonym_orgs_a,
  first_name_count_b, last_name_count_b, last_name_count_us_b, full_name_count_b, full_name_expected_b, known_homonym_orgs_b,
  boards_a_if_matched = orgs_if_matched_at_7_a, boards_b_if_matched = orgs_if_matched_at_7_b,
  metros_a_if_matched = cluster_cbsas_a, metros_b_if_matched = cluster_cbsas_b,
  max_boards_one_year_a = max_orgs_one_year_a, max_boards_one_year_b = max_orgs_one_year_b,
  reviewer_1 = "", decision_1 = "", confidence_1 = "", evidence_1 = "", source_url_1 = "",
  reviewer_2 = "", decision_2 = "", confidence_2 = "", evidence_2 = "", source_url_2 = "", notes = "")]
key <- R[, .(pair_id, band, name_match, commonness, lambda = signif(lambda, 3), score = round(score, 2),
             same_cbsa, same_ntee, cell_pairs, sampled, weight = round(weight, 2),
             review_priority, conflict, risk, corroborated, emp_a, emp_b)]

# ---- within-organization name variation (all records in the CSA) -----------------------
vt <- c(drops_middle = "Middle initial or name present in some filings, missing in others",
        nickname_switch = "First name switches between forms (nickname / formal / spelling)",
        uses_nickname = "Listed under a nickname (first name is not its root form)",
        surname_varies = "Surname varies (spelling, hyphenated or married name)",
        order_swapped = "First and last name swapped in some filings",
        honorific = "Honorific (Dr, Rev, Hon, Prof) in some filings")
mi_have <- lengths(prof$middle_initials) > 0
var_summary <- rbind(
  data.table(variation = unname(vt), records = vapply(names(vt), function(k) sum(VF[[k]]), 0L),
             share_of_records = vapply(names(vt), function(k) round(mean(VF[[k]]), 4), 0)),
  data.table(variation = "Of records that ever show a middle initial: share that also appear without it",
             records = sum(VF$drops_middle & mi_have), share_of_records = round(sum(VF$drops_middle & mi_have) / sum(mi_have), 4)))
set.seed(7)
var_examples <- rbindlist(lapply(names(vt), function(k) {
  ids <- VF$EMP_ID[VF[[k]]]; ids <- ids[sample(length(ids), min(10, length(ids)))]
  kk <- ix(ids)
  data.table(variation = vt[[k]], organization = onames[prof$org[kk]],
             forms_in_filings = vapply(prof$name_variants[kk], function(v) paste(unique(v), collapse = " / "), ""),
             years = vapply(prof$years[kk], function(y) if (length(y)) sprintf("%d-%d", min(y), max(y)) else "", ""))
}))
print(var_summary)

# ---- workbook -------------------------------------------------------------------------
library(openxlsx)
wb <- createWorkbook()
readme <- c(
  sprintf("Clerical review sample: cross-organization matches, %s (CSA %s)", unique(dl[`CSA Code` == CSA, `CSA Title`])[1], CSA),
  "",
  "Each row is two board or staff records from two different organizations that the matcher compared.",
  "Question: are A and B the same person?",
  "",
  "decision: same / different / unsure",
  "confidence: high / medium / low",
  "evidence: what decided it, e.g. 'bio on org B site lists board seat at org A', 'different middle names in filings',",
  "  'A is a physician in Durham, B a contractor in Cary', 'shared co-members', 'same years and titles'",
  "source_url: the page you used, if any",
  "",
  "Guidance",
  "- Look first at the shared board members and same_meta_org columns: other people on both boards make 'same' likely.",
  "- boards_*_if_matched: how many organizations the matcher would put this name on. A very common name on many boards",
  "  is the case we most need to check: decide only on evidence for THESE two organizations.",
  "- Search the name with each organization (web, LinkedIn, organization site, news, obituaries, NC Secretary of State",
  "  annual reports for officer lists). Bios cover current boards; for past years rely on the filings and co-members.",
  "- Do not decide from the name alone. If you find nothing either way, choose 'unsure' with low confidence.",
  "- Reviewer 2 works without looking at reviewer 1's columns. Disagreements are settled afterward.",
  "",
  "- review_priority and flags point at what to check; 'flag guide' explains every flag column. Flags are leads,",
  "  not verdicts: decide on the evidence for these two organizations.",
  "",
  "Sheets: review (fill in), flag guide, name variants (how names change inside one organization's own filings),",
  "lists (dropdown values). The key sheet holds the score and sampling weights:",
  "keep it hidden from reviewers until the review is done.")
addWorksheet(wb, "README"); writeData(wb, "README", data.frame(README = readme), colNames = FALSE)
setColWidths(wb, "README", 1, 130)
addWorksheet(wb, "review")
writeData(wb, "review", review, headerStyle = createStyle(textDecoration = "bold", fgFill = "#e8e8e4", wrapText = TRUE))
freezePane(wb, "review", firstRow = TRUE, firstCol = TRUE)
setColWidths(wb, "review", seq_along(review), pmin(40, pmax(10, nchar(names(review)) + 2)))
addWorksheet(wb, "lists")
writeData(wb, "lists", data.frame(decision = c("same", "different", "unsure"), confidence = c("high", "medium", "low")))
for (cn in c("decision_1", "decision_2")) dataValidation(wb, "review", cols = match(cn, names(review)), rows = 2:(nrow(review) + 1),
                                                         type = "list", value = "'lists'!$A$2:$A$4")
for (cn in c("confidence_1", "confidence_2")) dataValidation(wb, "review", cols = match(cn, names(review)), rows = 2:(nrow(review) + 1),
                                                           type = "list", value = "'lists'!$B$2:$B$4")
addWorksheet(wb, "name variants")
writeData(wb, "name variants", "How one person's name varies inside one organization's own filings (all records in the CSA)", startRow = 1)
writeData(wb, "name variants", var_summary, startRow = 3, headerStyle = createStyle(textDecoration = "bold"))
writeData(wb, "name variants", "Examples", startRow = nrow(var_summary) + 6)
writeData(wb, "name variants", var_examples, startRow = nrow(var_summary) + 7, headerStyle = createStyle(textDecoration = "bold"))
setColWidths(wb, "name variants", 1:4, c(70, 45, 70, 12))
addWorksheet(wb, "flag guide")
writeData(wb, "flag guide", data.frame(column = c(
  "review_priority", "flags", "first_name_relation", "middle_initials", "generational_suffix", "gender_conflict", "surname_relation",
  "years_overlap / years_gap", "family_name_in_org", "forms_in_own_org_*", "drops_middle_* / nickname_switch_* / surname_varies_*",
  "first/last/full_name_count_*", "last_name_count_us_*", "full_name_expected_*", "known_homonym_orgs_*",
  "boards_*_if_matched / metros_*_if_matched", "max_boards_one_year_*"),
  meaning = c(
  "high: a conflict (initials, suffix, gender or first names differ), or 2+ risk signals with nothing corroborating; medium: one uncorroborated risk signal; low: the rest. Corroboration = shared board members, same meta-org, or agreeing middle initials",
  "The flags that fired, in words",
  "exact / nickname (same root, e.g. Rich-Richard) / initial only / different",
  "agree / conflict / one missing / both missing",
  "Jr, Sr, II, III, IV: agree / conflict / one only / none",
  "Genders recorded for the two names differ",
  "exact / compound or partial (Lehman-Hajek vs Hajek) / spelling",
  "Years both served (any organization) / years between their service if none overlap; 10+ years apart may be a parent and child",
  "The surname appears in either organization's name: a family foundation, where relatives share names",
  "Every form of the name seen in that record's own organization's filings",
  "This person's name varies inside their own organization: a missing initial on the other side is weak evidence of a different person",
  "How many records in the CSA carry this first name, last name, and full name",
  "How many people nationally carry this surname (all profiles)",
  "Records with this full name expected by chance in the CSA from first- and last-name frequencies; well above 1 means a repeat is unsurprising",
  "Organizations in the CSA where two different people share this exact full name: proof that homonyms with this name exist locally",
  "How many organizations / metros the score-7 match would put this name on",
  "The most boards that match would give this name in a single year; real people rarely hold many at once")),
  headerStyle = createStyle(textDecoration = "bold"))
setColWidths(wb, "flag guide", 1:2, c(48, 140))
addWorksheet(wb, "key", visible = FALSE); writeData(wb, "key", key)
addWorksheet(wb, "strata", visible = FALSE); writeData(wb, "strata", cells)
f <- file.path(out, sprintf("review-sample-csa%s.xlsx", CSA))
saveWorkbook(wb, f, overwrite = TRUE)
fwrite(key, file.path(out, sprintf("review-key-csa%s.csv", CSA)))
fwrite(cells, file.path(out, sprintf("review-strata-csa%s.csv", CSA)))
stamp("review sheet: %s (%d pairs)", f, nrow(review))
