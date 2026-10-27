# _hard_cases.R
# A library of hard matching cases for calibrating the cross-org matcher.
# Sourced by the check scripts (06i, 06k, 06l), after _panel.R.
#
# Every case is a pair of within-org records (emp_a, emp_b) -- or one record
# (emp_b NA) when the problem sits inside a single record -- with:
#   category      what kind of hard case (see hard_case_categories())
#   label         expected_same / expected_different    (ground truth or near it)
#                 likely_same / likely_different        (strong evidence, still review)
#                 uncertain                             (needs a person)
#   label_basis   the evidence behind the label, in words
# plus the evidence a reviewer needs (name forms, organizations, years) and the
# matcher's score where there is one.
#
# Names individual people: the library lives in the data folder, never the
# repository. SYNTHID_DIR/partvii-v3_1/hard-cases/<category>/csa=<CSA>.parquet
# hard_cases_read() returns the whole library as one table.

HARD_CASES <- file.path(pv_root, "hard-cases")

hard_case_categories <- function() data.table(
  category = c("co_present_homonym", "suffix_conflict", "corroborated_surname_variant", "married_name_change",
               "within_org_two_surnames", "cross_org_surname_different", "cross_org_first_name_different",
               "middle_initial_conflict", "first_initial_only", "suffix_jr_vs_ii", "pooled_variant_only",
               "within_org_suffix_conflict", "within_org_two_first_names", "within_org_gender_mixed",
               "within_org_two_rows_one_year", "particle_variant", "rare_name_match", "same_first_name_succession"),
  label = c("expected_different", "expected_different", "expected_same", "likely_same",
            "likely_different", "likely_different", "likely_different",
            "likely_different", "uncertain", "uncertain", "uncertain",
            "expected_different", "likely_different", "likely_different",
            "likely_different", "likely_same", "likely_same", "likely_different"),
  meaning = c(
    "Same full name, two people in one organization in the same years (the within-org linker kept them apart)",
    "Matched pair whose generational suffixes differ (JR vs SR, II vs III): a parent and child",
    "Matched pair with different surnames, corroborated by 2+ other board members the two organizations share",
    "Within one organization, a record ends and a new one starts with the same first name, title and pay and a new surname",
    "One within-org record holds two established surnames that are not spelling, compound or particle variants",
    "Matched pair whose surnames are two different established surnames",
    "Matched pair whose first names are two different established first names (not a nickname, initial or spelling)",
    "Matched pair whose middle initials differ",
    "Matched pair where one side gives only a first initial",
    "Matched pair with JR on one side and II on the other",
    "Match found only through a name variant pooled within an organization",
    "One within-org record holds two generational suffixes (JR and SR, II and III): a parent and child merged",
    "One within-org record holds two different established first names (not a nickname, initial or spelling)",
    "One within-org record has rows recorded as female and rows recorded as male",
    "One within-org record holds two rows in one filing year with different names or titles: two people",
    "Surnames differ only by a particle or MAC/MC (VAN DEUSEN vs DEUSEN): one person",
    "Matched pair with an identical, rare full name and an agreeing middle initial: near-certain",
    "A board seat passes from one person to another who shares a first name, under a generic title: two people"))

hard_case_side <- function(P, emp, onames) {
  k <- match(emp, P$EMP_ID)
  org <- vapply(P$orgs[k], function(o) if (length(o)) o[[1]] else NA_character_, "")
  data.table(ein = org, org = unname(onames[org]),
             forms = vapply(P$name_variants[k], function(v) if (length(v)) paste(unique(v), collapse = " / ") else NA_character_, ""),
             years = vapply(P$years[k], function(y) if (length(y)) sprintf("%d-%d", min(y), max(y)) else NA_character_, ""))
}

write_hard_cases <- function(x, category, label_basis, P, onames, csa, label = NULL) {
  if (!nrow(x)) return(invisible(0L))
  cats <- hard_case_categories()
  stopifnot(category %in% cats$category)
  x <- as.data.table(x)
  if (!"emp_b" %in% names(x)) x[, emp_b := NA_character_]
  if (!"score" %in% names(x)) x[, score := NA_real_]
  if (!"detail" %in% names(x)) x[, detail := NA_character_]
  a <- hard_case_side(P, x$emp_a, onames); b <- hard_case_side(P, x$emp_b, onames)
  out <- data.table(
    case_id = sprintf("%s-%s-%05d", category, csa, seq_len(nrow(x))), csa = csa, category = category,
    label = if (is.null(label)) cats$label[match(category, cats$category)] else label, label_basis = label_basis,
    emp_a = x$emp_a, emp_b = x$emp_b, score = round(x$score, 2),
    forms_a = a$forms, org_a = a$org, ein_a = a$ein, years_a = a$years,
    forms_b = b$forms, org_b = b$org, ein_b = b$ein, years_b = b$years,
    detail = x$detail, added = format(Sys.Date()))
  f <- file.path(HARD_CASES, category, paste0("csa=", csa, ".parquet"))
  write_parquet_atomic(as.data.frame(out), f)
  stamp("hard cases: %s %s -> %s", format(nrow(out), big.mark = ","), category, f)
  invisible(nrow(out))
}

hard_cases_read <- function() {
  f <- Sys.glob(file.path(HARD_CASES, "*", "*.parquet"))
  rbindlist(lapply(f, function(x) as.data.table(arrow::read_parquet(x))), fill = TRUE)
}
