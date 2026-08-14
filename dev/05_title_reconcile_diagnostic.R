# =============================================================================
# 05_title_reconcile_diagnostic.R
#
# Phase-1 diagnostic for the title-reconciliation / role-disambiguation step.
#
# GOAL (not temporal correctness): inspect whether ambiguous titles
# (president, director, deputy, chair, vice president, and odd board names like
# "elder") are assigned the correct LEVEL -- board governance vs. paid
# officer/leadership vs. paid staff.
#
# Signal of record: the IRS filer's own Part VII position checkboxes
#   dtk.indiv.trustee.x / dtk.inst.trustee.x  -> filer says BOARD/TRUSTEE
#   dtk.officer.x                             -> filer says OFFICER
#   dtk.key.empl.x / dtk.high.comp.x          -> filer says PAID STAFF/KEY
# plus compensation (tot.comp > 0) and hours. These are absent on 990EZ, which
# is why this pass is scoped to FULL 990 x PAID STAFF only.
#
# Master table  : classified_slice.rds  (row-complete at title level; has strata,
#                 role flags, checkboxes, comp, org-level counts)
# Link attached : EMP_ID from linked_slice_enriched.rds (secondary cross-time lens)
# Merge key     : OBJECTID + TABLE_ID  (classified is master; enriched -> classified)
#
# Outputs (written to synthid/dev/):
#   title_diagnostic_summary.csv     bucket counts
#   title_diagnostic_flagged_rows.csv person-year rows flagged, w/ reason + signals
#   title_diagnostic_orgyears.csv    org-year level flags (paid-no-exec, etc.)
#
# Nothing here touches the titleclassifier package or its frozen regression.
# =============================================================================

suppressMessages(library(dplyr))

base    <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid/dev/"
datadir <- paste0(base, "data/")

clx <- readRDS(paste0(datadir, "classified_slice.rds"))
enr <- readRDS(paste0(datadir, "linked_slice_enriched.rds"))

# ---- attach EMP_ID (cross-time link) onto the classified master --------------
# enriched is expected unique on OBJECTID+TABLE_ID; classified is NOT (split
# titles fan a DTK line into >1 row). Joining link -> master duplicates the link
# onto each split-title row, which is correct (same person-year-line).
link <- enr %>%
  transmute(OBJECTID, TABLE_ID, EMP_ID, EMP_N_YEARS, EMP_N_RECORDS)
stopifnot(!anyDuplicated(paste(link$OBJECTID, link$TABLE_ID)))

d <- clx %>% left_join(link, by = c("OBJECTID", "TABLE_ID"))

# ---- helper signals ----------------------------------------------------------
b01 <- function(x) as.integer(x) %in% 1L          # NA-safe 0/1 -> TRUE only on 1
num <- function(x) suppressWarnings(as.numeric(x))

d <- d %>%
  mutate(
    paid        = num(tot.comp) > 0 & !is.na(num(tot.comp)),
    hrs         = num(tot.hours),
    cb_trustee  = b01(dtk.indiv.trustee.x) | b01(dtk.inst.trustee.x),
    cb_officer  = b01(dtk.officer.x),
    cb_key      = b01(dtk.key.empl.x) | b01(dtk.high.comp.x),
    # current classifier verdict, collapsed to a level
    lab_board   = b01(board),
    lab_exec    = b01(ceo) | b01(c.level),
    lab_emp     = b01(emp),
    lab_level   = case_when(
                    lab_exec  ~ "paid_exec",
                    lab_emp   ~ "paid_staff",
                    lab_board ~ "board",
                    TRUE      ~ "other"),
    # filer's own signal, collapsed to a level (checkbox + comp)
    filer_level = case_when(
                    cb_trustee & !cb_officer & !paid ~ "board",
                    cb_officer &  paid               ~ "paid_officer",
                    cb_key     |  paid               ~ "paid_staff",
                    cb_trustee                       ~ "board",
                    cb_officer                       ~ "paid_officer",
                    TRUE                             ~ "unknown")
  )

# ---- cohort: FULL 990 x PAID STAFF (org-year has >=1 compensated person) ------
org_paid <- d %>% group_by(OBJECTID) %>%
  summarise(any_paid = any(paid), formtype = formtype[1], .groups = "drop")

cohort_ids <- org_paid %>%
  filter(formtype == "990", any_paid) %>% pull(OBJECTID)

dc <- d %>% filter(OBJECTID %in% cohort_ids)

cat(sprintf("Cohort: full-990 x paid  |  org-years=%d  person-year-title rows=%d\n\n",
            length(cohort_ids), nrow(dc)))

# =============================================================================
# ROW-LEVEL SUSPICION FLAGS  (label vs. filer signal disagreement)
# =============================================================================
amb_titles <- c("BOARD PRESIDENT","PRESIDENT","VICE PRESIDENT","BOARD VICE PRESIDENT",
                "DIRECTOR","DEPUTY DIRECTOR","EXECUTIVE DIRECTOR",
                "CHAIR","BOARD CHAIR","VICE CHAIR","CHAIRMAN")
odd_board_kw <- "ELDER|DEACON|OVERSEER|WARDEN|REGENT|COMMANDER|EXALTED|GRAND|NOBLE|TRUSTEE|GOVERNOR|PRELATE|CHAPLAIN|SEXTON|MODERATOR|PRESBYTER"

dc <- dc %>%
  mutate(
    # (A) labeled BOARD but paid + officer box, no trustee box -> likely paid officer/exec
    F_board_but_paid_officer = lab_board & paid & cb_officer & !cb_trustee,
    # (B) labeled paid (emp/exec) but trustee box + unpaid + no officer box -> likely board
    F_paid_but_trustee_unpaid = (lab_emp | lab_exec) & cb_trustee & !paid & !cb_officer,
    # (C) ambiguous title whose label disagrees with filer level
    F_ambiguous_conflict = title.standard %in% amb_titles &
                           filer_level != "unknown" &
                           ((lab_board & filer_level %in% c("paid_officer","paid_staff")) |
                            ((lab_emp|lab_exec) & filer_level == "board")),
    row_flag = F_board_but_paid_officer | F_paid_but_trustee_unpaid | F_ambiguous_conflict
  )

# =============================================================================
# ORG-YEAR LEVEL FLAGS
# =============================================================================
orgyr <- dc %>%
  group_by(OBJECTID) %>%
  summarise(
    ein = ein[1], taxyr = taxyr[1], org.name = org.name[1],
    n_people   = n_distinct(person.id),
    n_rows     = n(),
    n_paid     = sum(paid),
    n_exec_lab = sum(lab_exec),
    n_emp_lab  = sum(lab_emp),
    n_board_lab= sum(lab_board),
    n_paid_officerbox = sum(paid & cb_officer),
    # a paid person who could be the missing exec: paid + (officer box | exec-ish title)
    has_exec_candidate = any(paid & (cb_officer |
                          grepl("PRESIDENT|DIRECTOR|CEO|CHIEF|MANAGER|ADMINISTRATOR",
                                 toupper(title.standard)))),
    .groups = "drop") %>%
  mutate(
    # your primary test: paid staff exist but NO exec surfaced
    O_paid_no_exec   = n_paid > 0 & n_exec_lab == 0,
    # everyone labeled board despite paid staff
    O_all_board_paid = n_paid > 0 & n_exec_lab == 0 & n_emp_lab == 0,
    # paid-no-exec but there IS a plausible exec to promote
    O_recoverable_exec = O_paid_no_exec & has_exec_candidate
  )

# ---- shared odd-name board titles: >=3 people, same title, all unpaid --------
shared_odd <- dc %>%
  group_by(OBJECTID, title.standard) %>%
  summarise(n = n_distinct(person.id),
            all_unpaid = all(!paid),
            any_officerbox = any(cb_officer),
            odd = any(grepl(odd_board_kw, toupper(paste(title.standard, title.raw)))),
            labeled_paid = any(lab_emp | lab_exec),
            .groups = "drop") %>%
  filter(n >= 3, all_unpaid, !any_officerbox, (odd | labeled_paid))

# ---- consistent unpaid "directors" labeled employee --------------------------
consistent_dirs <- dc %>%
  filter(grepl("\\bDIRECTOR\\b", toupper(title.standard)),
         !grepl("EXECUTIVE|DEPUTY|MANAGING", toupper(title.standard))) %>%
  group_by(OBJECTID) %>%
  summarise(n_dir = n_distinct(person.id),
            all_unpaid = all(!paid),
            any_trustee_box = any(cb_trustee),
            labeled_emp = any(lab_emp),
            .groups = "drop") %>%
  filter(n_dir >= 3, all_unpaid, labeled_emp)

# =============================================================================
# CROSS-TIME (secondary): person whose board-vs-paid label flips within an org
# =============================================================================
flip <- dc %>%
  filter(!is.na(EMP_ID)) %>%
  group_by(EMP_ID) %>%
  summarise(n_years = n_distinct(taxyr),
            levels  = paste(sort(unique(lab_level)), collapse="|"),
            flips_board_paid = any(lab_level == "board") &
                               any(lab_level %in% c("paid_exec","paid_staff")),
            .groups = "drop") %>%
  filter(n_years >= 2, flips_board_paid)

# =============================================================================
# SUMMARY
# =============================================================================
summ <- tibble::tribble(
  ~bucket, ~unit, ~n,
  "cohort org-years (full990 x paid)",        "org-year", length(cohort_ids),
  "cohort rows",                              "row",      nrow(dc),
  "ROW: board-labeled but paid+officer box",  "row",      sum(dc$F_board_but_paid_officer),
  "ROW: paid-labeled but trustee box+unpaid", "row",      sum(dc$F_paid_but_trustee_unpaid),
  "ROW: ambiguous-title label conflict",      "row",      sum(dc$F_ambiguous_conflict),
  "ROW: any suspicion flag",                  "row",      sum(dc$row_flag),
  "ORG: paid staff but NO exec surfaced",     "org-year", sum(orgyr$O_paid_no_exec),
  "ORG:   ...of which recoverable (has cand)","org-year", sum(orgyr$O_recoverable_exec),
  "ORG: paid but everyone labeled board",     "org-year", sum(orgyr$O_all_board_paid),
  "ORG: shared odd-name board titles (>=3)",  "org-title",nrow(shared_odd),
  "ORG: >=3 unpaid directors labeled emp",    "org-year", nrow(consistent_dirs),
  "TIME: persons flipping board<->paid",      "person",   nrow(flip)
)
cat("================ DIAGNOSTIC SUMMARY ================\n")
print(as.data.frame(summ), row.names = FALSE)
cat("\n")

# =============================================================================
# WRITE OUTPUTS
# =============================================================================
flagged_rows <- dc %>%
  filter(row_flag) %>%
  mutate(reason = trimws(paste(
           ifelse(F_board_but_paid_officer, "board_but_paid_officer;", ""),
           ifelse(F_paid_but_trustee_unpaid,"paid_but_trustee_unpaid;", ""),
           ifelse(F_ambiguous_conflict,     "ambiguous_conflict;", "")))) %>%
  transmute(OBJECTID, TABLE_ID, EMP_ID, ein, taxyr, org.name,
            person.id, title.raw, title.standard, strata.label,
            lab_level, filer_level,
            paid, tot.comp, tot.hours,
            cb_trustee, cb_officer, cb_key,
            reason) %>%
  arrange(ein, taxyr, org.name)

write.csv(summ,          paste0(base, "title_diagnostic_summary.csv"),      row.names = FALSE)
write.csv(flagged_rows,  paste0(base, "title_diagnostic_flagged_rows.csv"), row.names = FALSE)
write.csv(orgyr %>% filter(O_paid_no_exec | O_all_board_paid),
          paste0(base, "title_diagnostic_orgyears.csv"),   row.names = FALSE)

cat(sprintf("Wrote:\n  %s\n  %s (%d rows)\n  %s\n",
            "title_diagnostic_summary.csv",
            "title_diagnostic_flagged_rows.csv", nrow(flagged_rows),
            "title_diagnostic_orgyears.csv"))
