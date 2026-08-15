# =============================================================================
# 06_role_cascade_prototype.R
#
# Layer-1 / Layer-2 role-disambiguation cascade (prototype), Full 990 x Paid.
# Target home: titleclassifier step 09 (conditional_logic). Nothing here touches
# the package or its regression.
#
# LAYER 1 - POSITION  {board, officer, board_officer, staff, unknown}
#   assigned per person-year-title, primarily from the filer's own checkboxes,
#   with comp+title as corroboration (the officer box is imperfect: 18.6% of
#   clear exec titles lack it, so it is never trusted alone).
#
# LAYER 2 - FUNCTION / SENIORITY  {ceo, coo, cfo, officer, board, staff}
#   resolved per org-year among the officers: CEO by title else top-paid officer;
#   working-board fallback; de-facto-leader flag. Co-CEO when comp ties (captures
#   2-exec orgs and transition-year overlaps).
#
# Reports how many of the 497 missing-CEO orgs and the phantom board-president
# rows the cascade resolves, and writes the residual as a labeling queue.
# =============================================================================

suppressMessages(library(dplyr))
base    <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid/dev/"
d0      <- readRDS(paste0(base, "data/classified_slice.rds"))

b01 <- function(x) as.integer(x) %in% 1L
num <- function(x) suppressWarnings(as.numeric(x))

d <- d0 %>% mutate(
  paid   = num(tot.comp) > 0 & !is.na(num(tot.comp)),
  comp   = ifelse(is.na(num(tot.comp)), 0, num(tot.comp)),
  hrs    = ifelse(is.na(num(tot.hours)), 0, num(tot.hours)),
  cb_tru = b01(dtk.indiv.trustee.x) | b01(dtk.inst.trustee.x),
  cb_off = b01(dtk.officer.x),
  cb_key = b01(dtk.key.empl.x) | b01(dtk.high.comp.x),
  # nominal title signals (from crosswalk flags + keywords)
  t_all      = toupper(paste(title.raw, title.standard, title.v7)),
  ceo_title  = grepl("\\bCEO\\b|CHIEF EXEC|EXECUTIVE DIRECTOR|\\bEXEC DIR", t_all),
  coo_title  = grepl("CHIEF OPERATING|\\bCOO\\b", t_all),
  cfo_title  = grepl("CHIEF FINANC|\\bCFO\\b", t_all),
  exec_title = b01(ceo) | b01(c.level) | ceo_title | coo_title | cfo_title,
  board_title= b01(board) |
               grepl("BOARD|TRUSTEE|DIRECTOR|CHAIR|COMMITTEE|COUNCIL|ELDER|DEACON|REGENT|WARDEN", t_all)
)

# ---- cohort: full 990 x paid (org has >=1 compensated person) ----------------
cohort <- d %>% group_by(OBJECTID) %>%
  summarise(any_paid = any(paid), ft = formtype[1], .groups = "drop") %>%
  filter(ft == "990", any_paid) %>% pull(OBJECTID)
dc <- d %>% filter(OBJECTID %in% cohort)

# =============================================================================
# LAYER 1 - POSITION
# =============================================================================
dc <- dc %>% mutate(
  position = case_when(
    # a paid EXEC title outranks the key/high-comp box: on a full 990 execs are
    # often reported as "highly compensated employees" instead of checking officer.
    exec_title & paid & !cb_tru      ~ "officer",
    exec_title & paid &  cb_tru      ~ "board_officer",   # board member serving as exec
    cb_off &  cb_tru                 ~ "board_officer",   # working board
    cb_off & !cb_tru                 ~ "officer",
   !cb_off &  cb_tru                 ~ "board",
    cb_key                           ~ "staff",
    board_title & !paid              ~ "board",
    paid                             ~ "staff",
    TRUE                             ~ "unknown"),
  pos_source = case_when(
    exec_title & paid                ~ "title+pay",
    cb_off | cb_tru | cb_key         ~ "checkbox",
    TRUE                             ~ "title"),
  is_officer = position %in% c("officer","board_officer"),
  is_board   = position %in% c("board","board_officer")
)

# =============================================================================
# LAYER 1.5 - SPLIT-TITLE DEDUP: collapse to person level
# A conjoined title (CEO/PRESIDENT, CEO/EXECUTIVE DIRECTOR, PRESIDENT/CHAIRMAN)
# is fanned into >1 row by step 04, each carrying the SAME comp + checkboxes.
# Resolve ONE role per (org, person) so a person is never counted as 2 CEOs.
# Mark a single primary row per person so headcounts don't double-count.
# =============================================================================
pos_rank <- c(officer = 1, board_officer = 2, board = 3, staff = 4, unknown = 5)
dc <- dc %>% group_by(OBJECTID, person.id) %>%
  mutate(is_primary_row = seq_len(n()) == which.min(pos_rank[position])[1]) %>%
  ungroup()

# one record per (org, person): most-senior position, any-checkbox, max comp/hrs
pers <- dc %>% group_by(OBJECTID, person.id) %>%
  summarise(paid = any(paid), comp = max(comp), hrs = max(hrs),
            ceo_title = any(ceo_title), coo_title = any(coo_title),
            cfo_title = any(cfo_title),
            t_all = paste(unique(t_all), collapse = " "),
            position = names(pos_rank)[min(pos_rank[position])],
            .groups = "drop") %>%
  mutate(is_officer = position %in% c("officer","board_officer"),
         is_board   = position %in% c("board","board_officer"))

# =============================================================================
# LAYER 2 - FUNCTION / SENIORITY  (per org-year, on deduped persons)
# =============================================================================
resolve_org <- function(g) {                 # g = persons of one org-year
  g$func   <- ifelse(g$is_board & !g$is_officer, "board",
              ifelse(g$position == "staff", "staff", "officer"))
  g$func_src <- "position"

  paid_off <- which(g$is_officer & g$paid)
  ceo_idx  <- integer(0)

  if (length(paid_off)) {
    titled <- paid_off[g$ceo_title[paid_off]]
    if (length(titled)) {
      ceo_idx <- titled; ceo_src <- "title"
    } else {
      mx <- max(g$comp[paid_off])
      ceo_idx <- paid_off[g$comp[paid_off] == mx]      # ties -> co-CEO (distinct persons)
      ceo_src <- "top-paid-officer"
    }
  } else {
    wb <- which(g$position == "board_officer" & g$paid &
                grepl("PRESIDENT|CHAIR", g$t_all))
    if (length(wb)) {
      mx <- max(g$comp[wb]); ceo_idx <- wb[g$comp[wb] == mx]; ceo_src <- "working-board"
    } else {
      pb <- which(g$is_board & g$paid)
      if (length(pb)) {
        mx <- max(g$comp[pb]); ceo_idx <- pb[g$comp[pb] == mx]; ceo_src <- "de-facto-leader"
      } else ceo_src <- NA_character_
    }
  }
  if (length(ceo_idx)) { g$func[ceo_idx] <- "ceo"; g$func_src[ceo_idx] <- ceo_src }

  co <- which(g$is_officer & g$coo_title & g$func != "ceo"); g$func[co] <- "coo"
  cf <- which(g$is_officer & g$cfo_title & g$func != "ceo"); g$func[cf] <- "cfo"
  g
}
pers <- pers %>% group_by(OBJECTID) %>% group_modify(~resolve_org(.x)) %>% ungroup()

# map person-level resolution back onto every row (both split halves share it)
dc <- dc %>%
  left_join(pers %>% select(OBJECTID, person.id,
                            person_position = position, func, func_src),
            by = c("OBJECTID","person.id"))

# =============================================================================
# RESULTS
# =============================================================================
# org-level metrics computed on DEDUPED persons
orgm <- pers %>% group_by(OBJECTID) %>%
  summarise(has_ceo_after = any(func == "ceo"),
            n_ceo_after   = sum(func == "ceo"),   # distinct persons (pers is 1/person)
            ceo_inferred  = any(func == "ceo" & func_src %in%
                                c("top-paid-officer","working-board","de-facto-leader")),
            .groups = "drop")
org <- dc %>% group_by(OBJECTID) %>%
  summarise(had_exec_before = any(b01(ceo) | b01(c.level)),
            n_paid = sum(paid), .groups="drop") %>%
  left_join(orgm, by = "OBJECTID")

cat("================ CASCADE RESULTS (full-990 x paid) ================\n")
cat(sprintf("org-years: %d   rows: %d   persons: %d\n\n",
            nrow(org), nrow(dc), nrow(pers)))

cat("--- CEO identification (your primary test) ---\n")
cat(sprintf("orgs w/ exec BEFORE (ceo|c.level): %d (%.1f%%)\n",
            sum(org$had_exec_before), 100*mean(org$had_exec_before)))
cat(sprintf("orgs w/ CEO AFTER cascade:         %d (%.1f%%)\n",
            sum(org$has_ceo_after), 100*mean(org$has_ceo_after)))
miss_before <- org %>% filter(!had_exec_before)
cat(sprintf("of the %d missing-exec orgs: now resolved: %d | still none: %d\n",
            nrow(miss_before), sum(miss_before$has_ceo_after),
            sum(!miss_before$has_ceo_after)))
cat(sprintf("orgs w/ co-CEOs (>=2, 2-exec/transition): %d\n", sum(org$n_ceo_after>=2)))
cat(sprintf("CEO assignment inferred (not title-certain): %d orgs\n\n",
            sum(org$ceo_inferred, na.rm=TRUE)))

cat("--- Function distribution (deduped, one role per person) ---\n")
print(table(pers$func))
cat(sprintf("\nsplit-title dedup: %d rows collapse to %d persons (%d multi-row persons)\n",
            nrow(dc), nrow(pers), sum(table(paste(dc$OBJECTID, dc$person.id)) > 1)))

cat("\n--- Position layer vs. old label ---\n")
cat("position distribution (rows):\n"); print(table(dc$position))
cat("\nphantom board-president fix: rows old=board but position=officer/board_officer:\n")
ph <- dc %>% filter(b01(board), position %in% c("officer","board_officer"))
cat(sprintf("  %d rows (e.g. paid CEO/PRESIDENT split-halves reassigned)\n", nrow(ph)))

cat("\n--- Board capture: old board=1 now reassigned off board ---\n")
cat(sprintf("  old board=1 -> now non-board position: %d\n",
            sum(b01(dc$board) & !dc$is_board)))
cat(sprintf("  old board=0 -> now board position (recovered): %d\n",
            sum(!b01(dc$board) & dc$is_board)))

# residual = labeling queue: orgs still w/o ceo, + rows where checkbox<->title conflict
residual_orgs <- miss_before %>% filter(!has_ceo_after)
conflict_rows <- dc %>% filter((cb_tru & exec_title & !cb_off) |   # trustee box + exec title
                               (cb_off & board_title & !cb_tru & !paid)) # officer box + unpaid board title
write.csv(dc %>% transmute(OBJECTID, ein, taxyr, org.name, person.id, is_primary_row,
                           title.raw, title.standard, paid, comp, hrs,
                           cb_tru, cb_off, cb_key, position, person_position, pos_source,
                           func, func_src),
          paste0(base, "role_cascade_output.csv"), row.names=FALSE)
write.csv(conflict_rows %>% transmute(OBJECTID, ein, taxyr, org.name, person.id,
                           title.raw, title.standard, paid, cb_tru, cb_off, position, func),
          paste0(base, "role_cascade_labeling_queue.csv"), row.names=FALSE)

cat(sprintf("\nResidual: %d orgs still no CEO | %d checkbox<->title conflict rows (labeling queue)\n",
            nrow(residual_orgs), nrow(conflict_rows)))
cat("Wrote: role_cascade_output.csv, role_cascade_labeling_queue.csv\n")
