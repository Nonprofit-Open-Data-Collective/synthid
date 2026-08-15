# =============================================================================
# 10_role_passes.R  --  classifier-layer passes, one per scorecard category.
# Runs ON TOP of the cascade output (role_cascade_output.csv).
# Target home: titleclassifier step 09. Status: [x] impl [~] partial [ ] stub.
# =============================================================================
suppressMessages(library(dplyr))
base <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid/dev/"
num <- function(x) suppressWarnings(as.numeric(x))
LEAD <- "\\bCEO\\b|CHIEF|EXECUTIVE|PRESIDENT|\\bDIRECTOR\\b|MANAGER|ADMINISTRATOR|SUPERINTENDENT|PRINCIPAL|\\bDEAN\\b|\\bHEAD\\b|OFFICER"

# ---------------------------------------------------------------------------
# [x] PASS 1 - resolve leadership (designated CEO / co-CEO / interim /
#     chief_staff_imputed / board_governed). Imputed leaders are FLAGGED distinct
#     from designated CEOs; a genuine board-run org (no plausible paid lead) gets
#     NO CEO and is tagged board_governed instead of crowning a support staffer.
# ---------------------------------------------------------------------------
pass_resolve_leadership <- function(df) {
  p <- df %>% filter(is_primary_row) %>%
    mutate(T = toupper(paste(title.raw, title.standard)),
           lead_title = grepl(LEAD, T),
           interim = grepl("INTERIM|ACTING|\\bTEMP\\b", T),
           pres_chair = grepl("PRESIDENT|CHAIR", T) & !grepl("VICE", T),
           # FIX 1: a paid board president/chair (trustee box + few hours) is a
           # working/titular BOARD CHAIR, NOT a chief-staff CEO -> exclude from imputation
           board_chair_pattern = cb_tru & hrs < 25 & pres_chair,
           designated = func == "ceo" & func_src == "title")
  resolve <- function(g, key) {
    g$leader_type <- NA_character_
    des <- which(g$designated)
    if (length(des)) g$leader_type[des] <- ifelse(g$interim[des], "interim_ceo", "ceo_designated")
    n_desig <- length(des)
    if (n_desig >= 2) g$leader_type[g$leader_type == "ceo_designated"] <- "co_ceo"
    # reset any cascade-imputed CEO, then re-impute with the board-chair exclusion
    imp0 <- which(g$func == "ceo" & !g$designated)
    g$func[imp0] <- ifelse(g$person_position[imp0] %in% c("board","board_officer"),
                           "board", ifelse(g$paid[imp0], "staff", "board"))
    if (n_desig == 0 && any(g$paid)) {
      elig <- which(g$paid & !g$board_chair_pattern & (g$cb_off | g$lead_title))
      if (length(elig)) {
        pick <- elig[which.max(g$comp[elig])]
        g$func[pick] <- "ceo"; g$leader_type[pick] <- "chief_staff_imputed"
      }
    }
    g$org_leadership <- if (any(g$leader_type %in% c("ceo_designated","co_ceo","interim_ceo"))) "designated"
                        else if (any(g$leader_type == "chief_staff_imputed", na.rm=TRUE)) "chief_staff_imputed"
                        else if (any(g$paid)) "board_governed" else "no_paid"
    g
  }
  p <- p %>% group_by(OBJECTID) %>% group_modify(resolve) %>% ungroup()
  df %>% select(-any_of(c("leader_type","org_leadership","func"))) %>%
    left_join(p %>% select(OBJECTID, person.id, func, leader_type, org_leadership),
              by = c("OBJECTID","person.id"))
}

# ---------------------------------------------------------------------------
# [x] PASS 2 - annotate board officers (president/chair, secretary, treasurer).
#     NO hard one-per-org cap: multiples are allowed and tagged as
#     outgoing / incoming / interim / current, with concurrency noted.
# ---------------------------------------------------------------------------
pass_annotate_board_officers <- function(df) {
  ts <- toupper(df$title.standard); rw <- toupper(df$title.raw)
  onboard <- df$person_position %in% c("board","board_officer")
  df$board_officer_role <- case_when(
    onboard & grepl("BOARD PRESIDENT|BOARD CHAIR|CHAIRMAN|\\bCHAIR\\b", ts) ~ "president/chair",
    onboard & grepl("BOARD SECRETARY", ts) ~ "secretary",
    onboard & grepl("BOARD TREASURER", ts) ~ "treasurer", TRUE ~ NA_character_)
  df$board_officer_status <- case_when(
    is.na(df$board_officer_role) ~ NA_character_,
    grepl("FORMER|OUTGOING|PAST|RETIR|RESIGN|EMERIT|THROUGH|UNTIL", rw) ~ "outgoing",
    grepl("ELECT|INCOMING|EFFECTIVE|AS OF|FUTURE|\\bNEW\\b|BEGIN", rw) ~ "incoming",
    grepl("INTERIM|ACTING", rw) ~ "interim", TRUE ~ "current")
  # concurrency: >1 CURRENT distinct person in same org+role (allowed, just tagged)
  conc <- df %>% filter(is_primary_row, !is.na(board_officer_role), board_officer_status=="current") %>%
    count(OBJECTID, board_officer_role, name="n_current")
  df <- df %>% left_join(conc, by=c("OBJECTID","board_officer_role")) %>%
    mutate(board_officer_concurrency = case_when(
      is.na(board_officer_role) ~ NA_character_,
      board_officer_status!="current" ~ "transition",
      n_current>1 ~ "shared/concurrent", TRUE ~ "single"))
  df
}

# [x] PASS 3 - dir.vp unravel: (1) partition board members out, (2) route the
#     non-board residual UP (officer/c-level) or DOWN (manager) by title. The
#     up/down title routing is meant to migrate into the crosswalk (see the
#     dirvp_crosswalk_suggestions.csv the demo writes). Runs after add_role_coarse.
pass_unravel_dirvp <- function(df) {
  if (!"f_dirvp" %in% names(df)) { cat("[ ] unravel_dirvp: no f_dirvp flag; skipped\n"); return(df) }
  T <- toupper(paste(df$title.raw, df$title.standard))
  chief_t   <- grepl("CHIEF|\\bC[EFIOMTA]O\\b|GENERAL COUNSEL", T)
  senior_vp <- grepl("EXECUTIVE VICE|SENIOR VICE|\\bEVP\\b|\\bSVP\\b|VICE PRESIDENT OF (FINANCE|OPERAT|ADMINIST|DEVELOP|PROGRAM|EXTERNAL|BUSINESS|MARKET|STRATEG)", T)
  onboard   <- df$person_position %in% c("board","board_officer")
  isvp      <- df$f_dirvp %in% TRUE

  df$dirvp_disposition <- NA_character_
  df$dirvp_disposition[isvp & onboard] <- "board"                       # (1) partition board out
  resid <- isvp & !onboard                                             # (2) non-board residual
  up <- resid & (chief_t | senior_vp | (df$cb_off & df$comp >= 100000) |
                 df$func %in% c("cfo","coo","officer"))
  df$dirvp_disposition[up]            <- "up:officer/c-level"
  df$dirvp_disposition[resid & !up]   <- "down:manager"

  # apply routing to the coarse layer (never override a resolved CEO or BOARD)
  df$role_coarse[which(df$dirvp_disposition == "down:manager" &
                       !df$role_coarse %in% c("CEO","BOARD"))] <- "MANAGER"
  ix <- which(df$dirvp_disposition == "up:officer/c-level" &
              !df$role_coarse %in% c("CEO","OFFICER","BOARD"))
  df$role_coarse[ix] <- "OFFICER"

  cat(sprintf("[x] unravel_dirvp: %d dir.vp -> board %d | up(officer/c-level) %d | down(manager) %d\n",
      sum(isvp), sum(df$dirvp_disposition=="board", na.rm=TRUE),
      sum(df$dirvp_disposition=="up:officer/c-level", na.rm=TRUE),
      sum(df$dirvp_disposition=="down:manager", na.rm=TRUE)))
  df
}
# [ ] PASS 4 - assess c.level / mgr / spec reliability -> keep vs KEY collapse. STUB.
pass_assess_finegrained <- function(df) {
  cat("[ ] assess_finegrained STUB: c.level/spec coherent (keep); mgr small; ",
      "emit reliability + role_coarse\n"); df
}

# FIX 2: the high-comp checkbox alone lumps program managers with highly-paid
# non-managers. Split by title -> MANAGER vs KEY (true key employees).
MGR_T <- "MANAGER|DIRECTOR OF|PROGRAM DIRECTOR|SUPERVISOR|COORDINAT|ADMINISTRATOR|\\bDEAN\\b|PRINCIPAL|SUPERINTENDENT|\\bHEAD\\b|CHIEF"
add_role_coarse <- function(df) df %>% mutate(
  .mgr_t = grepl(MGR_T, toupper(paste(title.raw, title.standard))),
  role_coarse = case_when(
    func == "ceo"                                   ~ "CEO",
    person_position %in% c("board","board_officer") ~ "BOARD",
    func %in% c("cfo","coo","officer")              ~ "OFFICER",
    paid & cb_key & .mgr_t                          ~ "MANAGER",
    paid & cb_key                                   ~ "KEY",
    paid & .mgr_t                                   ~ "MANAGER",
    TRUE                                            ~ "STAFF")) %>% select(-.mgr_t)

role_refine <- function(df) df %>%
  pass_resolve_leadership() %>% pass_annotate_board_officers() %>%
  pass_assess_finegrained() %>% add_role_coarse() %>% pass_unravel_dirvp()

if (sys.nframe() == 0) {
  L <- function(x) x=="TRUE"|x==TRUE
  cas <- read.csv(paste0(base,"role_cascade_output.csv"), stringsAsFactors=FALSE) %>%
    mutate(across(c(is_primary_row,paid,cb_tru,cb_off,cb_key), L))
  # attach the crosswalk dir.vp flag from classified for the unravel pass
  clx <- readRDS(paste0(base,"data/classified_slice.rds"))
  dv <- clx %>% group_by(OBJECTID, person.id) %>%
    summarise(f_dirvp = any(as.integer(dir.vp) %in% 1L), .groups="drop")
  cas <- cas %>% left_join(dv, by=c("OBJECTID","person.id"))
  out <- role_refine(cas)
  prim <- out %>% filter(is_primary_row)
  paidorg <- prim %>% group_by(OBJECTID) %>% summarise(np=sum(paid), hc=any(func=="ceo"))
  cat("\n=== FIX 1 (leadership) ===\n")
  cat(sprintf("CEO persons: %d (was 1422)  | paid orgs with CEO: %.1f%% (was 100%% pre-fix)\n",
              sum(prim$func=="ceo"), 100*mean(paidorg$hc[paidorg$np>0])))
  print(prim %>% distinct(OBJECTID, org_leadership) %>% count(org_leadership))
  cat("leader_type (persons):\n"); print(count(prim, leader_type))
  cat("\n=== FIX 2 (KEY split) ===\n")
  cat("role_coarse (persons)  [KEY was 1264, now split KEY vs MANAGER]:\n")
  print(count(prim, role_coarse))

  cat("\n=== PASS 3 dir.vp unravel ===\n")
  print(prim %>% filter(f_dirvp) %>% count(dirvp_disposition))
  # crosswalk suggestions: for non-board dir.vp, majority up/down per title.standard
  sugg <- prim %>% filter(f_dirvp, dirvp_disposition!="board", !is.na(dirvp_disposition)) %>%
    group_by(title.standard) %>%
    summarise(n=n(), up=sum(dirvp_disposition=="up:officer/c-level"),
              down=sum(dirvp_disposition=="down:manager"),
              suggested_level = ifelse(up>=down,"OFFICER/C-LEVEL","MANAGER"),
              med_comp=round(median(comp)), .groups="drop") %>% arrange(desc(n))
  write.csv(sugg, paste0(base,"dirvp_crosswalk_suggestions.csv"), row.names=FALSE)
  cat("top dir.vp title.standard routings (-> crosswalk):\n")
  print(as.data.frame(head(sugg, 12)), row.names=FALSE)

  write.csv(out, paste0(base,"role_refined_output.csv"), row.names=FALSE)
  cat("\nWrote role_refined_output.csv, dirvp_crosswalk_suggestions.csv\n")
}
