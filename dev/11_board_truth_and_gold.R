# =============================================================================
# 11_board_truth_and_gold.R
# (1) INDEPENDENT board-truth metric: score cascade board labels against a
#     title-derived board signal that does NOT use the trustee box (breaks the
#     circularity in 09). (2) Stratified 150-row GOLD sample for hand-labeling.
# Run 10_role_passes.R first (role_refined_output.csv).
# =============================================================================
suppressMessages(library(dplyr))
base <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid/dev/"
L <- function(x) x=="TRUE"|x==TRUE; num <- function(x) suppressWarnings(as.numeric(x))
b01 <- function(x) as.integer(x) %in% 1L

r <- read.csv(paste0(base,"role_refined_output.csv"), stringsAsFactors=FALSE) %>%
  mutate(across(c(is_primary_row,paid,cb_tru,cb_off,cb_key), L)) %>% filter(is_primary_row)
clx <- readRDS(paste0(base,"data/classified_slice.rds"))
sub <- clx %>% group_by(OBJECTID, person.id) %>%
  summarise(v7=paste(unique(title.v7[!is.na(title.v7)]),collapse=" | "),
            f_dirvp=any(b01(dir.vp)), f_clevel=any(b01(c.level)),
            f_mgr=any(b01(mgr)), f_spec=any(b01(spec)), .groups="drop")
r <- r %>% left_join(sub, by=c("OBJECTID","person.id"))

# ---- INDEPENDENT board signal from TITLE only (no checkbox) ----
BOARD_TITLE <- "\\bBOARD\\b|\\bTRUSTEE\\b|CHAIR|COUNCIL|DELEGATE|\\bREGENT\\b|\\bELDER\\b|DEACON|VESTRY|OVERSEER"
r <- r %>% mutate(
  title_board = grepl(BOARD_TITLE, toupper(paste(title.standard, v7))),
  cascade_board = person_position %in% c("board","board_officer"),
  corrob = case_when(cb_tru & title_board ~ "both",
                     cb_tru & !title_board ~ "box-only (crosswalk gap)",
                     !cb_tru & title_board ~ "title-only (filer omission?)",
                     TRUE ~ "neither (uncertain)"))

cat("=========== INDEPENDENT BOARD-TRUTH (cascade board-labeled persons) ===========\n")
bl <- r %>% filter(cascade_board)
print(bl %>% count(corrob) %>% mutate(pct=round(n/sum(n),3)))
cat(sprintf("\ncascade board persons: %d | corroborated by >=1 independent signal: %.3f\n",
            nrow(bl), mean(bl$corrob!="neither (uncertain)")))
cat(sprintf("UNCERTAIN board (neither box nor board-title): %d  <- gold-review priority\n",
            sum(bl$corrob=="neither (uncertain)")))
# disagreement the OTHER way: trustee-box persons NOT labeled board (should be ~0 post-cascade)
cat(sprintf("trustee-box persons NOT cascade-board: %d\n", sum(r$cb_tru & !r$cascade_board)))

# =============================================================================
# STRATIFIED 150-ROW GOLD SAMPLE
# =============================================================================
set.seed(1234)
pick <- function(d, n, lbl) d %>% slice_sample(n=min(n, nrow(d))) %>% mutate(bucket=lbl)
board_gov_orgs <- r %>% filter(org_leadership=="board_governed") %>% pull(OBJECTID) %>% unique()

S <- bind_rows(
  pick(r %>% filter(leader_type=="ceo_designated"), 12, "ceo_designated"),
  pick(r %>% filter(leader_type=="co_ceo"), 12, "co_ceo"),
  pick(r %>% filter(leader_type=="chief_staff_imputed"), 16, "chief_staff_imputed"),
  pick(r %>% filter(OBJECTID %in% board_gov_orgs, paid), 12, "board_governed_org_paid"),
  pick(bl %>% filter(corrob=="both"), 10, "board_corroborated"),
  pick(bl %>% filter(corrob=="box-only (crosswalk gap)"), 16, "board_box_only"),
  pick(bl %>% filter(corrob=="title-only (filer omission?)"), 14, "board_title_only"),
  pick(r %>% filter(f_dirvp), 18, "dir.vp_ambiguous"),
  pick(r %>% filter(func=="officer", is.na(leader_type)), 10, "officer_non_ceo"),
  pick(r %>% filter(role_coarse=="KEY"), 8, "key_staff"),
  pick(r %>% filter(f_clevel), 10, "c.level"),
  pick(r %>% filter(f_spec), 10, "spec"),
  pick(r %>% filter(f_mgr), 10, "mgr")
) %>%
  transmute(bucket, OBJECTID, ein, taxyr, org.name, person.id,
            title.raw, title.v7=v7, title.standard,
            paid, comp, hrs, cb_tru, cb_off, cb_key,
            person_position, func, leader_type, role_coarse,
            board_corrob=corrob,
            gold_role="", gold_notes="")   # <- blanks for hand-labeling

cat(sprintf("\n=========== GOLD SAMPLE: %d rows across %d buckets ===========\n",
            nrow(S), n_distinct(S$bucket)))
print(count(S, bucket))
write.csv(S, paste0(base,"gold_sample_template.csv"), row.names=FALSE)
cat("\nWrote gold_sample_template.csv (fill gold_role / gold_notes)\n")
