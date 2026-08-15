# =============================================================================
# 08_board_crosswalk_patch.R
# Extract UNAMBIGUOUS board-title gaps -> proposed STANDARDIZATION patch
# (title.variant -> title.standard = BOARD MEMBER).
#
# Two gap flavors, both keyed on title.v7 (the cleaned pre-standardization form):
#   (a) unmapped: title.v7 -> title.standard = NA  (REGULAR/DELEGATE TRUSTEE...)
#   (b) mis-standardized: e.g. BOARD AND DIRECTOR -> "DIRECTOR" (board=0)
#
# A 1:1 crosswalk can only hold context-free board titles, so we keep only
# inherently-governance strings (TRUSTEE/COUNCIL/DELEGATE/REGENT/ELDER/...),
# EXCLUDE officer/exec sub-roles (they map to BOARD PRES/SEC/etc or are cascade
# territory), FLAG conjunction/dual titles for manual review, validate each with
# its trustee-box rate, and cross-reference the workbook.
# =============================================================================
suppressMessages({library(dplyr); library(readxl)})
base <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid/dev/"
xls  <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/titleclassifier/data-dev/title-taxonomy-map.xlsx"
d <- readRDS(paste0(base,"data/classified_slice.rds"))
b01 <- function(x) as.integer(x) %in% 1L
num <- function(x) suppressWarnings(as.numeric(x))
d <- d %>% mutate(
  paid=num(tot.comp)>0 & !is.na(num(tot.comp)),
  cb_tru=b01(dtk.indiv.trustee.x)|b01(dtk.inst.trustee.x),
  cb_off=b01(dtk.officer.x), full990=formtype=="990",
  v7=toupper(trimws(title.v7)))

GOV_KW  <- "\\bTRUSTEE\\b|BOARD OF|\\bBOARD\\b|COUNCIL(OR|LOR|MAN|WOMAN|MEMBER)?\\b|\\bDELEGATE\\b|\\bREGENT\\b|\\bELDER\\b|DEACON|VESTRY|\\bWARDEN\\b|OVERSEER|PRESBYTER"
# officer sub-roles (-> BOARD PRES/SEC/... not MEMBER) or exec (-> cascade)
OFF_KW  <- "\\bCEO\\b|CHIEF|EXECUTIVE|PRESIDENT|\\bVICE|\\bVP\\b|SECRETARY|TREASURER|\\bCHAIR|MANAGER|ADMINISTRATOR|SUPERINTENDENT|\\bDEAN\\b|PHYSICIAN|COUNSEL|DIRECTOR OF"
CONJ    <- " AND |/| & |,| - |CO-CHAIR"

cand <- d %>%
  filter(!is.na(v7), v7!="", !b01(board),
         grepl(GOV_KW, v7, perl=TRUE), !grepl(OFF_KW, v7, perl=TRUE)) %>%
  mutate(needs_review = grepl(CONJ, v7) | nchar(v7) >= 24) %>%   # conjunction / truncated
  group_by(v7) %>%
  summarise(n_rows=n(), n_persons=n_distinct(paste(OBJECTID,person.id)),
            n_full990=sum(full990),
            pct_trustee_box=round(mean(cb_tru[full990]),2),
            pct_unpaid=round(mean(!paid),2),
            pct_officer_box=round(mean(cb_off[full990]),2),
            cur_std=names(sort(table(ifelse(is.na(title.standard),"<NA>",title.standard)),
                               decreasing=TRUE))[1],
            needs_review=any(needs_review),
            ex_raw=names(sort(table(title.raw),decreasing=TRUE))[1],
            .groups="drop") %>%
  arrange(desc(n_persons))

# cross-reference workbook: is this variant already mapped / queued?
std  <- read_excel(xls, sheet="title-standardization") %>%
  mutate(k=toupper(trimws(title.variant)))
todo <- read_excel(xls, sheet="to-do") %>% mutate(k=toupper(trimws(title.variant)))
cand <- cand %>% mutate(
  in_std_sheet = v7 %in% std$k,
  in_todo      = v7 %in% todo$k,
  gap_type = case_when(cur_std=="<NA>" ~ "unmapped->NA",
                       TRUE ~ paste0("mis-mapped->", cur_std)),
  # evidence gates: a title is board only if the filers back it up
  evidence_board = (pct_trustee_box>=0.5) |
                   (is.nan(pct_trustee_box) & pct_unpaid>=0.8),
  officer_ish    = (!is.nan(pct_officer_box) & pct_officer_box>=0.5) | pct_unpaid<=0.3,
  decision = case_when(
    needs_review              ~ "REVIEW-conjunction/truncated",
    officer_ish               ~ "REVIEW-officer/paid",        # WARDEN, GRAND WARDEN...
    evidence_board            ~ "AUTO",
    TRUE                      ~ "REVIEW-lowconf"),
  confidence = case_when(pct_trustee_box>=0.6 ~ "high",
                         pct_trustee_box>=0.3 ~ "medium", TRUE ~ "low"),
  proposed.title.standard="BOARD MEMBER", proposed.board="X", proposed.mem="X")

auto <- cand %>% filter(decision=="AUTO")
rev  <- cand %>% filter(decision!="AUTO")

cat("=== AUTO-PROPOSE: evidence-backed board gaps -> BOARD MEMBER ===\n")
print(as.data.frame(auto %>% select(v7, n_persons, pct_trustee_box, pct_unpaid,
      pct_officer_box, gap_type, in_todo, confidence)), row.names=FALSE)
cat(sprintf("\nAUTO: %d titles, %d persons | REVIEW: %d titles, %d persons\n",
            nrow(auto), sum(auto$n_persons), nrow(rev), sum(rev$n_persons)))
cat("\n=== REVIEW (conjunction/truncated, officer/paid, or low-confidence) ===\n")
print(as.data.frame(rev %>% select(v7, n_persons, pct_trustee_box, pct_officer_box,
      decision, gap_type) %>% head(20)), row.names=FALSE)

patch <- cand %>% transmute(
  title.variant=v7, proposed.title.standard, proposed.board, proposed.mem,
  decision, n_persons, n_rows, pct_trustee_box, pct_unpaid, pct_officer_box,
  confidence, gap_type, in_std_sheet, in_todo, cur_std, ex_raw) %>%
  arrange(decision, desc(n_persons))
write.csv(patch, paste0(base,"board_crosswalk_patch.csv"), row.names=FALSE)
cat(sprintf("\nWrote board_crosswalk_patch.csv (%d titles, %d persons total)\n",
            nrow(patch), sum(patch$n_persons)))
