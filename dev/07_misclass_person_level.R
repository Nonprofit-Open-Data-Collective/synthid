# =============================================================================
# 07_misclass_person_level.R
# Person-level (OBJECTID + person.id) misclassification analysis, full 990.
# For each mismatch group, show pre-standardization (title.v7) and raw
# (title.raw, pre-cleaning) title distributions -> feeds crosswalk refinement.
# =============================================================================
suppressMessages(library(dplyr))
base <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid/dev/"
d <- readRDS(paste0(base,"data/classified_slice.rds"))
b01 <- function(x) as.integer(x) %in% 1L
num <- function(x) suppressWarnings(as.numeric(x))
f <- d %>% filter(formtype=="990") %>% mutate(
  paid=num(tot.comp)>0 & !is.na(num(tot.comp)),
  cb_tru=b01(dtk.indiv.trustee.x)|b01(dtk.inst.trustee.x),
  cb_off=b01(dtk.officer.x))

# collapse to PERSON: any-row checkbox/label/paid, keep title.v7 + raw variants
pers <- f %>% group_by(OBJECTID, person.id) %>%
  summarise(org.name=org.name[1], taxyr=taxyr[1],
            lab_ceo   = any(b01(ceo)),
            lab_clevel= any(b01(c.level)),
            lab_board = any(b01(board)),
            cb_off    = any(cb_off),
            cb_tru    = any(cb_tru),
            paid      = any(paid),
            v7        = paste(unique(title.v7[!is.na(title.v7)]), collapse=" | "),
            raw       = paste(unique(title.raw[!is.na(title.raw)]), collapse=" | "),
            .groups="drop")
cat("full-990 persons:", nrow(pers), "\n\n")

topn <- function(x, n=20){ t<-sort(table(x),decreasing=TRUE); head(t,n) }

# ===== GROUP A: labeled CEO (or c.level) but NO officer checkbox =====
cat("############ GROUP A: labeled CEO/c.level, NO officer box (person level) ############\n")
A <- pers %>% filter((lab_ceo|lab_clevel) & !cb_off)
cat(sprintf("persons: %d  | paid: %d  unpaid: %d  | also-trustee-box: %d\n\n",
            nrow(A), sum(A$paid), sum(!A$paid), sum(A$cb_tru)))
cat("--- top title.v7 (pre-standardization) ---\n"); print(topn(A$v7))
cat("\n--- top title.raw (pre-cleaning) ---\n");      print(topn(A$raw))

# ===== GROUP B: has trustee checkbox but NOT labeled board =====
cat("\n\n############ GROUP B: trustee box, NOT labeled board (person level) ############\n")
B <- pers %>% filter(cb_tru & !lab_board)
cat(sprintf("persons: %d  | paid: %d  unpaid: %d  | also-officer-box: %d\n\n",
            nrow(B), sum(B$paid), sum(!B$paid), sum(B$cb_off)))
cat("--- top title.v7 (pre-standardization) ---\n"); print(topn(B$v7))
cat("\n--- top title.raw (pre-cleaning) ---\n");      print(topn(B$raw))

write.csv(A, paste0(base,"misclass_A_ceo_no_officerbox.csv"), row.names=FALSE)
write.csv(B, paste0(base,"misclass_B_trusteebox_not_board.csv"), row.names=FALSE)
cat("\nWrote: misclass_A_ceo_no_officerbox.csv, misclass_B_trusteebox_not_board.csv\n")
