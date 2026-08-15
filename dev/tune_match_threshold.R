## Incremental match-threshold tuning (link_incremental / match_to_profiles).
##
## The score here is the PROFILE-vs-PROFILE match of a new wave against a frozen
## existing person set -- a different scale from the within-org record linker
## (title/salutation dropped, surname rarity is POPULATION-based, evidence comes
## from variant SETS not single strings). So the within-org default of 7 does not
## carry over; this script exists to set match_threshold empirically.
##
## ORACLE. Link the WHOLE panel across all years with the within-org record-pair
## linker (link_panel) and take each record's cluster id as the truth of "who is
## the same person". Then split the panel into an existing slice (2019-2020) and a
## held-out wave (2021), link each slice ALONE, and ask the profile matcher to
## reconnect the wave people to the existing people. A candidate (existing-person,
## wave-person) is a POSITIVE iff the two sub-clusters carry the same full-panel
## truth id. Because the wave year (2021) is disjoint from the existing years, no
## one-per-(org,year) collisions arise in this split -- every candidate is legal.
##
## SILVER-LABEL CAVEAT. The full-panel linker and the profile matcher share the
## same field comparators/weights, so this is a *silver* standard: the two differ
## in AGGREGATION (record-pair chains + year-greedy vs variant-set profiles +
## population surname rarity), which makes agreement informative but not gold.
## The real arbiter is manual review of the exported disagreements.

pkg <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid"
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
outdir <- file.path(pkg, "dev")

f <- "C:/Users/jdlec/Dropbox (Personal)/00 - DATA/COMPDATA/PANEL-2019-2021-W-NAMES-TITLES-AND-NAMES.CSV"
panel <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
message(sprintf("panel: %d rows, %d EINs, years %s",
                nrow(panel), length(unique(panel$ein)),
                paste(sort(unique(panel$taxyr)), collapse = ",")))

WAVE_YEAR <- max(panel$taxyr, na.rm = TRUE)   # hold out the latest year as the wave

## ---- score + label the candidates (expensive; cache) ----------------------
cache <- file.path(outdir, "match_candidate_scores.rds")
if (file.exists(cache)) {
  scored <- readRDS(cache)
  message(sprintf("loaded cached scored candidates: %d", nrow(scored)))
} else {
  message("Full-panel link for truth ids...")
  panel$TRUE_ID <- link_panel(panel)$EMP_ID

  ex <- panel[panel$taxyr <  WAVE_YEAR, , drop = FALSE]
  wv <- panel[panel$taxyr == WAVE_YEAR, , drop = FALSE]
  message(sprintf("existing (<%d): %d rows | wave (%d): %d rows",
                  WAVE_YEAR, nrow(ex), WAVE_YEAR, nrow(wv)))

  message("Linking existing and wave slices separately...")
  exl <- link_panel(ex); wvl <- link_panel(wv)
  exl$TRUE_ID <- ex$TRUE_ID   # link_panel preserves row order
  wvl$TRUE_ID <- wv$TRUE_ID

  modal <- function(v) names(sort(table(v), decreasing = TRUE))[1]
  ex_true <- tapply(exl$TRUE_ID, exl$EMP_ID, modal)
  wv_true <- tapply(wvl$TRUE_ID, wvl$EMP_ID, modal)

  message("Profiling + scoring same-org candidate pairs...")
  ex_prof <- build_person_profile(exl)
  wv_prof <- build_person_profile(wvl)
  cand <- same_org_candidate_pairs(ex_prof, wv_prof)
  common <- intersect(names(ex_prof), names(wv_prof))
  combined <- rbind(ex_prof[, common], wv_prof[, common])
  sw <- population_surname_weight(combined)
  scored <- score_candidate_pairs(cand, combined, surname_weight = sw, components = TRUE)

  scored$label <- as.integer(ex_true[scored$emp_a] == wv_true[scored$emp_b])
  ## carry names for review export
  nm <- function(p) stats::setNames(
    trimws(paste(ifelse(is.na(p$first_name), "", p$first_name),
                 ifelse(is.na(p$last_name), "", p$last_name))), p$EMP_ID)
  enm <- nm(ex_prof); wnm <- nm(wv_prof)
  scored$name_existing <- enm[scored$emp_a]
  scored$name_wave     <- wnm[scored$emp_b]
  ## total truth positives available among candidates (for recall denominator)
  attr(scored, "n_truth_pos_candidates") <- sum(scored$label == 1, na.rm = TRUE)
  saveRDS(scored, cache)
}

lab <- scored[!is.na(scored$label), ]
n_pos <- sum(lab$label == 1); n_neg <- sum(lab$label == 0)
cat(sprintf("\ncandidates: %d  (positives=%d  negatives=%d)\n", nrow(lab), n_pos, n_neg))
cat("\nscore quantiles among POSITIVES:\n")
print(round(quantile(lab$score[lab$label == 1], c(.01,.05,.1,.25,.5,.75,.9,.95,.99)), 2))
cat("\nscore quantiles among NEGATIVES:\n")
print(round(quantile(lab$score[lab$label == 0], c(.01,.05,.1,.25,.5,.75,.9,.95,.99)), 2))

## ---- pair-level threshold sweep -------------------------------------------
grid <- seq(floor(min(lab$score)), ceiling(max(lab$score)), by = 0.25)
sweep <- function(sub) do.call(rbind, lapply(grid, function(t) {
  pred <- sub$score >= t
  TP <- sum(pred & sub$label == 1); FP <- sum(pred & sub$label == 0)
  FN <- sum(!pred & sub$label == 1); TN <- sum(!pred & sub$label == 0)
  prec <- if (TP+FP > 0) TP/(TP+FP) else NA_real_; rec <- if (TP+FN>0) TP/(TP+FN) else NA_real_
  data.frame(threshold=t, TP=TP, FP=FP, FN=FN, TN=TN, precision=prec, recall=rec,
             f1 = if (is.na(prec) || prec+rec == 0) NA_real_ else 2*prec*rec/(prec+rec))
}))
sw <- sweep(lab)
best_f1 <- sw[which.max(sw$f1), ]
hp <- sw[!is.na(sw$precision) & sw$precision >= 0.99, ]
hp_pt <- if (nrow(hp)) hp[which.min(hp$threshold), ] else NULL

cat("\n== pair-level sweep (integer thresholds) ==\n")
print(sw[sw$threshold %% 1 == 0, c("threshold","precision","recall","f1","FP","FN")],
      row.names = FALSE, digits = 3)
cat(sprintf("\nmax-F1 threshold = %.2f  (P=%.3f R=%.3f F1=%.3f)\n",
            best_f1$threshold, best_f1$precision, best_f1$recall, best_f1$f1))
if (!is.null(hp_pt))
  cat(sprintf("lowest threshold with precision>=0.99: %.2f (P=%.3f R=%.3f)\n",
              hp_pt$threshold, hp_pt$precision, hp_pt$recall))

## ---- post-greedy operating points (the real decision) ---------------------
## Mirror match_to_profiles: greedy one-to-one over ALL legal edges >= t at once.
cat("\n== post-one-to-one operating points (accepted matches) ==\n")
op_points <- sort(unique(c(5, 6, 7, round(best_f1$threshold),
                           if (!is.null(hp_pt)) ceiling(hp_pt$threshold))))
for (t in op_points) {
  sub <- lab[lab$score >= t, ]
  acc <- if (nrow(sub)) greedy_one_to_one(
    data.frame(row_x=sub$emp_a, row_y=sub$emp_b, score=sub$score,
               label=sub$label, stringsAsFactors=FALSE)) else sub[0,]
  eprec <- if (nrow(acc)) mean(acc$label == 1) else NA_real_
  erec  <- if (n_pos) sum(acc$label == 1) / n_pos else NA_real_
  cat(sprintf("thr=%2.0f | accepted=%4d | precision=%.3f | recall=%.3f\n",
              t, nrow(acc), eprec, erec))
}

## ---- export disagreements for manual review -------------------------------
show <- c("name_existing","name_wave","score","label","sim_last","sim_first")
show <- intersect(show, names(lab))
fp <- lab[lab$label == 0 & lab$score >= 7, show]
fn <- lab[lab$label == 1 & lab$score <  7, show]
write.csv(fp[order(-fp$score), ], file.path(outdir,"review_match_false_positives.csv"), row.names=FALSE)
write.csv(fn[order(-fn$score), ], file.path(outdir,"review_match_false_negatives.csv"), row.names=FALSE)
cat(sprintf("\nreview files at default-7: FP=%d, FN=%d\n", nrow(fp), nrow(fn)))

## ---- plot ------------------------------------------------------------------
png(file.path(outdir, "match_threshold_tuning.png"), width=1100, height=480, res=110)
op <- par(mfrow=c(1,2), mar=c(4,4,3,1))
br <- seq(floor(min(lab$score)), ceiling(max(lab$score)), by=0.5)
hist(lab$score[lab$label==0], breaks=br, col=rgb(.85,.3,.3,.5), border=NA,
     main="Match score by truth label", xlab="match score", ylab="candidate pairs")
hist(lab$score[lab$label==1], breaks=br, col=rgb(.3,.5,.85,.5), border=NA, add=TRUE)
abline(v=best_f1$threshold, lty=2)
legend("topright", c("different person","same person","max-F1"),
       fill=c(rgb(.85,.3,.3,.5),rgb(.3,.5,.85,.5),NA), border=NA,
       lty=c(NA,NA,2), bty="n")
plot(sw$threshold, sw$precision, type="l", col="darkgreen", lwd=2, ylim=c(0,1),
     xlab="threshold", ylab="", main="Precision / Recall / F1")
lines(sw$threshold, sw$recall, col="darkorange", lwd=2)
lines(sw$threshold, sw$f1, col="purple", lwd=2)
abline(v=best_f1$threshold, lty=2)
legend("bottomleft", c("precision","recall","F1"),
       col=c("darkgreen","darkorange","purple"), lwd=2, bty="n")
par(op); dev.off()
cat("wrote match_threshold_tuning.png\n")
