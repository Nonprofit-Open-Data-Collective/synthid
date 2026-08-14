## Cross-organization linkage threshold tuning.
##
## Same shape as tune_threshold.R (the within-org tuner) but over the cross-org
## pipeline: build_person_profile -> person_blocking_keys -> candidate_pairs ->
## score_candidate_pairs, then sweep the accept threshold and resolve_cross_org
## at the promising operating points.
##
## The score scale differs from the within-org linker (title/salutation dropped,
## surname rarity is POPULATION-based not within-org), so the within-org default
## of 7 does NOT carry over -- that is exactly what this script exists to reset.
##
## SILVER LABEL CAVEAT. Within an org, year+org context makes the label oracle
## genuinely independent of the score. Cross-org there is no such side context:
## both the score and any name-only oracle see the same name evidence. The oracle
## below is therefore a *silver* standard -- it leans on signals the additive
## score treats only softly (a suffix-generation veto and a middle-initial veto)
## so it is not fully circular, but the real arbiter is the manual review of the
## exported disagreement/ambiguous samples. Treat the swept precision/recall as
## directional, and confirm the chosen threshold against hand-labeled pairs (drop
## them in as a `truth` column; see TRUTH below).

suppressMessages({ library(readr) })
pkg <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid"
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
outdir <- file.path(pkg, "dev")

## ---- linked panel (input already carries within-org EMP_IDs) --------------
## Default to the cached 100-EIN linked slice so this runs out of the box. For a
## real tuning run, point `linked_path` at the full linked panel (the output of
## link_panel() on PANEL-*-W-NAMES-TITLES...), and join BMF `state`/`ntee` first.
linked_path <- file.path(outdir, "data", "linked_full_panel_100eins.rds")
d <- readRDS(linked_path)
message(sprintf("linked panel: %s  (%d rows, %d persons)",
                basename(linked_path), nrow(d), length(unique(d$EMP_ID))))

## Geography / industry columns for the tighter blocking passes, if present.
state_col <- intersect(c("state", "STATE", "org.state"), names(d))[1]
ntee_col  <- intersect(c("ntee", "NTEE", "ntee.major", "NTEE1"), names(d))[1]
if (is.na(state_col)) state_col <- NULL
if (is.na(ntee_col))  ntee_col  <- NULL
if (is.null(state_col) && is.null(ntee_col))
  message("NOTE: no state/ntee columns found -- only the surname pass will fire. ",
          "Join BMF geography/industry for the strict/geo/industry passes.")

## ---- candidate pairs with scores (expensive; cache) -----------------------
cache <- file.path(outdir, "cross_org_candidate_scores.rds")
if (file.exists(cache)) {
  sc <- readRDS(cache)
  profiles <- attr(sc, "profiles")
} else {
  message("Building profiles and scoring cross-org candidate pairs...")
  profiles <- build_person_profile(d, state = state_col, ntee = ntee_col)
  keys <- person_blocking_keys(profiles)
  cand <- candidate_pairs(keys, profiles)
  sc <- score_candidate_pairs(cand, profiles, components = TRUE)
  attr(sc, "profiles") <- profiles
  saveRDS(sc, cache)
}
n_naive <- choose(nrow(profiles), 2)
message(sprintf("candidate pairs: %d  (naive all-pairs: %s  -> %.0fx reduction)",
                nrow(sc), format(n_naive, big.mark = ","), n_naive / max(nrow(sc), 1)))

## ---- per-person fields for the label oracle -------------------------------
ix   <- setNames(seq_len(nrow(profiles)), profiles$EMP_ID)
lnv  <- profiles$last_name_variants
fnv  <- profiles$first_name_variants
midv <- profiles$middle_initials
sufv <- profiles$suffixes
srar <- population_surname_weight(profiles)                 # canonical surname -> rarity
ln_can <- toupper(trimws(as.character(profiles$last_name)))

ia <- ix[sc$emp_a]; ib <- ix[sc$emp_b]

jw <- function(a, b) 1 - stringdist::stringdist(a, b, method = "jw")
best_jw <- function(a, b) {                                 # best JW over two sets
  if (!length(a) || !length(b)) return(NA_real_)
  g <- expand.grid(a = a, b = b, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  max(jw(g$a, g$b), na.rm = TRUE)
}
## first-name relation over the variant sets: yes / no / maybe (independent of
## the additive score's compare_first_names, via nickname roots + JW + initials).
first_relation <- function(fa, fb) {
  if (!length(fa) || !length(fb)) return("maybe")
  if (length(intersect(fa, fb))) return("yes")
  ra <- unique(unlist(lapply(fa, first_name_roots)))
  rb <- unique(unlist(lapply(fb, first_name_roots)))
  if (length(intersect(ra, rb))) return("yes")
  s <- best_jw(fa, fb)
  inits <- any(nchar(fa) == 1L) || any(nchar(fb) == 1L)
  if (inits && substr(sort(fa)[1], 1, 1) == substr(sort(fb)[1], 1, 1)) return("maybe")
  if (!is.na(s) && s >= 0.90) return("yes")
  if (!is.na(s) && s < 0.70) return("no")
  "maybe"
}

n <- nrow(sc)
last_sim <- numeric(n); first_rel <- character(n)
suffix_conflict <- logical(n); middle_conflict <- logical(n)
for (k in seq_len(n)) {
  a <- ia[k]; b <- ib[k]
  last_sim[k]  <- best_jw(lnv[[a]], lnv[[b]])
  first_rel[k] <- first_relation(fnv[[a]], fnv[[b]])
  sa <- sufv[[a]]; sb <- sufv[[b]]
  suffix_conflict[k] <- length(sa) > 0 && length(sb) > 0 && length(intersect(sa, sb)) == 0
  ma <- midv[[a]]; mb <- midv[[b]]
  middle_conflict[k] <- length(ma) > 0 && length(mb) > 0 && length(intersect(ma, mb)) == 0
}

## ---- silver label ---------------------------------------------------------
label <- rep(NA_integer_, n)
## confident negatives
label[suffix_conflict]                                                <- 0L
label[is.na(label) & middle_conflict & first_rel != "yes"]            <- 0L
label[is.na(label) & last_sim >= 0.90 & first_rel == "no"]            <- 0L  # same surname, different person
label[is.na(label) & last_sim <  0.80 & first_rel == "no"]            <- 0L
## confident positives
label[is.na(label) & last_sim >= 0.90 & first_rel == "yes" & !middle_conflict] <- 1L
## everything else stays NA (ambiguous)

## TRUTH override: if you have hand-labeled pairs, join a 0/1 `truth` column keyed
## on (emp_a, emp_b) and prefer it over the silver label:
##   truth <- read.csv("dev/cross_org_truth.csv")   # emp_a, emp_b, truth
##   key <- paste(sc$emp_a, sc$emp_b); tkey <- paste(truth$emp_a, truth$emp_b)
##   label[match(tkey, key)] <- truth$truth

sc$label   <- label
sc$last_sim <- last_sim
sc$first_rel <- first_rel
## common-surname slice: where cross-org false positives concentrate.
sc$common <- last_sim >= 0.90 &
  pmin(srar[ln_can[ia]], srar[ln_can[ib]], na.rm = TRUE) < 0.5

n_pos <- sum(label == 1, na.rm = TRUE); n_neg <- sum(label == 0, na.rm = TRUE)
n_amb <- sum(is.na(label))
cat(sprintf("\nlabeled positives : %d\nlabeled negatives : %d\nambiguous (NA)    : %d\n",
            n_pos, n_neg, n_amb))
if (n_pos == 0 || n_neg == 0)
  stop("Not enough labeled pairs to sweep -- check the input panel / oracle.")

## ---- score separation ------------------------------------------------------
qs <- c(0.01, .05, .10, .25, .5, .75, .9, .95, .99)
cat("\nscore quantiles among POSITIVES:\n"); print(round(quantile(sc$score[label==1 & !is.na(label)], qs), 2))
cat("\nscore quantiles among NEGATIVES:\n"); print(round(quantile(sc$score[label==0 & !is.na(label)], qs), 2))

## ---- pair-level threshold sweep -------------------------------------------
lab <- sc[!is.na(sc$label), ]
sweep <- function(sub) {
  grid <- seq(floor(min(sc$score)), ceiling(max(sc$score)), by = 0.25)
  do.call(rbind, lapply(grid, function(t) {
    pred <- sub$score >= t
    TP <- sum(pred & sub$label == 1); FP <- sum(pred & sub$label == 0)
    FN <- sum(!pred & sub$label == 1); TN <- sum(!pred & sub$label == 0)
    prec <- ifelse(TP+FP>0, TP/(TP+FP), NA); rec <- ifelse(TP+FN>0, TP/(TP+FN), NA)
    data.frame(threshold=t, TP=TP, FP=FP, FN=FN, TN=TN,
               precision=prec, recall=rec,
               f1=ifelse(is.na(prec)|is.na(rec)|prec+rec==0, NA, 2*prec*rec/(prec+rec)))
  }))
}
sw <- sweep(lab)
best_f1 <- sw[which.max(sw$f1), ]
hp <- sw[!is.na(sw$precision) & sw$precision >= 0.99, ]
hp_pt <- if (nrow(hp)) hp[which.min(hp$threshold), ] else NULL

cat("\n== pair-level sweep (every 1.0) ==\n")
print(sw[sw$threshold %% 1 == 0, c("threshold","precision","recall","f1","FP","FN")],
      row.names = FALSE, digits = 3)
cat(sprintf("\nmax-F1 threshold = %.2f  (P=%.3f R=%.3f F1=%.3f)\n",
            best_f1$threshold, best_f1$precision, best_f1$recall, best_f1$f1))
if (!is.null(hp_pt))
  cat(sprintf("lowest threshold with precision>=0.99: thr=%.2f (P=%.3f R=%.3f)\n",
              hp_pt$threshold, hp_pt$precision, hp_pt$recall))

## ---- common-surname slice (where cross-org FPs live) ----------------------
cat("\n== COMMON-SURNAME slice (shared common surname) ==\n")
comlab <- lab[lab$common, ]
cat(sprintf("labeled common-surname pairs: %d (pos=%d neg=%d)\n",
            nrow(comlab), sum(comlab$label==1), sum(comlab$label==0)))
if (nrow(comlab) > 20) {
  swc <- sweep(comlab)
  print(swc[swc$threshold %% 1 == 0, c("threshold","precision","recall","f1","FP","FN")],
        row.names = FALSE, digits = 3)
}

## ---- post-resolution operating points (the real decision) -----------------
op_points <- sort(unique(c(best_f1$threshold,
                           if (!is.null(hp_pt)) hp_pt$threshold,
                           round(best_f1$threshold) + c(-1, 0, 1))))
cat("\n== post-resolution operating points (resolve_cross_org) ==\n")
for (t in op_points) {
  edges <- sc[!is.na(sc$score) & sc$score >= t, ]
  map <- resolve_cross_org(edges, profiles)
  la <- edges[!is.na(edges$label), ]
  eprec <- if (nrow(la)) mean(la$label == 1) else NA
  erec  <- sum(la$label == 1) / n_pos
  interp <- sum(map$XORG_N_ORGS > 1L)
  nclust <- length(unique(map$XORG_ID[map$XORG_N_ORGS > 1L]))
  cat(sprintf("thr=%.2f | edges=%d | labeled-edge P=%.3f R=%.3f | interlocking persons=%d | x-org clusters=%d | org-collisions blocked=%d\n",
              t, nrow(edges), eprec, erec, interp, nclust, attr(map, "rejected_edges")))
}

## ---- export disagreements & ambiguous for manual review -------------------
lab_name <- function(id) {
  p <- profiles[match(id, profiles$EMP_ID), ]
  trimws(paste(p$first_name, p$last_name))
}
org_of <- function(id) vapply(profiles$orgs[match(id, profiles$EMP_ID)],
                              function(o) if (length(o)) o[[1]] else NA_character_, character(1))
sc$name_a <- lab_name(sc$emp_a); sc$name_b <- lab_name(sc$emp_b)
sc$org_a  <- org_of(sc$emp_a);   sc$org_b  <- org_of(sc$emp_b)
show <- c("name_a","org_a","name_b","org_b","pass","first_rel","last_sim",
          "sim_first_name","score","label","common")
thr0 <- round(best_f1$threshold)
false_pos <- sc[!is.na(sc$label) & sc$label==0 & sc$score>=thr0, show]
false_neg <- sc[!is.na(sc$label) & sc$label==1 & sc$score< thr0, show]
ambiguous <- sc[is.na(sc$label) & sc$score>=(thr0-2) & sc$score<(thr0+2), show]
write.csv(false_pos[order(-false_pos$score), ], file.path(outdir,"review_xorg_false_positives.csv"), row.names=FALSE)
write.csv(false_neg[order(false_neg$score), ],  file.path(outdir,"review_xorg_false_negatives.csv"), row.names=FALSE)
set.seed(1)
amb_s <- ambiguous[sample(nrow(ambiguous), min(300, nrow(ambiguous))), , drop = FALSE]
write.csv(amb_s[order(-amb_s$score), ], file.path(outdir,"review_xorg_ambiguous_sample.csv"), row.names=FALSE)
cat(sprintf("\nreview files (@thr~%d): FP=%d, FN=%d, ambiguous sampled=%d\n",
            thr0, nrow(false_pos), nrow(false_neg), nrow(amb_s)))

## ---- plot ------------------------------------------------------------------
png(file.path(outdir, "cross_org_threshold_tuning.png"), width=1100, height=480, res=110)
op <- par(mfrow=c(1,2), mar=c(4,4,3,1))
posv <- sc$score[label==1 & !is.na(label)]; negv <- sc$score[label==0 & !is.na(label)]
br <- seq(floor(min(sc$score)), ceiling(max(sc$score)), by=0.5)
hist(negv, breaks=br, col=rgb(.85,.3,.3,.5), border=NA, main="Cross-org score by label",
     xlab="match score", ylab="pairs", freq=TRUE)
hist(posv, breaks=br, col=rgb(.3,.5,.85,.5), border=NA, add=TRUE)
abline(v=best_f1$threshold, lty=2)
legend("topleft", c("negative","positive","max-F1"),
       fill=c(rgb(.85,.3,.3,.5),rgb(.3,.5,.85,.5),NA),
       border=NA, lty=c(NA,NA,2), bty="n")
plot(sw$threshold, sw$precision, type="l", col="darkgreen", lwd=2, ylim=c(0,1),
     xlab="threshold", ylab="", main="Precision / Recall / F1")
lines(sw$threshold, sw$recall, col="darkorange", lwd=2)
lines(sw$threshold, sw$f1, col="purple", lwd=2)
abline(v=best_f1$threshold, lty=2)
legend("bottomleft", c("precision","recall","F1"),
       col=c("darkgreen","darkorange","purple"), lwd=2, bty="n")
par(op); dev.off()
cat("wrote cross_org_threshold_tuning.png\n")
cat(sprintf("\nRECOMMENDED starting threshold for link_cross_org(): %.1f  (max-F1; ",
            best_f1$threshold))
cat(if (!is.null(hp_pt)) sprintf("use %.1f for precision>=0.99)\n", hp_pt$threshold) else "no >=0.99 point found)\n")
