pkg <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid"
suppressMessages(devtools::load_all(pkg, quiet=TRUE))
cmp <- readRDS(file.path(pkg,"dev/comparisons.rds"))
cs  <- readRDS(file.path(pkg,"dev/candidate_scores.rds"))   # has raw names + hand score
# join labels from oracle onto cmp by row keys
key <- function(df) paste(df$row_x, df$row_y, df$yr_x, df$yr_y)
jw <- function(a,b) 1-stringdist::stringdist(a,b,method="jw")
oracle <- function(cs){
  fx<-toupper(trimws(cs$first_name_x)); fy<-toupper(trimws(cs$first_name_y))
  lx<-toupper(trimws(cs$last_name_x));  ly<-toupper(trimws(cs$last_name_y))
  sx<-toupper(trimws(cs$suffix_x)); sy<-toupper(trimws(cs$suffix_y)); sx[is.na(sx)]<-""; sy[is.na(sy)]<-""
  last_sim<-jw(lx,ly); first_sim<-jw(fx,fy)
  equiv <- (fx==fy & nzchar(fx)) | nickname_equivalent_vec(fx,fy)
  rel <- rep("maybe", nrow(cs)); rel[equiv | first_sim>=0.90]<-"yes"; rel[!equiv & first_sim<0.70]<-"no"
  sconf <- nzchar(sx)&nzchar(sy)&sx!=sy
  lb <- rep(NA_integer_, nrow(cs)); lb[sconf]<-0L
  lb[is.na(lb)&last_sim>=0.90&rel=="no"]<-0L; lb[is.na(lb)&last_sim<0.80&rel=="no"]<-0L
  lb[is.na(lb)&last_sim>=0.90&rel=="yes"]<-1L; lb
}
cs$label <- oracle(cs)
cmp$label <- cs$label[match(key(cmp), key(cs))]
cat("labeled: pos", sum(cmp$label==1,na.rm=T), " neg", sum(cmp$label==0,na.rm=T), " NA", sum(is.na(cmp$label)), "\n")

# ---- EM (unsupervised) ----
em <- fit_match_model(cmp, method="em")
cat("\n== EM-learned Fellegi-Sunter weights vs hand-set ==\n")
w <- fs_weights(em); hand <- default_weights()
w$hand_weight <- hand[w$feature]
print(w)
cat("prior match rate p =", round(em$p,4), "\n")
cmp$p_em <- predict_match(em, cmp)

# ---- metrics helper ----
prc <- function(pred, lab){ TP<-sum(pred&lab==1);FP<-sum(pred&lab==0);FN<-sum(!pred&lab==1)
  c(P=ifelse(TP+FP>0,TP/(TP+FP),NA), R=TP/(TP+FN)) }

# ---- org-disjoint split ----
set.seed(42); orgs<-unique(cmp$org); test_orgs<-sample(orgs, length(orgs)%/%3)
te <- cmp$org %in% test_orgs; tr <- !te
labeled_te <- te & !is.na(cmp$label)
# logistic trained on TRAIN orgs
lg <- fit_match_model(cmp[tr,,drop=FALSE], method="logistic", labels=cmp$label[tr])
cmp$p_lg <- predict_match(lg, cmp)
# hand score on test
cmp$hand <- cs$score[match(key(cmp), key(cs))]

cat("\n== held-out (test orgs) precision/recall ==\n")
L <- cmp$label[labeled_te]
cat(sprintf("EM   p>=0.5 : P=%.3f R=%.3f\n", prc(cmp$p_em[labeled_te]>=0.5, L)[1], prc(cmp$p_em[labeled_te]>=0.5, L)[2]))
cat(sprintf("EM   p>=0.9 : P=%.3f R=%.3f\n", prc(cmp$p_em[labeled_te]>=0.9, L)[1], prc(cmp$p_em[labeled_te]>=0.9, L)[2]))
cat(sprintf("LOGIT p>=0.5: P=%.3f R=%.3f\n", prc(cmp$p_lg[labeled_te]>=0.5, L)[1], prc(cmp$p_lg[labeled_te]>=0.5, L)[2]))
cat(sprintf("HAND  s>=7  : P=%.3f R=%.3f\n", prc(cmp$hand[labeled_te]>=7, L)[1], prc(cmp$hand[labeled_te]>=7, L)[2]))

# concordance EM vs hand on accept decision (all pairs)
acc_em <- cmp$p_em>=0.5; acc_hand <- cmp$hand>=7
cat(sprintf("\nEM(p>=.5) vs HAND(s>=7) agree on %.2f%% of all %d candidate pairs\n",
            100*mean(acc_em==acc_hand), nrow(cmp)))
# calibration: among pairs with p_em in bins, actual match rate (labeled)
cat("\n== EM calibration (labeled pairs) ==\n")
br<-cut(cmp$p_em, c(-.01,.1,.3,.5,.7,.9,1.01)); 
for(b in levels(br)){ idx<-which(br==b & !is.na(cmp$label)); if(length(idx)>20) cat(sprintf("  p_em %s : actual match rate %.3f (n=%d)\n", b, mean(cmp$label[idx]==1), length(idx))) }
saveRDS(list(em=em, lg=lg), file.path(pkg,"dev/models.rds"))
