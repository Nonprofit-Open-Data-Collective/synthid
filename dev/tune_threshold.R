## Threshold tuning against an independent (silver) labeled slice.
##
## The label is produced by a policy that uses MORE information than the current
## score (nickname dictionary, suffix-generation veto, same-surname/different-
## first logic), so precision/recall vs threshold is a genuine evaluation and not
## circular with the score it is tuning. Genuinely ambiguous pairs are left
## unlabeled (NA) and sampled out for manual review.

suppressMessages({
  library(readr)
})
pkg <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid"
suppressMessages(devtools::load_all(pkg, quiet = TRUE))

f <- "C:/Users/jdlec/Dropbox (Personal)/00 - DATA/COMPDATA/PANEL-2019-2021-W-NAMES-TITLES-AND-NAMES.CSV"
outdir <- file.path(pkg, "dev")
d <- read_csv(f, show_col_types = FALSE, guess_max = 20000)

## ---- candidate pairs with scores (expensive; cache) -----------------------
cache <- file.path(outdir, "candidate_scores.rds")
if (file.exists(cache)) {
  cs <- readRDS(cache)
} else {
  message("Scoring all candidate pairs...")
  cs <- candidate_scores(d)
  saveRDS(cs, cache)
}
message(sprintf("candidate pairs: %d", nrow(cs)))

## ---- independent labeling oracle ------------------------------------------
jw <- function(a, b) 1 - stringdist::stringdist(a, b, method = "jw")

nick <- list(
  ROBERT=c("BOB","BOBBY","ROB","ROBBIE","BERT"), WILLIAM=c("BILL","BILLY","WILL","WILLIE","LIAM"),
  RICHARD=c("DICK","RICH","RICK","RICKY","RICHIE"), JOHN=c("JACK","JOHNNY","JON"),
  JAMES=c("JIM","JIMMY","JAMIE"), ELIZABETH=c("LIZ","LIZZY","BETH","BETSY","ELIZA","LISA","LIBBY","ELLIE"),
  MARGARET=c("MEG","PEGGY","MAGGIE","MARGE","GRETA","MADGE"), KATHERINE=c("KATE","KATIE","KATHY","KAT","KITTY"),
  CATHERINE=c("CATHY","KATE","KATIE","CATE"), CHARLES=c("CHARLIE","CHUCK","CHAS"),
  THOMAS=c("TOM","TOMMY"), MICHAEL=c("MIKE","MICKEY","MICK"), EDWARD=c("ED","EDDIE","TED","NED"),
  ANTHONY=c("TONY"), JOSEPH=c("JOE","JOEY"), DANIEL=c("DAN","DANNY"), DAVID=c("DAVE","DAVEY"),
  MATTHEW=c("MATT"), ANDREW=c("ANDY","DREW"), NICHOLAS=c("NICK","NICKY"),
  PATRICIA=c("PAT","PATTY","TRICIA"), PATRICK=c("PAT","PADDY"), SUSAN=c("SUE","SUZY","SUSIE"),
  JENNIFER=c("JEN","JENNY"), DEBORAH=c("DEB","DEBBIE"), BARBARA=c("BARB","BABS"),
  CHRISTOPHER=c("CHRIS","TOPHER"), STEPHEN=c("STEVE","STEVIE"), STEVEN=c("STEVE","STEVIE"),
  KENNETH=c("KEN","KENNY"), RONALD=c("RON","RONNIE"), DONALD=c("DON","DONNIE"),
  GERALD=c("GERRY","JERRY"), SAMUEL=c("SAM","SAMMY"), BENJAMIN=c("BEN","BENNY"),
  ALEXANDER=c("ALEX","AL","SANDY"), GREGORY=c("GREG"), TIMOTHY=c("TIM","TIMMY"),
  FREDERICK=c("FRED","FREDDIE"), THEODORE=c("TED","THEO"), VIRGINIA=c("GINNY","GINGER"),
  VICTORIA=c("VICKY","TORI"), REBECCA=c("BECKY","BECCA"), CYNTHIA=c("CINDY"),
  DOROTHY=c("DOT","DOTTIE","DORA"), ELEANOR=c("ELLIE","NELL","NORA"), FRANCIS=c("FRANK","FRANKIE"),
  FRANCES=c("FRAN","FRANNIE"), LAWRENCE=c("LARRY"), LEONARD=c("LEN","LENNY"),
  PHILIP=c("PHIL"), PHILLIP=c("PHIL"), RAYMOND=c("RAY"), WALTER=c("WALT"),
  HENRY=c("HANK","HARRY"), ALBERT=c("AL","BERT"), HAROLD=c("HAL","HARRY"),
  EUGENE=c("GENE"), VINCENT=c("VINCE","VINNIE"), ARTHUR=c("ART","ARTIE"),
  JOSHUA=c("JOSH"), ZACHARY=c("ZACH","ZAC"), NATHANIEL=c("NATE","NAT"),
  JONATHAN=c("JON","JONNY"), ABIGAIL=c("ABBY","ABBIE"), AMANDA=c("MANDY"),
  MELISSA=c("MISSY"), SANDRA=c("SANDY"), PAMELA=c("PAM"), PENELOPE=c("PENNY")
)
## name -> set of roots (itself + every canonical it is a variant of)
root_env <- new.env(parent = emptyenv())
add_root <- function(name, root) {
  cur <- root_env[[name]]
  root_env[[name]] <- unique(c(cur, root))
}
for (canon in names(nick)) {
  add_root(canon, canon)
  for (v in nick[[canon]]) add_root(v, canon)
}
roots_of <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  r <- root_env[[x]]
  if (is.null(r)) x else r
}

norm1 <- function(x) toupper(trimws(ifelse(is.na(x), "", x)))
fx <- norm1(cs$first_name_x); fy <- norm1(cs$first_name_y)
lx <- norm1(cs$last_name_x);  ly <- norm1(cs$last_name_y)
sx <- norm1(cs$suffix_x);     sy <- norm1(cs$suffix_y)

first_sim <- jw(fx, fy)
last_sim  <- jw(lx, ly)  # plain JW is enough (and fast) for the label oracle

## first-name relation: "yes" / "no" / "maybe"
known <- ls(root_env)
uniq <- unique(c(fx, fy)); uniq <- uniq[nzchar(uniq)]
rmap <- lapply(uniq, roots_of); names(rmap) <- uniq
share_root <- logical(nrow(cs))
## Beyond exact-equality, a shared root only matters when at least one side is a
## known nickname/canonical; restrict the (slow) set intersection to those rows.
relevant <- nzchar(fx) & nzchar(fy) & fx != fy & (fx %in% known | fy %in% known)
share_root[relevant] <- mapply(function(a, b) length(intersect(rmap[[a]], rmap[[b]])) > 0,
                               fx[relevant], fy[relevant])

first_rel <- rep("maybe", nrow(cs))
first_rel[fx == fy & nzchar(fx)]                <- "yes"
first_rel[first_rel != "yes" & share_root]      <- "yes"
first_rel[first_rel != "yes" & first_sim >= 0.90] <- "yes"
# clearly different first names
first_rel[first_sim < 0.70 & !share_root]       <- "no"
# single-initial vs full name is genuinely ambiguous -> maybe
init <- (nchar(fx) == 1L | nchar(fy) == 1L)
first_rel[init & substr(fx,1,1) == substr(fy,1,1)] <- "maybe"

suffix_conflict <- nzchar(sx) & nzchar(sy) & sx != sy

label <- rep(NA_integer_, nrow(cs))
## confident negatives
label[suffix_conflict]                               <- 0L
label[is.na(label) & last_sim >= 0.90 & first_rel == "no"] <- 0L  # same surname, different person
label[is.na(label) & last_sim <  0.80 & first_rel == "no"] <- 0L
## confident positives
label[is.na(label) & last_sim >= 0.90 & first_rel == "yes"] <- 1L
## everything else stays NA (ambiguous: surname change, initials, mid-sim)

cs$label <- label
cs$last_sim <- last_sim
cs$first_rel <- first_rel
cs$fam <- pmin(cs$surname_w_x, cs$surname_w_y) < 0.6 & last_sim >= 0.9

n_pos <- sum(label == 1, na.rm = TRUE)
n_neg <- sum(label == 0, na.rm = TRUE)
n_amb <- sum(is.na(label))
cat(sprintf("\nlabeled positives : %d\nlabeled negatives : %d\nambiguous (NA)    : %d\n",
            n_pos, n_neg, n_amb))

## ---- score separation ------------------------------------------------------
qs <- c(0.01, .05, .10, .25, .5, .75, .9, .95, .99)
cat("\nscore quantiles among POSITIVES:\n"); print(round(quantile(cs$score[label==1 & !is.na(label)], qs), 2))
cat("\nscore quantiles among NEGATIVES:\n"); print(round(quantile(cs$score[label==0 & !is.na(label)], qs), 2))

## ---- pair-level threshold sweep -------------------------------------------
lab <- cs[!is.na(cs$label), ]
sweep <- function(sub) {
  grid <- seq(floor(min(cs$score)), ceiling(max(cs$score)), by = 0.25)
  do.call(rbind, lapply(grid, function(t) {
    pred <- sub$score >= t
    TP <- sum(pred & sub$label == 1); FP <- sum(pred & sub$label == 0)
    FN <- sum(!pred & sub$label == 1); TN <- sum(!pred & sub$label == 0)
    prec <- ifelse(TP+FP>0, TP/(TP+FP), NA); rec <- TP/(TP+FN)
    data.frame(threshold=t, TP=TP, FP=FP, FN=FN, TN=TN,
               precision=prec, recall=rec,
               f1=ifelse(is.na(prec)|prec+rec==0, NA, 2*prec*rec/(prec+rec)))
  }))
}
sw <- sweep(lab)
best_f1 <- sw[which.max(sw$f1), ]
hp <- sw[!is.na(sw$precision) & sw$precision >= 0.99, ]
hp_pt <- if (nrow(hp)) hp[which.min(hp$threshold), ] else NULL

cat("\n== pair-level sweep (every 0.5) ==\n")
print(sw[sw$threshold %% 0.5 == 0,
         c("threshold","precision","recall","f1","FP","FN")], row.names = FALSE, digits = 3)
cat(sprintf("\nmax-F1 threshold = %.2f  (P=%.3f R=%.3f F1=%.3f)\n",
            best_f1$threshold, best_f1$precision, best_f1$recall, best_f1$f1))
if (!is.null(hp_pt))
  cat(sprintf("highest-recall point with precision>=0.99: thr=%.2f (P=%.3f R=%.3f)\n",
              hp_pt$threshold, hp_pt$precision, hp_pt$recall))
cur <- sw[sw$threshold == 5, ]
cat(sprintf("current default thr=5: P=%.3f R=%.3f F1=%.3f\n",
            cur$precision, cur$recall, cur$f1))

## ---- family-board slice ----------------------------------------------------
cat("\n== FAMILY-BOARD slice (shared surname within org) ==\n")
famlab <- lab[lab$fam, ]
cat(sprintf("labeled family pairs: %d (pos=%d neg=%d)\n",
            nrow(famlab), sum(famlab$label==1), sum(famlab$label==0)))
if (nrow(famlab) > 20) {
  swf <- sweep(famlab)
  print(swf[swf$threshold %% 1 == 0,
            c("threshold","precision","recall","f1","FP","FN")], row.names=FALSE, digits=3)
}

## ---- post-greedy operating points (the real decision) ---------------------
prep <- prepare_panel(d); work <- prep$work
greedy_all <- function(sub) {
  grp <- paste(sub$org, sub$yr_x, sub$yr_y)
  parts <- split(sub, grp)
  acc <- lapply(parts, function(g)
    greedy_one_to_one(data.frame(row_x=g$row_x, row_y=g$row_y, score=g$score,
                                 label=g$label, stringsAsFactors=FALSE)))
  do.call(rbind, acc)
}
op_points <- sort(unique(c(5, best_f1$threshold, if(!is.null(hp_pt)) hp_pt$threshold)))
cat("\n== post-one-to-one operating points ==\n")
for (t in op_points) {
  sub <- cs[cs$score >= t, ]
  acc <- greedy_all(sub)
  cl <- resolve_clusters(work, data.frame(row_x=acc$row_x, row_y=acc$row_y, score=1))
  nclust <- length(unique(cl$assignment$.emp_cluster))
  la <- acc[!is.na(acc$label), ]
  eprec <- mean(la$label == 1)
  erec <- sum(la$label == 1) / n_pos
  cat(sprintf("thr=%.2f | accepted edges=%d | labeled-edge P=%.3f R=%.3f | clusters=%d | collisions blocked=%d\n",
              t, nrow(acc), eprec, erec, nclust, cl$rejected_edges))
}

## ---- export disagreements & ambiguous for manual review -------------------
mkname <- function(f, l, s) trimws(gsub("\\s+", " ",
  paste(ifelse(is.na(f),"",f), ifelse(is.na(l),"",l), ifelse(is.na(s),"",s))))
cs$name_x <- mkname(cs$first_name_x, cs$last_name_x, cs$suffix_x)
cs$name_y <- mkname(cs$first_name_y, cs$last_name_y, cs$suffix_y)
show <- c("org","yr_x","yr_y","name_x","name_y","first_rel","last_sim","score","label","fam")
false_pos <- cs[!is.na(cs$label) & cs$label==0 & cs$score>=5, show]
false_neg <- cs[!is.na(cs$label) & cs$label==1 & cs$score< 5, show]
ambiguous <- cs[is.na(cs$label) & cs$score>=3 & cs$score<7, show]
write.csv(false_pos[order(-false_pos$score), ], file.path(outdir,"review_false_positives.csv"), row.names=FALSE)
write.csv(false_neg[order(false_neg$score), ],  file.path(outdir,"review_false_negatives.csv"), row.names=FALSE)
set.seed(1)
amb_s <- ambiguous[sample(nrow(ambiguous), min(300, nrow(ambiguous))), ]
write.csv(amb_s[order(-amb_s$score), ], file.path(outdir,"review_ambiguous_sample.csv"), row.names=FALSE)
cat(sprintf("\nreview files: FP=%d, FN=%d, ambiguous(3<=s<7) sampled=%d\n",
            nrow(false_pos), nrow(false_neg), nrow(amb_s)))

## ---- plot ------------------------------------------------------------------
png(file.path(outdir, "threshold_tuning.png"), width=1100, height=480, res=110)
op <- par(mfrow=c(1,2), mar=c(4,4,3,1))
posv <- cs$score[label==1 & !is.na(label)]; negv <- cs$score[label==0 & !is.na(label)]
br <- seq(floor(min(cs$score)), ceiling(max(cs$score)), by=0.5)
hist(negv, breaks=br, col=rgb(.85,.3,.3,.5), border=NA, main="Score by label",
     xlab="match score", ylab="pairs", freq=TRUE)
hist(posv, breaks=br, col=rgb(.3,.5,.85,.5), border=NA, add=TRUE)
abline(v=best_f1$threshold, lty=2); abline(v=5, lty=3, col="grey40")
legend("topright", c("negative","positive","max-F1","default 5"),
       fill=c(rgb(.85,.3,.3,.5),rgb(.3,.5,.85,.5),NA,NA),
       border=NA, lty=c(NA,NA,2,3), col=c(NA,NA,"black","grey40"), bty="n")
plot(sw$threshold, sw$precision, type="l", col="darkgreen", lwd=2, ylim=c(0,1),
     xlab="threshold", ylab="", main="Precision / Recall / F1")
lines(sw$threshold, sw$recall, col="darkorange", lwd=2)
lines(sw$threshold, sw$f1, col="purple", lwd=2)
abline(v=best_f1$threshold, lty=2); abline(v=5, lty=3, col="grey40")
legend("bottomleft", c("precision","recall","F1"),
       col=c("darkgreen","darkorange","purple"), lwd=2, bty="n")
par(op); dev.off()
cat("wrote threshold_tuning.png\n")
