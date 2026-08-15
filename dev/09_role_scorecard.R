# =============================================================================
# 09_role_scorecard.R  --  role-refinement accuracy dashboard.
#
# BEFORE = raw cascade (role_cascade_output.csv)
# AFTER  = refined, all passes (role_refined_output.csv, from 10_role_passes.R)
# GOLD   = real precision/recall on the 158 hand/AI-labeled rows.
#
# Tiers: OPTIMIZE {CEO, board} | ASSESS {c.level, dir.vp, mgr, spec} |
#        REPORT {board pres/sec/treas}.  Writes role_scorecard.csv.
# =============================================================================
suppressMessages(library(dplyr))
base <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid/dev/"
L <- function(x) x=="TRUE"|x==TRUE; b01 <- function(x) as.integer(x) %in% 1L
num <- function(x) suppressWarnings(as.numeric(x)); pct <- function(x) round(100*mean(x),1)

ref <- read.csv(paste0(base,"role_refined_output.csv"), stringsAsFactors=FALSE) %>%
  mutate(across(c(is_primary_row,paid,cb_tru,cb_off,cb_key), L)) %>% filter(is_primary_row)
cas <- read.csv(paste0(base,"role_cascade_output.csv"), stringsAsFactors=FALSE) %>%
  mutate(across(c(is_primary_row,paid,cb_tru,cb_off,cb_key), L)) %>% filter(is_primary_row)
clx <- readRDS(paste0(base,"data/classified_slice.rds"))
sub <- clx %>% group_by(OBJECTID,person.id) %>%
  summarise(f_clevel=any(b01(c.level)), f_mgr=any(b01(mgr)), f_spec=any(b01(spec)), .groups="drop")
ref <- ref %>% left_join(sub, by=c("OBJECTID","person.id"))

SC <- list(); add <- function(tier,cat,metric,before,after,target=""){
  SC[[length(SC)+1]] <<- data.frame(tier,category=cat,metric,before,after,target,stringsAsFactors=FALSE)}

# ---------- OPTIMIZE: CEO ----------
ceocov <- function(d) { o <- d %>% group_by(OBJECTID) %>% summarise(np=sum(paid),hc=any(func=="ceo"))
                        pct(o$hc[o$np>0]) }
add("OPTIMIZE","CEO","paid orgs with >=1 CEO (%)", ceocov(cas), ceocov(ref), "~high (board-gov valid)")
lt <- ref %>% distinct(OBJECTID, org_leadership) %>% count(org_leadership)
add("OPTIMIZE","CEO","orgs: designated / chief-staff / board-governed", "-",
    paste(lt$n[match(c("designated","chief_staff_imputed","board_governed"),lt$org_leadership)],collapse=" / "))
add("OPTIMIZE","CEO","co-CEO / interim persons","-",
    paste(sum(ref$leader_type=="co_ceo",na.rm=T), sum(ref$leader_type=="interim_ceo",na.rm=T),sep=" / "))

# ---------- OPTIMIZE: BOARD (independent title-vs-checkbox corroboration) ----------
BT <- "\\bBOARD\\b|\\bTRUSTEE\\b|CHAIR|COUNCIL|DELEGATE|\\bREGENT\\b|\\bELDER\\b|DEACON|VESTRY|OVERSEER"
boardcorr <- function(d){ d <- d %>% mutate(bd=person_position %in% c("board","board_officer"),
      tb=grepl(BT,toupper(paste(title.standard))))
  b <- d %>% filter(bd); pct(b$cb_tru | b$tb) }
add("OPTIMIZE","BOARD","board persons corroborated (box|title) (%)", boardcorr(cas), boardcorr(ref), ">=95")
bc <- function(d){ x <- d %>% group_by(OBJECTID) %>%
      summarise(pred=sum(person_position %in% c("board","board_officer")), truth=sum(cb_tru)) %>% filter(truth>0)
  pct(x$pred==x$truth)}
add("OPTIMIZE","BOARD","org board-count exact vs trustee box (%)", bc(cas), bc(ref), ">=90")

# ---------- ASSESS ----------
dv <- ref %>% filter(!is.na(dirvp_disposition)) %>% count(dirvp_disposition)
add("ASSESS","dir.vp","resolved: board / up(officer) / down(mgr)","1 fuzzy bucket",
    paste(dv$n[match(c("board","up:officer/c-level","down:manager"),dv$dirvp_disposition)],collapse=" / "),"partition+route")
for(f in c("f_clevel","f_mgr","f_spec")){
  s <- ref %>% filter(.data[[f]]); nm <- sub("f_","",f)
  add("ASSESS",nm,"n / %paid / %officer-box","-",
      sprintf("%d / %d%% / %d%%", nrow(s), round(pct(s$paid)), round(pct(s$cb_off))),
      if(nm=="clevel") "coherent-keep" else "assess->KEY?")
}

# ---------- role_coarse distribution (KEY split) ----------
rc <- ref %>% count(role_coarse)
add("SUMMARY","role_coarse","BOARD/CEO/OFFICER/MANAGER/KEY/STAFF","KEY=1264 lumped",
    paste(rc$n[match(c("BOARD","CEO","OFFICER","MANAGER","KEY","STAFF"),rc$role_coarse)],collapse="/"))

# ---------- REPORT: board pres/sec/treas ----------
for(rl in c("president/chair","secretary","treasurer")){
  o <- ref %>% group_by(OBJECTID) %>% summarise(n=sum(board_officer_role==rl,na.rm=T))
  add("REPORT",rl,"orgs 0 / 1 / >1 (%)","-",
      sprintf("%d / %d / %d", round(pct(o$n==0)),round(pct(o$n==1)),round(pct(o$n>1))))
}
conc <- ref %>% filter(board_officer_role=="president/chair") %>% count(board_officer_concurrency)
add("REPORT","president/chair","multiples: single/shared/transition","-",
    paste(conc$n[match(c("single","shared/concurrent","transition"),conc$board_officer_concurrency)],collapse="/"))

# ---------- GOLD: real accuracy on labeled sample ----------
gold <- read.csv(paste0(base,"gold_sample_labeled.csv"), stringsAsFactors=FALSE) %>%
  left_join(ref %>% select(OBJECTID,person.id,pred=role_coarse), by=c("OBJECTID","person.id")) %>%
  mutate(truth=case_when(
    gold_role %in% c("CEO","CO_CEO","INTERIM_CEO")~"CEO",
    gold_role %in% c("CFO","COO","C_LEVEL_OTHER","OFFICER")~"OFFICER",
    gold_role %in% c("BOARD_CHAIR","BOARD_OFFICER","BOARD_MEMBER")~"BOARD",
    gold_role=="MANAGER"~"MANAGER", gold_role=="KEY_EMPLOYEE"~"KEY",
    gold_role=="STAFF"~"STAFF", TRUE~"OTHER")) %>% filter(truth!="OTHER")

cat("================= ROLE SCORECARD (before=cascade, after=refined) =================\n")
SCdf <- do.call(rbind, SC)
for(t in c("OPTIMIZE","ASSESS","SUMMARY","REPORT")){
  cat("\n---",t,"---\n"); print(SCdf %>% filter(tier==t) %>% select(category,metric,before,after,target), row.names=FALSE)}

cat(sprintf("\n--- GOLD real accuracy (n=%d labeled) : overall coarse %.1f%% ---\n",
            nrow(gold), pct(gold$truth==gold$pred)))
for(c in c("CEO","BOARD","OFFICER","MANAGER","KEY")){
  tp<-sum(gold$truth==c & gold$pred==c); pr<-sum(gold$pred==c); re<-sum(gold$truth==c)
  cat(sprintf("  %-8s precision %3.0f%% (%d/%d)  recall %3.0f%% (%d/%d)\n", c,
      ifelse(pr>0,100*tp/pr,NA),tp,pr, ifelse(re>0,100*tp/re,NA),tp,re))}

write.csv(SCdf, paste0(base,"role_scorecard.csv"), row.names=FALSE)
cat("\nWrote role_scorecard.csv\n")
