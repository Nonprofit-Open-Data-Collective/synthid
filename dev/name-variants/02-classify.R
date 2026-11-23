# Classify first-name variant pairs into a draft first-name variant table.
#
# Input: pairs of first names that one within-org person (EMP_ID) carries
# across filings, national Part VII v3.1 build (first-name-variant-pairs.csv,
# from profiles/first_name_variants), plus the evidence from 01-pair-evidence.R
# and the public nickname tables (carltonnorthern/nicknames, onyxrev/
# common_nickname_csv) and synthid::nickname_table().
#
# Types kept in the table (two forms of one given name):
#   nickname  a different word for the name, incl. clipped short forms and
#             diminutives (BOB/ROBERT, CHRIS/CHRISTOPHER, KATIE/KATE)
#   spelling  the same name spelled differently (JEFFERY/JEFFREY, SEAN/SHAWN)
#   compound  one element of a compound given name (MARY/MARYANN)
#   cognate   the same name in another language (JOSE/JOSEPH, ROBERT/ROBERTO)
# Types written to separate files:
#   typo         a misspelling of one name (JOSPEH/JOSEPH); record error, not a variant
#   parse_error  not a first name (JP/JPMORGAN, WILLIAM/WILLIAMS)
#   questionable likely two different names / people (JOHN/JOSH, PAUL/PAULA)
#
# Pairs seen in 25+ EMP_IDs were reviewed by hand (the lists below). Pairs
# under 25 get the same rules and evidence checks but are written as
# candidates, with the checks they pass, for verification before promotion.
suppressMessages({library(data.table); library(stringdist)})
base <- file.path(Sys.getenv("SYNTHID_DIR", "C:/Users/jlecy/Documents/SYNTHID"), "partvii-v3_1")
ev   <- file.path(base, "name-variants")
repo <- "dev/name-variants"
tabs_dir <- Sys.getenv("NICKNAME_TABLES_DIR", "dev/name-variants/reference")   # carltonnorthern.csv, onyxrev.csv
MIN_N <- 25

key <- function(a, b) paste(a, b, sep = "/")
pairs_set <- function(x) { x <- strsplit(x, "/"); vapply(x, function(p) key(p[1], p[2]), "") }

# ---- hand review of the 25+ pairs ---------------------------------------
questionable <- c(
  # different names that differ by a letter or two
  "JOHN/JOSH","JIM/JOHN","JOE/JOHN","ROGER/ROBERT","LISA/LINDA","SHARON/SHANNON","DAN/DON","TIM/TOM",
  "ROB/RON","ROB/ROD","ROD/RON","JIM/TIM","RAY/ROY","BRENT/BRETT","BRET/BRENT","KEVIN/KELVIN",
  "BARRY/BERRY","MARIA/MARLA","MARY/MARCY","MARA/MARIA","MARIA/MARCIA","JODY/JUDY","JAN/JEN",
  "JAMIE/JANIE","JOE/JON","MACK/MARK","PAM/PAT","DALE/DAVE","HAROLD/HOWARD","GREG/GEORGE","BRAD/BRIAN",
  "TONY/TROY","KEITH/KENNETH","JERRY/JEREMY","GARY/GERRY","CHRIS/CHARLES","JAKE/JAMES","JACK/JAKE",
  "ALICE/ALICIA","JANET/JANICE","ROSS/RUSS","DANNY/DENNY","LINA/LINDA","ELISE/ELSIE","JOHN/JOSHUA",
  "WILLARD/WILLIAM","EDWIN/EDWARD","KATHRYN/KATHLEEN","KATHLEEN/KATHERINE","GERALD/GERARD",
  "LAURA/LAUREN","LAURA/LAUREL","LAUREN/LAUREEN","LARA/LAURA","ROLAND/RONALD","RHODA/RHONDA",
  "JOSH/JOSEPH","MELINA/MELINDA","JENNIE/JEANNIE","JANA/JANE","JAN/JANE","DANA/DIANA","KEN/KENT",
  "ANDRA/ANDREA","JESS/JESUS","JESSE/JESUS","JAY/JAMES","ERIC/RICK","SANDY/ALEXANDER",
  # forms of different genders
  "CARL/CAROL","MARY/MARTY","MICHELL/MITCHELL","JOE/JOEL","PAUL/PAULA","ROBERT/ROBERTA",
  "JUSTIN/JUSTINE","ERIC/ERICA","NORMA/NORMAN","DENIS/DENISE","FRANCES/FRANCIS","LOUIS/LOUISE",
  "ALEXANDER/ALEXANDRA","CHRISTIAN/CHRISTINA",
  # moved out after the CSA 450 match review (2026-11-23): the cross-org matches
  # they added were mostly two different people (different surnames, a MARY with
  # a second given name, no shared board members), and JOHN and JONATHAN also
  # appear as two people on one return about half as often as as one
  "JOHN/JONATHAN","JOHN/JOHNATHAN","MARY/MARIE","MARY/MARIA","MARIA/MARIE")
parse_error <- c("TIAA/TIAA-CREF","JP/JPMORGAN","WILLIAM/WILLIAMS","DAVID/DAVIS","TIM/TIME","CHRIS/CHRIST",
                 "DOUG/DOUGH","JOS/JOSE")
cognate <- c("ROBERT/ROBERTO","ANTHONY/ANTONIO","EDWARD/EDUARDO","ED/EDUARDO","FRANK/FRANCISCO",
             "MIKE/MIGUEL","TOM/TOMAS","TOMAS/THOMAS","ART/ARTURO","GUS/GUSTAVO","ALFRED/ALFREDO",
             "ALBERT/ALBERTO","RICARDO/RICHARD","JOE/JOSE","JOSE/JOSEPH","ERNIE/ERNESTO","JAIME/JAMES",
             "MICHEL/MICHAEL","ANDRE/ANDREW","ANDRES/ANDREW","NATALIA/NATALIE","SUSAN/SUSANA",
             "LUIS/LOUIS","ERIC/ERICH","LISA/LISE","MARIAM/MIRIAM",
             "JOANNA/JOHANNA","JULIA/JULIE")
# hand-confirmed typos the frequency rule can miss (both forms reasonably common, or short)
typo_manual <- c("BRAIN/BRIAN","MANUAL/MANUEL","MARY/MARRY","KAREN/KARAN","JOH/JOHN","BIL/BILL","JEF/JEFF",
                 "JIL/JILL","ROBER/ROBERT","JAME/JAMES","CHUCH/CHUCK","CHIRS/CHRIS","CHIS/CHRIS","DONAL/DONALD",
                 "RONAL/RONALD","REGINAL/REGINALD","RAYMON/RAYMOND","TIMOTH/TIMOTHY","MARTH/MARTHA",
                 "MARILY/MARILYN","BARBAR/BARBARA","CHERY/CHERYL","ELAIN/ELAINE","RICARDO/RICHARDO",
                 "MIGUEL/MIQUEL","ROBERT/ROBERTY","ROBERT/ROBERY","PAUL/PUAL","DAIVD/DAVID","DAVD/DAVID",
                 # a real nickname paired with a misspelled formal name
                 "CHRIS/CHRISTOPER","JOE/JOESPH","JOE/JOESEPH","JOE/JOSPEH","PAT/PATRICA","DAN/DANIAL",
                 "SAM/SAMUAL","MAJORIE/MARJORIE","CANDACE/CANDANCE")
# hand corrections to the type of kept pairs (the rules cannot tell a
# spelling from a nickname for these)
type_fix <- c("MALCOM/MALCOLM"="spelling","DEIDRE/DEIRDRE"="spelling","DIEDRE/DEIRDRE"="spelling",
              "HEIDE/HEIDI"="spelling","SONJA/SONYA"="spelling","SONIA/SONJA"="spelling","LINSEY/LINDSEY"="spelling",
              "ANNMARIE/ANNEMARIE"="spelling","ANN-MARIE/ANNE-MARIE"="spelling","BRIGETTE/BRIDGETTE"="spelling",
              "BRIDGET/BRIDGETTE"="spelling","JESSE/JESSEE"="spelling","RENE/RENEE"="spelling","JEAN/JEANNE"="spelling",
              "ASHLEY/ASHLEIGH"="spelling","MARGO/MARGOT"="spelling","RACHEL/RACHELLE"="spelling",
              "SUSAN/SUSANNE"="spelling","SUSAN/SUSANNA"="spelling",
              "CYNDI/CYNTHIA"="nickname","CINDI/CYNTHIA"="nickname","CYNDY/CYNTHIA"="nickname","MARTI/MARTHA"="nickname",
              "MARCI/MARCIA"="nickname","CHRISTIE/CHRISTINE"="nickname","CHRISTA/CHRISTINA"="nickname",
              "KRISTI/KRISTIN"="nickname","PATRICE/PATRICIA"="cognate")
# real variants the frequency rule would call typos (a rare but established spelling)
not_typo <- c("MICHAEL/MICHEAL","MIKE/MICHEAL","JEFF/JEFFEREY","STEPHANI/STEPHANIE","STEPHANY/STEPHANIE",
              "JENIFER/JENNIFER","JEN/JENIFER","MARIAM/MIRIAM","CYNDY/CYNTHIA","DEBORA/DEBORAH","DEB/DEBORA",
              "JOHNATHON/JONATHON","JONATHAN/JOHNATHON",
              # hand review of the 25+ typo calls: established spellings and short forms
              "SCOT/SCOTT","DOUGLAS/DOUGLASS","CHARLES/CHARLEY","ANTONY/ANTHONY","JASON/JAYSON",
              "CAROLYN/CAROLYNN","JULI/JULIE","NANCI/NANCY","KAREN/KARYN","SHARON/SHARYN","SUSAN/SUZAN",
              "CRIS/CHRIS","PHIL/PHILL","DON/DONN","JANET/JANEY","LIN/LINDA","MARI/MARY","AMI/AMY",
              "TONY/TONEY","HOLLY/HOLLEY","SUSAN/SUSANNA","ROB/ROBB","PAT/PATT","LESA/LISA","LESLEE/LESLIE",
              "LESLI/LESLIE","CATHY/CATHEY","KATHY/KATHEY","DEREK/DERECK","NICOLE/NICOLLE","HEIDE/HEIDI",
              "CHERYL/CHERYLE","JESSI/JESSICA","SANDIE/SANDRA","SALLY/SALLEY","DEBRA/DEBRAH","DEBRAH/DEBORAH",
              "MARILYN/MARYLIN","MARILYN/MARYLYN","ANGELA/ANGELIA","SHANNAN/SHANNON","SHANON/SHANNON",
              "JEFFRY/JEFFREY","JEFF/JEFFRY","AARON/ARRON","SHARON/SHARRON","JEANE/JEANNE","AUDRY/AUDREY",
              "CHRISTIN/CHRISTINE","GEOFFERY/GEOFFREY","REBECA/REBECCA","MELISA/MELISSA","MARGRET/MARGARET",
              "BARBRA/BARBARA","DORTHY/DOROTHY","PHYLIS/PHYLLIS","PHYLISS/PHYLLIS","PHILLIS/PHYLLIS",
              "TERESA/TERESSA","HELEN/HELLEN","ELANOR/ELEANOR","RACHEL/RACHEAL","JESSE/JESSEE",
              "MICHAL/MICHAEL","STEPHANIE/STEPHENIE","KATHRINE/KATHERINE","CATHRINE/CATHERINE")
compound_tail <- c("ANN","ANNE","MARIE","BETH","LOU","JO","JANE","KAY","ELLEN","MARY","LYNN","LEE","ANNA","ELLA")

# ---- data ---------------------------------------------------------------
r <- fread(file.path(base, "first-name-variant-pairs.csv"))[short_is_initial == FALSE]
r <- r[, .(name_short, name_long, n_emp_ids)]
co <- fread(file.path(ev, "cofiling-pairs.csv"))
gd <- fread(file.path(ev, "first-name-gender.csv"))
gcols <- setdiff(names(gd), "first_name")
gd[, n_rows := rowSums(.SD), .SDcols = gcols]
gd[, share_f := if ("F" %in% gcols) F / pmax(1, (if ("M" %in% gcols) M else 0) + F) else NA_real_]
r <- co[r, on = .(name_short, name_long)][is.na(n_filings_together), n_filings_together := 0L]
r <- gd[, .(name_short = first_name, freq_short = n_rows, f_short = share_f)][r, on = "name_short"]
r <- gd[, .(name_long  = first_name, freq_long  = n_rows, f_long  = share_f)][r, on = "name_long"]
for (v in c("freq_short", "freq_long")) set(r, which(is.na(r[[v]])), v, 0)

# public and package tables: two names are listed if one is the other's formal
# name or they share a formal root
env <- new.env(); sys.source("R/first_names.R", env); nt <- env$nickname_table()
edges <- list(synthid = data.table(formal = rep(names(nt), lengths(nt)), nick = unlist(nt)))
if (nzchar(tabs_dir)) {
  cn <- fread(file.path(tabs_dir, "carltonnorthern.csv"))
  edges$carlton <- cn[relationship == "has_nickname", .(formal = toupper(name1), nick = toupper(name2))]
  ox <- fread(file.path(tabs_dir, "onyxrev.csv"), strip.white = TRUE); setnames(ox, trimws(names(ox)))
  edges$onyx <- ox[, .(formal = toupper(trimws(name)), nick = toupper(trimws(nickname)))]
}
listed <- function(e, a, b) {
  roots <- unique(rbind(e[, .(name = nick, root = formal)], e[, .(name = formal, root = formal)]))
  ra <- roots[data.table(name = a, i = seq_along(a)), on = "name", nomatch = 0L]
  rb <- roots[data.table(name = b, i = seq_along(b)), on = "name", nomatch = 0L]
  seq_along(a) %in% unique(merge(ra[, .(i, root)], rb[, .(i, root)], by = c("i", "root"))$i)
}
for (t in names(edges)) set(r, j = paste0("in_", t), value = listed(edges[[t]], r$name_short, r$name_long))
in_cols <- grep("^in_", names(r), value = TRUE)
r[, in_table := Reduce(`|`, .SD), .SDcols = in_cols]
table_names <- unique(unlist(lapply(edges, function(e) c(e$formal, e$nick))))

# ---- evidence flags -----------------------------------------------------
r[, pair := key(name_short, name_long)]
r[, osa := stringdist(name_short, name_long, method = "osa")]
r[, sdx_same := phonetic(gsub("[^A-Z]", "", name_short)) == phonetic(gsub("[^A-Z]", "", name_long))]
r[, sdx_tail := substr(phonetic(gsub("[^A-Z]", "", name_short)), 2, 4) == substr(phonetic(gsub("[^A-Z]", "", name_long)), 2, 4)]
r[, prefix := startsWith(name_long, name_short)]
r[, freq_ratio := pmin(freq_short, freq_long) / pmax(1, pmax(freq_short, freq_long))]
r[, rare_form := fifelse(freq_short <= freq_long, name_short, name_long)]
# co-filing: the two forms appear as separate people with one surname on one return
r[, cofiling_rate := n_filings_together / n_emp_ids]
# gender: each form recorded mostly with a different gender (both forms seen 50+ times)
r[, gender_conflict := freq_short >= 50 & freq_long >= 50 & !is.na(f_short) & !is.na(f_long) &
      ((f_short >= 0.8 & f_long <= 0.2) | (f_short <= 0.2 & f_long >= 0.8))]

# ---- rules --------------------------------------------------------------
dim_suffix <- "(IE|Y|EY|I|EE)$"
base_of <- function(x) { b <- sub(dim_suffix, "", x); sub("([B-DF-HJ-NP-TV-Z])\\1$", "\\1", b) }
strip_vowel <- function(x) sub("[AEIOU]$", "", x)
is_compound <- function(s, l) {
  tail <- sub("^[- ]", "", substring(l, nchar(s) + 1))
  startsWith(l, s) & nchar(tail) >= 2 & tail %in% compound_tail
}
is_typo_rule <- function(d) d$freq_ratio < 0.02 & d$osa <= 2 & !(d$rare_form %in% table_names)

classify <- function(d) {
  s <- d$name_short; l <- d$name_long
  dim_one <- xor(grepl(dim_suffix, s), grepl(dim_suffix, l)) &
             (base_of(s) == base_of(l) | strip_vowel(base_of(s)) == strip_vowel(base_of(l)) |
              base_of(s) == strip_vowel(l) | base_of(l) == strip_vowel(s))
  spelling_like <- (d$sdx_same | d$sdx_tail & substr(s, 1, 1) %in% c("C","K","S","G","J") &
                    substr(l, 1, 1) %in% c("C","K","S","G","J")) &
                   abs(nchar(s) - nchar(l)) <= 1 & d$osa <= 2
  fcase(d$pair %in% questionable, "questionable",
        d$pair %in% parse_error, "parse_error",
        !grepl("^[A-Z][A-Z' -]*[A-Z]$", s) | !grepl("^[A-Z][A-Z' -]*[A-Z]$", l), "parse_error",
        d$pair %in% cognate, "cognate",
        d$pair %in% typo_manual, "typo",
        is_compound(s, l), "compound",
        gsub("[- ]", "", s) == gsub("[- ]", "", l), "spelling",
        is_typo_rule(d) & !(d$pair %in% not_typo), "typo",
        dim_one, "nickname",
        spelling_like, "spelling",
        d$in_table, "nickname",
        d$prefix, "nickname",
        default = "unclassified")
}
r[, type := classify(r)]
r[, reviewed := n_emp_ids >= MIN_N]
# frequent pairs that no rule placed were reviewed by hand and are real
# variants (STEVEN/STEPHEN, KATE/KATHLEEN, ...): call them nickname, or
# spelling when they sound alike
r[reviewed & type == "unclassified", type := fifelse(sdx_same | sdx_tail, "spelling", "nickname")]
r[pair %in% names(type_fix) & type %in% c("nickname", "spelling", "cognate", "compound"), type := type_fix[pair]]

# evidence red flags (applied to all pairs; for 25+ pairs they mark a hand call to re-check)
COFILE_MAX <- as.numeric(Sys.getenv("COFILE_MAX", "0.10"))
r[, flag_cofiling := cofiling_rate > COFILE_MAX & n_filings_together >= 3]
r[, flag_gender := gender_conflict]
r[, flags := trimws(paste(fifelse(flag_cofiling, "cofiling", ""), fifelse(flag_gender, "gender", "")))]

keep_types <- c("nickname", "spelling", "compound", "cognate")
cols <- c("name_short", "name_long", "type", "n_emp_ids", in_cols, "n_filings_together", "cofiling_rate",
          "freq_short", "freq_long", "f_short", "f_long", "flags")
# Flagged 25+ pairs were re-checked by hand and kept. Co-filing flags there
# are mostly diminutives that families also use to tell relatives apart
# (JOHN/JOHNNY Sr/Jr): real variants, weaker evidence. Gender flags there are
# unisex short forms (PAT, JAMIE, RENE): the gender field is inferred from the
# first name, so it cannot settle them. The flag stays in the table.
main <- r[reviewed & type %in% keep_types]
recheck <- main[nzchar(flags)]
quest <- r[reviewed & type == "questionable"]
errs  <- r[reviewed & type %in% c("typo", "parse_error")]

# candidates under 25: promote-ready if listed in a public table, or a rule
# type with support from 5+ EMP_IDs, both forms established names (100+ rows),
# and no red flag
cand <- r[reviewed == FALSE]
cand[, gate := fcase(type %in% c("questionable", "typo", "parse_error"), "excluded: rule",
                     nzchar(flags), "hold: red flag",
                     in_table & type %in% keep_types, "ready: in a public table",
                     type %in% c("spelling", "compound", "cognate") & n_emp_ids >= 5 & pmin(freq_short, freq_long) >= 100,
                       "ready: rule + support",
                     default = "hold: needs review")]

fwrite(main[order(-n_emp_ids), ..cols], file.path(repo, "first-name-variants.csv"))
fwrite(quest[order(-n_emp_ids), ..cols], file.path(repo, "first-name-variants-questionable.csv"))
fwrite(errs[order(-n_emp_ids), ..cols], file.path(repo, "first-name-variants-errors.csv"))
fwrite(cand[order(-n_emp_ids), c(cols, "gate"), with = FALSE], file.path(ev, "first-name-variants-candidates.csv"))

# Review sample for an error rate: up to 40 pairs per type from the main
# table, and 40 from each promote-ready candidate gate, drawn at random. A
# reviewer marks `same_name` (Y/N/?). With 0 errors in 40, the 95% upper
# bound on that type's error rate is about 7%. The sample is drawn once and
# then kept, so review calls on it survive a rebuild; set REDRAW_SAMPLE=1 to
# draw a new one.
if (!file.exists(file.path(repo, "review-sample.csv")) || Sys.getenv("REDRAW_SAMPLE") == "1") {
set.seed(20261123)
samp <- rbind(main[, stratum := paste0("main: ", type)],
              cand[startsWith(gate, "ready"), c(names(main)[names(main) != "stratum"], "gate"), with = FALSE][, stratum := gate],
              fill = TRUE)
samp <- samp[, .SD[sample(.N, min(.N, 40))], by = stratum]
fwrite(samp[, .(stratum, name_short, name_long, type, n_emp_ids, n_filings_together, flags, same_name = "", note = "")],
       file.path(repo, "review-sample.csv"))
}

cat("Pairs seen in", MIN_N, "+ EMP_IDs:", r[reviewed == TRUE, .N], "\n")
print(r[reviewed == TRUE, .(pairs = .N, emp_ids = sum(n_emp_ids)), by = type][order(-emp_ids)])
cat("\nMain-table pairs carrying an evidence flag:", nrow(recheck), "\n")
print(recheck[, .N, by = flags])
cat("\nTypo calls (25+), with frequency ratio:\n")
cat(errs[type == "typo"][order(-n_emp_ids)][, paste0(pair, " ", round(freq_ratio, 3))], sep = "; "); cat("\n")
cat("\nMain table:", nrow(main), "pairs\n"); print(main[, .N, by = type])
cat("\nCo-filing rate by type (25+ pairs): median and 90th percentile\n")
print(r[reviewed == TRUE, .(median = median(cofiling_rate), p90 = quantile(cofiling_rate, 0.9), share_flagged = mean(flag_cofiling)), by = type])
cat("\nUnder", MIN_N, ":", nrow(cand), "pairs,", sum(cand$n_emp_ids), "EMP_ID rows\n")
print(cand[, .(pairs = .N, emp_ids = sum(n_emp_ids)), by = gate][order(-emp_ids)])
