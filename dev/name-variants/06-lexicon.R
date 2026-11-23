# Add the sound-alike pairs to the variant table, and mark which rows the
# matcher should use and which its fuzzy comparator already covers.
#
# Rows added (from 04-sound-alikes.R, pairs seen in fewer than 25 EMP_IDs):
#   sound_alike  both forms are established names that sound alike (KRISTY/KRISTIE)
#   misspelling  one form is rare (typo rule in 02-classify.R) but sounds right (TODD/TOOD)
#
# Columns added to every row:
#   lexicon        TRUE for nickname/spelling/compound/cognate (the 25+ reviewed
#                  pairs); FALSE for sound_alike/misspelling. A comparator built
#                  from the table loads only lexicon == TRUE.
#   fuzzy_sim      the fuzzy part of compare_first_names(): Jaro-Winkler rescaled
#                  by (jw - 0.5) / 0.5, raised to 0.5 when Soundex agrees. Without
#                  the nickname lookup. Symmetric: fuzzy_sim(a, b) == fuzzy_sim(b, a).
#   fuzzy_points   what fuzzy_sim adds to the match score: 5 * (2 * sim - 1)
#                  (first_name weight 5 in default_weights())
#   fuzzy_covered  fuzzy_sim >= 0.8 (+3 points or more). A listed pair scores 0.95
#                  (+4.5), so listing a covered pair gains at most 1.5 points.
#
# Run from the package root after 05-chains.R. Rerunnable: rows added by an
# earlier run are dropped first.
suppressMessages({library(data.table); library(stringdist)})
ev <- file.path(Sys.getenv("SYNTHID_DIR", "C:/Users/jlecy/Documents/SYNTHID"), "partvii-v3_1", "name-variants")
f <- "dev/name-variants/first-name-variants.csv"
COVERED <- 0.8

fuzzy_sim <- function(a, b, floor = 0.5) {          # mirrors compare_first_names() minus the nickname step
  jw <- 1 - stringdist(a, b, method = "jw")
  base <- pmin(pmax((jw - floor) / (1 - floor), 0), 1)
  alpha <- function(v) gsub("[^A-Z]", "", v)
  ph <- phonetic(alpha(a)) == phonetic(alpha(b))
  pmax(base, ifelse(!is.na(ph) & ph, 0.5, 0))
}

t <- fread(f)
if ("lexicon" %in% names(t)) t <- t[lexicon == TRUE]
t[, intersect(names(t), c("lexicon", "fuzzy_sim", "fuzzy_points", "fuzzy_covered", "source")) := NULL]
t[, `:=`(lexicon = TRUE, source = "reviewed (25+ EMP_IDs)")]

s <- fread(file.path(ev, "first-name-sound-alikes.csv"))
cand <- fread(file.path(ev, "first-name-variants-candidates.csv"))
add <- cand[s[, .(name_short, name_long, kind)], on = .(name_short, name_long), nomatch = 0L]
add <- add[!t, on = .(name_short, name_long)]
add[, type := fifelse(kind == "misspelling that sounds right", "misspelling", "sound_alike")]
add[, `:=`(lexicon = FALSE, source = paste0("under 25: ", gate))]
add <- add[, intersect(names(t), names(add)), with = FALSE]

out <- rbind(t, add, fill = TRUE)
out[, fuzzy_sim := round(fuzzy_sim(name_short, name_long), 3)]
out[, fuzzy_points := round(5 * (2 * fuzzy_sim - 1), 2)]
out[, fuzzy_covered := fuzzy_sim >= COVERED]
lead <- c("name_short", "name_long", "type", "lexicon", "n_emp_ids", "fuzzy_sim", "fuzzy_points", "fuzzy_covered")
setcolorder(out, c(lead, setdiff(names(out), lead)))
setorder(out, -lexicon, -n_emp_ids)
fwrite(out, f)
# the copy the package ships: lexicon rows only, read by synthid::first_name_lexicon()
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
# cofiling = the two forms also appear as two people with one surname on one
# return (JOHN/JOHNNY, Sr and Jr); compare_first_names() can score these lower
fwrite(out[lexicon == TRUE, .(name_short, name_long, type, n_emp_ids, cofiling = grepl("cofiling", flags))],
       "inst/extdata/first-name-variants.csv")

cat("Rows:", nrow(out), " lexicon:", sum(out$lexicon), " outside lexicon:", sum(!out$lexicon), "\n\n")
print(out[, .(pairs = .N, fuzzy_covered = sum(fuzzy_covered), share_covered = round(mean(fuzzy_covered), 3),
              penalized = sum(fuzzy_sim < 0.5), median_sim = median(fuzzy_sim)), by = .(type, lexicon)][order(-lexicon, -pairs)])
cat("\nLexicon pairs the fuzzy comparator does not cover (the ones listing adds most to):",
    out[lexicon == TRUE & fuzzy_covered == FALSE, .N], "\n")
