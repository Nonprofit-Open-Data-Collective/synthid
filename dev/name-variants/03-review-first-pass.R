# First-pass review of review-sample.csv (by Claude, 2026-11-23), to be
# checked by a person. Every pair not listed below is marked Y (two forms of
# one given name). N = different names; ? = plausible but uncertain.
# Writes same_name/note into review-sample.csv and prints error rates by
# stratum, with a 95% Wilson upper bound counting ? as half an error.
suppressMessages(library(data.table))
f <- "dev/name-variants/review-sample.csv"
s <- fread(f, colClasses = list(character = c("same_name", "note")))
calls <- rbindlist(list(
  list("JANE/JANET", "?", "distinct modern names (Janet began as a diminutive); co-filing 0.32"),
  list("LISA/LISSA", "?", "LISSA is also a short form of MELISSA"),
  list("ELLEN/EILEEN", "?", "distinct names, often confused; co-filing 0.29"),
  list("BRYAN/BRYON", "?", "BRYON may be a misspelling of BYRON"),
  list("JOSE/JOSEPH", "?", "co-filing 0.79: often two people in one family"),
  list("MARIA/MARIE", "?", "distinct names; co-filing 0.13"),
  list("MARY/MARIA", "?", "distinct names; co-filing 0.17"),
  list("ANDRE/ANDREW", "?", "distinct names in US use"),
  list("ALAN/ALEN", "?", "ALEN is probably a misspelling"),
  list("LAN/LANA", "N", "LAN is a distinct (often Vietnamese) name"),
  list("RAY/REY", "?", "REY is a distinct Spanish name or a typo"),
  list("PATRINA/PETRINA", "?", "related but distinct names"),
  list("BRENNA/BREANNA", "N", "distinct names"),
  list("DINA/DIANA", "N", "distinct names"),
  list("ALISA/ALISHA", "?", "pronounced differently"),
  list("WILLIE/WILFRED", "?", "one EMP_ID; WILLIE usually WILLIAM"),
  list("DINA/ADINA", "?", "one EMP_ID"),
  list("KAY/KATE", "?", "KAY is also a name of its own"),
  list("MONTY/LAMONT", "?", "rare pairing"),
  list("CASS/CASWELL", "?", "one EMP_ID"),
  list("SHAY/SHARON", "?", "SHAY is usually a name of its own"),
  list("CARL/CHARLIE", "?", "linked only through CHARLES / KARL")
))
setnames(calls, c("pair", "call", "why"))
s[, pair := paste(name_short, name_long, sep = "/")]
s[, same_name := "Y"][, note := ""]
s[calls, on = "pair", `:=`(same_name = i.call, note = i.why)]
fwrite(s[, !"pair"], f)

wilson_upper <- function(x, n, z = 1.96) {
  p <- x / n; (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)
}
res <- s[, .(n = .N, Y = sum(same_name == "Y"), unsure = sum(same_name == "?"), N = sum(same_name == "N")), by = stratum]
res[, `:=`(error_rate = round((N + unsure / 2) / n, 3), upper_95 = round(wilson_upper(N + unsure / 2, n), 3))]
print(res)
