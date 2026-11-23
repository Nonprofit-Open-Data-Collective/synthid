# Coverage of the variant pairs by synthid::nickname_table() and the two public
# tables in reference/ (first pass; 02-classify.R supersedes it). Run from the package root.
suppressMessages({library(data.table); library(stringdist)})
S <- "dev/name-variants/reference"
OUTDIR <- file.path(Sys.getenv("SYNTHID_DIR", "C:/Users/jlecy/Documents/SYNTHID"), "partvii-v3_1")
r <- fread(file.path(OUTDIR, "first-name-variant-pairs.csv"))
r <- r[short_is_initial == FALSE & grepl("^[A-Z]+$", name_short) & grepl("^[A-Z]+$", name_long)]

# each table -> formal/nickname edges (upper case)
env <- new.env(); sys.source("R/first_names.R", env)
nt <- env$nickname_table()
synthid <- data.table(formal = rep(names(nt), lengths(nt)), nick = unlist(nt))
cn <- fread(file.path(S, "carltonnorthern.csv"))
carlton <- cn[relationship == "has_nickname", .(formal = toupper(name1), nick = toupper(name2))]
ox <- fread(file.path(S, "onyxrev.csv"), strip.white = TRUE)
setnames(ox, trimws(names(ox)))
onyx <- ox[, .(formal = toupper(trimws(name)), nick = toupper(trimws(nickname)))]
tabs <- list(synthid = synthid, carlton = carlton, onyx = onyx)

# two names are equivalent if one is the other's formal name or they share a formal root
equiv <- function(edges, a, b) {
  roots <- unique(rbind(edges[, .(name = nick, root = formal)], edges[, .(name = formal, root = formal)]))
  ra <- roots[data.table(name = a, i = seq_along(a)), on = "name", nomatch = 0L]
  rb <- roots[data.table(name = b, i = seq_along(b)), on = "name", nomatch = 0L]
  hit <- unique(merge(ra[, .(i, root)], rb[, .(i, root)], by = c("i", "root"))$i)
  seq_along(a) %in% hit
}
for (t in names(tabs)) set(r, j = paste0("in_", t), value = equiv(tabs[[t]], r$name_short, r$name_long))
r[, in_any := in_synthid | in_carlton | in_onyx]
r[, jw := 1 - stringdist(name_short, name_long, method = "jw", p = 0.1)]
r[, kind := fcase(in_any, "in a table",
                  startsWith(name_long, name_short), "prefix / truncation",
                  jw >= 0.9 | stringdist(name_short, name_long, method = "osa") <= 1, "spelling variant",
                  default = "other / unrelated")]

cov <- function(d) rbindlist(lapply(c("synthid", "carlton", "onyx", "any"), function(t) {
  h <- d[[paste0("in_", t)]]
  data.table(table = t, pairs_matched = sum(h), share_of_pairs = round(mean(h), 4),
             emp_ids_matched = sum(d$n_emp_ids[h]), share_of_emp_ids = round(sum(d$n_emp_ids[h]) / sum(d$n_emp_ids), 4))
}))
cat("Full-name pairs (no initials):", nrow(r), " EMP_ID pair rows:", sum(r$n_emp_ids), "\n\n")
cat("== Coverage, all pairs\n"); print(cov(r))
cat("\n== Coverage, pairs seen 25+ times (", r[n_emp_ids >= 25, .N], ")\n"); print(cov(r[n_emp_ids >= 25]))
cat("\n== Table sizes and how many of their pairs appear in the data\n")
seen <- rbind(r[, .(a = name_short, b = name_long)], r[, .(a = name_long, b = name_short)])
print(rbindlist(lapply(names(tabs), function(t) { e <- unique(tabs[[t]])
  hit <- e[seen, on = c(formal = "a", nick = "b"), nomatch = 0L]
  data.table(table = t, formal_names = uniqueN(e$formal), pairs = nrow(e), pairs_seen_in_data = uniqueN(hit)) })))
cat("\n== Pairs seen 25+ times, by kind\n")
print(r[n_emp_ids >= 25, .(pairs = .N, emp_ids = sum(n_emp_ids)), by = kind][order(-emp_ids)])
cat("\n== Pairs only in carlton or onyx (not synthid), top 25\n")
print(r[in_any & !in_synthid][order(-n_emp_ids)][1:25, .(name_short, name_long, n_emp_ids, in_carlton, in_onyx)])
cat("\n== Top 40 'other / unrelated' pairs in no table\n")
print(r[kind == "other / unrelated"][order(-n_emp_ids)][1:40, .(name_short, name_long, n_emp_ids, jw = round(jw, 2))])
cat("\n== Top 20 prefix pairs in no table\n")
print(r[kind == "prefix / truncation"][order(-n_emp_ids)][1:20, .(name_short, name_long, n_emp_ids)])
fwrite(r[order(-n_emp_ids)], file.path(OUTDIR, "first-name-variant-pairs-vs-tables.csv"))
cat("\nwrote", file.path(OUTDIR, "first-name-variant-pairs-vs-tables.csv"), "\n")
