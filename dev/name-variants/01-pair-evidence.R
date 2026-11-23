# Independent evidence for first-name variant pairs, national Part VII v3.1 build.
#
# For every pair of first names (short form first, ties alphabetical -- the
# same order as first-name-variant-pairs.csv) this counts how often the two
# names appear as SEPARATE people with the same surname on the same filing.
# Two forms of one person's name rarely sit side by side on one return; two
# different people (JOHN / JOSH SMITH, father and son) do. It also tallies the
# gender recorded with each first name, to flag pairs whose names are usually
# of different genders (MICHAEL / MICHELLE).
#
# Reads  SYNTHID_DIR/partvii-v3_1/people-by-ein/bucket=NN/part-0.parquet
# Writes SYNTHID_DIR/partvii-v3_1/name-variants/cofiling-pairs.csv
#        SYNTHID_DIR/partvii-v3_1/name-variants/first-name-gender.csv
suppressMessages({library(arrow); library(data.table)})
base <- file.path(Sys.getenv("SYNTHID_DIR", "C:/Users/jlecy/Documents/SYNTHID"), "partvii-v3_1")
out  <- file.path(base, "name-variants"); dir.create(out, showWarnings = FALSE)
files <- list.files(file.path(base, "people-by-ein"), pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)
co <- list(); gd <- list()
for (i in seq_along(files)) {
  x <- as.data.table(read_parquet(files[i], col_select = c("OBJECTID", "first_name", "last_name", "gender")))
  x[, `:=`(first_name = toupper(trimws(first_name)), last_name = toupper(trimws(last_name)))]
  x <- x[!is.na(first_name) & nchar(first_name) > 1 & !is.na(last_name) & nzchar(last_name)]
  gd[[i]] <- x[, .N, by = .(first_name, gender)]
  u <- unique(x[, .(OBJECTID, last_name, first_name)])
  u <- u[u[, .N, by = .(OBJECTID, last_name)][N > 1 & N <= 8], on = .(OBJECTID, last_name)]
  if (nrow(u)) {
    p <- u[u, on = .(OBJECTID, last_name), allow.cartesian = TRUE, nomatch = 0L][first_name < i.first_name]
    sw <- nchar(p$first_name) > nchar(p$i.first_name)
    p <- data.table(name_short = fifelse(sw, p$i.first_name, p$first_name),
                    name_long  = fifelse(sw, p$first_name, p$i.first_name))
    co[[i]] <- p[, .(n_filings_together = .N), by = .(name_short, name_long)]
  }
  if (i %% 10 == 0) cat(format(Sys.time(), "%H:%M:%S"), "bucket", i, "of", length(files), "\n")
}
co <- rbindlist(co)[, .(n_filings_together = sum(n_filings_together)), by = .(name_short, name_long)]
gd <- rbindlist(gd)[, .(N = sum(N)), by = .(first_name, gender)]
gd <- dcast(gd, first_name ~ gender, value.var = "N", fill = 0)
fwrite(co, file.path(out, "cofiling-pairs.csv"))
fwrite(gd, file.path(out, "first-name-gender.csv"))
cat("done:", nrow(co), "co-filing pairs;", nrow(gd), "first names\n")
