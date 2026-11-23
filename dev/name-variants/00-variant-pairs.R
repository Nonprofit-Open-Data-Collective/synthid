# Every pair of first-name forms one within-org person (EMP_ID) carries,
# national Part VII v3.1 build. Pairs are written short form first (ties
# alphabetical); a person with three forms gives three pairs. Counted by EMP_ID.
# Reads SYNTHID_DIR/partvii-v3_1/profiles; writes first-name-variant-pairs.csv there.
base <- file.path(Sys.getenv("SYNTHID_DIR", "C:/Users/jlecy/Documents/SYNTHID"), "partvii-v3_1")
suppressMessages({library(arrow); library(dplyr); library(data.table)})
d <- open_dataset(file.path(base, "profiles"))
p <- d |> select(EMP_ID, n_records, first_name_variants) |> collect() |> as.data.table()
cat("EMP_IDs:", nrow(p), "\n")
long <- p[, .(v = unlist(first_name_variants)), by = .(EMP_ID, n_records)]
long <- unique(long[!is.na(v) & nzchar(trimws(v))][, v := toupper(trimws(v))])
nv <- long[, .N, by = EMP_ID]
cat("EMP_IDs with any first name:", nrow(nv), "\n")
print(nv[, .(emp_ids = .N), keyby = .(n_variants = pmin(N, 6L))])
multi <- long[EMP_ID %in% nv[N > 1, EMP_ID]]
setorder(multi, EMP_ID, v)
# all pairs within an EMP_ID; order each pair short first (ties alphabetical)
pairs <- multi[, { if (.N > 1) { cb <- combn(v, 2); .(a = cb[1, ], b = cb[2, ]) } }, by = EMP_ID]
sw <- nchar(pairs$a) > nchar(pairs$b)
pairs[, `:=`(name_short = fifelse(sw, b, a), name_long = fifelse(sw, a, b))]
res <- pairs[, .(n_emp_ids = .N), by = .(name_short, name_long)]
res[, `:=`(len_short = nchar(name_short), len_long = nchar(name_long),
           short_is_initial = nchar(name_short) == 1L,
           long_starts_with_short = startsWith(name_long, name_short))]
setorder(res, -n_emp_ids)
cat("pair rows:", nrow(pairs), " distinct pairs:", nrow(res), "\n")
cat("pairs where short side is a single initial:", sum(res[short_is_initial == TRUE, n_emp_ids]), "\n")
out <- file.path(base, "first-name-variant-pairs.csv")
fwrite(res, out); cat("wrote", out, "\n")
cat("\nTop 40 overall:\n"); print(res[1:40])
cat("\nTop 40 excluding single-letter initials:\n"); print(res[short_is_initial == FALSE][1:40])
