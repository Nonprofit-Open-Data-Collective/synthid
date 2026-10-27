# 06i-variant-test.R
# How many cross-org matches depend on the name variants a person shows inside
# their own organization? Profiles already pool every surname, first name,
# middle initial and suffix seen for a within-org EMP_ID, block on a key per
# surname variant, and score the best match across the sets. This rebuilds
# each profile from one form only -- the modal surname, the most formal first
# name, one middle initial, one suffix -- and re-runs blocking and scoring on
# 06b's pairs (scoring >= 5 with the pooled profiles).
#
# A match that the single-form profile loses is a false negative that pooling
# avoids. Reasons: the pair no longer shares a surname key (blocking), or the
# score falls below 7 (scoring); for the latter, which pooled set made the
# difference.
#
# Writes dev/partvii-panel/csa-test-<CSA>/variant-test.csv (counts); examples
# with names to the data folder (csa=<CSA>/review/variant-test-examples.csv).
#
#   Rscript dev/partvii-panel/06i-variant-test.R [csa=450]

source("dev/partvii-panel/_panel.R")
source("dev/partvii-panel/_hard_cases.R")
build_step("06i-variant-test")
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
TH <- 7
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
prof <- readRDS(file.path(d, "profiles.rds"))$prof
sw <- readRDS(file.path(pv_root, "xorg-csa", "prep", "surname-weight.rds"))
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa")))
U <- cand[csa == TRUE & !is.na(score) & score >= 5, .(emp_a, emp_b, score)]
fmt <- function(v) trimws(format(v, big.mark = ","))

# single-form profiles
one <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (length(v)) v[1] else character(0) }
red <- prof[prof$EMP_ID %in% c(U$emp_a, U$emp_b), c("EMP_ID", "last_name", "first_name", "gender",
             "last_name_variants", "first_name_variants", "middle_initials", "suffixes", "name_variants")]
red$last_name_variants  <- lapply(red$last_name, function(x) if (is.na(x) || !nzchar(x)) character(0) else x)
red$first_name_variants <- lapply(red$first_name, function(x) if (is.na(x) || !nzchar(x)) character(0) else x)
red$middle_initials     <- lapply(red$middle_initials, one)
red$suffixes            <- lapply(red$suffixes, one)

t0 <- Sys.time()
s1 <- as.data.table(synthid::score_candidate_pairs(as.data.frame(U[, .(emp_a, emp_b)]), red, surname_weight = sw))
U[s1, on = c("emp_a", "emp_b"), score_single := i.score]
stamp("rescored %s pairs with single-form profiles in %.1f min", fmt(nrow(U)), as.numeric(Sys.time() - t0, units = "mins"))

# blocking: do the two single-form surnames still share a key?
keys1 <- setNames(lapply(red$last_name, function(x) if (is.na(x) || !nzchar(x)) character(0) else synthid::surname_blocking_keys(x)), red$EMP_ID)
U[, blocked_single := !mapply(function(a, b) length(intersect(keys1[[a]], keys1[[b]])) > 0, emp_a, emp_b)]

# which pooled set mattered, for each pair
pv <- as.data.table(prof)[EMP_ID %in% c(U$emp_a, U$emp_b), .(EMP_ID, nl = lengths(last_name_variants), nf = lengths(first_name_variants),
                                                            nm = lengths(middle_initials))]
setkey(pv, EMP_ID)
U[, `:=`(multi_surname = pv[emp_a, nl] > 1 | pv[emp_b, nl] > 1, multi_first = pv[emp_a, nf] > 1 | pv[emp_b, nf] > 1,
         multi_middle = pv[emp_a, nm] > 1 | pv[emp_b, nm] > 1)]
M <- U[score >= TH]
M[, outcome := fcase(blocked_single, "lost: no shared surname key (blocking)",
                     is.na(score_single) | score_single < TH, "lost: score falls below 7",
                     default = "kept")]
res <- M[, .N, by = outcome][, share := round(N / sum(N), 4)]
gain <- U[score < TH & !blocked_single & score_single >= TH, .N]
why <- M[outcome != "kept", .(pairs = .N), by = .(multi_surname, multi_first, multi_middle)][order(-pairs)]
print(res); cat(sprintf("\nPairs below 7 with pooled profiles but 7+ with single forms: %s\n", fmt(gain)))
cat("\nLost matches by which pooled sets the two people have (more than one form):\n"); print(why)
fwrite(rbind(res[, .(outcome, pairs = N, share)], data.table(outcome = "gained with single forms (pooled < 7, single >= 7)", pairs = gain, share = NA)),
       file.path(fig, "variant-test.csv"))
fwrite(why, file.path(fig, "variant-test-why.csv"))

# examples, with names: data folder only
nm_of <- setNames(vapply(prof$name_variants, function(v) paste(unique(v), collapse = " / "), ""), prof$EMP_ID)
set.seed(11)
ex <- M[outcome != "kept"][sample(.N, min(40, .N))][, .(outcome, score = round(score, 2), score_single = round(score_single, 2),
                                                         forms_a = nm_of[emp_a], forms_b = nm_of[emp_b])]
fwrite(ex, file.path(d, "review", "variant-test-examples.csv"))
onames <- as.data.table(read_parquet(file.path(pv_root, "orgs", "orgs.parquet"), col_select = c("ein", "org.name")))
onames <- setNames(onames$org.name, onames$ein)
write_hard_cases(M[outcome != "kept", .(emp_a, emp_b, score, detail = sprintf("%s; single-form score %.2f", outcome, score_single))],
                 "pooled_variant_only", "Matched only through a name variant pooled within an organization",
                 as.data.table(prof), onames, CSA)
print(head(ex, 15))
