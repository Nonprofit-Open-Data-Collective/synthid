## Generate the link-review flags, the human review queue, and the peopleparser
## parse-fail log from an enriched linked slice, using the package API.
##
## Usage:  Rscript dev/generate_link_review.R [input.rds] [output_dir]
##
## Supersedes the exploratory dev/flag_link_review.R and
## dev/categorize_link_review.R (now folded into R/flag_links.R).

suppressMessages(pkgload::load_all(quiet = TRUE))

args   <- commandArgs(trailingOnly = TRUE)
infile <- if (length(args) >= 1) args[[1]] else "dev/data/linked_slice_enriched.rds"
outdir <- if (length(args) >= 2) args[[2]] else "dev/data"

linked <- readRDS(infile)

## 1. all flags, one row per multi-record person, sorted review-first
flags <- flag_links(linked)
print(flags)
saveRDS(flags, file.path(outdir, "link_review_flags.rds"))
utils::write.csv(as.data.frame(flags),
                 file.path(outdir, "link_review_flags.csv"), row.names = FALSE)

## 2. the residual human queue -- genuinely questionable links only
queue <- link_review_queue(flags)
utils::write.csv(queue, file.path(outdir, "link_review_queue.csv"), row.names = FALSE)
cat(sprintf("\nreview queue: %d cases -> %s\n",
            nrow(queue), file.path(outdir, "link_review_queue.csv")))

## 3. parse-fail log for peopleparser (input -> wrong parse -> expected)
pf <- parse_fail_log(linked,
                     path = file.path(outdir, "peopleparser_parse_fails.csv"))
cat(sprintf("parse-fail log: %d records -> %s\n",
            nrow(pf), file.path(outdir, "peopleparser_parse_fails.csv")))
cat("  by defect_type:\n"); print(table(pf$defect_type))

## 3a. STRUCTURED bucket -- candidate title/credential tokens, marked against the
## live peopleparser list when that package is on the search path.
known <- tryCatch(toupper(peopleparser::known_titles()$abbr),
                  error = function(e) character())
if (!length(known)) message("  (peopleparser not installed; in_known left FALSE)")
toks <- parse_fail_tokens(pf, known = known)
utils::write.csv(toks, file.path(outdir, "peopleparser_token_candidates.csv"),
                 row.names = FALSE)
cat(sprintf("token candidates: %d (%d novel) -> %s\n",
            nrow(toks), sum(!toks$in_known),
            file.path(outdir, "peopleparser_token_candidates.csv")))
print(toks[!toks$in_known, c("token", "field", "n_records", "example")], row.names = FALSE)

## 3b. EYEBALL bucket -- suspect name-variant sets, one row per person, for the
## defect-adjacent categories where a parser error may hide in the variance.
eye_cats <- c("parser_honorific_glue", "surname_credential",
              "surname_compound", "review")
eye <- as.data.frame(flags)[!is.na(flags$category) & flags$category %in% eye_cats,
  c("emp_id", "ein", "org_name", "category", "names_seen", "first_seen", "last_seen")]
utils::write.csv(eye, file.path(outdir, "peopleparser_suspect_variants.csv"),
                 row.names = FALSE)
cat(sprintf("suspect variants: %d persons -> %s\n",
            nrow(eye), file.path(outdir, "peopleparser_suspect_variants.csv")))
