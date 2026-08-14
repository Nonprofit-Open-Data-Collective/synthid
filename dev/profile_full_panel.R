#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Runtime profile: run the synthid detector on FULL 15-year (2010-2024) EIN
# panels and time each pipeline stage. Uses the cached raw subset built by
# dev/build_raw_subset.R (1,000 full-panel-consistent EINs across all 15 years).
#
# Usage: Rscript dev/profile_full_panel.R [N_EINS]
#   N_EINS: how many of the cached EINs to profile (default: all cached).
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(titleclassifier)
  library(peopleparser); library(googlesheets4)
  devtools::load_all(".", quiet = TRUE)   # synthid from source (not installed)
})

args <- commandArgs(trailingOnly = TRUE)
cache <- readRDS("dev/data/raw_subset_1000_15yr.rds")
eins  <- cache$eins
if (length(args) >= 1) eins <- head(eins, as.integer(args[[1]]))
raw   <- cache$raw[cache$raw$EIN2 %in% eins, , drop = FALSE]
N     <- length(eins)
yrs   <- sort(unique(raw$TAX_YEAR))

tf <- function(expr) { t0 <- Sys.time(); v <- force(expr)
  attr(v, "secs") <- as.numeric(Sys.time() - t0, units = "secs"); v }
secs <- function(v) attr(v, "secs")

cat(sprintf("=== PROFILE: %d EINs, years %d-%d (%d yrs), %d raw rows ===\n",
            N, min(yrs), max(yrs), length(yrs), nrow(raw)))

# --- Stage 1: classify titles ----------------------------------------------
gs4_deauth()
classified <- tf(titleclassifier::classify_titles(raw, preserve_input = TRUE))
cat(sprintf("[1] classify_titles : %7.1fs  (%d -> %d rows)\n",
            secs(classified), nrow(raw), nrow(classified)))

# --- Stage 2: collapse to one row per person-year --------------------------
keep_cols <- c("ein","taxyr","dtk.name","org.name","title.standard","title.order",
               "OBJECTID","TABLE_ID","F9_07_COMP_DTK_NAME_PERS","title.raw",
               "tot.hours","tot.comp")
keep_cols <- intersect(keep_cols, names(as.data.frame(classified)))
person_year <- tf({
  as.data.frame(classified)[, keep_cols] %>%
    mutate(title.order = suppressWarnings(as.numeric(.data$title.order)),
           title.order = ifelse(is.na(.data$title.order), 1, .data$title.order)) %>%
    group_by(.data$ein, .data$taxyr, .data$dtk.name) %>%
    slice_min(.data$title.order, n = 1, with_ties = FALSE) %>% ungroup()
})
cat(sprintf("[2] collapse py     : %7.1fs  (%d -> %d person-years)\n",
            secs(person_year), nrow(classified), nrow(person_year)))

# --- Stage 3: parse names --------------------------------------------------
parsed <- tf(peopleparser::parse_names(person_year$dtk.name))
cat(sprintf("[3] parse_names     : %7.1fs\n", secs(parsed)))

# --- assemble synthid input ------------------------------------------------
df <- data.frame(
  PERSON_YEAR_ID = synthid::person_year_id(person_year$OBJECTID, person_year$TABLE_ID),
  ein = as.character(person_year$ein), taxyr = as.integer(person_year$taxyr),
  org.name = person_year$org.name, name = parsed$name,
  salutation = parsed$salutation, first_name = parsed$first_name,
  middle_name = parsed$middle_name, last_name = parsed$last_name,
  suffix = parsed$suffix, gender = parsed$gender,
  title.standard = person_year$title.standard, stringsAsFactors = FALSE)

# --- Stage 4: link_panel (the choose(Y,2) scaling target) ------------------
linked <- tf(synthid::link_panel(df, verbose = FALSE))
cat(sprintf("[4] link_panel      : %7.1fs\n", secs(linked)))

# --- summary ---------------------------------------------------------------
n_rec  <- nrow(linked)
n_pers <- length(unique(linked$EMP_ID))
multi  <- linked %>% group_by(.data$EMP_ID) %>%
  summarise(nyr = n_distinct(.data$taxyr), .groups = "drop")
n_multi <- sum(multi$nyr > 1)
t_total <- secs(classified) + secs(person_year) + secs(parsed) + secs(linked)

cat("\n=== SUMMARY ===\n")
cat(sprintf("EINs profiled          : %d\n", N))
cat(sprintf("raw rows               : %d\n", nrow(raw)))
cat(sprintf("person-year records    : %d\n", n_rec))
cat(sprintf("distinct persons       : %d  (%.1f%% compression)\n",
            n_pers, 100 * (1 - n_pers / n_rec)))
cat(sprintf("multi-year persons     : %d\n", n_multi))
cat(sprintf("\nStage timings (s): classify=%.1f  collapse=%.1f  parse=%.1f  link=%.1f  TOTAL=%.1f\n",
            secs(classified), secs(person_year), secs(parsed), secs(linked), t_total))
cat(sprintf("Per-EIN            : link=%.1f ms   total=%.1f ms\n",
            1000 * secs(linked) / N, 1000 * t_total / N))
cat(sprintf("Extrapolate to all %d consistent EINs (linear):  link=%.1f min   total=%.1f min\n",
            24875, secs(linked) / N * 24875 / 60, t_total / N * 24875 / 60))

saveRDS(linked, sprintf("dev/data/linked_full_panel_%deins.rds", N))
cat(sprintf("\nSaved linked -> dev/data/linked_full_panel_%deins.rds\n", N))
