#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# One-off: retro-fit PERSON_YEAR_ID + source keys + evaluation fields onto the
# EXISTING test slice (dev/data/linked_slice.rds), which predates the pipeline
# change in dev/01_preprocess_year.R. Reproduces exactly what a fresh
# preprocess -> link_panel run now emits, by collapsing the classified slice to
# one row per person-year (primary title) and joining it back to the linked
# slice on (ein, taxyr, name). Future runs get these columns natively; this
# script is only for the pre-change artifact.
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({library(dplyr)})
source("R/id.R")   # person_year_id()

classified <- readRDS("dev/data/classified_slice.rds")
linked     <- readRDS("dev/data/linked_slice.rds")

# Collapse classified (one row per title) to one row per person-year, keeping the
# primary title -- the same grain and slice_min policy as dev/01_preprocess_year.R.
person_year <- classified %>%
  transmute(
    ein, taxyr, dtk.name,
    OBJECTID, TABLE_ID,
    name.raw  = F9_07_COMP_DTK_NAME_PERS,
    title.raw,
    tot.hours, tot.comp,
    title.order = ifelse(is.na(suppressWarnings(as.numeric(title.order))), 1,
                         suppressWarnings(as.numeric(title.order)))
  ) %>%
  group_by(ein, taxyr, dtk.name) %>%
  slice_min(title.order, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(PERSON_YEAR_ID = person_year_id(OBJECTID, TABLE_ID)) %>%
  select(ein, taxyr, dtk.name, PERSON_YEAR_ID, OBJECTID, TABLE_ID,
         name.raw, title.raw, tot.hours, tot.comp)

# Join onto the linked slice by (ein, taxyr, name); name == dtk.name by construction.
enriched <- linked %>%
  left_join(person_year, by = c("ein", "taxyr", "name" = "dtk.name"))

# --- report -----------------------------------------------------------------
n <- nrow(enriched)
matched <- sum(!is.na(enriched$PERSON_YEAR_ID))
cat(sprintf("linked rows           : %d\n", n))
cat(sprintf("PERSON_YEAR_ID filled : %d (%.1f%%)\n", matched, 100 * matched / n))
cat(sprintf("PERSON_YEAR_ID unique : %s\n",
            length(unique(enriched$PERSON_YEAR_ID[!is.na(enriched$PERSON_YEAR_ID)])) == matched))
cat(sprintf("name.raw blank (institutional / no NAME_PERS): %d\n",
            sum(trimws(ifelse(is.na(enriched$name.raw), "", enriched$name.raw)) == "")))
cat("new columns:", paste(setdiff(names(enriched), names(linked)), collapse = ", "), "\n\n")

out <- "dev/data/linked_slice_enriched.rds"
saveRDS(enriched, out)
cat("wrote", out, "\n")
print(utils::head(enriched[, c("PERSON_YEAR_ID","EMP_ID","ein","taxyr","name",
                               "name.raw","title.standard","title.raw",
                               "tot.hours","tot.comp")], 5))
