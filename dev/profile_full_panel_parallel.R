#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Full 15-year (2010-2024) panel runtime profile via the PRODUCTION parallel
# path: preprocess once (classify+collapse+parse), then fan link_panel across
# EIN chunks with furrr multisession (reclin2 single-threaded per worker).
#
# Usage: Rscript dev/profile_full_panel_parallel.R [N_EINS] [N_WORKERS]
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(titleclassifier)
  library(peopleparser); library(googlesheets4); library(future); library(furrr)
})
devtools::load_all(".", quiet = TRUE)

args      <- commandArgs(trailingOnly = TRUE)
cache     <- readRDS("dev/data/raw_subset_1000_15yr.rds")
eins      <- cache$eins
if (length(args) >= 1 && nzchar(args[[1]])) eins <- head(eins, as.integer(args[[1]]))
N_WORKERS <- if (length(args) >= 2) as.integer(args[[2]]) else 7L
raw       <- cache$raw[cache$raw$EIN2 %in% eins, , drop = FALSE]
N         <- length(eins)
PKG_DIR   <- normalizePath(".", winslash = "/")

tf <- function(expr){t0<-Sys.time(); v<-force(expr); attr(v,"secs")<-as.numeric(Sys.time()-t0,units="secs"); v}
secs <- function(v) attr(v, "secs")
cat(sprintf("=== PARALLEL PROFILE: %d EINs x 15yr, %d raw rows, %d workers ===\n",
            N, nrow(raw), N_WORKERS))

# --- preprocess (classify + collapse + parse), timed as one block ----------
gs4_deauth()
classified <- tf(titleclassifier::classify_titles(raw, preserve_input = TRUE))
cat(sprintf("[1] classify_titles : %7.1fs  (%d -> %d rows)\n", secs(classified), nrow(raw), nrow(classified)))
keep <- intersect(c("ein","taxyr","dtk.name","org.name","title.standard","title.order",
                    "OBJECTID","TABLE_ID","F9_07_COMP_DTK_NAME_PERS","title.raw","tot.hours","tot.comp"),
                  names(as.data.frame(classified)))
person_year <- tf(as.data.frame(classified)[,keep] %>%
  mutate(title.order = suppressWarnings(as.numeric(.data$title.order)),
         title.order = ifelse(is.na(.data$title.order),1,.data$title.order)) %>%
  group_by(.data$ein,.data$taxyr,.data$dtk.name) %>%
  slice_min(.data$title.order,n=1,with_ties=FALSE) %>% ungroup())
cat(sprintf("[2] collapse py     : %7.1fs  (-> %d person-years)\n", secs(person_year), nrow(person_year)))
parsed <- tf(peopleparser::parse_names(person_year$dtk.name))
cat(sprintf("[3] parse_names     : %7.1fs\n", secs(parsed)))

df <- data.frame(
  PERSON_YEAR_ID = synthid::person_year_id(person_year$OBJECTID, person_year$TABLE_ID),
  ein=as.character(person_year$ein), taxyr=as.integer(person_year$taxyr),
  org.name=person_year$org.name, name=parsed$name, salutation=parsed$salutation,
  first_name=parsed$first_name, middle_name=parsed$middle_name, last_name=parsed$last_name,
  suffix=parsed$suffix, gender=parsed$gender, title.standard=person_year$title.standard,
  stringsAsFactors=FALSE)

# --- parallel linkage: split distinct EINs into N_WORKERS chunks -----------
ueins  <- unique(df$ein)
chunk  <- (seq_along(ueins)-1) %% N_WORKERS + 1
chunks <- split(ueins, chunk)
plan(multisession, workers = N_WORKERS)
link_chunk <- function(chunk_eins){
  Sys.setenv(OMP_NUM_THREADS="1"); try(data.table::setDTthreads(1L), silent=TRUE)
  suppressMessages(devtools::load_all(PKG_DIR, quiet=TRUE))
  sub <- df[df$ein %in% chunk_eins,,drop=FALSE]
  lk  <- synthid::link_panel(sub)
  data.frame(ein=lk$ein, taxyr=lk$taxyr, EMP_ID=lk$EMP_ID, stringsAsFactors=FALSE)
}
linked <- tf(future_map_dfr(chunks, link_chunk,
  .options=furrr_options(seed=TRUE, globals=c("df","PKG_DIR"), packages=c("devtools"))))
plan(sequential)
cat(sprintf("[4] link_panel (par): %7.1fs  (%d workers)\n", secs(linked), N_WORKERS))

# --- summary ---------------------------------------------------------------
n_rec<-nrow(linked); n_pers<-length(unique(linked$EMP_ID))
multi<-linked %>% group_by(.data$EMP_ID) %>% summarise(nyr=n_distinct(.data$taxyr),.groups="drop")
t_pre <- secs(classified)+secs(person_year)+secs(parsed)
t_all <- t_pre + secs(linked)
cat("\n=== SUMMARY (parallel) ===\n")
cat(sprintf("EINs                   : %d   workers: %d\n", N, N_WORKERS))
cat(sprintf("raw rows               : %d\n", nrow(raw)))
cat(sprintf("person-year records    : %d\n", n_rec))
cat(sprintf("distinct persons       : %d  (%.1f%% compression)\n", n_pers, 100*(1-n_pers/n_rec)))
cat(sprintf("multi-year persons     : %d\n", sum(multi$nyr>1)))
cat(sprintf("\nTimings(s): classify=%.1f collapse=%.1f parse=%.1f link_par=%.1f | preproc=%.1f TOTAL=%.1f (%.1f min)\n",
            secs(classified),secs(person_year),secs(parsed),secs(linked),t_pre,t_all,t_all/60))
cat(sprintf("Per-EIN wall-clock     : link=%.1f ms  total=%.1f ms\n", 1000*secs(linked)/N, 1000*t_all/N))
cat(sprintf("Extrapolate to all 24,875 consistent EINs @ %d workers: link=%.1f min  total=%.1f min\n",
            N_WORKERS, secs(linked)/N*24875/60, t_all/N*24875/60))
saveRDS(linked, sprintf("dev/data/linked_full_panel_par_%deins.rds", N))
cat(sprintf("Saved -> dev/data/linked_full_panel_par_%deins.rds\n", N))
