#!/usr/bin/env Rscript
# Memoized-only timing gated against the saved single-thread baseline (EMP_IDs
# from dev/data/linked_full_panel_100eins.rds, baseline wall-clock 767.9s this
# session / 754.8s pilot). Confirms byte-identical clustering + reports speedup.
suppressMessages(devtools::load_all(".", quiet=TRUE))
BASE_S <- 767.9
ls <- readRDS("dev/data/linked_full_panel_100eins.rds")
df <- ls[, setdiff(names(ls), c("EMP_ID","EMP_N_RECORDS","EMP_N_YEARS"))]
base_emp <- ls$EMP_ID
N <- length(unique(df$ein))
options(synthid.memoize_comparators=TRUE)
t0 <- Sys.time(); m <- synthid::link_panel(df)
ms <- as.numeric(Sys.time()-t0, units="secs")
options(synthid.memoize_comparators=FALSE)
ok <- identical(as.character(base_emp), m$EMP_ID)
cat(sprintf("=== MEMOIZED-ONLY, %d EINs x 15yr, %d person-years ===\n", N, nrow(df)))
cat(sprintf("[gate] identical EMP_ID vs saved baseline: %s  (persons=%d)\n", ok, length(unique(m$EMP_ID))))
cat(sprintf("[base]     link (single-thread) : %8.1fs   (%.1f ms/EIN)\n", BASE_S, 1000*BASE_S/N))
cat(sprintf("[memoized] link (single-thread) : %8.1fs   (%.1f ms/EIN)\n", ms, 1000*ms/N))
cat(sprintf("=== SPEEDUP: %.2fx  (%.1f%% faster) ===\n", BASE_S/ms, 100*(1-ms/BASE_S)))
cat(sprintf("Single-thread over 24,875 consistent EINs: %.1f h -> %.1f h\n", BASE_S/N*24875/3600, ms/N*24875/3600))
cat(sprintf("With 7 workers (~4.5x): ~%.1f h -> ~%.1f h\n", BASE_S/N*24875/3600/4.5, ms/N*24875/3600/4.5))
