pkg <- "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/00-GITHUB/synthid"
suppressMessages(devtools::load_all(pkg, quiet=TRUE)); suppressMessages(library(readr))
d <- read_csv("C:/Users/jdlec/Dropbox (Personal)/00 - DATA/COMPDATA/PANEL-2019-2021-W-NAMES-TITLES-AND-NAMES.CSV", show_col_types=FALSE, guess_max=20000)
cmp <- candidate_comparisons(d)
saveRDS(cmp, file.path(pkg,"dev/comparisons.rds"))
cat("saved comparisons:", nrow(cmp), "cols:", paste(names(cmp),collapse=","), "\n")
