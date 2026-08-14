## Example: assign stable EMP_IDs to the 2019-2021 development panel.
## Run from the package root, e.g.
##   Rscript scripts/run_panel_2019_2021.R
suppressMessages({
  library(readr)
  library(synthid) # or devtools::load_all(".")
})

infile <- Sys.getenv(
  "SYNTHID_PANEL",
  unset = "C:/Users/jdlec/Dropbox (Personal)/00 - DATA/COMPDATA/PANEL-2019-2021-W-NAMES-TITLES-AND-NAMES.CSV"
)

panel <- read_csv(infile, show_col_types = FALSE, guess_max = 20000)

linked <- link_panel(panel, threshold = 5, verbose = TRUE)
synthid_report(linked)

out <- sub("\\.csv$", "-WITH-EMP-ID.csv", infile, ignore.case = TRUE)
write_csv(linked, out)
message("Wrote: ", out)
