# 00-cbsa.R
# ZIP -> CBSA crosswalk, and the per-filing organization CBSA the linker needs.
#
# The metro pass of synthid::blocking_passes() keys on a CBSA, which crosses a
# state line by construction, so a two-state metro interlock (New York-Newark,
# Kansas City, Philadelphia-Camden, Washington) blocks at a tight pass instead
# of falling through to the surname pass and being dropped for exceeding
# max_block_size. Nothing in the panel carries a CBSA until this runs.
#
# Writes SYNTHID_DIR/cbsa/:
#   zip-cbsa.parquet       zip, cbsa, share     one row per (zip, cbsa) pair
#   objectid-cbsa.parquet  OBJECTID, org.cbsa   the filing's CBSA, for 02-people.R
#   build-info-cbsa.csv    route, vintage, counts, date
#
# zip-cbsa.parquet keeps every (zip, cbsa) pair above MIN_SHARE of the ZIP, so a
# straddling ZIP is recorded in full. objectid-cbsa.parquet is one row per
# filing, though, because `org.cbsa` is a scalar column on the people rows and
# one row per person per filing is what the linker asserts -- so a straddling
# ZIP contributes its largest-share CBSA there. build_person_profile() still
# builds a *set* per person, unioned over their filings, so someone who files
# from two metros gets both keys; what is not yet reachable is both CBSAs of a
# single straddling ZIP. That would need build_person_profile() to accept a
# delimited or list column, and is left as a follow-up: zip-cbsa.parquet already
# holds the data for it.
#
# A ZIP in no CBSA simply gets no row, the metro pass emits nothing for that
# organization, and it still blocks on strict / geo / industry / surname.
#
# One delineation vintage is used for all 15 tax years. The delineations changed
# in 2013, 2018 and 2023; blocking needs keys that *collide*, not keys that are
# historically right for each year, and mixing vintages would give one
# organization different CBSA codes in different years and split its bucket.
#
#   HUD route (one hop, needs a free token in HUD_API_TOKEN):
#     Rscript dev/partvii-panel/00-cbsa.R hud
#   County route (no account; point the two env vars at downloaded CSVs):
#     ZIP_COUNTY_FILE=... ZIP_COUNTY_COLS=zcta,county,share \
#     COUNTY_CBSA_FILE=... COUNTY_CBSA_COLS=fipscounty,cbsacode \
#     Rscript dev/partvii-panel/00-cbsa.R county
#
# HUD: type 3 is zip-cbsa. Use it rather than type 8 (cbsa-zip) -- HUD warns the
# two directions are not inverses. Files are quarterly, and are on 2020 Census
# geographies from the 2023 Q1 release. Token: https://huduser.gov/portal/dataset/uspszip-api.html
# County route sources: Census ZCTA-to-county relationship file, and the OMB
# Bulletin 23-01 delineation (NBER mirrors it as CSV).

source("dev/partvii-panel/_panel.R")
args  <- commandArgs(trailingOnly = TRUE)
ROUTE <- if (length(args) >= 1) args[1] else "hud"
stopifnot(ROUTE %in% c("hud", "county"))

MIN_SHARE  <- 0.05        # drop a (zip, cbsa) pair holding less of the ZIP than this
DROP_CBSA  <- "99999"     # HUD's non-CBSA sentinel -- confirm against the download
HUD_TYPE   <- 3L          # zip-cbsa
HUD_BASE   <- "https://www.huduser.gov/hudapi/public/usps"
# Queried one state at a time: that is the form HUD documents, and each response
# stays small. query=all may work in a single call; it is not relied on here.
HUD_QUERY  <- c(state.abb, "DC", "PR", "VI", "GU", "AS", "MP")

cb_dir <- file.path(sd_root, "cbsa")
dir.create(cb_dir, recursive = TRUE, showWarnings = FALSE)

pad5 <- function(v) {
  d <- gsub("[^0-9]", "", as.character(v))
  ifelse(nzchar(d), sprintf("%05d", suppressWarnings(as.integer(d))), NA_character_)
}

# ---- route 1: HUD USPS crosswalk --------------------------------------------------
fetch_hud <- function() {
  token <- Sys.getenv("HUD_API_TOKEN")
  if (!nzchar(token)) {
    stop("00-cbsa.R: set HUD_API_TOKEN (free account at ",
         "https://huduser.gov/portal/dataset/uspszip-api.html), or run the county route.",
         call. = FALSE)
  }
  one <- function(q) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, Authorization = paste("Bearer", token))
    r <- curl::curl_fetch_memory(sprintf("%s?type=%d&query=%s", HUD_BASE, HUD_TYPE, q), handle = h)
    if (r$status_code != 200L) {
      stop(sprintf("00-cbsa.R: HUD returned %d for query=%s: %s", r$status_code, q,
                   substr(rawToChar(r$content), 1, 300)), call. = FALSE)
    }
    j <- jsonlite::fromJSON(rawToChar(r$content), simplifyDataFrame = TRUE)
    res <- j$data$results
    if (is.null(res) || !nrow(res)) return(NULL)
    d <- as.data.table(res)
    setnames(d, tolower(names(d)))
    d[, query := q]
    d
  }
  out <- rbindlist(lapply(HUD_QUERY, function(q) {
    d <- tryCatch(one(q), error = function(e) { stamp("  %s: %s", q, conditionMessage(e)); NULL })
    stamp("  %s: %s rows", q, if (is.null(d)) "0" else format(nrow(d), big.mark = ","))
    d
  }), fill = TRUE)
  if (!nrow(out)) stop("00-cbsa.R: HUD returned no rows at all", call. = FALSE)

  miss <- setdiff(c("zip", "cbsa"), names(out))
  if (length(miss)) {
    stop("00-cbsa.R: HUD response has no ", paste(miss, collapse = "/"),
         " column. Columns: ", paste(names(out), collapse = ", "), call. = FALSE)
  }
  # tot_ratio is the share of the ZIP's addresses in that CBSA. If the column is
  # named differently in a future release, every pair is kept instead.
  share_col <- intersect(c("tot_ratio", "totratio", "tot_ratio_"), names(out))[1]
  if (is.na(share_col)) {
    warning("00-cbsa.R: no tot_ratio column; keeping every (zip, cbsa) pair. ",
            "Columns: ", paste(names(out), collapse = ", "), call. = FALSE)
    out[, share := 1]
  } else {
    out[, share := suppressWarnings(as.numeric(get(share_col)))]
  }
  list(x = out[, .(zip, cbsa, share)],
       vintage = paste("HUD USPS zip-cbsa (type 3), fetched", format(Sys.Date())))
}

# ---- route 2: ZIP -> county -> CBSA ----------------------------------------------
# No guessing at column names: each file's columns are named explicitly, so a
# changed header fails with a message instead of silently mapping the wrong field.
read_named <- function(path_env, cols_env, n_expect) {
  path <- Sys.getenv(path_env)
  cols <- strsplit(Sys.getenv(cols_env), ",", fixed = TRUE)[[1]]
  if (!nzchar(path) || !file.exists(path)) {
    stop("00-cbsa.R: set ", path_env, " to an existing file", call. = FALSE)
  }
  if (length(cols) != n_expect) {
    stop("00-cbsa.R: ", cols_env, " needs ", n_expect, " comma-separated column names",
         call. = FALSE)
  }
  d <- fread(path, colClasses = "character")
  miss <- setdiff(cols, names(d))
  if (length(miss)) {
    stop("00-cbsa.R: ", path, " has no column(s) ", paste(miss, collapse = ", "),
         ". Columns: ", paste(names(d), collapse = ", "), call. = FALSE)
  }
  d[, ..cols]
}

fetch_county <- function() {
  zc <- read_named("ZIP_COUNTY_FILE", "ZIP_COUNTY_COLS", 3L)   # zip, county, share
  setnames(zc, c("zip", "county", "share"))
  cc <- read_named("COUNTY_CBSA_FILE", "COUNTY_CBSA_COLS", 2L) # county, cbsa
  setnames(cc, c("county", "cbsa"))
  zc[, county := sprintf("%05d", suppressWarnings(as.integer(gsub("[^0-9]", "", county))))]
  cc[, county := sprintf("%05d", suppressWarnings(as.integer(gsub("[^0-9]", "", county))))]
  cc <- unique(cc[!is.na(county)], by = c("county", "cbsa"))
  x <- merge(zc, cc, by = "county", allow.cartesian = TRUE)
  stamp("ZIP-county %s rows, county-CBSA %s counties -> %s pairs",
        format(nrow(zc), big.mark = ","), format(nrow(cc), big.mark = ","),
        format(nrow(x), big.mark = ","))
  # a ZIP can reach one CBSA through several counties: add those shares up
  x[, share := suppressWarnings(as.numeric(share))]
  x <- x[, .(share = sum(share, na.rm = TRUE)), by = .(zip, cbsa)]
  list(x = x, vintage = sprintf("%s + %s, read %s", basename(Sys.getenv("ZIP_COUNTY_FILE")),
                                basename(Sys.getenv("COUNTY_CBSA_FILE")), format(Sys.Date())))
}

# ---- the two pure transforms ------------------------------------------------------
# Kept as functions so they can be exercised without the API or the panel:
# CBSA_DRY_RUN=1 sources this file for its definitions and runs nothing.

# Pad, drop the unusable, collapse to one row per pair, cut the small shares.
normalize_pairs <- function(x, min_share = MIN_SHARE, drop_cbsa = DROP_CBSA) {
  x <- as.data.table(x)[, .(zip, cbsa, share)]
  x[, `:=`(zip = pad5(zip), cbsa = pad5(cbsa), share = suppressWarnings(as.numeric(share)))]
  x <- x[!is.na(zip) & !is.na(cbsa) & !(cbsa %in% drop_cbsa)]
  x[is.na(share), share := 0]
  # one row per (zip, cbsa): keep the largest share if the source repeats a pair
  setorder(x, zip, cbsa, -share)
  x <- unique(x, by = c("zip", "cbsa"))
  x <- x[share >= min_share]
  setorder(x, zip, -share)
  x[]
}

# One CBSA per filing: the largest share, ties broken by the lower code so the
# column is deterministic across runs.
filing_cbsa <- function(hdr, xw) {
  oc <- merge(as.data.table(hdr), as.data.table(xw)[, .(zip, cbsa, share)],
              by = "zip", allow.cartesian = TRUE)
  setorder(oc, OBJECTID, -share, cbsa)
  oc <- unique(oc, by = "OBJECTID")[, .(OBJECTID, org.cbsa = cbsa)]
  setorder(oc, OBJECTID)
  oc[]
}

# Per filing, not per organization, so it follows an organization that moves --
# the same reason the state comes from the filing header rather than the BMF.
header_zips <- function(years = YEARS) {
  h <- rbindlist(lapply(years, function(y) {
    d <- as.data.table(read_parquet(eff_path("F9-P00-T00-HEADER", y)))
    zipc <- grep("ORG_ADDR.*ZIP|ADDR.*ZIP|^ZIP", names(d), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(zipc)) {
      stop("00-cbsa.R: no ZIP column in the ", y, " header. Address columns: ",
           paste(grep("ADDR", names(d), ignore.case = TRUE, value = TRUE), collapse = ", "),
           call. = FALSE)
    }
    if (y == years[1]) stamp("header ZIP column: %s", zipc)
    # ZIP+4 and formatting noise: the first five digits are the ZIP
    d[, .(OBJECTID = as.character(OBJECTID),
          zip = pad5(substr(gsub("[^0-9]", "", get(zipc)), 1, 5)))]
  }), fill = TRUE)
  unique(h[!is.na(zip)], by = "OBJECTID")
}

# ---- run ---------------------------------------------------------------------------
if (!nzchar(Sys.getenv("CBSA_DRY_RUN"))) {
  stamp("route: %s", ROUTE)
  got <- if (ROUTE == "hud") fetch_hud() else fetch_county()
  n_raw <- nrow(got$x)
  xw <- normalize_pairs(got$x)

  multi <- xw[, .N, by = zip][N > 1L]
  stamp("%s source rows -> %s pairs at share >= %.2f", format(n_raw, big.mark = ","),
        format(nrow(xw), big.mark = ","), MIN_SHARE)
  stamp("%s ZIPs, %s CBSAs; %s ZIPs (%.1f%%) span more than one CBSA",
        format(uniqueN(xw$zip), big.mark = ","), format(uniqueN(xw$cbsa), big.mark = ","),
        format(nrow(multi), big.mark = ","), 100 * nrow(multi) / max(1L, uniqueN(xw$zip)))
  stopifnot(nrow(xw) > 0, !anyDuplicated(xw[, .(zip, cbsa)]),
            all(nchar(xw$zip) == 5L), all(nchar(xw$cbsa) == 5L))
  write_parquet(xw, file.path(cb_dir, "zip-cbsa.parquet"))

  hdr <- header_zips()
  oc  <- filing_cbsa(hdr, xw)
  n_multi <- nrow(merge(hdr, xw[, .(zip)], by = "zip", allow.cartesian = TRUE)) - nrow(oc)
  stamp("%s filings have a ZIP; %s got a CBSA (%.1f%%); %s extra pairs from straddling ZIPs resolved to the largest share",
        format(nrow(hdr), big.mark = ","), format(nrow(oc), big.mark = ","),
        100 * nrow(oc) / max(1L, nrow(hdr)), format(n_multi, big.mark = ","))
  stopifnot(!anyDuplicated(oc$OBJECTID))
  write_parquet(oc, file.path(cb_dir, "objectid-cbsa.parquet"))

  info <- data.table(route = ROUTE, vintage = got$vintage, min_share = MIN_SHARE,
                     zip_cbsa_pairs = nrow(xw), zips = uniqueN(xw$zip),
                     cbsas = uniqueN(xw$cbsa), filings_with_cbsa = nrow(oc),
                     date = Sys.time())
  fwrite(info, file.path(cb_dir, "build-info-cbsa.csv"))
  print(info)
  stamp("wrote %s", cb_dir)
}
