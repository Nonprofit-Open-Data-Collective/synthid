#' Build a canonical, human-readable per-record identifier string
#'
#' Produces one string per row that encodes the organization, tax year, name, and
#' standardized title. The string is meant to be human-legible for debugging and
#' to be fed to [create_emp_ids()] for hashing. Where two rows produce an identical
#' string (a genuine duplicate record within an organization-year) a `-D{k}`
#' occurrence suffix is appended so the returned vector is always unique.
#'
#' This replaces the draft `create_hash_string()`, which interpolated the literal
#' strings `"EIN2"` and `OID` instead of the corresponding columns and whose
#' argument names did not match the call site.
#'
#' @param df A data frame.
#' @param org_id,year,name,title Column names in `df` for the organization id, tax
#'   year, person name, and standardized title.
#' @return A unique character vector of length `nrow(df)`.
#' @examples
#' df <- data.frame(
#'   ein = c("11", "11"), taxyr = c(2019, 2020),
#'   name = c("Jane Roe", "Jane Roe"), title.standard = c("CEO", "CEO")
#' )
#' build_id_string(df)
#' @export
build_id_string <- function(df,
                            org_id = "ein",
                            year = "taxyr",
                            name = "name",
                            title = "title.standard") {
  for (col in c(org_id, year, name)) {
    if (!col %in% names(df)) {
      stop("build_id_string(): column '", col, "' not found in data frame.", call. = FALSE)
    }
  }
  clean <- function(x) toupper(gsub("\\s+", "-", trimws(ifelse(is.na(x), "", as.character(x)))))

  org <- clean(df[[org_id]])
  yr <- ifelse(is.na(df[[year]]), "", as.character(df[[year]]))
  nm <- clean(df[[name]])
  ti <- if (title %in% names(df)) clean(df[[title]]) else ""

  base <- paste0("EIN-", org, "-Y-", yr, "-NAME-", nm, "-TITLE-", ti)

  ## Disambiguate genuine duplicate strings deterministically.
  occ <- stats::ave(seq_along(base), base, FUN = seq_along)
  ifelse(occ > 1L, paste0(base, "-D", occ), base)
}

#' Build deterministic per-record person-year identifiers from source keys
#'
#' Hashes the native filing/person key `(OBJECTID, TABLE_ID)` into a stable
#' `PYID-` identifier. This is the durable back-link that ties a Part VII record
#' to its source row across the raw, classified, and linked panels -- distinct
#' from the cross-year cluster id [create_emp_ids()] stamps as `EMP_ID`.
#'
#' `OBJECTID` is parser-independent (it comes from the IRS filing index, not the
#' XML body parse); `TABLE_ID` is the stored per-person key within a filing.
#' `(OBJECTID, TABLE_ID)` is unique and non-missing across the raw Part VII data,
#' so unlike a name/title key it needs no disambiguation and is unaffected by
#' downstream name or title cleaning. Both tokens are drawn from `[A-Z0-9-]`, so
#' the `|` field delimiter below can never occur inside them.
#'
#' The recipe is frozen under `keyspec`; bump it if the inputs or delimiter ever
#' change, so old and new ids never silently collide.
#'
#' @section Future work -- CONTENT_ID:
#' `PERSON_YEAR_ID` depends on `TABLE_ID`, which is parser-derived and has broken
#' on pathological XML in the past. A parser revision that renumbers `TABLE_ID`
#' would shift every id. A parser-independent `CONTENT_ID` -- a hash of
#' normalized native content fields (name variants + raw title + compensation) --
#' could be stored alongside as a recovery bridge to re-map old<->new ids for the
#' records whose content is stable. Not implemented yet; note that a parser
#' change severe enough to break `TABLE_ID` for a record would usually corrupt
#' that record's content extraction too, so `CONTENT_ID` mainly insures against a
#' *global* renumbering of otherwise-clean records. Persisting the raw `OBJECTID`
#' and `TABLE_ID` columns (see `dev/01_preprocess_year.R`) keeps that bridge
#' buildable after the fact.
#'
#' @param object_id,table_id Equal-length character vectors: the raw `OBJECTID`
#'   and `TABLE_ID` source columns.
#' @param keyspec Version tag baked into the hash; identifies the recipe.
#' @return A character vector of `PYID-...` identifiers, one per input row.
#' @examples
#' person_year_id(c("OID-1", "OID-1"), c("TID-00001", "TID-00002"))
#' @export
person_year_id <- function(object_id, table_id, keyspec = "v1") {
  if (length(object_id) != length(table_id)) {
    stop("person_year_id(): object_id and table_id must be the same length.",
         call. = FALSE)
  }
  ## '|' is a safe field delimiter: OBJECTID/TABLE_ID are drawn from [A-Z0-9-].
  key <- paste(keyspec, as.character(object_id), as.character(table_id), sep = "|")
  paste0("PYID-", purrr::map_chr(key, rlang::hash))
}

#' Deterministic short hash of a single string
#'
#' Hashes `x` with [rlang::hash()] and formats the first 32 hex characters into
#' dash-separated 4-character groups prefixed with `EMP-`. Deterministic across
#' sessions, so the same input always yields the same id.
#'
#' @param x A single character string.
#' @return A character scalar such as `"EMP-1A2B-3C4D-..."`.
#' @keywords internal
hash_emp_id <- function(x) {
  hash <- rlang::hash(x)
  start <- seq(1, 29, 4)
  stop <- seq(4, 32, 4)
  toupper(paste0("EMP-", paste0(substring(hash, start, stop), collapse = "-")))
}

#' Hash a vector of strings into deterministic EMP identifiers
#'
#' @param v A character vector.
#' @return A character vector of `EMP-...` identifiers, one per element of `v`.
#' @examples
#' create_emp_ids(c("a", "b", "a"))
#' @export
create_emp_ids <- function(v) {
  purrr::map_chr(v, hash_emp_id)
}
