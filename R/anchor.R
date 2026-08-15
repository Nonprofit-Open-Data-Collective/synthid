#' Choose a cluster's stable anchor key
#'
#' Reduces the member records of one person cluster to a single deterministic
#' *anchor key* -- the durable string that mints the person's `EMP_ID` under the
#' anchored id scheme ([anchor_emp_id()]). Unlike the legacy hash-of-membership
#' recipe, an anchor depends only on the person's *first-seen* record, so adding
#' later records (a new wave) -- or even earlier records (a backfill) -- to a
#' person never changes their id.
#'
#' The anchor record is the cluster's deterministic earliest: earliest tax `year`,
#' ties broken by preferring a record that carries a native filing key, then by
#' `object_id`, `table_id`, and finally the fallback `row_uid`. The returned key
#' is that record's [person_year_id()] when both native keys are present, else its
#' `row_uid`. Missing years sort last, so a record with a known year always
#' out-anchors one without.
#'
#' `emp_anchor_key()` operates on the members of a *single* cluster (equal-length
#' vectors) and returns one key; callers split by cluster and apply it per group.
#'
#' @param year Tax years of the cluster's records (coerced to integer).
#' @param object_id,table_id Native filing keys (`OBJECTID`, `TABLE_ID`) of the
#'   records, or `NULL` when unavailable; the anchor then falls back to `row_uid`.
#' @param row_uid Per-record fallback keys (e.g. [build_id_string()] output),
#'   used for tie-breaking and as the anchor key when native keys are absent.
#' @param keyspec Version tag forwarded to [person_year_id()].
#' @return A single character anchor key.
#' @seealso [anchor_emp_id()], [person_year_id()]
#' @examples
#' emp_anchor_key(year = c(2021, 2019, 2020),
#'                object_id = c("O3", "O1", "O2"),
#'                table_id  = c("T3", "T1", "T2"),
#'                row_uid   = c("r3", "r1", "r2"))   # anchors on the 2019 record
#' @export
emp_anchor_key <- function(year, object_id = NULL, table_id = NULL, row_uid,
                           keyspec = "v1") {
  n <- length(row_uid)
  if (n == 0L) return(NA_character_)
  yr  <- suppressWarnings(as.integer(year))
  oid <- if (is.null(object_id)) rep(NA_character_, n) else as.character(object_id)
  tid <- if (is.null(table_id))  rep(NA_character_, n) else as.character(table_id)
  ruid <- as.character(row_uid)

  ## Earliest year first (unknown years last); among ties prefer a record that
  ## has a native key (NA sorts last), then lexical object_id / table_id / row_uid
  ## for a fully deterministic, row-order-independent pick.
  ord <- order(yr, oid, tid, ruid, na.last = TRUE, method = "radix")
  a <- ord[1]

  if (!is.na(oid[a]) && nzchar(oid[a]) && !is.na(tid[a]) && nzchar(tid[a])) {
    person_year_id(oid[a], tid[a], keyspec = keyspec)
  } else {
    ruid[a]
  }
}

#' Mint an anchored EMP_ID from an anchor key
#'
#' Hashes one or more anchor keys ([emp_anchor_key()]) into `EMP-...` person ids.
#' A scheme tag is folded into the hashed string so anchored ids occupy a
#' different keyspace than the legacy hash-of-membership ids and the two can never
#' silently collide.
#'
#' @param anchor_key Character vector of anchor keys.
#' @param keyspec Version tag baked into the hashed string; bump it if the id
#'   recipe ever changes.
#' @return A character vector of `EMP-...` ids, one per `anchor_key`.
#' @seealso [emp_anchor_key()], [create_emp_ids()]
#' @examples
#' anchor_emp_id(c("PYID-abc", "PYID-def"))
#' @export
anchor_emp_id <- function(anchor_key, keyspec = "v1") {
  create_emp_ids(paste0("ANCHOR|", keyspec, "|", as.character(anchor_key)))
}

#' Mint an anchored cross-org XORG_ID from a founding member's EMP_ID
#'
#' The cross-org analogue of [anchor_emp_id()]: hashes the anchor member's
#' (already-stable) `EMP_ID` into an `XORG-...` interlock id. Anchoring the id to
#' one founding member -- rather than to the whole member set -- means adding a
#' person to an interlock (e.g. on a cross-org refresh after a new wave) does not
#' change the cluster's `XORG_ID`, the same stability [anchor_emp_id()] gives at
#' the person level. A scheme tag keeps these off the legacy hash-of-membership
#' `XORG_ID` keyspace.
#'
#' @param anchor_emp_id Character vector of anchor `EMP_ID`s (one per cluster).
#' @param keyspec Version tag baked into the hashed string.
#' @return A character vector of `XORG-...` ids.
#' @seealso [anchor_emp_id()], [resolve_cross_org()]
#' @export
anchor_xorg_id <- function(anchor_emp_id, keyspec = "v1") {
  key <- paste0("XANCHOR|", keyspec, "|", as.character(anchor_emp_id))
  paste0("XORG-", toupper(substr(purrr::map_chr(key, rlang::hash), 1, 12)))
}

#' Re-mint a linked panel's EMP_IDs under the anchored scheme
#'
#' One-time migration for a panel linked under the legacy hash-of-membership id
#' recipe. Cluster membership is taken as-is from the existing `EMP_ID` column
#' (this does **not** re-link), and each cluster is re-stamped with an anchored id
#' ([emp_anchor_key()] -> [anchor_emp_id()]) plus its `EMP_ANCHOR` key. Because
#' clustering is untouched, the result is identical to what a fresh anchored
#' [link_panel()] run would produce for the same data.
#'
#' @param df A linked panel carrying `emp_id` plus the organization, year, and
#'   name columns named in `cols` (and, ideally, the native `OBJECTID`/`TABLE_ID`
#'   key columns for durable anchoring).
#' @param cols Column mapping; see [synthid_cols()].
#' @param emp_id Name of the existing person-id column to re-mint (default
#'   `"EMP_ID"`).
#' @return A list with `panel` (`df` with `EMP_ID` re-minted and `EMP_ANCHOR`
#'   added) and `crosswalk` (`old_emp_id`, `new_emp_id`, `emp_anchor`) so
#'   downstream tables keyed on the old ids can follow the rename.
#' @seealso [emp_anchor_key()], [anchor_emp_id()], [link_panel()]
#' @export
remint_anchored <- function(df, cols = synthid_cols(), emp_id = "EMP_ID") {
  stopifnot(is.data.frame(df))
  if (!emp_id %in% names(df)) {
    stop("remint_anchored(): id column '", emp_id, "' not found.", call. = FALSE)
  }
  ruid <- build_id_string(
    df, org_id = cols$org_id, year = cols$year, name = cols$name,
    title = if ("title.standard" %in% names(df)) "title.standard" else cols$name
  )
  yr <- df[[cols$year]]
  getk <- function(k) {
    nm <- cols[[k]]
    if (!is.null(nm) && nm %in% names(df)) as.character(df[[nm]]) else NULL
  }
  oid <- getk("object_id"); tid <- getk("table_id")
  sub <- function(v, i) if (is.null(v)) NULL else v[i]

  old <- as.character(df[[emp_id]])
  ix <- split(seq_along(old), old)
  anchors <- vapply(ix, function(i)
    emp_anchor_key(yr[i], sub(oid, i), sub(tid, i), ruid[i]), character(1))
  newid <- anchor_emp_id(anchors)

  pos <- match(old, names(ix))
  df$EMP_ANCHOR <- unname(anchors)[pos]
  df[[emp_id]] <- unname(newid)[pos]

  crosswalk <- data.frame(
    old_emp_id = names(ix), new_emp_id = unname(newid),
    emp_anchor = unname(anchors), stringsAsFactors = FALSE
  )
  list(panel = df, crosswalk = crosswalk)
}
