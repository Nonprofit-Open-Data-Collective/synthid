#' Reduce an EMP_ID cluster to a canonical person profile
#'
#' Collapses every record of a within-organization person (all rows sharing an
#' `EMP_ID`, as stamped by [link_panel()]) into one structured profile row for
#' cross-organization linkage. Deliberately does **not** flatten the cluster to a
#' single canonical name string: the variant *sets* are what carry a person from
#' one organization's filing to another's (a maiden/married surname, an OCR
#' variant, a nickname), so each name field keeps both a deterministic canonical
#' value (for display and as a primary blocking key) and the full set of observed
#' variants (for blocking recall and pairwise scoring).
#'
#' The profile is the unit compared in the cross-org stage. Stage 1 (candidate
#' generation) is a hash/inverted-index lookup on blocking keys rather than a
#' materialized within-block cross product: a profile emits one key per surname
#' variant (`last_name_keys`, phonetic), lands in every matching bucket, and only
#' profiles sharing a bucket are scored -- so maiden/married/OCR variants are not
#' silently dropped, and a common surname never materializes an all-pairs block.
#'
#' @param df A linked panel: the output of [link_panel()] (or any frame carrying
#'   `EMP_ID` plus parsed name components). Multiple rows per `EMP_ID`.
#' @param cols Column mapping; see [synthid_cols()]. Uses `org_id`, `year`,
#'   `name`, and the name-component features (`first_name`, `middle_name`,
#'   `last_name`, `suffix`, `gender`).
#' @param emp_id Name of the person-cluster id column (default `"EMP_ID"`).
#' @param title Name of the standardized-title column, classified into a coarse
#'   role via [classify_title_role()] (default `"title.standard"`; skipped if
#'   absent).
#' @param state,ntee Optional column names for organization geography and NTEE
#'   industry. When present they are carried into the profile as variant sets and
#'   are available as relaxable blocking dimensions; when `NULL` or absent they
#'   are skipped (join them from the BMF beforehand if you want them).
#' @param overrides Passed to [classify_title_role()] for title tail cases.
#' @return A data frame, one row per `EMP_ID`, with scalar summary columns and
#'   list-columns for the variant sets:
#'   \describe{
#'     \item{`EMP_ID`, `n_records`, `n_orgs`, `n_years`, `first_year`,
#'       `last_year`}{Cluster size and span. `EMP_ANCHOR` (the person's anchor
#'       key) is carried through too when the input panel has it, so an
#'       incremental match can inherit it ([match_to_profiles()]).}
#'     \item{`last_name`, `last_name_variants`, `last_name_keys`}{Modal surname;
#'       the distinct surnames observed; their phonetic (Soundex) blocking keys.}
#'     \item{`first_name`, `first_name_variants`, `first_name_roots`,
#'       `first_initial`}{Most-complete formal given name; distinct given names;
#'       their canonical nickname roots ([first_name_roots()]); leading initial.}
#'     \item{`middle_initials`, `suffixes`, `gender`}{Union of middle initials and
#'       suffixes; modal gender (`NA` on tie/unknown).}
#'     \item{`name_variants`}{Distinct full-name strings seen for the person.}
#'     \item{`primary_role`, `roles`, `is_board`}{Modal coarse role; the role set;
#'       whether the person *ever* held a `BOARD` seat.}
#'     \item{`titles`, `orgs`, `years`}{Distinct standardized titles,
#'       organization ids, and tax years for the person.}
#'     \item{`states`, `ntee`}{Present only when `state`/`ntee` are supplied.}
#'   }
#'   List-columns hold one character vector per person; unnest `last_name_keys`
#'   (with `EMP_ID`) to get the long key table for the stage-1 hash join.
#' @examples
#' \dontrun{
#' linked <- link_panel(panel)
#' profiles <- build_person_profile(linked)
#' # long blocking-key table for the cross-org hash join:
#' keys <- do.call(rbind, Map(function(id, k) data.frame(EMP_ID = id, key = k),
#'                            profiles$EMP_ID, profiles$last_name_keys))
#' }
#' @seealso [classify_title_role()], [first_name_roots()]
#' @export
build_person_profile <- function(df, cols = synthid_cols(),
                                 emp_id = "EMP_ID",
                                 title = "title.standard",
                                 state = NULL, ntee = NULL,
                                 overrides = NULL) {
  stopifnot(is.data.frame(df))
  if (!emp_id %in% names(df)) {
    stop("build_person_profile(): id column '", emp_id, "' not found. ",
         "Run link_panel() first.", call. = FALSE)
  }

  up <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x[x == "" | x == "NA"] <- NA
    x
  }
  col <- function(nm) if (!is.null(nm) && nm %in% names(df)) up(df[[nm]]) else NULL

  id     <- as.character(df[[emp_id]])
  last   <- col("last_name")
  first  <- col("first_name")
  middle <- col("middle_name")
  suffix <- col("suffix")
  gender <- col("gender")
  fullnm <- col(cols$name)
  org    <- as.character(df[[cols$org_id]])
  yr     <- suppressWarnings(as.integer(df[[cols$year]]))
  role   <- if (!is.null(title) && title %in% names(df)) {
    as.character(classify_title_role(df[[title]], overrides = overrides, levels = FALSE))
  } else NULL
  titl   <- col(title)
  st     <- col(state)
  nt     <- col(ntee)

  ## Fall back to the full name where a component is entirely absent.
  if (is.null(last))  last  <- fullnm
  if (is.null(first)) first <- rep(NA_character_, length(id))

  idx <- split(seq_along(id), id)
  uset <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (!length(v)) character(0) else sort(unique(v)) }

  ## Deterministic modal value: most frequent, ties broken by longest then
  ## alphabetical, so the canonical is stable across runs and row orders.
  modal <- function(v) {
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return(NA_character_)
    tb <- table(v)
    cand <- names(tb)[tb == max(tb)]
    cand[order(-nchar(cand), cand)][1]
  }
  ## Most-complete formal given name: prefer a non-initial; among those, modal,
  ## ties -> longest -> alphabetical. Falls back to an initial only if that is all
  ## the person ever has.
  formal_first <- function(v) {
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return(NA_character_)
    full <- v[nchar(v) > 1L]
    modal(if (length(full)) full else v)
  }
  phon <- function(v) {
    if (!length(v)) return(character(0))
    p <- suppressWarnings(stringdist::phonetic(gsub("[^A-Z]", "", v)))
    sort(unique(p[!is.na(p) & nzchar(p)]))
  }
  roots_of <- function(v) {
    if (!length(v)) return(character(0))
    sort(unique(unlist(lapply(v, first_name_roots), use.names = FALSE)))
  }

  ## Carry the anchor key through when present (per-cluster constant), so a
  ## matched incremental wave-person can inherit the existing person's anchor.
  anchor_of <- if ("EMP_ANCHOR" %in% names(df)) {
    a <- as.character(df$EMP_ANCHOR)
    vapply(idx, function(r) { v <- a[r]; v <- v[!is.na(v)]; if (length(v)) v[1] else NA_character_ },
           character(1))
  } else NULL

  n <- length(idx)
  emp <- names(idx)
  scal <- data.frame(
    EMP_ID = emp,
    n_records = integer(n), n_orgs = integer(n), n_years = integer(n),
    first_year = integer(n), last_year = integer(n),
    last_name = character(n), first_name = character(n),
    first_initial = character(n), gender = character(n),
    primary_role = character(n), is_board = logical(n),
    stringsAsFactors = FALSE
  )
  ln_var <- ln_key <- fn_var <- fn_root <- mid <- suf <- nmv <-
    rol <- ttl <- orgs <- yrs <- sts <- nts <- vector("list", n)

  for (i in seq_len(n)) {
    r  <- idx[[i]]
    lv <- uset(last[r]); fv <- uset(first[r])
    scal$n_records[i] <- length(r)
    scal$n_orgs[i]    <- length(unique(org[r]))
    yy <- yr[r]; yy <- yy[!is.na(yy)]
    scal$n_years[i]   <- length(unique(yy))
    scal$first_year[i] <- if (length(yy)) min(yy) else NA_integer_
    scal$last_year[i]  <- if (length(yy)) max(yy) else NA_integer_
    yrs[[i]] <- sort(unique(yy))
    ln <- modal(last[r]); fn <- formal_first(first[r])
    scal$last_name[i]  <- ln
    scal$first_name[i] <- fn
    scal$first_initial[i] <- if (!is.na(fn)) substr(fn, 1, 1) else NA_character_
    scal$gender[i] <- modal(gender[r])
    ln_var[[i]] <- lv; ln_key[[i]] <- phon(lv)
    fn_var[[i]] <- fv; fn_root[[i]] <- roots_of(fv)
    mid[[i]] <- uset(substr(middle[r], 1, 1)); suf[[i]] <- uset(suffix[r])
    nmv[[i]] <- uset(fullnm[r]); orgs[[i]] <- uset(org[r])
    if (!is.null(role)) {
      rv <- uset(role[r]); rol[[i]] <- rv
      scal$primary_role[i] <- modal(role[r])
      scal$is_board[i] <- "BOARD" %in% rv
    } else { rol[[i]] <- character(0); scal$primary_role[i] <- NA_character_; scal$is_board[i] <- NA }
    ttl[[i]] <- uset(titl[r])
    if (!is.null(st)) sts[[i]] <- uset(st[r])
    if (!is.null(nt)) nts[[i]] <- uset(nt[r])
  }

  scal$last_name_variants  <- ln_var
  scal$last_name_keys      <- ln_key
  scal$first_name_variants <- fn_var
  scal$first_name_roots    <- fn_root
  scal$middle_initials     <- mid
  scal$suffixes            <- suf
  scal$name_variants       <- nmv
  scal$roles               <- rol
  scal$titles              <- ttl
  scal$orgs                <- orgs
  scal$years               <- yrs
  if (!is.null(st)) scal$states <- sts
  if (!is.null(nt)) scal$ntee   <- nts
  if (!is.null(anchor_of)) scal$EMP_ANCHOR <- unname(anchor_of)

  ## Column order: scalars, then the variant list-columns.
  ord <- c("EMP_ID", if (!is.null(anchor_of)) "EMP_ANCHOR",
           "n_records", "n_orgs", "n_years", "first_year", "last_year",
           "last_name", "last_name_variants", "last_name_keys",
           "first_name", "first_name_variants", "first_name_roots", "first_initial",
           "middle_initials", "suffixes", "gender", "name_variants",
           "primary_role", "roles", "is_board", "titles", "orgs", "years",
           if (!is.null(st)) "states", if (!is.null(nt)) "ntee")
  scal[, ord, drop = FALSE]
}
