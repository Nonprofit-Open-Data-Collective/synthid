#' Token sets used by the link-review flaggers
#'
#' Defaults for the honorifics that a parser may fuse onto a given name and the
#' post-nominal credential tokens that may leak into a surname. Exposed as
#' functions so a caller can extend them (e.g. `flag_links(cred = c(credential_tokens(), "RDN"))`).
#'
#' @return A character vector of upper-case tokens.
#' @name synthid_tokens
NULL

#' @rdname synthid_tokens
#' @export
honorific_tokens <- function() {
  c("MR", "MRS", "MS", "MISS", "DR", "REV", "HON", "PROF", "FR", "SR",
    "SISTER", "RABBI", "PASTOR", "FATHER", "SIR", "DAME")
}

#' @rdname synthid_tokens
#' @export
credential_tokens <- function() {
  ## Deliberately excludes ambiguous two-letter tokens that collide with real
  ## name particles or initials (e.g. MA, DO, PA, BS, PE, FA, DM); extend the set
  ## per-call if a corpus needs them.
  c("RT", "RN", "MD", "DDS", "DMD", "PHD", "PHARMD", "CAE", "RDMS", "RVT", "RM",
    "RMSBS", "MSM", "MSRS", "CRA", "CNMT", "MBA", "ESQ", "CPA", "JD", "LCSW",
    "MSW", "MPH", "MHA", "FACHE", "FASRT", "CFRE", "RTR", "RRT", "FNP", "APRN",
    "AIA", "EDD")
}

## ---- internal helpers -------------------------------------------------------

.norm_ws <- function(s) gsub("\\s+", " ", toupper(trimws(ifelse(is.na(s), "", s))))

## Minimum pairwise similarity among the distinct non-empty values of a cluster,
## using a vectorised comparator cmp(a, b). Returns 1 when < 2 distinct values.
.min_pairwise <- function(vals, cmp) {
  u <- unique(vals[nzchar(vals)])
  if (length(u) < 2L) return(1)
  cb <- utils::combn(length(u), 2L)
  min(cmp(u[cb[1L, ]], u[cb[2L, ]]))
}

## Max normalised string distance among distinct non-empty values (0 if < 2).
.max_dist <- function(vals, method, normalize = FALSE) {
  u <- unique(vals[nzchar(vals)])
  if (length(u) < 2L) return(0)
  d <- stringdist::stringdistmatrix(u, u, method = method)
  if (normalize) {
    nc <- outer(nchar(u), nchar(u), pmax)
    d <- d / ifelse(nc == 0, 1, nc)
  }
  max(d[upper.tri(d)])
}

## Is one distinct surname another with a single leading letter removed?
## (e.g. "WSMITH" vs "SMITH"). Returns the (glued, clean) pairs found.
.single_letter_glue_pairs <- function(last_vals) {
  u <- unique(last_vals[nzchar(last_vals)])
  if (length(u) < 2L) return(NULL)
  out <- list()
  cb <- utils::combn(length(u), 2L)
  for (k in seq_len(ncol(cb))) {
    a <- u[cb[1L, k]]; b <- u[cb[2L, k]]
    if (nchar(a) == nchar(b) + 1L && substr(a, 2L, nchar(a)) == b) {
      out[[length(out) + 1L]] <- c(glued = a, clean = b)
    } else if (nchar(b) == nchar(a) + 1L && substr(b, 2L, nchar(b)) == a) {
      out[[length(out) + 1L]] <- c(glued = b, clean = a)
    }
  }
  out
}

## Split each first name into (honorific, remainder) where a honorific is fused
## directly onto the name, e.g. "MRWILLIAM" -> ("MR", "WILLIAM"). The raw pattern
## is far too loose on its own -- it also splits ordinary names ("FRANK" -> "FR" +
## "ANK") -- so callers must additionally require the remainder to be a *real*
## given name (a known name, or another record's first name for the same person).
.honorific_glue_parts <- function(first_key, hon = honorific_tokens()) {
  re <- paste0("^(", paste(hon, collapse = "|"), ")([A-Z]{3,})$")
  m <- regmatches(first_key, regexec(re, first_key))
  hit <- vapply(m, length, integer(1)) == 3L
  hont <- rep(NA_character_, length(first_key))
  rem  <- rep(NA_character_, length(first_key))
  if (any(hit)) {
    mm <- do.call(rbind, m[hit])
    hont[hit] <- mm[, 2L]; rem[hit] <- mm[, 3L]
  }
  list(hit = hit, honorific = hont, remainder = rem)
}

## Known given names (canonicals + nicknames) used to confirm a glue remainder.
.known_first_names <- function() nickname_cache()$known

## ---- primary categoriser ----------------------------------------------------

## Assign one primary category to a person-cluster's records `g` (a data frame
## with columns first_key, last_key, raw_key). Priority order below is chosen so
## that the residual "review" bucket holds only genuinely unexplained divergence.
.categorize_cluster <- function(g, last_sim, first_sim, name_lv, name_jw,
                                hon, known, cred, last_floor, first_floor,
                                div_lv, div_jw) {
  fk <- g$first_key; lk <- g$last_key
  Fst <- unique(fk[nzchar(fk)]); L <- unique(lk[nzchar(lk)])

  ## order swap: two distinct raw names with the same token multiset
  rn <- unique(g$raw_key[nzchar(g$raw_key)])
  swap <- FALSE
  if (length(rn) > 1L) {
    rt <- lapply(rn, function(s) sort(preprocess_name(s)))
    for (i in seq_len(length(rn) - 1L)) for (j in (i + 1L):length(rn)) {
      if (length(rt[[i]]) && identical(rt[[i]], rt[[j]]) && rn[i] != rn[j]) swap <- TRUE
    }
  }

  ## honorific glue: the full name must not itself be a known name (else FREDDIE
  ## splits to FR+EDDIE), and the remainder must be a known name or a cluster sibling
  gp <- .honorific_glue_parts(Fst, hon)
  hon_glue <- any(gp$hit & !(Fst %in% known) &
                    (gp$remainder %in% known | gp$remainder %in% Fst))
  honorific <- hon_glue || length(.single_letter_glue_pairs(L)) > 0L
  last_tokens <- unique(unlist(strsplit(L, "[ -]")))
  credential <- length(L) > 1L && any(last_tokens %in% cred)
  compound   <- length(L) > 1L && last_sim >= last_floor

  if (swap) "order_swap"
  else if (honorific) "parser_honorific_glue"
  else if (credential) "surname_credential"
  else if (compound) "surname_compound"
  else if (last_sim >= last_floor && first_sim >= first_floor) "nickname_or_spelling"
  else "review"
}

#' Flag questionable cross-year person links for review
#'
#' Scores every *multi-record* person cluster produced by [link_panel()] on how
#' consistent its member records are from year to year, and sorts each flagged
#' cluster into one primary explanation. The intent is triage: separate the small
#' set of genuinely questionable links (same surname, materially different given
#' name, or a near-miss surname that may be two different people) from the large
#' set whose apparent disagreement has a benign, mechanical cause (a nickname, a
#' maiden/compound surname, a field-order swap, or an upstream parser artifact).
#'
#' A cluster is *flagged* when any of three criteria trip: the parsed first names
#' disagree, the parsed last names disagree, or the raw name strings diverge
#' beyond `divergence_lv` (normalised Levenshtein) or `divergence_jw`
#' (Jaro-Winkler). Every flagged cluster is then assigned, in priority order, to
#' the first matching `category`:
#' \describe{
#'   \item{`order_swap`}{two raw names share the same tokens in different order
#'     (e.g. `"SMITH BRAD"` vs `"BRAD SMITH"`) -- a correct link.}
#'   \item{`parser_honorific_glue`}{a honorific is fused onto the given name
#'     (`"MRWILLIAM"`) or a single letter onto the surname (`"WSMITH"`) -- a
#'     correct link exposing an upstream parser defect (see [parse_fail_log()]).}
#'   \item{`surname_credential`}{post-nominal credentials leaked into the surname
#'     (`"MCNEIL"` vs `"RM RDMS RVT CRA MCNEIL"`) -- also a parser defect.}
#'   \item{`surname_compound`}{the surnames reconcile as a maiden/hyphenated or
#'     reordered compound (`compare_last_names` \eqn{\ge} `last_sim_floor`).}
#'   \item{`nickname_or_spelling`}{surnames agree and the given names agree as a
#'     nickname, initial, or minor spelling variant ([compare_first_names]
#'     \eqn{\ge} `first_sim_floor`) -- a correct link.}
#'   \item{`review`}{residual: nothing benign explains the divergence. This is the
#'     human queue; see [link_review_queue()].}
#' }
#'
#' @param df A linked panel: the output of [link_panel()] (or any frame carrying
#'   the person id, org, year, raw name, and parsed given/surname columns).
#' @param emp_id,org_id,org_name,year Column names for the cross-year person id,
#'   organization id, organization name, and tax year.
#' @param raw_name,first,last Column names for the raw (pre-parse) name string and
#'   the parsed given and surname components.
#' @param divergence_lv,divergence_jw Raw-name divergence thresholds above which a
#'   cluster is flagged (normalised Levenshtein and Jaro-Winkler distance).
#' @param last_sim_floor,first_sim_floor Similarity floors (via
#'   [compare_last_names]/[compare_first_names]) at or above which surnames or
#'   given names are treated as agreeing when assigning a category.
#' @param hon,cred Token sets for honorific-glue and credential detection; see
#'   [honorific_tokens()] and [credential_tokens()].
#' @return A data frame with one row per multi-record person, class
#'   `"synthid_link_flags"`, ordered with the review queue first. Key columns:
#'   `category`, `review_flag`, `review_score`, the per-criterion flags
#'   (`flag_first_disagree`, `flag_last_disagree`, `flag_name_divergence`), the
#'   agreement metrics (`last_sim`, `first_sim`, `name_lv_norm`, `name_jw`), and
#'   the distinct values seen (`names_seen`, `first_seen`, `last_seen`). A summary
#'   (thresholds, person and flag counts, category tally) is attached as the
#'   `"synthid_flags"` attribute.
#' @seealso [link_review_queue()] for the residual queue, [parse_fail_log()] for
#'   the parser-defect handoff.
#' @examples
#' \dontrun{
#' linked <- link_panel(panel)
#' flags  <- flag_links(linked)
#' table(flags$category)
#' review <- link_review_queue(flags)
#' }
#' @export
flag_links <- function(df,
                       emp_id = "EMP_ID", org_id = "ein",
                       org_name = "org.name", year = "taxyr",
                       raw_name = "name.raw", first = "first_name",
                       last = "last_name",
                       divergence_lv = 0.15, divergence_jw = 0.15,
                       last_sim_floor = 0.85, first_sim_floor = 0.85,
                       hon = honorific_tokens(), cred = credential_tokens()) {
  stopifnot(is.data.frame(df))
  ## raw name is optional-ish: fall back to the display name column if absent.
  if (!raw_name %in% names(df) && "name" %in% names(df)) raw_name <- "name"
  need <- c(emp_id, org_id, year, raw_name, first, last)
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    stop("flag_links(): missing required column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  has_orgname <- org_name %in% names(df)

  d <- data.frame(
    emp_id    = as.character(df[[emp_id]]),
    ein       = as.character(df[[org_id]]),
    org_name  = if (has_orgname) as.character(df[[org_name]]) else NA_character_,
    year      = df[[year]],
    raw_key   = .norm_ws(df[[raw_name]]),
    raw_disp  = as.character(df[[raw_name]]),
    first_key = toupper(trimws(ifelse(is.na(df[[first]]), "", df[[first]]))),
    last_key  = toupper(trimws(ifelse(is.na(df[[last]]), "", df[[last]]))),
    stringsAsFactors = FALSE
  )

  ## multi-record persons only -- disagreement is undefined for singletons.
  n_rec <- stats::ave(seq_len(nrow(d)), d$emp_id, FUN = length)
  dm <- d[n_rec > 1L, , drop = FALSE]
  if (!nrow(dm)) {
    empty <- .empty_flags()
    attr(empty, "synthid_flags") <- list(n_persons = 0L, n_flagged = 0L)
    return(empty)
  }
  sp <- split(seq_len(nrow(dm)), dm$emp_id)
  known <- .known_first_names()

  rows <- lapply(sp, function(idx) {
    g <- dm[idx, , drop = FALSE]
    fk <- g$first_key; lk <- g$last_key; nk <- g$raw_key
    Fst <- unique(fk[nzchar(fk)]); L <- unique(lk[nzchar(lk)])
    last_sim  <- .min_pairwise(lk, compare_last_names)
    first_sim <- .min_pairwise(fk, compare_first_names)
    name_lv   <- .max_dist(nk, "lv", normalize = TRUE)
    name_jw   <- .max_dist(nk, "jw")

    f_first <- length(Fst) > 1L
    f_last  <- length(L) > 1L
    f_div   <- name_lv >= divergence_lv || name_jw >= divergence_jw
    flagged <- f_first || f_last || f_div

    cat <- if (flagged) {
      .categorize_cluster(g, last_sim, first_sim, name_lv, name_jw,
                          hon, known, cred, last_sim_floor, first_sim_floor,
                          divergence_lv, divergence_jw)
    } else NA_character_

    data.frame(
      emp_id = g$emp_id[1L], ein = g$ein[1L], org_name = g$org_name[1L],
      n_records = nrow(g), n_years = length(unique(g$year)),
      years = paste(sort(unique(g$year)), collapse = ";"),
      n_distinct_name = length(unique(nk[nzchar(nk)])),
      n_distinct_first = length(Fst), n_distinct_last = length(L),
      name_lv_norm = round(name_lv, 3), name_jw = round(name_jw, 3),
      last_sim = round(last_sim, 3), first_sim = round(first_sim, 3),
      flag_first_disagree = f_first, flag_last_disagree = f_last,
      flag_name_divergence = f_div, review_flag = flagged,
      category = cat,
      names_seen = paste(unique(g$raw_disp[nzchar(g$raw_disp)]), collapse = " | "),
      first_seen = paste(Fst, collapse = " | "),
      last_seen  = paste(L, collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, rows)

  res$review_score <- with(res,
    2.0 * flag_last_disagree + 1.5 * flag_first_disagree +
      3.0 * name_lv_norm + 2.0 * name_jw)

  ## report review queue first, then benign categories; worst score on top.
  lvl <- c("review", "surname_credential", "order_swap", "parser_honorific_glue",
           "surname_compound", "nickname_or_spelling")
  ord_cat <- factor(res$category, levels = lvl)
  res <- res[order(is.na(res$category), ord_cat, -res$review_score), , drop = FALSE]
  rownames(res) <- NULL

  flagged <- res[res$review_flag, , drop = FALSE]
  attr(res, "synthid_flags") <- list(
    thresholds = list(divergence_lv = divergence_lv, divergence_jw = divergence_jw,
                      last_sim_floor = last_sim_floor, first_sim_floor = first_sim_floor),
    n_persons = nrow(res), n_flagged = nrow(flagged),
    categories = table(factor(flagged$category, levels = lvl))
  )
  class(res) <- c("synthid_link_flags", "data.frame")
  res
}

.empty_flags <- function() {
  cols <- c("emp_id", "ein", "org_name", "n_records", "n_years", "years",
            "n_distinct_name", "n_distinct_first", "n_distinct_last",
            "name_lv_norm", "name_jw", "last_sim", "first_sim",
            "flag_first_disagree", "flag_last_disagree", "flag_name_divergence",
            "review_flag", "category", "names_seen", "first_seen", "last_seen",
            "review_score")
  res <- as.data.frame(matrix(nrow = 0, ncol = length(cols),
                              dimnames = list(NULL, cols)))
  class(res) <- c("synthid_link_flags", "data.frame")
  res
}

#' @export
print.synthid_link_flags <- function(x, ...) {
  r <- attr(x, "synthid_flags")
  cat("synthid link-review flags\n-------------------------\n")
  cat(sprintf("multi-record persons : %d\n", r$n_persons %||% 0L))
  cat(sprintf("flagged for review   : %d\n", r$n_flagged %||% 0L))
  if (!is.null(r$categories)) {
    cat("by category:\n")
    for (nm in names(r$categories)) {
      cat(sprintf("  %-22s %d\n", nm, r$categories[[nm]]))
    }
  }
  invisible(x)
}

#' The residual human-review queue
#'
#' Convenience accessor: the `category == "review"` rows of a [flag_links()]
#' result -- the links whose cross-year divergence has no benign mechanical
#' explanation -- ordered worst-first by `review_score`.
#'
#' @param flags A `"synthid_link_flags"` data frame from [flag_links()].
#' @return A data frame (the review-queue subset), with the class attribute
#'   dropped so it prints as an ordinary frame.
#' @examples
#' \dontrun{
#' q <- link_review_queue(flag_links(linked))
#' utils::write.csv(q, "link_review_queue.csv", row.names = FALSE)
#' }
#' @export
link_review_queue <- function(flags) {
  stopifnot(is.data.frame(flags))
  q <- flags[!is.na(flags$category) & flags$category == "review", , drop = FALSE]
  q <- q[order(-q$review_score), , drop = FALSE]
  class(q) <- "data.frame"
  rownames(q) <- NULL
  q
}

## ---- parse-fail log ---------------------------------------------------------

#' Record-level parser-defect log for peopleparser
#'
#' Scans a panel for records whose parsed name components bear a *syntactic*
#' defect that upstream name parsing should fix, and emits one row per offending
#' record with the raw input, the wrong output, and -- where it can be inferred --
#' the expected value. These input/wrong/expected triples are directly usable as
#' \pkg{peopleparser} regression fixtures.
#'
#' Three defect types are detected:
#' \describe{
#'   \item{`honorific_glue`}{a honorific fused onto the given name (`"MRWILLIAM"`
#'     -> expected `"WILLIAM"`). Self-evident per record.}
#'   \item{`credential_in_surname`}{post-nominal credential tokens inside the
#'     surname (`"RM RDMS RVT CRA MCNEIL"` -> expected `"MCNEIL"`). Self-evident
#'     per record; the expected surname is the non-credential remainder.}
#'   \item{`surname_letter_glue`}{a single stray letter fused onto the surname
#'     (`"WSMITH"`). Detected only by *consensus* -- another record of the same
#'     person carries the clean surname (`"SMITH"`), which becomes the expected
#'     value -- so this type requires a linked, multi-record person.}
#' }
#'
#' Unlike [flag_links()], which only inspects clusters whose members disagree,
#' the self-evident defects are scanned across every record, so a defect that is
#' consistent across a person's years (and therefore never trips a link flag) is
#' still reported.
#'
#' @inheritParams flag_links
#' @param middle,suffix,salutation Optional parsed-component columns, echoed into
#'   the log when present so a fix can be verified against the full parse.
#' @param object_id,table_id Optional source-key columns (see [person_year_id()]);
#'   echoed as the record key when present.
#' @param path Optional file path; when given, the log is written there with
#'   [utils::write.csv()] and returned invisibly.
#' @return A data frame with one row per defective record: the record key, the
#'   `raw_name` input, the parsed components, `defect_type`, `evidence`,
#'   `expected_first`/`expected_last` (`NA` where not inferable), and `source`
#'   (`"self-evident"` or `"consensus"`). Rows are unique per record even when a
#'   record trips more than one detector (the defects are concatenated).
#' @seealso [flag_links()], [honorific_tokens()], [credential_tokens()]
#' @examples
#' \dontrun{
#' log <- parse_fail_log(linked, path = "peopleparser_parse_fails.csv")
#' table(log$defect_type)
#' }
#' @export
parse_fail_log <- function(df,
                           emp_id = "EMP_ID", org_id = "ein",
                           org_name = "org.name", year = "taxyr",
                           raw_name = "name.raw", first = "first_name",
                           last = "last_name", middle = "middle_name",
                           suffix = "suffix", salutation = "salutation",
                           object_id = "OBJECTID", table_id = "TABLE_ID",
                           hon = honorific_tokens(), cred = credential_tokens(),
                           path = NULL) {
  stopifnot(is.data.frame(df))
  if (!raw_name %in% names(df) && "name" %in% names(df)) raw_name <- "name"
  need <- c(emp_id, org_id, year, raw_name, first, last)
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    stop("parse_fail_log(): missing required column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  getcol <- function(nm) if (nm %in% names(df)) as.character(df[[nm]]) else NA_character_

  d <- data.frame(
    .row      = seq_len(nrow(df)),
    object_id = getcol(object_id), table_id = getcol(table_id),
    emp_id    = as.character(df[[emp_id]]), ein = as.character(df[[org_id]]),
    org_name  = getcol(org_name), year = df[[year]],
    raw_name  = as.character(df[[raw_name]]),
    salutation = getcol(salutation), first_name = as.character(df[[first]]),
    middle_name = getcol(middle), last_name = as.character(df[[last]]),
    suffix    = getcol(suffix),
    stringsAsFactors = FALSE
  )
  d$first_key <- toupper(trimws(ifelse(is.na(d$first_name), "", d$first_name)))
  d$last_key  <- toupper(trimws(ifelse(is.na(d$last_name), "", d$last_name)))

  known <- .known_first_names()
  defects <- vector("list", 0L)
  add <- function(rows, type, evidence, exp_first = NA_character_,
                  exp_last = NA_character_, source = "self-evident") {
    if (!length(rows)) return(invisible())
    defects[[length(defects) + 1L]] <<- data.frame(
      .row = rows, defect_type = type, evidence = evidence,
      expected_first = exp_first, expected_last = exp_last,
      source = source, stringsAsFactors = FALSE)
    invisible()
  }

  ## (1) honorific glued onto given name. The remainder must be a *real* given
  ## name -- a known name (self-evident), or another first name carried by the
  ## same person (consensus) -- otherwise "FRANK" would look like "FR"+"ANK".
  gp <- .honorific_glue_parts(d$first_key, hon)
  sib_first <- tapply(d$first_key, d$emp_id, function(v) list(unique(v[nzchar(v)])))
  rem_is_sibling <- gp$hit & !is.na(gp$remainder) & mapply(
    function(rem, id) !is.na(rem) && rem %in% sib_first[[id]][[1L]],
    gp$remainder, d$emp_id)
  rem_is_known <- gp$hit & gp$remainder %in% known
  ## exclude names that are themselves known (FREDDIE = FR + EDDIE)
  full_is_known <- d$first_key %in% known
  glue <- which(gp$hit & !full_is_known & (rem_is_known | rem_is_sibling))
  if (length(glue)) {
    src <- ifelse(rem_is_known[glue], "self-evident", "consensus")
    add(glue, "honorific_glue",
        sprintf("honorific '%s' fused onto given name", gp$honorific[glue]),
        exp_first = gp$remainder[glue], source = src)
  }

  ## (2) credential token inside the surname -- self-evident per record
  toks <- strsplit(d$last_key, "[ -]")
  cred_hit <- which(vapply(toks, function(t) any(t %in% cred), logical(1)) &
                      vapply(toks, function(t) any(!(t %in% cred) & nzchar(t)), logical(1)))
  if (length(cred_hit)) {
    ev <- vapply(cred_hit, function(i) {
      t <- toks[[i]]; paste(t[t %in% cred], collapse = " ")
    }, character(1))
    expl <- vapply(cred_hit, function(i) {
      t <- toks[[i]]; paste(t[!(t %in% cred) & nzchar(t)], collapse = " ")
    }, character(1))
    add(cred_hit, "credential_in_surname",
        sprintf("credential token(s) in surname: %s", ev), exp_last = expl)
  }

  ## (3) single stray letter glued onto surname -- consensus only (multi-record)
  n_rec <- stats::ave(seq_len(nrow(d)), d$emp_id, FUN = length)
  dm <- d[n_rec > 1L, , drop = FALSE]
  if (nrow(dm)) {
    for (idx in split(seq_len(nrow(dm)), dm$emp_id)) {
      L <- unique(dm$last_key[idx][nzchar(dm$last_key[idx])])
      pairs <- .single_letter_glue_pairs(L)
      for (p in pairs) {
        rows <- dm$.row[idx][dm$last_key[idx] == p[["glued"]]]
        add(rows, "surname_letter_glue",
            sprintf("leading '%s' glued onto surname", substr(p[["glued"]], 1L, 1L)),
            exp_last = p[["clean"]], source = "consensus")
      }
    }
  }

  if (!length(defects)) {
    out <- d[0L, setdiff(names(d), c(".row", "first_key", "last_key"))]
    out$defect_type <- character(0); out$evidence <- character(0)
    out$expected_first <- character(0); out$expected_last <- character(0)
    out$source <- character(0)
  } else {
    dd <- do.call(rbind, defects)
    ## collapse multiple defects on one record into a single row
    join <- function(v) paste(unique(v[!is.na(v) & nzchar(v)]), collapse = "; ")
    parts <- lapply(split(dd, dd$.row), function(g) data.frame(
      .row = g$.row[1L],
      defect_type = join(g$defect_type), evidence = join(g$evidence),
      expected_first = join(g$expected_first), expected_last = join(g$expected_last),
      source = join(g$source), stringsAsFactors = FALSE))
    agg <- do.call(rbind, parts)
    base <- d[match(agg$.row, d$.row),
              setdiff(names(d), c(".row", "first_key", "last_key"))]
    out <- cbind(base, agg[, -1L, drop = FALSE])
    out <- out[order(out$emp_id, out$year), , drop = FALSE]
    rownames(out) <- NULL
  }

  if (!is.null(path)) {
    utils::write.csv(out, path, row.names = FALSE)
    return(invisible(out))
  }
  out
}

## ---- token roll-up for the peopleparser dev pipeline ------------------------

.split_tokens <- function(s) {
  t <- unlist(strsplit(toupper(ifelse(is.na(s), "", s)), "[ -]"))
  t[nzchar(t)]
}

#' Roll a parse-fail log up into candidate title/credential tokens
#'
#' Reduces the record-level [parse_fail_log()] to the small, structured list a
#' \pkg{peopleparser} maintainer actually acts on: the distinct tokens that leaked
#' into a name field, one row each, with frequency and an example. Tokens are
#' recovered structurally from the log's `last_name`/`expected_last` and
#' `first_name`/`expected_first` columns (the set-difference of the wrong parse
#' and the expected value) -- no text parsing of the evidence string.
#'
#' The `credential_in_surname` rows yield the post-nominal tokens that belong in
#' the prefix/suffix strip list and [known_titles()]; the `honorific_glue` rows
#' yield the honorific that failed to split (typically already known -- the defect
#' is tokenisation, not a missing token, so `in_known` will be `TRUE`).
#' `surname_letter_glue` contributes nothing (a stray initial is not a title).
#'
#' @param log A data frame from [parse_fail_log()].
#' @param known Optional character vector of already-known tokens (e.g.
#'   `toupper(peopleparser::known_titles()$abbr)`); each candidate is marked
#'   `in_known`. Tokens with `in_known == FALSE` are the add-list.
#' @return A data frame, one row per `(token, field)`: `token`, `field`
#'   (`"surname"` or `"given"`), `defect_type`, `n_records`, `n_persons`,
#'   `example` (a raw name showing it), and `in_known`. Ordered by `in_known`
#'   (novel first) then `n_records` descending.
#' @seealso [parse_fail_log()]
#' @examples
#' \dontrun{
#' log <- parse_fail_log(linked)
#' # pass the live peopleparser list as `known` to flag which tokens are novel:
#' # known <- toupper(peopleparser::known_titles()$abbr)
#' toks <- parse_fail_tokens(log, known = c("MR", "DR", "MD", "PHD"))
#' subset(toks, !in_known)          # the tokens to add upstream
#' }
#' @export
parse_fail_tokens <- function(log, known = character()) {
  stopifnot(is.data.frame(log))
  known <- toupper(known)
  rows <- vector("list", 0L)

  ## credential tokens left in the surname = tokens(last_name) - tokens(expected_last)
  cr <- which(grepl("credential_in_surname", log$defect_type))
  for (i in cr) {
    leaked <- setdiff(.split_tokens(log$last_name[i]), .split_tokens(log$expected_last[i]))
    for (tk in leaked) rows[[length(rows) + 1L]] <- data.frame(
      token = tk, field = "surname", defect_type = "credential_in_surname",
      emp_id = log$emp_id[i], example = log$raw_name[i], stringsAsFactors = FALSE)
  }
  ## honorific that failed to split = first_name with the expected given name removed
  hg <- which(grepl("honorific_glue", log$defect_type))
  for (i in hg) {
    fk <- toupper(log$first_name[i]); ex <- toupper(log$expected_first[i])
    tk <- if (nzchar(ex) && endsWith(fk, ex)) substr(fk, 1L, nchar(fk) - nchar(ex)) else NA
    if (!is.na(tk) && nzchar(tk)) rows[[length(rows) + 1L]] <- data.frame(
      token = tk, field = "given", defect_type = "honorific_glue",
      emp_id = log$emp_id[i], example = log$raw_name[i], stringsAsFactors = FALSE)
  }
  if (!length(rows)) {
    return(data.frame(token = character(0), field = character(0),
                      defect_type = character(0), n_records = integer(0),
                      n_persons = integer(0), example = character(0),
                      in_known = logical(0), stringsAsFactors = FALSE))
  }
  r <- do.call(rbind, rows)
  key <- paste(r$token, r$field, r$defect_type, sep = "\r")
  agg <- lapply(split(r, key), function(g) data.frame(
    token = g$token[1L], field = g$field[1L], defect_type = g$defect_type[1L],
    n_records = nrow(g), n_persons = length(unique(g$emp_id)),
    example = g$example[1L], stringsAsFactors = FALSE))
  out <- do.call(rbind, agg)
  out$in_known <- out$token %in% known
  out <- out[order(out$in_known, -out$n_records, out$token), , drop = FALSE]
  rownames(out) <- NULL
  out
}
