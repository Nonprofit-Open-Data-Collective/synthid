#' Default component weights for the person-match score
#'
#' Base weights for each name/attribute field. The final contribution of a field
#' is `weight * (2 * similarity - 1)`, so an identical field adds `+weight`, a
#' completely different field subtracts `weight`, and a missing field on either
#' side contributes `0` (neutral). The `last_name` weight is additionally scaled,
#' per pair, by a within-organization surname-rarity factor (see
#' [surname_weight()]): on a family-dominated board, agreement on the shared
#' surname carries almost no weight and the first name / suffix / gender do the
#' discriminating.
#'
#' @return A named numeric vector of base weights.
#' @export
default_weights <- function() {
  c(
    last_name      = 6.0,  # frequency-adjusted per pair
    first_name     = 5.0,
    middle_name    = 1.5,
    suffix         = 3.0,  # decisive generation splitter when present
    salutation     = 1.0,
    gender         = 2.0,  # soft: inferred, so a weak signal
    title.standard = 0.5   # low on purpose: avoid leaking titles into the ID
  )
}

#' Exact-match comparator (NA-safe)
#'
#' Returns `1` when two values are identical, `0` when they differ, and `NA` when
#' either is missing. Used for categorical fields (gender, suffix, salutation)
#' where graded string similarity is not meaningful.
#'
#' @param x,y Vectors of equal length.
#' @return Numeric vector of `1`/`0`/`NA`.
#' @keywords internal
cmp_exact <- function(x, y) {
  as.numeric(x == y)
}

#' Within-organization surname-rarity weight, per record
#'
#' For each record, estimates how much information "the surname agrees" carries
#' inside that record's organization. If every distinct person in an organization
#' shares one surname (a family board) the factor approaches `0`; a unique surname
#' gives `1`. Distinct people are approximated by distinct full-name strings so
#' that the same person appearing in several years is not counted repeatedly.
#'
#' @param df Working data frame.
#' @param org_id,name,last_name Column names for the organization id, the full
#'   name, and the surname.
#' @return Numeric vector in `[0, 1]`, one value per row of `df`.
#' @export
surname_weight <- function(df, org_id = "ein", name = "name", last_name = "last_name") {
  org <- as.character(df[[org_id]])
  nm <- toupper(trimws(as.character(df[[name]])))
  ln <- toupper(trimws(as.character(df[[last_name]])))

  ## distinct people per org (approx by distinct full name)
  dp <- stats::ave(nm, org, FUN = function(v) length(unique(v)))
  dp <- as.numeric(dp)
  ## distinct people in org sharing this surname
  key <- paste(org, ln, sep = "\r")
  cnt <- stats::ave(nm, key, FUN = function(v) length(unique(v)))
  cnt <- as.numeric(cnt)

  factor <- ifelse(dp <= 1, 1, log(dp / cnt) / log(dp))
  pmin(pmax(factor, 0), 1)
}

#' Score a table of compared pairs
#'
#' Combines the per-field similarities in `cmp` into a single additive match
#' score, applying the per-pair surname-rarity adjustment to the `last_name`
#' field.
#'
#' @param cmp A data frame of comparison columns (numeric similarities in
#'   `[0, 1]`, `NA` where a field was missing on either side). Column names must
#'   match the names of `weights`.
#' @param weights Named numeric vector of base weights (see [default_weights()]).
#' @param sw_x,sw_y Per-pair surname-rarity weights for the two sides (from
#'   [surname_weight()], indexed to the pairs).
#' @param last_field Name of the surname field to frequency-adjust.
#' @return Numeric vector of scores, one per row of `cmp`.
#' @keywords internal
score_pairs <- function(cmp, weights, sw_x, sw_y, last_field = "last_name") {
  feats <- intersect(names(weights), names(cmp))
  score <- numeric(nrow(cmp))
  for (f in feats) {
    s <- as.numeric(cmp[[f]])
    strength <- 2 * s - 1
    if (identical(f, last_field)) {
      # Frequency-adjust the surname signal ASYMMETRICALLY:
      #  - agreement (strength >= 0): weight by the COMMONER side (pmin) -- a
      #    shared surname is only as informative as its more-frequent half, so a
      #    rare compound ("COHEN-GANTSOUDES") cannot lend full weight to a match
      #    made only on its common token ("COHEN");
      #  - disagreement (strength < 0): weight by the RARER side (pmax) -- two
      #    different surnames are strong evidence of different people, and that
      #    penalty must not be diluted just because one side is a common name
      #    (otherwise a nickname + gender match could override it).
      factor <- ifelse(strength >= 0, pmin(sw_x, sw_y), pmax(sw_x, sw_y))
      contrib <- weights[[f]] * factor * strength
    } else {
      contrib <- weights[[f]] * strength
    }
    contrib[is.na(contrib)] <- 0
    score <- score + contrib
  }
  score
}
