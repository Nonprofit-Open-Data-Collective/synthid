#' Split a (possibly compound) name into comparable tokens
#'
#' Trims surrounding whitespace and splits on spaces and hyphens so that compound
#' or hyphenated surnames produced by imperfect name parsing (e.g. `"DREW-MCLANE"`,
#' `"ANDREWS MCLANE"`) can be compared token-by-token rather than as opaque
#' strings.
#'
#' @param name A single character string.
#' @return A character vector of upper-cased tokens (empty tokens dropped).
#' @examples
#' preprocess_name("Andrews-McLane")
#' @export
preprocess_name <- function(name) {
  if (is.na(name)) {
    return(character(0))
  }
  name <- trimws(name)
  parts <- unlist(strsplit(name, split = " |-"))
  parts <- parts[nzchar(parts)]
  toupper(parts)
}

#' Similarity between two (possibly compound) surnames
#'
#' Returns the maximum of two views, which together are robust to the two ways a
#' parser mangles a surname without changing the person:
#' \describe{
#'   \item{whole-string}{Jaro-Winkler on the names with all separators removed.
#'     Rescues separator noise where token splitting fails --- `"MCLANE"` vs
#'     `"MC-LANE"` collapse to the same string (1.0), and `"VANDERBERG"` vs
#'     `"VAN DER BERG"` likewise.}
#'   \item{coverage token-match}{Greedy one-to-one Jaro-Winkler matching of the
#'     tokens, summing matched similarities and dividing by the *smaller* token
#'     count. Rescues a dropped or reordered token --- `"ANDREWS-MCLANE"` vs
#'     `"MCLANE"` is 1.0 because the shorter name is fully covered --- while
#'     penalising two different multi-token surnames that merely share one token:
#'     `"WEINER-COHEN"` vs `"COHEN-GANTSOUDES"` scores low, not 1.0.}
#' }
#' The earlier "best single token" rule returned 1.0 for the latter case, letting
#' two different people who share one surname token look identical; dividing by
#' the smaller token count is what closes that leak.
#'
#' The max of the two views is finally rescaled by `(v - rescale) / (1 - rescale)`
#' so that unrelated surnames (Jaro-Winkler rarely drops below ~0.5) land near 0
#' and penalise in scoring, instead of reading as "half a match".
#'
#' @param x,y Single character strings (each may be compound/hyphenated).
#' @param floor Minimum token-pair similarity counted as a match in the coverage
#'   view.
#' @param rescale Lower anchor for the final rescaling; similarities at or below
#'   it map to 0.
#' @return A similarity in `[0, 1]` (`1` = identical); `NA_real_` if either name is
#'   missing or empty.
#' @examples
#' compare_two_names("ANDREWS-MCLANE", "MCLANE")        # dropped token -> ~1
#' compare_two_names("WEINER-COHEN", "COHEN-GANTSOUDES") # shared token only -> low
#' @export
compare_two_names <- function(x, y, floor = 0.80, rescale = 0.5) {
  xs <- preprocess_name(x)
  ys <- preprocess_name(y)
  if (length(xs) == 0L || length(ys) == 0L) {
    return(NA_real_)
  }

  ## (a) whole-string JW after removing separators
  strip <- function(s) gsub("[^A-Z0-9]", "", toupper(s))
  whole <- 1 - stringdist::stringdist(strip(x), strip(y), method = "jw")

  ## (b) coverage token-match: greedy one-to-one, divided by smaller token count
  sim <- outer(xs, ys, function(a, b) 1 - stringdist::stringdist(a, b, method = "jw"))
  num <- 0
  denom <- min(length(xs), length(ys))
  repeat {
    mx <- max(sim)
    if (!is.finite(mx) || mx < floor) break
    ij <- which(sim == mx, arr.ind = TRUE)[1, ]
    num <- num + mx
    sim[ij[1], ] <- -Inf
    sim[, ij[2]] <- -Inf
    if (all(!is.finite(sim))) break
  }
  coverage <- num / denom

  ## Rescale so that unrelated surnames (Jaro-Winkler floors near ~0.5) land near
  ## 0 and therefore penalise in scoring, rather than reading as "half a match".
  ## Genuine matches stay at 1. Without this a clear surname mismatch contributes
  ## ~0 (strength ~0) and a nickname + gender match can wrongly override it.
  val <- max(whole, coverage, na.rm = TRUE)
  min(max((val - rescale) / (1 - rescale), 0), 1)
}

#' Vectorised compound-name comparator
#'
#' The comparator object passed to [reclin2::compare_pairs()] for the surname
#' field. Gives the same value as [compare_two_names()] for every pair of two
#' equal-length character vectors.
#'
#' Most surnames are a single token (no space or hyphen). For a pair of single
#' tokens the coverage view is the token Jaro-Winkler when it reaches the
#' floor and 0 otherwise, so the whole comparison reduces to two vectorised
#' [stringdist::stringdist()] calls. Pairs where either side is compound go
#' through [compare_two_names()] one at a time.
#'
#' @param names1,names2 Character vectors of equal length.
#' @param floor,rescale As in [compare_two_names()].
#' @return A numeric vector of similarities in `[0, 1]`.
#' @examples
#' compare_last_names(c("MCLANE", "SMITH"), c("MC-LANE", "SMYTHE"))
#' @export
compare_last_names <- function(names1, names2, floor = 0.80, rescale = 0.5) {
  n <- length(names1)
  out <- rep(NA_real_, n)
  if (n == 0L) return(out)
  x <- as.character(names1); y <- as.character(names2)
  xt <- trimws(x); yt <- trimws(y)
  ## a side with no tokens (NA, empty, only separators) gives NA
  has_x <- !is.na(x) & grepl("[^ -]", xt)
  has_y <- !is.na(y) & grepl("[^ -]", yt)
  single <- has_x & has_y & !grepl("[ -]", xt) & !grepl("[ -]", yt)

  if (any(single)) {
    a <- toupper(xt[single]); b <- toupper(yt[single])
    strip <- function(s) gsub("[^A-Z0-9]", "", toupper(s))
    whole <- 1 - stringdist::stringdist(strip(x[single]), strip(y[single]), method = "jw")
    tok <- 1 - stringdist::stringdist(a, b, method = "jw")
    coverage <- ifelse(is.finite(tok) & tok >= floor, tok, 0)
    val <- pmax(whole, coverage, na.rm = TRUE)
    out[single] <- pmin(pmax((val - rescale) / (1 - rescale), 0), 1)
  }
  multi <- which(has_x & has_y & !single)
  if (length(multi)) {
    tx <- lapply(x[multi], preprocess_name)
    ty <- lapply(y[multi], preprocess_name)
    nx <- lengths(tx); ny <- lengths(ty)
    ## One side a single token: the greedy match takes only the best token pair,
    ## so coverage is the best token Jaro-Winkler when it reaches the floor.
    one <- pmin(nx, ny) == 1L
    if (any(one)) {
      k <- which(one)
      s <- ifelse(nx[k] == 1L, vapply(tx[k], `[`, "", 1L), vapply(ty[k], `[`, "", 1L))
      other <- ifelse(nx[k] == 1L, ty[k], tx[k])
      len <- lengths(other)
      tok <- 1 - stringdist::stringdist(rep(s, len), unlist(other, use.names = FALSE), method = "jw")
      grp <- rep(seq_along(k), len)
      best <- vapply(split(tok, grp), max, 0, USE.NAMES = FALSE)
      coverage <- ifelse(is.finite(best) & best >= floor, best, 0)
      strip <- function(v) gsub("[^A-Z0-9]", "", toupper(v))
      ii <- multi[k]
      whole <- 1 - stringdist::stringdist(strip(x[ii]), strip(y[ii]), method = "jw")
      val <- pmax(whole, coverage, na.rm = TRUE)
      out[ii] <- pmin(pmax((val - rescale) / (1 - rescale), 0), 1)
    }
    ## both sides compound: greedy one-to-one matching, one pair at a time
    for (i in multi[!one]) out[i] <- compare_two_names(x[i], y[i], floor = floor, rescale = rescale)
  }
  out
}
