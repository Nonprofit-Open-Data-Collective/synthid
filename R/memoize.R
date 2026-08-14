#' Memoize an element-wise pair comparator
#'
#' Wraps a comparator `fn(x, y)` (a function taking two equal-length character
#' vectors and returning a numeric similarity per element) so that each distinct
#' `(x, y)` value-pair is evaluated at most once for the lifetime of the wrapper.
#'
#' The cross-year linker compares the *same* surname / first-name strings over and
#' over: a person present in all 15 years has their name re-compared against every
#' coworker in each of the `choose(15, 2) = 105` year-pairs. The underlying string
#' work (tokenising, Jaro-Winkler, phonetic keys) is identical every time. Caching
#' by the raw string pair collapses that repetition to the number of *distinct*
#' pairs actually seen, which within one organization is small.
#'
#' The wrapper is a pure accelerator: for any input it returns byte-identical
#' output to `fn`, so a memoized run produces the same edges, clusters, and
#' `EMP_ID`s as an unmemoized one. It only pays off when the wrapper instance is
#' reused across many calls (as [match_comparators()] does within a single
#' [link_panel()] run); a fresh wrapper per call would just add overhead.
#'
#' @param fn An element-wise comparator, `function(x, y) -> numeric`.
#' @return A function with the same contract as `fn`, backed by a per-instance
#'   cache. `NA` inputs and `NA` results are cached and returned faithfully.
#' @keywords internal
memoize_comparator <- function(fn) {
  force(fn)   # resolve now: callers wrap in a loop that reassigns the slot `fn`
              # would otherwise lazily point back to -- yielding self-recursion.
  cache <- new.env(parent = emptyenv())
  # Control-char delimiters that cannot occur in an upper-cased name field, so
  # the composite key is unambiguous and a genuine NA never collides with the
  # literal string "NA". Built from raw bytes to keep the source ASCII-clean.
  SEP      <- rawToChar(as.raw(31L))  # unit separator between the two values
  NA_TOKEN <- rawToChar(as.raw(1L))   # stand-in for a missing value
  function(x, y) {
    x <- as.character(x); y <- as.character(y)
    xk <- ifelse(is.na(x), NA_TOKEN, x)
    yk <- ifelse(is.na(y), NA_TOKEN, y)
    key <- paste0(xk, SEP, yk)

    # Look every key up in one C-level call; misses come back as NULL.
    got <- mget(key, envir = cache, ifnotfound = list(NULL))
    hit <- !vapply(got, is.null, logical(1L))
    out <- rep(NA_real_, length(key))
    if (any(hit)) out[hit] <- as.numeric(unlist(got[hit], use.names = FALSE))

    miss <- which(!hit)
    if (length(miss)) {
      mk <- key[miss]
      firstU <- !duplicated(mk)          # one representative per distinct miss
      uk <- mk[firstU]
      uv <- as.numeric(fn(x[miss][firstU], y[miss][firstU]))
      for (t in seq_along(uk)) assign(uk[t], uv[t], envir = cache)
      out[miss] <- uv[match(mk, uk)]      # fan the distinct results back out
    }
    out
  }
}
