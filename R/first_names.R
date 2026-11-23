#' Nickname / diminutive dictionary
#'
#' Maps each canonical given name to its common nicknames and diminutives. Used to
#' recognise that, e.g., `BOB` and `ROBERT` denote the same first name. Some
#' nicknames map to more than one canonical (e.g. `AL` -> `ALBERT`, `ALEXANDER`).
#' [compare_first_names()] treats a canonical and one of its listed nicknames as
#' a pair; two nicknames of one canonical (`LISA`, `BETH`) are not a pair. The
#' canonical roots also feed [first_name_roots()] in person profiles. The larger,
#' data-built [first_name_lexicon()] adds to these pairs in the comparator.
#'
#' @return A named list: each element is a canonical name, its value the character
#'   vector of nicknames.
#' @export
nickname_table <- function() {
  list(
    ROBERT=c("BOB","BOBBY","ROB","ROBBIE","BERT"), WILLIAM=c("BILL","BILLY","WILL","WILLIE","LIAM"),
    RICHARD=c("DICK","RICH","RICK","RICKY","RICHIE"), JOHN=c("JACK","JOHNNY","JON"),
    JAMES=c("JIM","JIMMY","JAMIE"), ELIZABETH=c("LIZ","LIZZY","BETH","BETSY","ELIZA","LISA","LIBBY","ELLIE"),
    MARGARET=c("MEG","PEGGY","MAGGIE","MARGE","GRETA","MADGE"), KATHERINE=c("KATE","KATIE","KATHY","KAT","KITTY"),
    CATHERINE=c("CATHY","KATE","KATIE","CATE"), CHARLES=c("CHARLIE","CHUCK","CHAS"),
    THOMAS=c("TOM","TOMMY"), MICHAEL=c("MIKE","MICKEY","MICK"), EDWARD=c("ED","EDDIE","TED","NED"),
    ANTHONY=c("TONY"), JOSEPH=c("JOE","JOEY"), DANIEL=c("DAN","DANNY"), DAVID=c("DAVE","DAVEY"),
    MATTHEW=c("MATT"), ANDREW=c("ANDY","DREW"), NICHOLAS=c("NICK","NICKY"),
    PATRICIA=c("PAT","PATTY","TRICIA"), PATRICK=c("PAT","PADDY"), SUSAN=c("SUE","SUZY","SUSIE"),
    JENNIFER=c("JEN","JENNY"), DEBORAH=c("DEB","DEBBIE"), BARBARA=c("BARB","BABS"),
    CHRISTOPHER=c("CHRIS","TOPHER"), STEPHEN=c("STEVE","STEVIE"), STEVEN=c("STEVE","STEVIE"),
    KENNETH=c("KEN","KENNY"), RONALD=c("RON","RONNIE"), DONALD=c("DON","DONNIE"),
    GERALD=c("GERRY","JERRY"), SAMUEL=c("SAM","SAMMY"), BENJAMIN=c("BEN","BENNY"),
    ALEXANDER=c("ALEX","AL","SANDY"), GREGORY=c("GREG"), TIMOTHY=c("TIM","TIMMY"),
    FREDERICK=c("FRED","FREDDIE"), THEODORE=c("TED","THEO"), VIRGINIA=c("GINNY","GINGER"),
    VICTORIA=c("VICKY","TORI"), REBECCA=c("BECKY","BECCA"), CYNTHIA=c("CINDY"),
    DOROTHY=c("DOT","DOTTIE","DORA"), ELEANOR=c("ELLIE","NELL","NORA"), FRANCIS=c("FRANK","FRANKIE"),
    FRANCES=c("FRAN","FRANNIE"), LAWRENCE=c("LARRY"), LEONARD=c("LEN","LENNY"),
    PHILIP=c("PHIL"), PHILLIP=c("PHIL"), RAYMOND=c("RAY"), WALTER=c("WALT"),
    HENRY=c("HANK","HARRY"), ALBERT=c("AL","BERT"), HAROLD=c("HAL","HARRY"),
    EUGENE=c("GENE"), VINCENT=c("VINCE","VINNIE"), ARTHUR=c("ART","ARTIE"),
    JOSHUA=c("JOSH"), ZACHARY=c("ZACH","ZAC"), NATHANIEL=c("NATE","NAT"),
    JONATHAN=c("JON","JONNY"), ABIGAIL=c("ABBY","ABBIE"), AMANDA=c("MANDY"),
    MELISSA=c("MISSY"), SANDRA=c("SANDY"), PAMELA=c("PAM"), PENELOPE=c("PENNY")
  )
}

## Package-internal cache for the nickname lookups.
.synthid_cache <- new.env(parent = emptyenv())

nickname_cache <- function() {
  if (!is.null(.synthid_cache$varset)) return(.synthid_cache)
  nick <- nickname_table()
  varset <- new.env(parent = emptyenv())   # canonical -> its variants
  roots <- new.env(parent = emptyenv())    # any name  -> canonicals it belongs to
  add_root <- function(name, root) roots[[name]] <- unique(c(roots[[name]], root))
  for (canon in names(nick)) {
    varset[[canon]] <- nick[[canon]]
    add_root(canon, canon)
    for (v in nick[[canon]]) add_root(v, canon)
  }
  .synthid_cache$varset <- varset
  .synthid_cache$roots <- roots
  .synthid_cache$canon <- names(nick)
  .synthid_cache$known <- union(names(nick), unlist(nick, use.names = FALSE))
  .synthid_cache
}

#' Canonical roots of a first name
#'
#' @param x A single upper-cased name token.
#' @return Character vector of canonical roots (the name itself if unknown), or
#'   `character(0)` for a missing/empty name.
#' @keywords internal
first_name_roots <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  r <- nickname_cache()$roots[[x]]
  if (is.null(r)) x else r
}

#' Are two first names a direct nickname pair? (vectorised)
#'
#' Direct means one is a diminutive of the other -- a canonical and one of its
#' listed variants (`ROBERT`/`BOB`). Two *sibling* variants of the same canonical
#' (`LISA` and `BETH`, both from `ELIZABETH`) are **not** equivalent: a person
#' goes by one diminutive, not both, so treating them as the same first name
#' would wrongly merge two different people.
#'
#' @param a,b Equal-length upper-cased name vectors.
#' @return Logical vector.
#' @keywords internal
nickname_equivalent_vec <- function(a, b) {
  cc <- nickname_cache()
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  res <- logical(length(a))
  ## A direct pair needs one side to be a canonical whose variant list contains
  ## the other; restrict the per-row check to rows where a canonical is present.
  rel <- nzchar(a) & nzchar(b) & a != b & (a %in% cc$canon | b %in% cc$canon)
  if (any(rel)) {
    res[rel] <- mapply(function(x, y) {
      (x %in% cc$canon && y %in% cc$varset[[x]]) ||
        (y %in% cc$canon && x %in% cc$varset[[y]])
    }, a[rel], b[rel])
  }
  res
}

#' First-name variant lexicon
#'
#' Pairs of first names that are two forms of one given name, built from the
#' names one person carries across their own organization's Part VII filings
#' (national 2010-2024 build; pairs seen for 25+ people, reviewed by hand). Each
#' pair has a type: `nickname` (incl. short forms and diminutives, `BOB`/`ROBERT`),
#' `spelling` (`JEFFERY`/`JEFFREY`), `compound` (`MARY`/`MARYANN`) or `cognate`
#' (the same name in another language, `JOSE`/`JOSEPH`). See
#' `vignette("first-name-variants")` for how it was built.
#'
#' @return A data frame with columns `name_short`, `name_long`, `type`,
#'   `n_emp_ids` (people in the build carrying the pair) and `cofiling` (`TRUE`
#'   when the two forms also appear as two people with one surname on one
#'   return, e.g. `JOHN`/`JOHNNY` for a father and son).
#' @seealso [compare_first_names()], [nickname_table()]
#' @export
first_name_lexicon <- function() {
  if (!is.null(.synthid_cache$lexicon)) return(.synthid_cache$lexicon)
  f <- system.file("extdata", "first-name-variants.csv", package = "synthid")
  lx <- if (nzchar(f)) {
    utils::read.csv(f, stringsAsFactors = FALSE, na.strings = "")
  } else {
    data.frame(name_short = character(0), name_long = character(0), type = character(0), n_emp_ids = integer(0))
  }
  .synthid_cache$lexicon <- lx
  lx
}

#' Similarity the comparator gives a listed pair, by type
#'
#' A cognate is true for some people (an anglicised name) and not for others,
#' so it is weighted below a nickname or spelling.
#' @keywords internal
first_name_type_sim <- function() c(nickname = 0.95, spelling = 0.95, compound = 0.90, cognate = 0.85)

## Hashed pair lookups: legacy (nickname_table() direct pairs) and full
## (legacy plus the lexicon; a lexicon type wins over the legacy "nickname").
pair_cache <- function() {
  if (!is.null(.synthid_cache$pairs_full)) return(.synthid_cache)
  key <- function(a, b) ifelse(a < b, paste(a, b, sep = "|"), paste(b, a, sep = "|"))
  legacy <- new.env(hash = TRUE); full <- new.env(hash = TRUE); cofiling <- new.env(hash = TRUE)
  nick <- nickname_table()
  for (canon in names(nick)) for (v in nick[[canon]]) {
    k <- key(canon, v); legacy[[k]] <- "nickname"; full[[k]] <- "nickname"
  }
  lx <- first_name_lexicon()
  if (nrow(lx)) {
    k <- key(toupper(lx$name_short), toupper(lx$name_long))
    for (i in seq_along(k)) full[[k[i]]] <- lx$type[i]
    if ("cofiling" %in% names(lx)) for (kk in k[as.logical(lx$cofiling) %in% TRUE]) cofiling[[kk]] <- TRUE
  }
  .synthid_cache$pairs_legacy <- legacy
  .synthid_cache$pairs_full <- full
  .synthid_cache$pairs_cofiling <- cofiling
  .synthid_cache$pair_key <- key
  .synthid_cache
}

#' Type of a listed first-name pair (vectorised)
#'
#' Looks up each pair, in either order, in the pairs the comparator uses: the
#' direct pairs of [nickname_table()] and, unless
#' `options(synthid.first_name_lexicon = FALSE)`, the [first_name_lexicon()].
#'
#' @param a,b Equal-length upper-cased name vectors.
#' @param lexicon Use the lexicon as well as [nickname_table()].
#' @return Character vector of types (`NA` where the pair is not listed).
#' @keywords internal
first_name_pair_type <- function(a, b, lexicon = getOption("synthid.first_name_lexicon", TRUE)) {
  cc <- pair_cache()
  env <- if (isTRUE(lexicon)) cc$pairs_full else cc$pairs_legacy
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  out <- rep(NA_character_, length(a))
  ok <- nzchar(a) & nzchar(b) & a != b
  if (any(ok)) {
    hit <- mget(cc$pair_key(a[ok], b[ok]), envir = env, ifnotfound = list(NA_character_))
    out[ok] <- unlist(hit, use.names = FALSE)
  }
  out
}

#' Is a lexicon pair flagged as co-filing? (vectorised)
#' @inheritParams first_name_pair_type
#' @return Logical vector: `TRUE` where the pair is in [first_name_lexicon()]
#'   with `cofiling = TRUE`.
#' @keywords internal
first_name_pair_cofiling <- function(a, b) {
  cc <- pair_cache()
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  out <- logical(length(a))
  ok <- nzchar(a) & nzchar(b) & a != b
  if (any(ok)) {
    hit <- mget(cc$pair_key(a[ok], b[ok]), envir = cc$pairs_cofiling, ifnotfound = list(FALSE))
    out[ok] <- unlist(hit, use.names = FALSE)
  }
  out
}

#' Compare two first names, with nickname, initial, and phonetic handling
#'
#' The comparator used for the `first_name` field. Plain Jaro-Winkler is a poor
#' fit for given names in two ways: it misses nickname pairs (`BOB` vs `ROBERT`
#' score low) and it rarely drops below ~0.5 for unrelated names (so a genuinely
#' different first name adds little discriminating penalty). This comparator fixes
#' both:
#' \itemize{
#'   \item exact match -> `1`;
#'   \item a listed pair ([first_name_lexicon()] or a direct pair of
#'     [nickname_table()]) -> by type: nickname or spelling `0.95`, compound
#'     `0.9`, cognate `0.85`, or the fuzzy score below if that is higher. Only
#'     listed pairs count: two nicknames of one name (`LISA`, `BETH`) are not
#'     a pair. With `options(synthid.first_name_lexicon = FALSE)` only the
#'     [nickname_table()] pairs are used, each at `0.95` (the behaviour before
#'     the lexicon). Lexicon pairs flagged `cofiling` (forms that also appear
#'     as two people on one return, `JOHN`/`JOHNNY`, often a father and son)
#'     are capped at `0.85`; set `options(synthid.first_name_cofiling_sim = x)`
#'     for another cap, or `NA` for none;
#'   \item single initial vs full name -> `0.6` if the initial agrees, else `0`
#'     (an initial is weak evidence, neither a match nor a strong mismatch);
#'   \item otherwise Jaro-Winkler rescaled by `(jw - floor) / (1 - floor)` so
#'     unrelated names fall toward `0` and actually penalise, with a Soundex
#'     agreement raising the result to at least `0.5` (a sound-alike is not
#'     penalised, but -- because Soundex over-collapses -- is not asserted as a
#'     match either).
#' }
#'
#' @param a,b Character vectors of equal length.
#' @param floor JW rescaling floor (default `0.5`).
#' @return Numeric vector of similarities in `[0, 1]`; `NA` where a name is
#'   missing.
#' @examples
#' compare_first_names(c("BOB", "ALEXIS", "JON"), c("ROBERT", "MARCELLA", "JONATHAN"))
#' @export
compare_first_names <- function(a, b, floor = 0.5) {
  clean <- function(v) {
    v <- toupper(trimws(as.character(v)))
    v[is.na(v)] <- ""
    v
  }
  a <- clean(a); b <- clean(b)
  n <- length(a)
  out <- rep(NA_real_, n)
  ok <- nzchar(a) & nzchar(b)

  ex <- ok & a == b
  out[ex] <- 1

  rem <- ok & !ex
  ia <- nchar(a) == 1L; ib <- nchar(b) == 1L
  init <- rem & (ia | ib)
  same1 <- substr(a, 1, 1) == substr(b, 1, 1)
  out[init & same1] <- 0.6
  out[init & !same1] <- 0.0
  rem <- rem & !init

  lexicon <- isTRUE(getOption("synthid.first_name_lexicon", TRUE))
  typ <- rep(NA_character_, n)
  if (any(rem)) typ[rem] <- first_name_pair_type(a[rem], b[rem], lexicon = lexicon)
  listed <- rem & !is.na(typ)
  if (!lexicon) {                                  # legacy: a listed pair is 0.95, nothing else
    out[listed] <- 0.95
    rem <- rem & !listed
  }

  if (any(rem)) {
    jw <- 1 - stringdist::stringdist(a[rem], b[rem], method = "jw")
    base <- pmin(pmax((jw - floor) / (1 - floor), 0), 1)
    alpha <- function(v) gsub("[^A-Z]", "", v)
    pa <- suppressWarnings(stringdist::phonetic(alpha(a[rem])))
    pb <- suppressWarnings(stringdist::phonetic(alpha(b[rem])))
    phon <- !is.na(pa) & !is.na(pb) & pa == pb
    base[phon] <- pmax(base[phon], 0.5)
    ts <- unname(first_name_type_sim()[typ[rem]])
    ts[is.na(ts)] <- 0
    cap <- getOption("synthid.first_name_cofiling_sim", 0.85)
    if (!is.na(cap)) {                             # co-filing pairs (JOHN/JOHNNY) capped lower
      cf <- first_name_pair_cofiling(a[rem], b[rem])
      ts[cf] <- pmin(ts[cf], cap)
    }
    out[rem] <- pmax(base, ts)
  }
  out
}
