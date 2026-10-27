# Batched, restartable linkage for panels too large to link in one session.
#
# link_panel() links within an organization, so the panel can be cut into
# batches of whole organizations and each batch linked on its own. A batch
# holds every year of its organizations, which is all within-org linkage needs.
# batch_panel() writes the batch files; link_panel_batched() links each one
# (and optionally builds its person profiles) in its own R process, records
# every start / finish / failure in a run ledger, and on a re-run picks up
# where an earlier run stopped.

#' Assign records to batches by a key
#'
#' Maps each key (normally the EIN) to a batch id, a zero-padded string such as
#' `"07"`. Every record of a key lands in the same batch, so a batch always holds
#' whole organizations.
#'
#' Two methods:
#' * `"digits"`: the key's last digits, when `n_batches` is a power of ten
#'   (100 batches = the last two digits). Readable and stable: an EIN's batch can
#'   be read off the EIN itself.
#' * `"hash"`: a stable hash of the key modulo `n_batches`, for any key or batch
#'   count. Deterministic across sessions and machines.
#'
#' `"auto"` uses `"digits"` when `n_batches` is a power of ten and every key ends
#' in enough digits, and `"hash"` otherwise.
#'
#' @param key A vector of keys; missing values are an error.
#' @param n_batches Number of batches.
#' @param method `"auto"`, `"digits"` or `"hash"`.
#' @return A character vector of batch ids, one per element of `key`.
#' @examples
#' batch_id(c("13-6227614", "23-7047397"), 100)
#' batch_id(c("SMITH", "JONES"), 16)
#' @export
batch_id <- function(key, n_batches = 100L, method = c("auto", "digits", "hash")) {
  method <- match.arg(method)
  n_batches <- as.integer(n_batches)
  if (length(n_batches) != 1L || is.na(n_batches) || n_batches < 1L) {
    stop("batch_id(): n_batches must be a positive integer.", call. = FALSE)
  }
  key <- as.character(key)
  if (anyNA(key) || any(!nzchar(key))) {
    stop("batch_id(): key has missing or empty values; every record needs a batch.", call. = FALSE)
  }
  width <- nchar(as.character(n_batches - 1L))
  k <- log10(n_batches)
  pow10 <- n_batches > 1L && abs(k - round(k)) < 1e-9
  digit_ok <- pow10 && all(grepl(sprintf("[0-9]{%d}$", as.integer(round(k))), key))
  if (method == "auto") method <- if (digit_ok) "digits" else "hash"
  if (method == "digits") {
    if (!pow10) stop("batch_id(): method = \"digits\" needs n_batches to be a power of ten.", call. = FALSE)
    if (!digit_ok) stop("batch_id(): method = \"digits\" needs every key to end in ",
                        round(k), " digit(s).", call. = FALSE)
    return(substr(key, nchar(key) - width + 1L, nchar(key)))
  }
  # Hash each distinct key once: the first 7 hex digits fit in an integer.
  u <- unique(key)
  h <- vapply(u, function(x) strtoi(substr(rlang::hash(x), 1L, 7L), 16L), integer(1), USE.NAMES = FALSE)
  sprintf(paste0("%0", width, "d"), (h %% n_batches)[match(key, u)])
}

#' Split a panel into batch files of whole organizations
#'
#' Writes one parquet file per batch, `dir/<prefix>=NN/part-0.parquet`, the
#' input to [link_panel_batched()]. Records are assigned with [batch_id()] on the
#' organization id, so a batch holds every year of each of its organizations.
#'
#' Every record needs a stable row id (`id_col`) so linked results can be joined
#' back to the records. If it is missing and the panel has the source keys
#' `OBJECTID` and `TABLE_ID` (see [synthid_cols()]), it is built with
#' [person_year_id()].
#'
#' @param df The stacked panel: a data frame, or a character vector of parquet
#'   files to read and stack (needs \pkg{arrow}).
#' @param dir Folder to write the batches into.
#' @param n_batches Number of batches.
#' @param cols Column mapping; see [synthid_cols()].
#' @param by Column to batch on (default: the organization id). Any column that
#'   keeps each organization whole is safe for [link_panel()].
#' @param method Passed to [batch_id()].
#' @param id_col Stable per-record id column.
#' @param prefix Folder name prefix, `"batch"` gives `batch=00`.
#' @param overwrite Allow writing into a `dir` that already has batch folders.
#'   Folders of batches that no longer exist (a smaller `n_batches`) are removed.
#' @return A data frame with one row per batch: `batch`, `records`, `orgs`,
#'   `max_org` (records in its largest organization) and `cost` (the sum over
#'   organizations of records squared, which is what drives linkage time).
#'   Also written to `dir/batches.csv`.
#' @seealso [link_panel_batched()]
#' @export
batch_panel <- function(df, dir, n_batches = 100L, cols = synthid_cols(), by = cols$org_id,
                        method = c("auto", "digits", "hash"), id_col = "PERSON_YEAR_ID",
                        prefix = "batch", overwrite = FALSE) {
  need_pkg("arrow", "batch_panel()")
  method <- match.arg(method)
  if (is.character(df)) {
    tbl <- arrow::Scanner$create(arrow::open_dataset(df))$ToTable()
    df <- as.data.frame(tbl)
    rm(tbl)
  }
  if (!is.data.frame(df)) stop("batch_panel(): df must be a data frame or parquet file paths.", call. = FALSE)
  if (!by %in% names(df)) stop("batch_panel(): column '", by, "' not found.", call. = FALSE)
  if (!id_col %in% names(df)) {
    if (all(c(cols$object_id, cols$table_id) %in% names(df))) {
      df[[id_col]] <- person_year_id(df[[cols$object_id]], df[[cols$table_id]])
    } else {
      stop("batch_panel(): no '", id_col, "' column, and no ", cols$object_id, "/", cols$table_id,
           " to build it from. Add a unique id per record.", call. = FALSE)
    }
  }
  if (anyDuplicated(df[[id_col]])) stop("batch_panel(): '", id_col, "' is not unique.", call. = FALSE)

  old <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  old <- old[startsWith(basename(old), paste0(prefix, "="))]
  if (length(old) && !overwrite) {
    stop("batch_panel(): ", dir, " already has ", length(old), " batch folder(s); ",
         "set overwrite = TRUE to replace them.", call. = FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  b <- batch_id(df[[by]], n_batches, method)
  idx <- split(seq_len(nrow(df)), b)
  org <- as.character(df[[cols$org_id]])
  yr <- if (cols$year %in% names(df)) df[[cols$year]] else NULL
  summ <- do.call(rbind, lapply(names(idx), function(k) {
    i <- idx[[k]]
    i <- if (is.null(yr)) i[order(org[i])] else i[order(org[i], yr[i])]
    write_parquet_atomic(df[i, , drop = FALSE], batch_file(dir, prefix, k))
    n <- as.numeric(table(org[i]))
    data.frame(batch = k, records = length(i), orgs = length(n), max_org = max(n), cost = sum(n^2),
               stringsAsFactors = FALSE)
  }))
  stale <- setdiff(basename(old), paste0(prefix, "=", names(idx)))
  if (length(stale)) unlink(file.path(dir, stale), recursive = TRUE)
  utils::write.csv(summ, file.path(dir, "batches.csv"), row.names = FALSE)
  summ
}

#' Link a batched panel, one batch per R process, with restart
#'
#' Runs [link_panel()] on every batch written by [batch_panel()], and optionally
#' [build_person_profile()] on the result, which is the input to
#' cross-organization linkage. Each batch runs in a fresh R process (via
#' \pkg{callr}), so memory is returned to the system after every batch, and up
#' to `workers` batches run at once. The costliest batches start first.
#'
#' Outputs, under `out_dir`:
#' * `emp-map/<prefix>=NN/part-0.parquet`: `id_col`, organization id, year,
#'   `EMP_ID`, `EMP_ANCHOR`, `EMP_N_RECORDS`, `EMP_N_YEARS` (and
#'   `EMP_LINK_CONF` for `method = "em"`). Join it to the records on `id_col`.
#' * `profiles/<prefix>=NN/part-0.parquet`: one row per person (with
#'   `"profile"` in `steps`).
#' * `runs/<prefix>=NN.rds`: counts, timings and package versions per batch.
#' * `run-ledger.csv`: one row per batch start, finish, failure or
#'   interruption, with the time and process id.
#'
#' @section Restart:
#' Every output is written under a temporary name and renamed into place once
#' complete, so an output file is either absent or whole, and a batch is done
#' when its output exists. With `resume = TRUE` (the default), a re-run:
#' 1. removes temporary files left by a crash;
#' 2. reads the ledger, and marks as `"interrupted"` every batch whose last
#'    event is `"started"` by a process that no longer exists (a shutdown,
#'    a crash, or a killed session);
#' 3. skips batches whose outputs exist, and batches still running in another
#'    live process;
#' 4. re-runs the rest: never-started, interrupted and (with
#'    `retry_failed = TRUE`) failed batches.
#'
#' So after any interruption, call the function again with the same arguments.
#' [batch_status()] shows where a run stands without starting anything.
#' With `"profile"` in `steps` and an existing emp-map, only the profile is
#' built, from the emp-map and the batch file; the linkage is not repeated.
#'
#' @param dir Folder of batch files from [batch_panel()].
#' @param out_dir Folder for the outputs, ledger and run records.
#' @param steps `"link"`, `"profile"`, or both.
#' @param batches Batch ids to run (default: every batch in `dir`).
#' @param workers Batches to run at once.
#' @param resume Skip batches already done (see Restart). `FALSE` re-runs every
#'   selected batch and overwrites its outputs.
#' @param retry_failed On resume, re-run batches whose last run failed.
#' @param min_free_gb Start a new batch only while at least this much memory is
#'   free (needs \pkg{ps}; `0` turns the check off).
#' @param isolate Run each batch in its own R process (needs \pkg{callr}).
#'   `FALSE` runs the batches one after another in this session.
#' @param cols Column mapping; see [synthid_cols()].
#' @param id_col Stable per-record id column (see [batch_panel()]).
#' @param prefix Batch folder prefix used by [batch_panel()].
#' @param link_args,profile_args Lists of further arguments to [link_panel()]
#'   and [build_person_profile()], e.g.
#'   `profile_args = list(state = "org.state", ntee = "org.ntee")`.
#' @param verbose Print progress.
#' @return A data frame with one row per selected batch: `batch`, `status`
#'   (`"done"`, `"failed"`, `"complete"` = skipped as already done,
#'   `"skipped: failed earlier"`, `"skipped: running elsewhere"`,
#'   `"skipped: no emp-map"`), `records`, `persons`, `link_secs`,
#'   `profile_secs` and `error`.
#' @seealso [batch_panel()], [batch_status()]
#' @examples
#' \dontrun{
#' batch_panel(panel, "batches", n_batches = 100)
#' res <- link_panel_batched("batches", "linked", workers = 8, min_free_gb = 12,
#'                           profile_args = list(state = "org.state", ntee = "org.ntee"))
#' # after a crash or restart, the same call picks up where it stopped:
#' res <- link_panel_batched("batches", "linked", workers = 8, min_free_gb = 12,
#'                           profile_args = list(state = "org.state", ntee = "org.ntee"))
#' batch_status("linked")
#' }
#' @export
link_panel_batched <- function(dir, out_dir, steps = c("link", "profile"), batches = NULL,
                               workers = 1L, resume = TRUE, retry_failed = TRUE,
                               min_free_gb = 0, isolate = TRUE, cols = synthid_cols(),
                               id_col = "PERSON_YEAR_ID", prefix = "batch",
                               link_args = list(), profile_args = list(), verbose = TRUE) {
  need_pkg("arrow", "link_panel_batched()")
  steps <- match.arg(steps, c("link", "profile"), several.ok = TRUE)
  if (isolate) need_pkg("callr", "link_panel_batched(isolate = TRUE)")
  say <- function(...) if (verbose) message(format(Sys.time(), "%H:%M:%S"), " | ", sprintf(...))

  all_b <- list_batches(dir, prefix)
  if (!length(all_b)) stop("link_panel_batched(): no ", prefix, "=NN folders in ", dir, call. = FALSE)
  if (is.null(batches)) batches <- all_b
  batches <- as.character(batches)
  miss <- setdiff(batches, all_b)
  if (length(miss)) stop("link_panel_batched(): no batch file for ", paste(miss, collapse = " "), call. = FALSE)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  f_in   <- function(b) batch_file(dir, prefix, b)
  f_map  <- function(b) batch_file(file.path(out_dir, "emp-map"), prefix, b)
  f_prof <- function(b) batch_file(file.path(out_dir, "profiles"), prefix, b)
  f_run  <- function(b) file.path(out_dir, "runs", paste0(prefix, "=", b, ".rds"))
  dir.create(file.path(out_dir, "runs"), recursive = TRUE, showWarnings = FALSE)

  # ---- read the ledger: what an earlier run left -------------------------------
  interrupted <- ledger_recover(out_dir, say)
  last <- ledger_last(out_dir)
  last <- last[last$batch %in% batches, , drop = FALSE]
  elsewhere <- last$batch[last$event == "started" & vapply(last$pid, pid_alive, logical(1))]
  failed_before <- last$batch[last$event == "failed"]

  # ---- decide what each batch needs ----------------------------------------------
  has_map <- file.exists(vapply(batches, f_map, ""))
  has_prof <- file.exists(vapply(batches, f_prof, ""))
  need_link <- "link" %in% steps & (!resume | !has_map)
  need_prof <- "profile" %in% steps & (!resume | !has_prof | need_link)
  status <- ifelse(need_link | need_prof, "todo", "complete")
  status[status == "todo" & !need_link & !has_map] <- "skipped: no emp-map"
  # A batch another live runner is working on is never started twice.
  status[status == "todo" & batches %in% elsewhere] <- "skipped: running elsewhere"
  if (resume && !retry_failed) status[status == "todo" & batches %in% failed_before] <- "skipped: failed earlier"
  res <- data.frame(batch = batches, status = status, records = NA_integer_, persons = NA_integer_,
                    link_secs = NA_real_, profile_secs = NA_real_, error = NA_character_,
                    stringsAsFactors = FALSE)
  todo <- batches[status == "todo"]
  say("%d of %d batches to run (%d complete, %d interrupted earlier, %d skipped), %d worker(s)",
      length(todo), length(batches), sum(status == "complete"), length(intersect(interrupted, todo)),
      sum(startsWith(status, "skipped")), if (isolate) workers else 1L)
  if (!length(todo)) return(res)

  # ---- costliest first ------------------------------------------------------------
  cost <- vapply(todo, function(b) {
    if (need_link[match(b, batches)]) {
      n <- as.numeric(table(as.data.frame(arrow::read_parquet(f_in(b), col_select = cols$org_id))[[1]]))
      sum(n^2)
    } else as.numeric(arrow::ParquetFileReader$create(f_in(b))$num_rows)
  }, 0)
  queue <- todo[order(-cost)]

  versions <- vapply(c("synthid", "titleclassifier", "peopleparser"), function(p)
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), "")
  job <- function(b) {
    i <- match(b, batches)
    list(b = b, f_in = f_in(b), f_map = f_map(b), f_prof = f_prof(b), f_run = f_run(b),
         do_link = need_link[i], do_profile = need_prof[i], cols = cols, id_col = id_col,
         link_args = link_args, profile_args = profile_args, versions = versions,
         write_atomic = write_parquet_atomic)
  }
  step_of <- function(b) { i <- match(b, batches); paste(c("link", "profile")[c(need_link[i], need_prof[i])], collapse = "+") }
  record <- function(b, r, pid) {
    i <- match(b, res$batch)
    if (is.null(r$error)) {
      res[i, c("status", "records", "persons", "link_secs", "profile_secs")] <<-
        list("done", r$records, r$persons, r$link_secs, r$profile_secs)
      ledger_append(out_dir, step_of(b), b, "done", pid,
                    sprintf("%d records, %d persons", r$records, r$persons))
      say("batch %s done: %d records -> %d persons", b, r$records, r$persons)
    } else {
      res$status[i] <<- "failed"; res$error[i] <<- r$error
      ledger_append(out_dir, step_of(b), b, "failed", pid, r$error)
      say("batch %s FAILED: %s", b, r$error)
    }
  }

  # ---- in this session, one at a time ---------------------------------------------
  if (!isolate) {
    for (b in queue) {
      ledger_append(out_dir, step_of(b), b, "started", Sys.getpid())
      done <- FALSE
      on.exit(if (!done) ledger_append(out_dir, step_of(b), b, "interrupted", Sys.getpid(),
                                       "stopped in this session"), add = FALSE)
      r <- tryCatch(do.call(run_batch, job(b)), error = function(e) list(error = conditionMessage(e)))
      done <- TRUE
      record(b, r, Sys.getpid())
    }
    on.exit()
    return(res)
  }

  # ---- one fresh R process per batch ---------------------------------------------
  use_ps <- min_free_gb > 0 && requireNamespace("ps", quietly = TRUE)
  free_gb <- function() if (use_ps) ps::ps_system_memory()$avail / 1e9 else Inf
  worker <- run_batch
  environment(worker) <- globalenv()   # self-contained: callr sends it to the child
  running <- list()
  # If this session is stopped (Ctrl-C, error), stop the children and say so in
  # the ledger; a hard kill of the session is caught by the next run instead.
  on.exit({
    for (b in names(running)) {
      p <- running[[b]]
      if (p$is_alive()) {
        p$kill()
        ledger_append(out_dir, step_of(b), b, "interrupted", p$get_pid(), "runner stopped")
      }
    }
  }, add = TRUE)
  while (length(queue) || length(running)) {
    for (b in names(running)) {
      p <- running[[b]]
      if (p$is_alive()) next
      r <- tryCatch(p$get_result(), error = function(e) list(error = conditionMessage(e)))
      record(b, r, p$get_pid())
      running[[b]] <- NULL
    }
    while (length(queue) && length(running) < workers && free_gb() > min_free_gb) {
      b <- queue[1]; queue <- queue[-1]
      args <- job(b)
      environment(args$write_atomic) <- globalenv()
      running[[b]] <- callr::r_bg(worker, args = args, supervise = TRUE)
      ledger_append(out_dir, step_of(b), b, "started", running[[b]]$get_pid())
      say("batch %s started (%d running%s)", b, length(running),
          if (use_ps) sprintf(", %.0f GB free", free_gb()) else "")
      if (use_ps) Sys.sleep(5)   # let it load its batch before checking memory again
    }
    if (length(running)) Sys.sleep(1)
  }
  res
}

#' Where a batched linkage run stands
#'
#' Reads the run ledger written by [link_panel_batched()] and the output folders,
#' without starting anything.
#'
#' @param out_dir The `out_dir` of [link_panel_batched()].
#' @param prefix Batch folder prefix.
#' @return A data frame with one row per batch in the ledger or with an output:
#'   `batch`, `has_map`, `has_profile`, and the batch's last ledger event
#'   (`time`, `step`, `event`, `pid`, `detail`), plus `state`: `"done"`,
#'   `"running"` (started by a live process), `"interrupted"` (started by a
#'   process that no longer exists, or marked so), `"failed"`, or `"not run"`.
#' @export
batch_status <- function(out_dir, prefix = "batch") {
  last <- ledger_last(out_dir)
  maps <- list_batches(file.path(out_dir, "emp-map"), prefix)
  profs <- list_batches(file.path(out_dir, "profiles"), prefix)
  b <- sort(unique(c(last$batch, maps, profs)))
  out <- data.frame(batch = b, has_map = b %in% maps, has_profile = b %in% profs,
                    stringsAsFactors = FALSE)
  m <- match(b, last$batch)
  for (k in c("time", "step", "event", "pid", "detail")) out[[k]] <- last[[k]][m]
  alive <- vapply(out$pid, pid_alive, logical(1))
  out$state <- ifelse(is.na(out$event), ifelse(out$has_map, "done", "not run"),
               ifelse(out$event == "started", ifelse(alive, "running", "interrupted"),
               ifelse(out$event == "done", "done", out$event)))
  out
}

# ---- internals -------------------------------------------------------------------

need_pkg <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(what, " needs the '", pkg, "' package: install.packages(\"", pkg, "\")", call. = FALSE)
  }
}

batch_file <- function(root, prefix, b) file.path(root, paste0(prefix, "=", b), "part-0.parquet")

list_batches <- function(root, prefix) {
  f <- Sys.glob(file.path(root, paste0(prefix, "=*"), "part-0.parquet"))
  sort(sub(paste0("^", prefix, "="), "", basename(dirname(f))))
}

# Write a parquet file whole or not at all: write to a temporary name in the same
# folder, then rename. Self-contained (namespaced calls only) so it can be sent
# to a callr child.
write_parquet_atomic <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  arrow::write_parquet(x, tmp, ...)
  if (file.exists(path)) file.remove(path)
  if (!file.rename(tmp, path)) stop("could not rename ", tmp, " to ", path)
  invisible(path)
}

ledger_file <- function(out_dir) file.path(out_dir, "run-ledger.csv")

ledger_append <- function(out_dir, step, batch, event, pid, detail = "") {
  f <- ledger_file(out_dir)
  row <- data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), step = step, batch = batch,
                    event = event, pid = as.integer(pid), detail = gsub("[\r\n]+", " ", detail),
                    stringsAsFactors = FALSE)
  utils::write.table(row, f, sep = ",", append = file.exists(f), col.names = !file.exists(f),
                     row.names = FALSE, qmethod = "double")
}

ledger_read <- function(out_dir) {
  f <- ledger_file(out_dir)
  empty <- data.frame(time = character(0), step = character(0), batch = character(0),
                      event = character(0), pid = integer(0), detail = character(0),
                      stringsAsFactors = FALSE)
  if (!file.exists(f)) return(empty)
  lg <- utils::read.csv(f, colClasses = "character", na.strings = character(0))
  lg$pid <- suppressWarnings(as.integer(lg$pid))
  lg
}

# The last ledger event of each batch.
ledger_last <- function(out_dir) {
  lg <- ledger_read(out_dir)
  lg[!duplicated(lg$batch, fromLast = TRUE), , drop = FALSE]
}

pid_alive <- function(pid) {
  if (is.na(pid)) return(FALSE)
  if (requireNamespace("ps", quietly = TRUE)) {
    return(tryCatch(ps::ps_is_running(ps::ps_handle(as.integer(pid))), error = function(e) FALSE))
  }
  identical(as.integer(pid), Sys.getpid())
}

# Clear temp files left by a crash, and mark every batch whose last event is
# "started" by a dead process as interrupted. Returns those batch ids.
ledger_recover <- function(out_dir, say = function(...) NULL) {
  tmp <- list.files(out_dir, pattern = "[.]tmp-[0-9]+$", recursive = TRUE, full.names = TRUE)
  if (length(tmp)) {
    file.remove(tmp)
    say("removed %d partial file(s) left by an earlier run", length(tmp))
  }
  last <- ledger_last(out_dir)
  hung <- last[last$event == "started" & !vapply(last$pid, pid_alive, logical(1)), , drop = FALSE]
  for (i in seq_len(nrow(hung))) {
    ledger_append(out_dir, hung$step[i], hung$batch[i], "interrupted", hung$pid[i],
                  paste("started", hung$time[i], "and never finished"))
  }
  if (nrow(hung)) say("%d batch(es) were interrupted by an earlier run and will be re-run: %s",
                      nrow(hung), paste(hung$batch, collapse = " "))
  hung$batch
}

# One batch: link and/or profile. Self-contained -- it runs in a callr child,
# which sees none of this namespace's internals -- so it calls only exported
# synthid functions and the helpers passed to it.
run_batch <- function(b, f_in, f_map, f_prof, f_run, do_link, do_profile, cols, id_col,
                      link_args, profile_args, versions, write_atomic) {
  secs <- function(t) as.numeric(difftime(Sys.time(), t, units = "secs"))
  df <- as.data.frame(arrow::read_parquet(f_in))
  if (!id_col %in% names(df)) stop("batch ", b, ": no '", id_col, "' column")
  emp_cols <- c("EMP_ID", "EMP_ANCHOR", "EMP_N_RECORDS", "EMP_N_YEARS", "EMP_LINK_CONF")
  t_link <- NA_real_
  if (do_link) {
    t0 <- Sys.time()
    lk <- do.call(synthid::link_panel, c(list(df, cols = cols), link_args))
    t_link <- secs(t0)
    if (nrow(lk) != nrow(df) || anyDuplicated(lk[[id_col]])) {
      stop("batch ", b, ": link_panel() returned ", nrow(lk), " rows for ", nrow(df))
    }
    keep <- intersect(c(id_col, cols$org_id, cols$year, emp_cols), names(lk))
    map <- lk[, keep, drop = FALSE]
  } else {
    map <- as.data.frame(arrow::read_parquet(f_map))
    i <- match(df[[id_col]], map[[id_col]])
    if (nrow(map) != nrow(df) || anyNA(i)) {
      stop("batch ", b, ": emp-map has ", nrow(map), " rows for ", nrow(df), " records, ",
           sum(is.na(i)), " records unmatched -- the emp-map is from other batch files")
    }
    lk <- df
    for (k in intersect(emp_cols, names(map))) lk[[k]] <- map[[k]][i]
  }
  t_prof <- NA_real_
  n_persons <- length(unique(lk$EMP_ID))
  if (do_profile) {
    t0 <- Sys.time()
    prof <- do.call(synthid::build_person_profile, c(list(lk, cols = cols), profile_args))
    t_prof <- secs(t0)
    if (nrow(prof) != n_persons) stop("batch ", b, ": ", nrow(prof), " profiles for ", n_persons, " persons")
    write_atomic(prof, f_prof)
  }
  # The emp-map last: once it exists, the batch is complete.
  if (do_link) write_atomic(map, f_map)
  run <- list(batch = b, records = nrow(df), persons = n_persons, link_secs = t_link,
              profile_secs = t_prof, versions = versions, date = Sys.time())
  dir.create(dirname(f_run), recursive = TRUE, showWarnings = FALSE)
  saveRDS(run, f_run)
  run[c("records", "persons", "link_secs", "profile_secs")]
}
