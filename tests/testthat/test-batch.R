# Six organizations, two per batch with 10 batches by EIN digit ("1", "2", "3").
batched_panel <- function() {
  p <- make_panel()
  out <- do.call(rbind, lapply(1:3, function(k) { q <- p; q$ein <- paste0(q$ein, k); q }))
  out$PERSON_YEAR_ID <- sprintf("PY%03d", seq_len(nrow(out)))
  out
}
tmp_dir <- function() { d <- tempfile("synthid-batch-"); dir.create(d); d }
by_id <- function(map, panel) map$EMP_ID[match(panel$PERSON_YEAR_ID, map$PERSON_YEAR_ID)]
read_map <- function(out, b) as.data.frame(arrow::read_parquet(file.path(out, "emp-map", paste0("batch=", b), "part-0.parquet")))

test_that("batch_id uses EIN digits or a stable hash, and keeps keys whole", {
  expect_equal(batch_id(c("13-6227614", "23-7047397", "1000"), 100), c("14", "97", "00"))
  expect_equal(batch_id(c("5", "15"), 10), c("5", "5"))
  h <- batch_id(c("SMITH", "JONES", "SMITH"), 16)
  expect_equal(h[1], h[3])
  expect_true(all(as.integer(h) %in% 0:15))
  expect_equal(nchar(h), c(2L, 2L, 2L))
  expect_identical(h, batch_id(c("SMITH", "JONES", "SMITH"), 16))   # deterministic
  expect_equal(batch_id("ABC", 100), batch_id("ABC", 100, "hash"))   # no digits -> hash
  expect_error(batch_id(c("1", NA), 10), "missing")
  expect_error(batch_id("AB", 100, "digits"), "digit")
  expect_error(batch_id("12", 16, "digits"), "power of ten")
})

test_that("batch_panel writes whole organizations, one file per batch", {
  skip_if_not_installed("arrow")
  p <- batched_panel(); d <- tmp_dir(); on.exit(unlink(d, recursive = TRUE))
  s <- batch_panel(p, d, n_batches = 10)
  expect_equal(s$batch, c("1", "2", "3"))
  expect_equal(sum(s$records), nrow(p))
  expect_equal(s$orgs, c(2L, 2L, 2L))
  expect_true(file.exists(file.path(d, "batches.csv")))
  back <- do.call(rbind, lapply(s$batch, function(b)
    as.data.frame(arrow::read_parquet(file.path(d, paste0("batch=", b), "part-0.parquet")))))
  expect_setequal(back$PERSON_YEAR_ID, p$PERSON_YEAR_ID)
  expect_true(all(tapply(batch_id(back$ein, 10), back$ein, function(x) length(unique(x))) == 1))
  expect_error(batch_panel(p, d, n_batches = 10), "overwrite")
  p$PERSON_YEAR_ID <- NULL
  expect_error(batch_panel(p, tmp_dir(), n_batches = 10), "unique id")
})

test_that("batched linkage gives the same EMP_IDs as linking the whole panel", {
  skip_if_not_installed("arrow")
  p <- batched_panel(); d <- tmp_dir(); out <- tmp_dir()
  on.exit(unlink(c(d, out), recursive = TRUE))
  batch_panel(p, d, n_batches = 10)
  res <- link_panel_batched(d, out, isolate = FALSE, verbose = FALSE)
  expect_equal(res$status, rep("done", 3))
  map <- do.call(rbind, lapply(res$batch, read_map, out = out))
  whole <- link_panel(p, verbose = FALSE)
  expect_identical(by_id(map, p), whole$EMP_ID)
  prof <- file.path(out, "profiles", paste0("batch=", res$batch), "part-0.parquet")
  expect_true(all(file.exists(prof)))
  expect_equal(sum(res$persons), length(unique(whole$EMP_ID)))
  lg <- read.csv(file.path(out, "run-ledger.csv"), colClasses = "character")
  expect_equal(sort(lg$event), sort(rep(c("started", "done"), 3)))
})

test_that("a re-run picks up where an interrupted run stopped", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("ps")
  p <- batched_panel(); d <- tmp_dir(); out <- tmp_dir()
  on.exit(unlink(c(d, out), recursive = TRUE))
  batch_panel(p, d, n_batches = 10)
  link_panel_batched(d, out, isolate = FALSE, verbose = FALSE)
  first <- read_map(out, "2")

  # Nothing to do on a second call.
  again <- link_panel_batched(d, out, isolate = FALSE, verbose = FALSE)
  expect_equal(again$status, rep("complete", 3))

  # Simulate a shutdown during batch 2: its outputs gone, a partial file left,
  # and a "started" row from a process that no longer exists.
  unlink(file.path(out, c("emp-map", "profiles"), "batch=2"), recursive = TRUE)
  dir.create(file.path(out, "emp-map", "batch=2"), recursive = TRUE)
  writeLines("partial", file.path(out, "emp-map", "batch=2", "part-0.parquet.tmp-999999"))
  ledger_append(out, "link+profile", "2", "started", 999999L)
  expect_equal(batch_status(out)$state[batch_status(out)$batch == "2"], "interrupted")

  res <- link_panel_batched(d, out, isolate = FALSE, verbose = FALSE)
  expect_equal(res$status, c("complete", "done", "complete"))
  expect_identical(read_map(out, "2")$EMP_ID, first$EMP_ID)
  expect_length(list.files(out, "tmp-", recursive = TRUE), 0)
  lg <- read.csv(file.path(out, "run-ledger.csv"), colClasses = "character")
  hit <- lg[lg$batch == "2", ]
  expect_equal(tail(hit$event, 3), c("interrupted", "started", "done"))
  expect_equal(batch_status(out)$state, rep("done", 3))
})

test_that("profiles are rebuilt from an existing emp-map without relinking", {
  skip_if_not_installed("arrow")
  p <- batched_panel(); d <- tmp_dir(); out <- tmp_dir()
  on.exit(unlink(c(d, out), recursive = TRUE))
  batch_panel(p, d, n_batches = 10)
  link_panel_batched(d, out, steps = "link", isolate = FALSE, verbose = FALSE)
  expect_false(dir.exists(file.path(out, "profiles")))
  res <- link_panel_batched(d, out, steps = "profile", isolate = FALSE, verbose = FALSE)
  expect_equal(res$status, rep("done", 3))
  expect_true(all(is.na(res$link_secs)))
  expect_true(all(!is.na(res$profile_secs)))
})

test_that("a failed batch is recorded, and retried only when asked", {
  skip_if_not_installed("arrow")
  p <- batched_panel(); d <- tmp_dir(); out <- tmp_dir()
  on.exit(unlink(c(d, out), recursive = TRUE))
  batch_panel(p, d, n_batches = 10)
  f1 <- file.path(d, "batch=1", "part-0.parquet")
  bad <- as.data.frame(arrow::read_parquet(f1)); bad$name <- NULL
  invisible(gc())   # Windows: release arrow's memory map of f1 before overwriting it
  arrow::write_parquet(bad, f1)
  res <- link_panel_batched(d, out, isolate = FALSE, verbose = FALSE)
  expect_equal(res$status, c("failed", "done", "done"))
  expect_false(is.na(res$error[1]))
  expect_equal(batch_status(out)$state, c("failed", "done", "done"))
  res2 <- link_panel_batched(d, out, isolate = FALSE, retry_failed = FALSE, verbose = FALSE)
  expect_equal(res2$status, c("skipped: failed earlier", "complete", "complete"))
  res3 <- link_panel_batched(d, out, isolate = FALSE, verbose = FALSE)
  expect_equal(res3$status[1], "failed")
})

test_that("batches run in separate processes give the same result", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  skip_if_not_installed("callr")
  p <- batched_panel(); d <- tmp_dir(); out <- tmp_dir()
  on.exit(unlink(c(d, out), recursive = TRUE))
  batch_panel(p, d, n_batches = 10)
  res <- link_panel_batched(d, out, workers = 2, verbose = FALSE)
  expect_equal(res$status, rep("done", 3))
  map <- do.call(rbind, lapply(res$batch, read_map, out = out))
  expect_identical(by_id(map, p), link_panel(p, verbose = FALSE)$EMP_ID)
})
