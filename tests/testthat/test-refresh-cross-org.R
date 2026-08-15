## Cross-org refresh: anchored XORG_IDs survive a wave, and the diff surfaces
## the new / grown interlocks.

rec_x <- function(ein, yr, first, last, gender, title, oid, tid) {
  data.frame(
    ein = ein, taxyr = yr, name = trimws(paste(first, last)),
    salutation = NA_character_, first_name = first, middle_name = NA_character_,
    last_name = last, suffix = "", gender = gender, title.standard = title,
    OBJECTID = oid, TABLE_ID = tid, stringsAsFactors = FALSE
  )
}

test_that("anchor_xorg_id is deterministic, prefixed, and off the legacy keyspace", {
  a <- anchor_xorg_id(c("EMP-1", "EMP-2", "EMP-1"))
  expect_equal(a[1], a[3])
  expect_false(a[1] == a[2])
  expect_true(all(grepl("^XORG-", a)))
  expect_equal(anchor_xorg_id("EMP-1"), anchor_xorg_id("EMP-1"))
})

test_that("an interlock's XORG_ID is anchored to its founding (earliest) member and survives a new member", {
  # GANTSOUDES (rare surname) sits on org 100 (from 2019) and org 200 (from 2020):
  # one cross-org person spanning two orgs.
  base <- link_panel(rbind(
    rec_x("100", 2019, "MARIA", "GANTSOUDES", "F", "CEO", "O1", "T1"),
    rec_x("100", 2020, "MARIA", "GANTSOUDES", "F", "CEO", "O2", "T1"),
    rec_x("200", 2020, "MARIA", "GANTSOUDES", "F", "DIRECTOR", "O3", "T2")
  ))
  xo0 <- link_cross_org(base)
  gx <- xo0$assignment[xo0$assignment$XORG_N_ORGS > 1, ]
  expect_gt(nrow(gx), 0)                       # the interlock exists
  xid0 <- unique(gx$XORG_ID)
  expect_length(xid0, 1L)

  # A THIRD org (300, from 2021) adds Maria to the same interlock cluster.
  grown <- link_panel(rbind(
    rec_x("100", 2019, "MARIA", "GANTSOUDES", "F", "CEO", "O1", "T1"),
    rec_x("100", 2020, "MARIA", "GANTSOUDES", "F", "CEO", "O2", "T1"),
    rec_x("200", 2020, "MARIA", "GANTSOUDES", "F", "DIRECTOR", "O3", "T2"),
    rec_x("300", 2021, "MARIA", "GANTSOUDES", "F", "TRUSTEE", "O4", "T3")
  ))
  xo1 <- link_cross_org(grown)
  gx1 <- xo1$assignment[xo1$assignment$XORG_N_ORGS > 1, ]
  xid1 <- unique(gx1$XORG_ID)

  # The founding member (org-100, first_year 2019) still anchors the cluster, so
  # the XORG_ID is unchanged even though a member (and an org) was added.
  expect_length(xid1, 1L)
  expect_identical(xid1, xid0)
  expect_equal(unique(gx1$XORG_N_ORGS), 3L)
})

test_that("refresh_cross_org reports new and newly-interlocking people vs a prior run", {
  existing <- link_panel(rbind(
    rec_x("100", 2019, "MARIA", "GANTSOUDES", "F", "CEO", "O1", "T1"),
    rec_x("200", 2019, "PAUL",  "OKONKWO",   "M", "DIRECTOR", "O2", "T2")
  ))
  xo0 <- link_cross_org(existing)                 # no interlocks yet
  expect_equal(sum(xo0$assignment$XORG_N_ORGS > 1), 0L)

  # wave: Maria joins org 200's board (now interlocks), and a brand-new person appears.
  wave_stamped <- link_incremental(existing, rbind(
    rec_x("200", 2020, "MARIA", "GANTSOUDES", "F", "TRUSTEE", "O3", "T5"),  # interlock
    rec_x("300", 2020, "SVEN",  "HAALAND",    "M", "CEO", "O4", "T6")       # new person/org
  ))$new_stamped
  merged <- rbind(existing[names(wave_stamped)], wave_stamped)

  ref <- refresh_cross_org(merged, prior = xo0)
  expect_false(is.null(ref$changes))
  expect_gte(ref$changes$n_interlocking_after, 2L)   # Maria now spans 2 orgs
  expect_gt(ref$changes$n_interlocking_after, ref$changes$n_interlocking_before)
  # Maria (org-100 EMP_ID) went from non-interlocking to interlocking.
  maria_100 <- existing$EMP_ID[existing$ein == "100" & existing$last_name == "GANTSOUDES"]
  expect_true(maria_100 %in% ref$changes$newly_interlocking_persons)
})

test_that("refresh_cross_org requires a linked panel", {
  raw <- rec_x("100", 2019, "MARIA", "GANTSOUDES", "F", "CEO", "O1", "T1")
  expect_error(refresh_cross_org(raw), "EMP_ID")
})
