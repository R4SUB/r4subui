test_that("compare_readiness returns the expected structure", {
  before <- suppressMessages(generate_demo_evidence(60, seed = 1))
  after  <- suppressMessages(generate_demo_evidence(60, seed = 2))

  cmp <- compare_readiness(before, after)

  expect_type(cmp, "list")
  expect_named(cmp, c("sci", "band", "n_critical", "pillars",
                      "diff", "diff_summary"))
  expect_named(cmp$sci, c("before", "after", "delta"))
  expect_named(cmp$band, c("before", "after", "changed"))
  expect_s3_class(cmp$pillars, "data.frame")
  expect_identical(names(cmp$pillars),
                   c("pillar", "score_before", "score_after", "delta", "weight"))
  expect_true(is.logical(cmp$band$changed))
})

test_that("the SCI and critical deltas match an independent computation", {
  before <- suppressMessages(generate_demo_evidence(80, seed = 10))
  after  <- suppressMessages(generate_demo_evidence(80, seed = 20))

  cmp <- compare_readiness(before, after)

  sb <- r4subscore::compute_sci(r4subscore::compute_pillar_scores(before))
  sa <- r4subscore::compute_sci(r4subscore::compute_pillar_scores(after))

  expect_equal(cmp$sci$before, sb$SCI)
  expect_equal(cmp$sci$after, sa$SCI)
  expect_equal(cmp$sci$delta, sa$SCI - sb$SCI)
  expect_equal(cmp$n_critical$delta, sa$n_critical - sb$n_critical)
  expect_equal(cmp$band$before, sb$band)
  expect_equal(cmp$band$after, sa$band)
})

test_that("comparing a snapshot with itself shows no movement", {
  ev <- suppressMessages(generate_demo_evidence(50, seed = 7))

  cmp <- compare_readiness(ev, ev)

  expect_equal(cmp$sci$delta, 0)
  expect_false(cmp$band$changed)
  expect_equal(unname(cmp$diff_summary[["n_improved"]]), 0)
  expect_equal(unname(cmp$diff_summary[["n_regressed"]]), 0)
  expect_true(all(abs(cmp$pillars$delta) < 1e-9))
})

test_that("pillars are ordered by the size of the move", {
  before <- suppressMessages(generate_demo_evidence(60, seed = 3))
  after  <- suppressMessages(generate_demo_evidence(60, seed = 4))

  cmp <- compare_readiness(before, after)
  ad <- abs(cmp$pillars$delta)
  # Non-increasing absolute deltas (NA last).
  expect_false(is.unsorted(rev(ad[!is.na(ad)])))
})

test_that("diff_summary carries the evidence_diff_summary metrics", {
  before <- suppressMessages(generate_demo_evidence(60, seed = 5))
  after  <- suppressMessages(generate_demo_evidence(60, seed = 6))

  cmp <- compare_readiness(before, after)
  expect_true(all(c("n_improved", "n_regressed", "n_added", "n_removed",
                    "net_delta") %in% names(cmp$diff_summary)))
  expect_true(is.numeric(cmp$diff_summary))
})

test_that("invalid inputs are rejected", {
  ev <- suppressMessages(generate_demo_evidence(20))
  expect_error(compare_readiness(data.frame(), ev), "baseline")
  expect_error(compare_readiness(ev, "not a data frame"), "current")
  expect_error(compare_readiness(ev, data.frame()), "current")
})

test_that("the compare server computes a comparison from a baseline", {
  before <- suppressMessages(generate_demo_evidence(60, seed = 1))
  after  <- suppressMessages(generate_demo_evidence(60, seed = 2))

  shiny::testServer(
    mod_compare_server,
    args = list(evidence_rv = shiny::reactive(after)),
    {
      # Before a baseline is set, the comparison reactive is silent.
      expect_null(baseline_store())

      # Set the baseline directly (the upload path is exercised in the app).
      baseline_store(before)
      session$flushReact()

      cmp <- comparison()
      expect_type(cmp, "list")
      expect_equal(cmp$sci$after,
                   r4subscore::compute_sci(
                     r4subscore::compute_pillar_scores(after))$SCI)

      # The headline and diff outputs render without error.
      expect_no_error(as.character(output$headline_boxes))
      expect_no_error(as.character(output$diff_boxes))
      expect_no_error(as.character(output$pillar_table))
      expect_no_error(as.character(output$changes_table))
    }
  )
})

# ---------------------------------------------------------------------------
# Additional substantive coverage for compare_readiness() (shiny-free).
# These only read engine outputs (compute_pillar_scores / compute_sci /
# evidence_diff[_summary]); they introduce no new scoring semantics.
# ---------------------------------------------------------------------------

# All-pass / all-fail transforms give a deterministic, maximal move without
# depending on the random demo seed.
.all_pass <- function(x) { x$result <- "pass"; x$severity <- "info"; x }
.all_fail <- function(x) { x$result <- "fail"; x$severity <- "critical"; x }

test_that("self-compare produces an empty diff and all-zero counts", {
  ev <- suppressMessages(generate_demo_evidence(70, seed = 42))

  cmp <- compare_readiness(ev, ev)

  # The row-level diff of a snapshot against itself is empty.
  expect_equal(nrow(cmp$diff), 0L)

  # Every movement count is exactly zero (not just improved/regressed).
  s <- cmp$diff_summary
  for (nm in c("n_added", "n_removed", "n_changed",
               "n_improved", "n_regressed", "net_delta")) {
    expect_equal(unname(s[[nm]]), 0, info = nm)
  }
  expect_equal(cmp$sci$delta, 0)
  expect_equal(cmp$n_critical$delta, 0)
  expect_false(cmp$band$changed)
})

test_that("SCI delta is positive for an improvement and negative for a regression", {
  ev   <- suppressMessages(generate_demo_evidence(60, seed = 11))
  good <- .all_pass(ev)
  bad  <- .all_fail(ev)

  up   <- compare_readiness(bad, good)   # before worse, after better
  down <- compare_readiness(good, bad)   # before better, after worse

  expect_gt(up$sci$delta, 0)
  expect_lt(down$sci$delta, 0)
  # The two directions are mirror images of the same move.
  expect_equal(up$sci$delta, -down$sci$delta)

  # The delta is exactly after - before, cross-checked against the engine.
  sb <- r4subscore::compute_sci(r4subscore::compute_pillar_scores(bad))
  sg <- r4subscore::compute_sci(r4subscore::compute_pillar_scores(good))
  expect_equal(up$sci$delta, sg$SCI - sb$SCI)

  # An improvement cannot leave more critical findings than it started with.
  expect_lte(up$n_critical$after, up$n_critical$before)
})

test_that("a decision-band move is detected only when the band actually changes", {
  ev   <- suppressMessages(generate_demo_evidence(60, seed = 12))
  good <- .all_pass(ev)
  bad  <- .all_fail(ev)

  moved <- compare_readiness(bad, good)
  expect_true(moved$band$changed)
  expect_false(identical(moved$band$before, moved$band$after))
  # The recorded bands are the engine's own bands for each snapshot.
  expect_equal(moved$band$before,
               r4subscore::compute_sci(
                 r4subscore::compute_pillar_scores(bad))$band)
  expect_equal(moved$band$after,
               r4subscore::compute_sci(
                 r4subscore::compute_pillar_scores(good))$band)

  # No change in the underlying data -> no band move.
  flat <- compare_readiness(good, good)
  expect_false(flat$band$changed)
  expect_identical(flat$band$before, flat$band$after)
})

test_that("a one-sided pillar yields an NA delta that sorts last", {
  ev <- suppressMessages(generate_demo_evidence(80, seed = 13))
  # Drop a whole pillar's indicators from the baseline so that pillar exists
  # only in the 'after' snapshot.
  dropped <- "usability"
  base_missing <- ev[ev$indicator_domain != dropped, , drop = FALSE]

  cmp <- compare_readiness(base_missing, ev)

  # The dropped pillar has no 'before' score, hence an NA delta.
  row <- cmp$pillars[cmp$pillars$pillar == dropped, , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_true(is.na(row$score_before))
  expect_true(is.na(row$delta))

  # NA deltas sort to the bottom: once an NA appears, no finite delta follows.
  d <- cmp$pillars$delta
  if (anyNA(d)) {
    first_na <- which(is.na(d))[1]
    expect_true(all(is.na(d[first_na:length(d)])))
  }
  # Among the finite deltas, |delta| is non-increasing (largest mover first).
  ad <- abs(d[!is.na(d)])
  expect_false(is.unsorted(rev(ad)))
})

test_that("improved/regressed/added/removed counts match evidence_diff_summary", {
  before <- suppressMessages(generate_demo_evidence(75, seed = 14))
  after  <- suppressMessages(generate_demo_evidence(75, seed = 15))

  cmp <- compare_readiness(before, after)

  # diff_summary is exactly the engine's summary of the engine's diff.
  d <- r4subcore::evidence_diff(before, after)
  s <- r4subcore::evidence_diff_summary(d)
  expect_equal(cmp$diff_summary, stats::setNames(s$value, s$metric))

  # The headline counts are consistent with the row-level diff it carries.
  chg <- cmp$diff$change
  dir <- cmp$diff$direction
  expect_equal(unname(cmp$diff_summary[["n_added"]]),
               sum(chg == "added"))
  expect_equal(unname(cmp$diff_summary[["n_removed"]]),
               sum(chg == "removed"))
  expect_equal(unname(cmp$diff_summary[["n_changed"]]),
               sum(chg == "changed"))
  expect_equal(unname(cmp$diff_summary[["n_improved"]]),
               sum(dir == "improved", na.rm = TRUE))
  expect_equal(unname(cmp$diff_summary[["n_regressed"]]),
               sum(dir == "regressed", na.rm = TRUE))
})

test_that("an added-only snapshot is summarised as a single addition", {
  base <- suppressMessages(generate_demo_evidence(40, seed = 16))
  cur  <- rbind(base, base[1, , drop = FALSE])
  cur$indicator_id[nrow(cur)] <- "X-ADDED-ONLY"

  cmp <- compare_readiness(base, cur)
  s <- cmp$diff_summary
  expect_equal(unname(s[["n_added"]]), 1)
  expect_equal(unname(s[["n_removed"]]), 0)
  expect_equal(unname(s[["n_changed"]]), 0)

  added <- cmp$diff[cmp$diff$change == "added", , drop = FALSE]
  expect_equal(nrow(added), 1L)
  expect_true("X-ADDED-ONLY" %in% added$indicator_id)
  # The new indicator is never reported as removed.
  expect_false(any(cmp$diff$change == "removed"))
})

test_that("a removed-only snapshot is summarised as a single removal", {
  cur  <- suppressMessages(generate_demo_evidence(40, seed = 17))
  base <- rbind(cur, cur[1, , drop = FALSE])
  base$indicator_id[nrow(base)] <- "X-REMOVED-ONLY"

  cmp <- compare_readiness(base, cur)
  s <- cmp$diff_summary
  expect_equal(unname(s[["n_removed"]]), 1)
  expect_equal(unname(s[["n_added"]]), 0)
  expect_equal(unname(s[["n_changed"]]), 0)

  removed <- cmp$diff[cmp$diff$change == "removed", , drop = FALSE]
  expect_equal(nrow(removed), 1L)
  expect_true("X-REMOVED-ONLY" %in% removed$indicator_id)
  expect_false(any(cmp$diff$change == "added"))
})

test_that("empty and NULL inputs raise informative errors", {
  ev <- suppressMessages(generate_demo_evidence(20, seed = 18))
  expect_error(compare_readiness(NULL, ev), "baseline")
  expect_error(compare_readiness(ev, NULL), "current")
  expect_error(compare_readiness(ev[0, , drop = FALSE], ev), "baseline")
  expect_error(compare_readiness(ev, ev[0, , drop = FALSE]), "current")
  # A non-data.frame object is rejected by the same guard.
  expect_error(compare_readiness(as.list(ev), ev), "baseline")
})

test_that("the compare server loads a baseline from an uploaded RDS file", {
  before <- suppressMessages(generate_demo_evidence(60, seed = 19))
  after  <- suppressMessages(generate_demo_evidence(60, seed = 20))

  rds <- tempfile(fileext = ".rds")
  saveRDS(before, rds)
  on.exit(unlink(rds), add = TRUE)

  shiny::testServer(
    mod_compare_server,
    args = list(evidence_rv = shiny::reactive(after)),
    {
      expect_null(baseline_store())

      # Drive the real upload path: a file input plus the Compare button.
      session$setInputs(
        baseline_file = data.frame(
          name = "baseline.rds", size = 1L, type = "",
          datapath = rds, stringsAsFactors = FALSE
        ),
        load_baseline = 1
      )
      session$flushReact()

      # The observeEvent read, validated, and stored the uploaded baseline.
      expect_false(is.null(baseline_store()))
      expect_equal(nrow(baseline_store()), nrow(before))

      cmp <- comparison()
      expect_type(cmp, "list")
      expect_equal(
        cmp$sci$before,
        r4subscore::compute_sci(
          r4subscore::compute_pillar_scores(before))$SCI)

      # The diff-driven outputs render once a baseline is in place.
      expect_no_error(as.character(output$headline_boxes))
      expect_no_error(as.character(output$diff_boxes))
      expect_no_error(as.character(output$changes_table))
    }
  )
})
