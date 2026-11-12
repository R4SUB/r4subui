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
