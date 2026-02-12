test_that("generate_demo_evidence returns valid evidence", {
  skip_if_not_installed("r4subcore")
  ev <- generate_demo_evidence(n_rows = 20)

  expect_true(is.data.frame(ev))
  expect_equal(nrow(ev), 20)
  expect_true(r4subcore::validate_evidence(ev))
})

test_that("generate_demo_evidence covers all domains", {
  skip_if_not_installed("r4subcore")
  ev <- generate_demo_evidence(n_rows = 100)

  domains <- unique(ev$indicator_domain)
  expect_true("quality" %in% domains)
  expect_true("trace" %in% domains)
  expect_true("risk" %in% domains)
  expect_true("usability" %in% domains)
})

test_that("generate_demo_evidence is reproducible (seed)", {
  skip_if_not_installed("r4subcore")
  ev1 <- generate_demo_evidence(n_rows = 10)
  ev2 <- generate_demo_evidence(n_rows = 10)

  # Same seed = same data (ignoring timestamps)
  expect_equal(ev1$indicator_id, ev2$indicator_id)
  expect_equal(ev1$severity, ev2$severity)
  expect_equal(ev1$result, ev2$result)
})

test_that("generate_demo_evidence respects study_id", {
  skip_if_not_installed("r4subcore")
  ev <- generate_demo_evidence(n_rows = 5, study_id = "MY-STUDY")
  expect_true(all(ev$study_id == "MY-STUDY"))
})
