test_that("filter_evidence_by_indicator keeps only the chosen indicator", {
  ev <- suppressMessages(generate_demo_evidence(60))
  id <- ev$indicator_id[[1]]

  sub <- filter_evidence_by_indicator(ev, id)

  expect_s3_class(sub, "data.frame")
  expect_identical(names(sub), names(ev))
  expect_true(all(sub$indicator_id == id))
  expect_equal(nrow(sub), sum(ev$indicator_id == id))
})

test_that("a NULL, NA, or empty selection returns the evidence unchanged", {
  ev <- suppressMessages(generate_demo_evidence(30))

  expect_identical(filter_evidence_by_indicator(ev, NULL), ev)
  expect_identical(filter_evidence_by_indicator(ev, NA_character_), ev)
  expect_identical(filter_evidence_by_indicator(ev, ""), ev)
  expect_identical(filter_evidence_by_indicator(ev, character(0)), ev)
})

test_that("an unknown indicator yields zero rows with the same structure", {
  ev <- suppressMessages(generate_demo_evidence(30))

  sub <- filter_evidence_by_indicator(ev, "NO-SUCH-INDICATOR")
  expect_equal(nrow(sub), 0L)
  expect_identical(names(sub), names(ev))
})

test_that("invalid inputs are rejected", {
  ev <- suppressMessages(generate_demo_evidence(20))

  expect_error(filter_evidence_by_indicator("not a data frame", "X"),
               "data.frame")
  expect_error(filter_evidence_by_indicator(ev[, setdiff(names(ev), "indicator_id")], "X"),
               "indicator_id")
  expect_error(filter_evidence_by_indicator(ev, c("A", "B")),
               "single value")
})

test_that("the evidence server applies the drill-down reactive", {
  ev <- suppressMessages(generate_demo_evidence(60))
  id <- ev$indicator_id[[1]]
  drill <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_evidence_server,
    args = list(evidence_rv = shiny::reactive(ev),
                drill_rv = drill),
    {
      session$setInputs(filter_domain = "All", filter_severity = "All",
                        filter_result = "All", filter_source = "All")
      # No drill-down: all rows pass through.
      expect_equal(nrow(filtered()), nrow(ev))

      # Drill into one indicator: the table narrows to its rows.
      drill(id)
      session$flushReact()
      expect_true(all(filtered()$indicator_id == id))
      expect_equal(nrow(filtered()), sum(ev$indicator_id == id))
    }
  )
})

test_that("the indicators server returns the chosen drill indicator", {
  ev <- suppressMessages(generate_demo_evidence(60))

  shiny::testServer(
    mod_indicators_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      id <- r4subscore::sci_explain(ev)$indicator_contributions$indicator_id[[1]]
      session$setInputs(drill_indicator = id, drill_btn = 1)
      expect_equal(session$returned(), id)
    }
  )
})
