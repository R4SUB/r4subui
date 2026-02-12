test_that("render_evidence_table returns HTML for valid evidence", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = "ADSL",
    source_name = "test", source_version = NA_character_,
    indicator_id = "Q1", indicator_name = "Q1",
    indicator_domain = "quality",
    severity = "high", result = "fail",
    metric_value = NA_real_, metric_unit = NA_character_,
    message = "Missing variable", location = "ADSL:AGE",
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)

  html <- render_evidence_table(ev)
  expect_true(inherits(html, "shiny.tag.list") || inherits(html, "shiny.tag"))
})

test_that("render_evidence_table handles empty data.frame", {
  html <- render_evidence_table(data.frame())
  # Should return a message, not error
  expect_true(inherits(html, "shiny.tag"))
})

test_that("render_evidence_table respects max_rows", {
  skip_if_not_installed("r4subcore")
  ctx <- r4subcore::r4sub_run_context(study_id = "T", environment = "DEV")
  ev <- data.frame(
    asset_type = "dataset", asset_id = paste0("DS", 1:10),
    source_name = "test", source_version = NA_character_,
    indicator_id = paste0("Q", 1:10),
    indicator_name = paste0("Check ", 1:10),
    indicator_domain = "quality",
    severity = "low", result = "pass",
    metric_value = NA_real_, metric_unit = NA_character_,
    message = paste0("Msg ", 1:10), location = NA_character_,
    evidence_payload = "{}", stringsAsFactors = FALSE
  )
  ev <- r4subcore::as_evidence(ev, ctx = ctx)

  html <- render_evidence_table(ev, max_rows = 3L)
  html_str <- as.character(html)
  # Should contain truncation message
  expect_true(grepl("Showing 3 of 10", html_str))
})

test_that("render_evidence_table handles missing columns gracefully", {
  df <- data.frame(x = 1, y = 2)
  html <- render_evidence_table(df, columns = c("indicator_id", "severity"))
  # No matching columns
  expect_true(inherits(html, "shiny.tag"))
})

test_that("r4sub_value_box returns a value_box", {
  vb <- r4sub_value_box("Test", 42, theme = "primary")
  expect_true(inherits(vb, "shiny.tag"))
})
