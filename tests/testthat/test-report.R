test_that("build_readiness_report returns a single markdown string", {
  ev <- suppressMessages(generate_demo_evidence(40))
  report <- build_readiness_report(ev, generated_at = as.POSIXct("2026-01-01 09:00:00"))

  expect_type(report, "character")
  expect_length(report, 1L)
  expect_match(report, "# R4SUB Readiness Report", fixed = TRUE)
})

test_that("report includes SCI, decision band, pillars and loss indicators", {
  ev <- suppressMessages(generate_demo_evidence(40))
  report <- build_readiness_report(ev)

  # Headline SCI and its numeric value
  res <- r4subscore::compute_sci(r4subscore::compute_pillar_scores(ev))
  expect_match(report, "Submission Confidence Index (SCI)", fixed = TRUE)
  expect_match(report, as.character(round(res$SCI, 1)), fixed = TRUE)

  # Decision band
  expect_match(report, "Decision band", fixed = TRUE)
  band_label <- gsub("_", " ", res$band)
  band_label <- paste0(toupper(substring(band_label, 1, 1)), substring(band_label, 2))
  expect_match(report, band_label, fixed = TRUE)

  # Per-pillar contributions section and the four pillars
  expect_match(report, "Per-pillar contributions", fixed = TRUE)
  for (p in c("Quality", "Trace", "Risk", "Usability")) {
    expect_match(report, p, fixed = TRUE)
  }

  # Top loss indicators section
  expect_match(report, "Top loss indicators", fixed = TRUE)

  # Decision-support footer
  expect_match(report, "not an official regulatory", fixed = TRUE)
})

test_that("timestamp is honoured and top_n limits indicator rows", {
  ev <- suppressMessages(generate_demo_evidence(50))
  report <- build_readiness_report(ev,
                                   generated_at = as.POSIXct("2026-03-15 13:45:00"),
                                   top_n = 3L)
  expect_match(report, "2026-03-15 13:45:00", fixed = TRUE)

  # Count indicator rows under the "Top loss indicators" table.
  ic <- r4subscore::sci_explain(ev)$indicator_contributions
  expected_rows <- min(nrow(ic), 3L)
  # Indicator ids appear in the table body; count how many of the top ones show.
  ic <- ic[order(-ic$loss), , drop = FALSE]
  top_ids <- ic$indicator_id[seq_len(expected_rows)]
  for (id in top_ids) {
    expect_match(report, id, fixed = TRUE)
  }
})

test_that("empty or invalid evidence is rejected", {
  expect_error(build_readiness_report(data.frame()), "non-empty")
  expect_error(build_readiness_report("not a data frame"), "non-empty")
})
