# Tests for the FMEA tab: the pure display helpers (fmea_register_view and
# fmea_pareto_summary) and the module server. The helpers only reorder and
# read values that 'r4subrisk' computes; they introduce no new scoring.

# ---------------------------------------------------------------------------
# fmea_register_view(): ordering and column selection (shiny- and r4subrisk-free)
# ---------------------------------------------------------------------------

.toy_register <- function() {
  data.frame(
    risk_id     = c("A", "B", "C", "D"),
    description = c("da", "db", "dc", "dd"),
    category    = c("quality", "trace", "quality", "usability"),
    probability = c(3, 5, 1, 3),
    impact      = c(3, 5, 1, 3),
    detectability = c(2, 3, 1, 3),
    rpn         = c(18, 75, 1, 27),
    risk_level  = c("medium", "high", "low", "medium"),
    stringsAsFactors = FALSE
  )
}

test_that("fmea_register_view orders by descending RPN and keeps columns", {
  reg <- .toy_register()
  v <- fmea_register_view(reg)

  expect_s3_class(v, "data.frame")
  # Highest RPN first.
  expect_equal(v$rpn, c(75, 27, 18, 1))
  expect_equal(v$risk_id, c("B", "D", "A", "C"))
  # Default display columns, in order.
  expect_identical(names(v),
                   c("risk_id", "description", "category", "probability",
                     "impact", "detectability", "rpn", "risk_level"))
  # Row names reset to 1..n.
  expect_identical(rownames(v), as.character(seq_len(nrow(v))))
})

test_that("fmea_register_view respects max_rows and a custom column set", {
  reg <- .toy_register()
  v <- fmea_register_view(reg, columns = c("risk_id", "rpn"), max_rows = 2L)
  expect_equal(nrow(v), 2L)
  expect_identical(names(v), c("risk_id", "rpn"))
  expect_equal(v$rpn, c(75, 27))
})

test_that("fmea_register_view ties keep their original (stable) order", {
  reg <- data.frame(
    risk_id = c("first", "second", "third"),
    rpn     = c(50, 50, 10),
    stringsAsFactors = FALSE
  )
  v <- fmea_register_view(reg, columns = c("risk_id", "rpn"))
  # The two RPN-50 rows keep input order: first, then second.
  expect_equal(v$risk_id, c("first", "second", "third"))
})

test_that("fmea_register_view handles empty input and a missing rpn column", {
  expect_equal(nrow(fmea_register_view(data.frame())), 0L)
  expect_error(
    fmea_register_view(data.frame(risk_id = "A", stringsAsFactors = FALSE)),
    "rpn"
  )
})

# ---------------------------------------------------------------------------
# fmea_pareto_summary(): reads headline figures (shiny- and r4subrisk-free)
# ---------------------------------------------------------------------------

.toy_pareto <- function() {
  p <- data.frame(
    rank      = 1:3,
    risk_id   = c("B", "D", "A"),
    rpn       = c(75, 27, 18),
    vital_few = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  attr(p, "total_rpn")   <- 120
  attr(p, "n_vital_few") <- 2L
  attr(p, "cum_cutoff")  <- 0.8
  p
}

test_that("fmea_pareto_summary reads values straight from the Pareto object", {
  s <- fmea_pareto_summary(.toy_pareto())
  expect_equal(s$n_risks, 3L)
  expect_equal(s$total_rpn, 120)
  expect_equal(s$n_vital_few, 2L)
  expect_equal(s$cum_cutoff, 0.8)
  expect_equal(s$top_rpn, 75)
  expect_equal(s$vital_few_share, 2 / 3)
})

test_that("fmea_pareto_summary falls back when attributes are absent", {
  p <- data.frame(rpn = c(10, 5, 5), vital_few = c(TRUE, FALSE, FALSE))
  s <- fmea_pareto_summary(p)
  expect_equal(s$total_rpn, 20)       # sum of rpn
  expect_equal(s$n_vital_few, 1L)     # sum of vital_few
  expect_true(is.na(s$cum_cutoff))
  expect_equal(s$top_rpn, 10)
})

test_that("fmea_pareto_summary rejects a non-data.frame", {
  expect_error(fmea_pareto_summary(list(a = 1)), "risk_pareto")
})

# ---------------------------------------------------------------------------
# Integration with r4subrisk and the module server.
# ---------------------------------------------------------------------------

test_that("the helpers agree with the r4subrisk pipeline", {
  skip_if_not_installed("r4subrisk")
  ev <- suppressMessages(generate_demo_evidence(60, seed = 3))
  risks <- suppressMessages(r4subrisk::evidence_to_risks(ev))
  skip_if(is.null(risks) || nrow(risks) == 0L)
  reg <- suppressMessages(r4subrisk::create_risk_register(risks))
  par <- r4subrisk::risk_pareto(reg)

  v <- fmea_register_view(reg)
  # Non-increasing RPN (highest-priority first).
  expect_false(is.unsorted(rev(v$rpn)))

  s <- fmea_pareto_summary(par)
  expect_equal(s$n_risks, nrow(par))
  expect_equal(s$total_rpn, unname(attr(par, "total_rpn")))
  expect_equal(s$n_vital_few, unname(as.integer(attr(par, "n_vital_few"))))
})

test_that("the FMEA server builds a register, Pareto, and renders outputs", {
  skip_if_not_installed("r4subrisk")
  ev <- suppressMessages(generate_demo_evidence(60, seed = 4))

  shiny::testServer(
    mod_fmea_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      reg <- register()
      skip_if(is.null(reg))
      expect_true("rpn" %in% names(reg))

      p <- pareto()
      expect_false(is.null(p))
      expect_true(all(c("rpn", "cum_share") %in% names(p)))

      # Every output renders without error.
      html <- as.character(output$fmea_summary)
      expect_true(any(grepl("Total RPN|Vital Few", html)))
      expect_no_error(as.character(output$register_table))
      expect_no_error(as.character(output$pareto_table))
    }
  )
})

test_that("the FMEA server shows a placeholder when no risk is derived", {
  skip_if_not_installed("r4subrisk")
  # All-pass evidence yields no risks, so the register and Pareto are NULL and
  # the tab falls back to its guidance message instead of erroring.
  ev <- suppressMessages(generate_demo_evidence(40, seed = 4))
  ev$result <- "pass"
  ev$severity <- "info"

  shiny::testServer(
    mod_fmea_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      expect_null(register())
      expect_null(pareto())
      html <- as.character(output$fmea_summary)
      expect_true(any(grepl("r4subrisk", html)))
    }
  )
})
