# testServer coverage for the 0.2.0 dashboard additions:
# targets, Monte Carlo risk, trace impact, authority comparison.

demo_ev2 <- function() suppressMessages(generate_demo_evidence(40))

test_that("targets server computes gap to a target SCI", {
  ev <- demo_ev2()
  shiny::testServer(
    mod_targets_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      session$setInputs(target = 85)
      g <- gap()
      expect_true(is.numeric(g$current_sci))
      expect_equal(g$target_sci, 85)
      expect_true("max_lift" %in% names(g$pillars))
      html <- as.character(output$gap_boxes)
      expect_true(any(grepl("Current SCI|Biggest Lever", html)))
    }
  )
})

test_that("risk server runs a Monte Carlo simulation", {
  skip_if_not_installed("r4subrisk")
  skip_if_not_installed("r4subdata")
  ev <- r4subdata::evidence_pharma
  shiny::testServer(
    mod_risk_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      mc <- mc_result()
      expect_false(is.null(mc))
      expect_true(all(c("total", "total_draws", "per_risk") %in% names(mc)))
      expect_true("prob_critical" %in% names(mc$per_risk))
      html <- as.character(output$mc_summary)
      expect_true(any(grepl("Total RPN", html)))
    }
  )
})

test_that("trace server returns downstream impact for a source variable", {
  skip_if_not_installed("r4subdata")
  ev <- demo_ev2()
  shiny::testServer(
    mod_trace_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      tm <- trace_model_demo()
      expect_false(is.null(tm))
      session$setInputs(changed_var = "DM.AGE")
      imp <- impact_result()
      expect_false(is.null(imp))
      expect_gt(nrow(imp), 0L)
      expect_true(all(c("dataset", "variable", "depth", "path") %in% names(imp)))
    }
  )
})

test_that("authority server renders a cross-authority comparison", {
  skip_if_not_installed("r4subprofile")
  ev <- demo_ev2()
  shiny::testServer(
    mod_authority_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      cmp <- authority_comparison()
      expect_false(is.null(cmp))
      expect_gt(nrow(cmp), 1L)
      expect_true(all(c("w_quality", "ready_min") %in% names(cmp)))
      html <- as.character(output$compare_table)
      expect_true(any(grepl("authority|ready_min", html)))
    }
  )
})
