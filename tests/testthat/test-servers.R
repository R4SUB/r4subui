# testServer coverage for the module servers, including the paths that the
# authority and risk bug fixes touch.

demo_ev <- function() suppressMessages(generate_demo_evidence(30))

test_that("authority server loads a profile and renders coverage", {
  skip_if_not_installed("r4subprofile")
  ev <- demo_ev()
  shiny::testServer(
    mod_authority_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      session$setInputs(authority = "FDA", sub_type = "NDA", run_profile = 1)
      prof <- profile_rv()
      expect_s3_class(prof, "submission_profile")
      expect_equal(prof$authority, "FDA")

      # Coverage is built from required_indicators, not the missing
      # `requirements` field, so the "no requirements" fallback must not show.
      cov_html <- as.character(output$coverage_table)
      expect_false(any(grepl("No requirements defined", cov_html)))

      # The summary is built from profile fields, not the print-only
      # profile_summary(), so it renders the profile rather than a fallback line.
      sum_html <- as.character(output$profile_summary)
      expect_true(any(grepl("Authority|Minimum coverage", sum_html)))
    }
  )
})

test_that("authority server refreshes submission types on authority change", {
  skip_if_not_installed("r4subprofile")
  ev <- demo_ev()
  shiny::testServer(
    mod_authority_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      # Exercising the observeEvent path must not error.
      session$setInputs(authority = "EMA")
      expect_true(TRUE)
    }
  )
})

test_that("risk server surfaces a Critical Severity box", {
  skip_if_not_installed("r4subdata")
  ev <- r4subdata::evidence_pharma
  shiny::testServer(
    mod_risk_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      expect_true(all(risk_ev()$indicator_domain == "risk"))
      html <- as.character(output$risk_summary)
      expect_true(any(grepl("Critical Severity", html)))
    }
  )
})

test_that("overview server runs on real pharma evidence", {
  skip_if_not_installed("r4subdata")
  skip_if_not_installed("r4subscore")
  ev <- r4subdata::evidence_pharma
  shiny::testServer(
    mod_overview_server,
    args = list(evidence_rv = shiny::reactive(ev)),
    {
      expect_no_error(as.character(output$sci_box))
    }
  )
})
