#' Risk Register Module UI
#'
#' Displays the risk register table with severity distribution and top risks.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_risk_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Risk Register"),
    shiny::uiOutput(ns("risk_summary")),
    htmltools::hr(),
    htmltools::h4("Risk Distribution"),
    shiny::plotOutput(ns("risk_chart"), height = "300px"),
    htmltools::hr(),
    htmltools::h4("Risk Details"),
    shiny::uiOutput(ns("risk_table")),
    htmltools::hr(),
    htmltools::h4("Monte Carlo Risk Simulation"),
    htmltools::p(
      "Treats each FMEA score as uncertain and simulates the distribution of ",
      "Risk Priority Numbers across the register.",
      class = "text-muted small"
    ),
    shiny::uiOutput(ns("mc_summary")),
    shiny::plotOutput(ns("mc_chart"), height = "300px"),
    shiny::uiOutput(ns("mc_table"))
  )
}


#' Risk Register Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_risk_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    risk_ev <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      ev[ev$indicator_domain == "risk", , drop = FALSE]
    })

    output$risk_summary <- shiny::renderUI({
      ev <- risk_ev()
      if (nrow(ev) == 0L) {
        return(htmltools::p("No risk evidence loaded.", class = "text-muted"))
      }
      n_critical <- sum(ev$severity == "critical", na.rm = TRUE)
      n_high     <- sum(ev$severity == "high",     na.rm = TRUE)
      n_fail     <- sum(ev$result   == "fail",     na.rm = TRUE)

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("Total Risks", nrow(ev), theme = "info"),
        r4sub_value_box("Critical Severity", n_critical, theme = if (n_critical > 0) "danger" else "success"),
        r4sub_value_box("High Severity", n_high, theme = if (n_high > 0) "danger" else "success"),
        r4sub_value_box("Failed Checks", n_fail, theme = if (n_fail > 0) "danger" else "success")
      )
    })

    output$risk_chart <- shiny::renderPlot({
      ev <- risk_ev()
      shiny::req(nrow(ev) > 0L)

      severity_levels <- c("critical", "high", "medium", "low", "info")
      counts <- sapply(severity_levels, function(s) sum(ev$severity == s, na.rm = TRUE))
      cols <- c(critical = "#C0392B", high = "#E74C3C", medium = "#F39C12",
                low = "#27AE60", info = "#95A5A6")

      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      par(mar = c(4, 6, 2, 2))
      barplot(
        counts,
        names.arg = severity_levels,
        col       = cols[severity_levels],
        border    = NA,
        ylab      = "Count",
        main      = "Risk Evidence by Severity",
        las       = 1
      )
    })

    output$risk_table <- shiny::renderUI({
      ev <- risk_ev()
      if (nrow(ev) == 0L) {
        return(htmltools::p("No risk evidence available.", class = "text-muted"))
      }
      cols <- intersect(
        c("indicator_id", "indicator_name", "severity", "result",
          "metric_value", "message", "location"),
        names(ev)
      )
      render_evidence_table(ev, columns = cols, max_rows = 50L)
    })

    # Monte Carlo simulation over the register derived from the full evidence.
    # Guarded on r4subrisk (a suggested package).
    mc_result <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      if (!requireNamespace("r4subrisk", quietly = TRUE)) return(NULL)
      tryCatch({
        risks <- r4subrisk::evidence_to_risks(ev)
        if (is.null(risks) || nrow(risks) == 0L) return(NULL)
        reg <- r4subrisk::create_risk_register(risks)
        r4subrisk::risk_monte_carlo(reg, n = 2000L, seed = 42L)
      }, error = function(e) NULL)
    })

    output$mc_summary <- shiny::renderUI({
      mc <- mc_result()
      if (is.null(mc)) {
        return(htmltools::p(
          "Monte Carlo needs the r4subrisk package and at least one risk.",
          class = "text-muted"
        ))
      }
      n_likely <- sum(mc$per_risk$prob_critical >= 0.5)
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("Total RPN (mean)", unname(mc$total[["mean"]]),
                        theme = "info"),
        r4sub_value_box("90% Low", unname(mc$total[["p05"]]), theme = "success"),
        r4sub_value_box("90% High", unname(mc$total[["p95"]]), theme = "warning"),
        r4sub_value_box(
          "Likely Critical", n_likely,
          theme = if (n_likely > 0) "danger" else "success",
          subtitle = "risks with P >= 0.5"
        )
      )
    })

    output$mc_chart <- shiny::renderPlot({
      mc <- mc_result()
      shiny::req(mc)

      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      par(mar = c(4, 4, 2, 2))
      hist(
        mc$total_draws,
        breaks = 30,
        col    = "#2C6DB5",
        border = "white",
        xlab   = "Total RPN",
        main   = "Simulated Total RPN Distribution"
      )
      abline(v = mc$point_total, col = "#E74C3C", lwd = 2, lty = 2)
    })

    output$mc_table <- shiny::renderUI({
      mc <- mc_result()
      if (is.null(mc)) {
        return(htmltools::p("No Monte Carlo results.", class = "text-muted"))
      }
      pr <- mc$per_risk[, c("risk_id", "rpn", "mc_mean", "p05", "p95",
                            "prob_critical"), drop = FALSE]
      render_evidence_table(pr, columns = names(pr), max_rows = 50L)
    })

    invisible(NULL)
  })
}
