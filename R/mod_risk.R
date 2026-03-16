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
    shiny::uiOutput(ns("risk_table"))
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
      n_high   <- sum(ev$severity == "high",   na.rm = TRUE)
      n_medium <- sum(ev$severity == "medium",  na.rm = TRUE)
      n_low    <- sum(ev$severity == "low",     na.rm = TRUE)
      n_fail   <- sum(ev$result   == "fail",    na.rm = TRUE)

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("Total Risks", nrow(ev), theme = "info"),
        r4sub_value_box("High Severity", n_high, theme = if (n_high > 0) "danger" else "success"),
        r4sub_value_box("Medium Severity", n_medium, theme = if (n_medium > 0) "warning" else "success"),
        r4sub_value_box("Failed Checks", n_fail, theme = if (n_fail > 0) "danger" else "success")
      )
    })

    output$risk_chart <- shiny::renderPlot({
      ev <- risk_ev()
      shiny::req(nrow(ev) > 0L)

      severity_levels <- c("high", "medium", "low", "info")
      counts <- sapply(severity_levels, function(s) sum(ev$severity == s, na.rm = TRUE))
      cols <- c(high = "#E74C3C", medium = "#F39C12", low = "#3498DB", info = "#95A5A6")

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

    invisible(NULL)
  })
}
