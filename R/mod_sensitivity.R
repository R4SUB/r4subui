#' Sensitivity Analysis Module UI
#'
#' Allows users to explore SCI sensitivity under different weight scenarios.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_sensitivity_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Weight Scenario Configuration"),
    htmltools::p("Adjust pillar weights and see how SCI changes.",
                 class = "text-muted"),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::sliderInput(ns("w_quality"), "Quality",
                         min = 0, max = 1, value = 0.35, step = 0.05),
      shiny::sliderInput(ns("w_trace"), "Trace",
                         min = 0, max = 1, value = 0.25, step = 0.05),
      shiny::sliderInput(ns("w_risk"), "Risk",
                         min = 0, max = 1, value = 0.25, step = 0.05),
      shiny::sliderInput(ns("w_usability"), "Usability",
                         min = 0, max = 1, value = 0.15, step = 0.05)
    ),
    shiny::uiOutput(ns("weight_validation")),
    shiny::actionButton(ns("compute_btn"), "Compute SCI",
                        class = "btn-primary mb-3"),
    htmltools::hr(),
    shiny::uiOutput(ns("custom_sci_result")),
    htmltools::hr(),
    htmltools::h4("Pre-defined Scenarios"),
    shiny::uiOutput(ns("scenario_table"))
  )
}


#' Sensitivity Analysis Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_sensitivity_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$weight_validation <- shiny::renderUI({
      total <- input$w_quality + input$w_trace + input$w_risk + input$w_usability
      total <- round(total, 2)
      if (abs(total - 1) < 0.01) {
        htmltools::p(paste0("Weight sum: ", total, " (valid)"),
                     class = "text-success")
      } else {
        htmltools::p(paste0("Weight sum: ", total, " (must equal 1.0)"),
                     class = "text-danger fw-bold")
      }
    })

    shiny::observeEvent(input$compute_btn, {
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      total <- input$w_quality + input$w_trace + input$w_risk + input$w_usability
      if (abs(total - 1) > 0.01) {
        shiny::showNotification("Weights must sum to 1.0", type = "error")
        return()
      }

      w <- c(quality = input$w_quality, trace = input$w_trace,
             risk = input$w_risk, usability = input$w_usability)
      cfg <- r4subscore::sci_config_default(pillar_weights = w)
      ps <- r4subscore::compute_pillar_scores(ev, config = cfg)
      res <- r4subscore::compute_sci(ps, config = cfg)

      output$custom_sci_result <- shiny::renderUI({
        band_label <- gsub("_", " ", res$band)
        theme <- switch(res$band,
                        ready = "success", minor_gaps = "warning",
                        conditional = "warning", high_risk = "danger",
                        "primary")
        bslib::layout_columns(
          col_widths = c(6, 6),
          r4sub_value_box("Custom SCI", res$SCI, theme = theme),
          r4sub_value_box("Band", band_label, theme = theme)
        )
      })
    })

    # Pre-defined scenarios
    output$scenario_table <- shiny::renderUI({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      grid <- data.frame(
        quality   = c(0.35, 0.40, 0.25, 0.25),
        trace     = c(0.25, 0.20, 0.35, 0.25),
        risk      = c(0.25, 0.30, 0.15, 0.25),
        usability = c(0.15, 0.10, 0.25, 0.25)
      )

      result <- tryCatch(
        r4subscore::sci_sensitivity_analysis(ev, grid),
        error = function(e) NULL
      )

      if (is.null(result) || nrow(result) == 0L) {
        return(htmltools::p("Could not compute scenarios.", class = "text-muted"))
      }

      render_evidence_table(
        result,
        columns = names(result),
        max_rows = 20L
      )
    })

    invisible(NULL)
  })
}
