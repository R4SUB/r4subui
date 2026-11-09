#' Indicators Module UI
#'
#' Displays indicator-level scores and a breakdown of contributions.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_indicators_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Indicator Scores"),
    shiny::uiOutput(ns("indicator_table")),
    htmltools::hr(),
    htmltools::h4("SCI Explainability"),
    shiny::uiOutput(ns("explain_table")),
    htmltools::hr(),
    htmltools::h4("Drill Down to Evidence"),
    htmltools::p(
      "Pick a contributing indicator and open its rows in the Evidence tab.",
      class = "text-muted small"
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::selectInput(ns("drill_indicator"), "Indicator",
                         choices = c("None" = ""), selected = ""),
      htmltools::div(
        class = "d-flex align-items-end h-100",
        shiny::actionButton(ns("drill_btn"), "View Evidence",
                            class = "btn-primary w-100")
      )
    )
  )
}


#' Indicators Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return A reactive returning the `indicator_id` the user last chose to drill
#'   into (a single string), or `NULL` before any selection. The app wires this
#'   to the Evidence tab so the chosen indicator filters the evidence rows.
#'
#' @export
mod_indicators_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$indicator_table <- shiny::renderUI({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      scores <- r4subscore::compute_indicator_scores(ev)
      scores$indicator_score <- round(scores$indicator_score * 100, 1)
      names(scores)[names(scores) == "indicator_score"] <- "score_pct"

      render_evidence_table(
        scores,
        columns = c("indicator_id", "indicator_name", "indicator_domain",
                     "n_evidence", "score_pct"),
        max_rows = 200L
      )
    })

    output$explain_table <- shiny::renderUI({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      expl <- r4subscore::sci_explain(ev)
      contrib <- expl$indicator_contributions
      if (nrow(contrib) == 0L) {
        return(htmltools::p("No contributions to display.", class = "text-muted"))
      }

      render_evidence_table(
        contrib,
        columns = c("indicator_id", "indicator_domain",
                     "indicator_score", "loss", "pct_of_total_loss"),
        max_rows = 200L
      )
    })

    # Keep the drill-down picker in sync with the contributing indicators.
    shiny::observe({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      expl <- r4subscore::sci_explain(ev)
      contrib <- expl$indicator_contributions
      ids <- if (is.data.frame(contrib) && nrow(contrib) > 0L) {
        sort(unique(contrib$indicator_id))
      } else {
        character(0)
      }
      shiny::updateSelectInput(
        session, "drill_indicator",
        choices = c(c("None" = ""), ids)
      )
    })

    # Expose the indicator the user asked to drill into.
    drill_selection <- shiny::eventReactive(input$drill_btn, {
      sel <- input$drill_indicator
      if (is.null(sel) || !nzchar(sel)) NULL else sel
    })

    drill_selection
  })
}
