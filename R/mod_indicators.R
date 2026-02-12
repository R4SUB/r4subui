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
    shiny::uiOutput(ns("explain_table"))
  )
}


#' Indicators Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
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

    invisible(NULL)
  })
}
