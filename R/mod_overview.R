#' Overview Module UI
#'
#' Displays the SCI gauge, pillar score cards, and evidence summary counts.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::uiOutput(ns("sci_box")),
      shiny::uiOutput(ns("band_box")),
      shiny::uiOutput(ns("evidence_count_box"))
    ),
    htmltools::hr(),
    htmltools::h4("Pillar Scores"),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::uiOutput(ns("quality_box")),
      shiny::uiOutput(ns("trace_box")),
      shiny::uiOutput(ns("risk_box")),
      shiny::uiOutput(ns("usability_box"))
    ),
    htmltools::hr(),
    htmltools::h4("Evidence Summary"),
    shiny::uiOutput(ns("evidence_summary_table"))
  )
}


#' Overview Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_overview_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    sci_result <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      ps <- r4subscore::compute_pillar_scores(ev)
      r4subscore::compute_sci(ps)
    })

    pillar_scores <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      r4subscore::compute_pillar_scores(ev)
    })

    # SCI value box
    output$sci_box <- shiny::renderUI({
      res <- sci_result()
      bc <- band_colors()
      color <- unname(bc[res$band])
      if (is.na(color)) color <- bc[["unclassified"]]

      theme_name <- switch(
        res$band,
        ready       = "success",
        minor_gaps  = "warning",
        conditional = "warning",
        high_risk   = "danger",
        "primary"
      )
      r4sub_value_box("SCI Score", res$SCI, theme = theme_name,
                      subtitle = paste0("0-100 scale"))
    })

    # Band box
    output$band_box <- shiny::renderUI({
      res <- sci_result()
      band_label <- gsub("_", " ", res$band)
      band_label <- paste0(toupper(substring(band_label, 1, 1)),
                           substring(band_label, 2))
      theme_name <- switch(
        res$band,
        ready       = "success",
        minor_gaps  = "warning",
        conditional = "warning",
        high_risk   = "danger",
        "primary"
      )
      r4sub_value_box("Decision Band", band_label, theme = theme_name)
    })

    # Evidence count box
    output$evidence_count_box <- shiny::renderUI({
      ev <- evidence_rv()
      r4sub_value_box("Evidence Rows", nrow(ev), theme = "info",
                      subtitle = "Total evidence items")
    })

    # Pillar score boxes
    render_pillar_box <- function(pillar_name, output_id) {
      output[[output_id]] <- shiny::renderUI({
        ps <- pillar_scores()
        row <- ps[ps$pillar == pillar_name, , drop = FALSE]
        if (nrow(row) == 0L || is.na(row$pillar_score[1])) {
          return(r4sub_value_box(
            title = paste0(toupper(substring(pillar_name, 1, 1)),
                           substring(pillar_name, 2)),
            value = "N/A", theme = "secondary",
            subtitle = "No evidence"
          ))
        }
        score_pct <- round(row$pillar_score[1] * 100, 1)
        theme_name <- if (score_pct >= 80) "success" else
          if (score_pct >= 60) "warning" else "danger"
        r4sub_value_box(
          title = paste0(toupper(substring(pillar_name, 1, 1)),
                         substring(pillar_name, 2)),
          value = paste0(score_pct, "%"),
          theme = theme_name,
          subtitle = paste0(row$n_indicators, " indicator(s)")
        )
      })
    }

    render_pillar_box("quality", "quality_box")
    render_pillar_box("trace", "trace_box")
    render_pillar_box("risk", "risk_box")
    render_pillar_box("usability", "usability_box")

    # Evidence summary table
    output$evidence_summary_table <- shiny::renderUI({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      summary_df <- r4subcore::evidence_summary(ev)
      if (nrow(summary_df) == 0L) {
        return(htmltools::p("No evidence summary available.", class = "text-muted"))
      }
      render_evidence_table(
        summary_df,
        columns = names(summary_df),
        max_rows = 100L
      )
    })

    invisible(NULL)
  })
}
