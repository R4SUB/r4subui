#' Readiness Targets Module UI
#'
#' Lets the reviewer set a target SCI and shows the gap to it, plus the pillar
#' that would move the score the most (the biggest lever).
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_targets_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Readiness Target"),
    shiny::sliderInput(ns("target"), "Target SCI",
                       min = 0, max = 100, value = 85, step = 1),
    htmltools::hr(),
    shiny::uiOutput(ns("gap_boxes")),
    htmltools::hr(),
    htmltools::h4("Biggest Levers"),
    htmltools::p(
      "Pillars ordered by the SCI points that closing them would add.",
      class = "text-muted small"
    ),
    shiny::uiOutput(ns("levers_table"))
  )
}


#' Readiness Targets Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_targets_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    gap <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      target <- if (is.null(input$target)) 85 else input$target
      ps <- r4subscore::compute_pillar_scores(ev)
      sr <- r4subscore::compute_sci(ps)
      r4subscore::sci_gap_to_target(sr, target)
    })

    output$gap_boxes <- shiny::renderUI({
      g <- gap()
      met <- isTRUE(g$met)
      # Biggest lever is the first pillar row (already ordered by max_lift).
      lever <- if (nrow(g$pillars) > 0L) g$pillars$pillar[1] else "n/a"
      lever_pts <- if (nrow(g$pillars) > 0L) g$pillars$max_lift[1] else NA_real_

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("Current SCI", g$current_sci, theme = "info"),
        r4sub_value_box("Target SCI", g$target_sci, theme = "primary"),
        r4sub_value_box(
          "Gap", g$gap,
          theme = if (met) "success" else "warning",
          subtitle = if (met) "Target met" else "Points to target"
        ),
        r4sub_value_box(
          "Biggest Lever", lever, theme = "secondary",
          subtitle = paste0("up to ", lever_pts, " SCI pts")
        )
      )
    })

    output$levers_table <- shiny::renderUI({
      g <- gap()
      df <- g$pillars[, c("pillar", "current", "weight", "max_lift"), drop = FALSE]
      names(df) <- c("pillar", "current", "weight", "max_lift")
      render_evidence_table(df, columns = names(df), max_rows = 10L)
    })

    invisible(NULL)
  })
}
