#' Pillar Detail Module UI
#'
#' Displays a bar chart of pillar scores and per-pillar evidence breakdown.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_pillar_detail_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Pillar Score Chart"),
    shiny::plotOutput(ns("pillar_chart"), height = "300px"),
    htmltools::hr(),
    htmltools::h4("Pillar Contribution to SCI"),
    shiny::uiOutput(ns("pillar_contrib_table")),
    htmltools::hr(),
    htmltools::h4("Result Distribution by Domain"),
    shiny::plotOutput(ns("result_dist_chart"), height = "300px")
  )
}


#' Pillar Detail Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_pillar_detail_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    pillar_data <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      r4subscore::compute_pillar_scores(ev)
    })

    output$pillar_chart <- shiny::renderPlot({
      ps <- pillar_data()
      shiny::req(ps, nrow(ps) > 0L)

      scores <- ifelse(is.na(ps$pillar_score), 0, ps$pillar_score * 100)
      cols <- ifelse(scores >= 80, "#27AE60",
                     ifelse(scores >= 60, "#F39C12", "#E74C3C"))

      par(mar = c(4, 6, 2, 2))
      bp <- barplot(
        scores,
        names.arg = ps$pillar,
        col       = cols,
        border    = NA,
        ylim      = c(0, 100),
        ylab      = "Score (%)",
        main      = "Pillar Scores",
        las       = 1,
        horiz     = FALSE
      )
      text(bp, scores + 3, paste0(round(scores, 1), "%"), cex = 0.9, font = 2)
      abline(h = c(50, 70, 85), lty = 2, col = "#BDC3C7")
    })

    output$pillar_contrib_table <- shiny::renderUI({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      expl <- r4subscore::sci_explain(ev)
      pc <- expl$pillar_contributions
      if (nrow(pc) == 0L) {
        return(htmltools::p("No pillar data.", class = "text-muted"))
      }

      render_evidence_table(
        pc,
        columns = c("pillar", "pillar_score", "n_indicators",
                     "weight", "contribution", "loss"),
        max_rows = 10L
      )
    })

    output$result_dist_chart <- shiny::renderPlot({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      domains <- c("quality", "trace", "risk", "usability")
      results <- c("pass", "warn", "fail")
      rc <- result_colors()

      # Build count matrix
      mat <- matrix(0, nrow = length(results), ncol = length(domains),
                    dimnames = list(results, domains))
      for (d in domains) {
        for (r in results) {
          mat[r, d] <- sum(ev$indicator_domain == d & ev$result == r,
                           na.rm = TRUE)
        }
      }

      par(mar = c(4, 4, 2, 8), xpd = TRUE)
      barplot(
        mat,
        beside = TRUE,
        col    = unname(rc[results]),
        border = NA,
        ylab   = "Count",
        main   = "Results by Domain",
        las    = 1
      )
      legend("topright", inset = c(-0.2, 0),
             legend = results,
             fill = unname(rc[results]),
             border = NA, bty = "n", cex = 0.9)
    })

    invisible(NULL)
  })
}
