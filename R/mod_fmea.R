#' FMEA Module UI
#'
#' Displays the Failure Mode and Effects Analysis (FMEA) risk register derived
#' from the loaded evidence: a sortable register table ordered by Risk Priority
#' Number (RPN), and a Pareto view that highlights the "vital few" risks that
#' account for most of the total RPN. All of the numbers come from 'r4subrisk'
#' (via [r4subrisk::evidence_to_risks()], [r4subrisk::create_risk_register()],
#' and [r4subrisk::risk_pareto()]); this tab only displays them.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_fmea_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Risk Register (FMEA)"),
    htmltools::p(
      "Derives a Failure Mode and Effects Analysis register from the loaded ",
      "evidence and ranks each risk by its Risk Priority Number ",
      "(RPN = probability x impact x detectability). All scores are computed ",
      "by the 'r4subrisk' package; this tab only displays them.",
      class = "text-muted"
    ),
    shiny::uiOutput(ns("fmea_summary")),
    htmltools::hr(),
    htmltools::h4("Pareto of Risk Priority"),
    htmltools::p(
      "Risks sorted by RPN with the cumulative share of total RPN. The 'vital ",
      "few' risks (up to the cut-off share) are shown in a darker colour.",
      class = "text-muted small"
    ),
    shiny::plotOutput(ns("pareto_chart"), height = "320px"),
    htmltools::hr(),
    htmltools::h4("FMEA Register"),
    shiny::uiOutput(ns("register_table")),
    htmltools::hr(),
    htmltools::h4("Pareto Table"),
    shiny::uiOutput(ns("pareto_table"))
  )
}


#' FMEA Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_fmea_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # FMEA register derived from the full evidence. Guarded on r4subrisk
    # (a suggested package) and on the evidence yielding at least one risk.
    register <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      if (!requireNamespace("r4subrisk", quietly = TRUE)) return(NULL)
      tryCatch({
        risks <- suppressMessages(r4subrisk::evidence_to_risks(ev))
        if (is.null(risks) || nrow(risks) == 0L) return(NULL)
        suppressMessages(r4subrisk::create_risk_register(risks))
      }, error = function(e) NULL)
    })

    pareto <- shiny::reactive({
      reg <- register()
      if (is.null(reg) || nrow(reg) == 0L) return(NULL)
      tryCatch(
        r4subrisk::risk_pareto(reg),
        error = function(e) NULL
      )
    })

    no_data_msg <- function() {
      htmltools::p(
        "The FMEA register needs the 'r4subrisk' package and evidence that ",
        "yields at least one risk.",
        class = "text-muted"
      )
    }

    output$fmea_summary <- shiny::renderUI({
      p <- pareto()
      if (is.null(p)) return(no_data_msg())
      s <- fmea_pareto_summary(p)
      cutoff_pct <- if (is.na(s$cum_cutoff)) "cut-off" else
        paste0(round(s$cum_cutoff * 100), "%")

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("Total Risks", s$n_risks, theme = "info"),
        r4sub_value_box("Total RPN", round(s$total_rpn, 0), theme = "info"),
        r4sub_value_box("Highest RPN", round(s$top_rpn, 0),
                        theme = if (s$top_rpn > 0) "warning" else "secondary"),
        r4sub_value_box(
          "Vital Few", s$n_vital_few,
          theme = if (s$n_vital_few > 0) "danger" else "success",
          subtitle = paste0("drive ", cutoff_pct, " of RPN")
        )
      )
    })

    output$pareto_chart <- shiny::renderPlot({
      p <- pareto()
      shiny::req(p, nrow(p) > 0L)

      rpn <- p$rpn
      cum_share <- if ("cum_share" %in% names(p)) p$cum_share else
        cumsum(rpn) / sum(rpn)
      vital <- if ("vital_few" %in% names(p)) p$vital_few else rep(TRUE, length(rpn))
      bar_cols <- ifelse(vital, "#2C6DB5", "#AEB6BF")
      labs <- if ("rank" %in% names(p)) as.character(p$rank) else
        as.character(seq_along(rpn))

      ymax <- max(rpn, na.rm = TRUE) * 1.1
      if (!is.finite(ymax) || ymax <= 0) ymax <- 1

      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))
      graphics::par(mar = c(4, 4, 3, 4))

      bp <- graphics::barplot(
        rpn,
        names.arg = labs,
        col       = bar_cols,
        border    = NA,
        ylab      = "RPN",
        xlab      = "Risk (ranked by RPN)",
        main      = "Risk Pareto",
        las       = 1,
        ylim      = c(0, ymax)
      )

      # Cumulative-share line mapped onto the RPN axis; a right-hand axis
      # relabels the same positions as a 0-100% scale.
      graphics::lines(bp, cum_share * ymax, type = "o", pch = 16,
                      col = "#E74C3C", lwd = 2)
      graphics::axis(side = 4, at = seq(0, ymax, length.out = 6),
                     labels = paste0(seq(0, 100, 20), "%"), las = 1)
      graphics::mtext("Cumulative share of RPN", side = 4, line = 2.5)

      cutoff <- attr(p, "cum_cutoff")
      if (!is.null(cutoff) && !is.na(cutoff)) {
        graphics::abline(h = cutoff * ymax, col = "#7F8C8D", lty = 2)
      }
    })

    output$register_table <- shiny::renderUI({
      reg <- register()
      if (is.null(reg) || nrow(reg) == 0L) return(no_data_msg())
      disp <- fmea_register_view(reg, max_rows = 100L)
      render_evidence_table(disp, columns = names(disp), max_rows = 100L)
    })

    output$pareto_table <- shiny::renderUI({
      p <- pareto()
      if (is.null(p) || nrow(p) == 0L) {
        return(htmltools::p("No Pareto data available.", class = "text-muted"))
      }
      disp <- as.data.frame(p, stringsAsFactors = FALSE)
      if ("rpn_share" %in% names(disp)) {
        disp$rpn_share <- paste0(round(disp$rpn_share * 100, 1), "%")
      }
      if ("cum_share" %in% names(disp)) {
        disp$cum_share <- paste0(round(disp$cum_share * 100, 1), "%")
      }
      cols <- intersect(
        c("rank", "risk_id", "description", "category", "rpn", "risk_level",
          "rpn_share", "cum_share", "vital_few"),
        names(disp)
      )
      render_evidence_table(disp, columns = cols, max_rows = 100L)
    })

    invisible(NULL)
  })
}
