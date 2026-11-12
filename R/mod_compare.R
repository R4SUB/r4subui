#' Compare Module UI
#'
#' Lets the reviewer upload a baseline evidence snapshot and compare the current
#' (loaded) evidence against it: the SCI and decision-band move, the per-pillar
#' score deltas, and a summary of which indicators improved or regressed. All of
#' the numbers come from the engine packages via [compare_readiness()].
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_compare_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Compare Against a Baseline"),
    htmltools::p(
      "Upload an earlier evidence snapshot (CSV/RDS) to see how the current ",
      "data has moved. The current data is whatever is loaded in the sidebar; ",
      "the upload here is the 'before' snapshot.",
      class = "text-muted"
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::fileInput(ns("baseline_file"), "Baseline Evidence (CSV/RDS)",
                       accept = c(".csv", ".rds")),
      htmltools::div(
        class = "d-flex align-items-end h-100",
        shiny::actionButton(ns("load_baseline"), "Compare",
                            class = "btn-primary w-100")
      )
    ),
    shiny::uiOutput(ns("status")),
    htmltools::hr(),
    htmltools::h4("Headline Change"),
    shiny::uiOutput(ns("headline_boxes")),
    htmltools::hr(),
    htmltools::h4("Evidence Changes"),
    shiny::uiOutput(ns("diff_boxes")),
    htmltools::hr(),
    htmltools::h4("Per-pillar Change"),
    shiny::plotOutput(ns("pillar_delta_chart"), height = "280px"),
    shiny::uiOutput(ns("pillar_table")),
    htmltools::hr(),
    htmltools::h4("Changed Indicators"),
    htmltools::p(
      "Indicators whose result or severity moved between the two snapshots.",
      class = "text-muted small"
    ),
    shiny::uiOutput(ns("changes_table"))
  )
}


#' Compare Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning the current (validated) evidence
#'   data.frame, as loaded in the dashboard sidebar.
#' @return Invisible `NULL`.
#'
#' @export
mod_compare_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Baseline ("before") snapshot, populated from the upload control.
    baseline_store <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$load_baseline, {
      file <- input$baseline_file
      if (is.null(file)) {
        shiny::showNotification("No baseline file selected.", type = "warning")
        return()
      }

      ext <- tolower(tools::file_ext(file$name))
      loaded <- tryCatch({
        if (ext == "rds") {
          readRDS(file$datapath)
        } else if (ext == "csv") {
          utils::read.csv(file$datapath, stringsAsFactors = FALSE)
        } else {
          shiny::showNotification("Unsupported file type.", type = "error")
          return(NULL)
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message),
                                type = "error")
        NULL
      })

      if (is.null(loaded)) return()

      ok <- tryCatch({
        r4subcore::validate_evidence(loaded)
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste("Baseline validation failed:", e$message), type = "error")
        FALSE
      })

      if (ok) {
        baseline_store(loaded)
        shiny::showNotification(
          paste0("Baseline loaded (", nrow(loaded), " rows)."),
          type = "message")
      }
    })

    comparison <- shiny::reactive({
      base <- baseline_store()
      cur  <- evidence_rv()
      shiny::req(base, cur, nrow(base) > 0L, nrow(cur) > 0L)
      compare_readiness(base, cur)
    })

    output$status <- shiny::renderUI({
      cur <- evidence_rv()
      if (is.null(cur) || nrow(cur) == 0L) {
        return(htmltools::div(
          class = "alert alert-warning",
          "Load current evidence in the sidebar first, then upload a baseline."
        ))
      }
      if (is.null(baseline_store())) {
        return(htmltools::div(
          class = "alert alert-info",
          "Upload a baseline snapshot and click Compare."
        ))
      }
      NULL
    })

    output$headline_boxes <- shiny::renderUI({
      cmp <- comparison()
      sci_delta <- round(cmp$sci$delta, 1)
      delta_theme <- if (sci_delta > 0) "success" else
        if (sci_delta < 0) "danger" else "secondary"
      band_from <- .format_band(cmp$band$before)
      band_to   <- .format_band(cmp$band$after)

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("SCI Before", round(cmp$sci$before, 1), theme = "info"),
        r4sub_value_box("SCI After", round(cmp$sci$after, 1), theme = "info"),
        r4sub_value_box(
          "SCI Change",
          paste0(if (sci_delta > 0) "+" else "", sci_delta),
          theme = delta_theme,
          subtitle = "points"
        ),
        r4sub_value_box(
          "Decision Band",
          if (cmp$band$changed) paste0(band_from, " -> ", band_to) else band_to,
          theme = if (cmp$band$changed) "warning" else "secondary",
          subtitle = if (cmp$band$changed) "changed" else "unchanged"
        )
      )
    })

    output$diff_boxes <- shiny::renderUI({
      cmp <- comparison()
      s <- cmp$diff_summary
      getn <- function(nm) if (nm %in% names(s)) unname(s[[nm]]) else NA
      net <- round(getn("net_delta"), 2)

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("Improved", getn("n_improved"), theme = "success",
                        subtitle = "indicators"),
        r4sub_value_box("Regressed", getn("n_regressed"), theme = "danger",
                        subtitle = "indicators"),
        r4sub_value_box(
          "Added / Removed",
          paste0(getn("n_added"), " / ", getn("n_removed")),
          theme = "info"
        ),
        r4sub_value_box(
          "Net Delta",
          paste0(if (!is.na(net) && net > 0) "+" else "", net),
          theme = if (!is.na(net) && net > 0) "success" else
            if (!is.na(net) && net < 0) "danger" else "secondary"
        )
      )
    })

    output$pillar_delta_chart <- shiny::renderPlot({
      cmp <- comparison()
      p <- cmp$pillars
      shiny::req(nrow(p) > 0L)
      # Keep a stable, readable pillar order for the chart.
      p <- p[order(p$pillar), , drop = FALSE]
      deltas <- ifelse(is.na(p$delta), 0, p$delta * 100)
      cols <- ifelse(deltas >= 0, "#27AE60", "#E74C3C")

      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar))
      graphics::par(mar = c(4, 6, 2, 2))
      bp <- graphics::barplot(
        deltas,
        names.arg = p$pillar,
        col       = cols,
        border    = NA,
        ylab      = "Score change (pts)",
        main      = "Per-pillar Score Change",
        las       = 1,
        horiz     = FALSE
      )
      graphics::abline(h = 0, col = "#7F8C8D")
      graphics::text(bp, deltas, labels = paste0(round(deltas, 1)),
                     pos = ifelse(deltas >= 0, 3, 1), cex = 0.9, font = 2)
    })

    output$pillar_table <- shiny::renderUI({
      cmp <- comparison()
      p <- cmp$pillars
      disp <- data.frame(
        pillar       = p$pillar,
        score_before = paste0(round(p$score_before * 100, 1), "%"),
        score_after  = paste0(round(p$score_after * 100, 1), "%"),
        delta_pts    = round(p$delta * 100, 1),
        weight       = round(p$weight, 2),
        stringsAsFactors = FALSE
      )
      render_evidence_table(disp, columns = names(disp), max_rows = 10L)
    })

    output$changes_table <- shiny::renderUI({
      cmp <- comparison()
      diff <- cmp$diff
      if (!is.data.frame(diff) || nrow(diff) == 0L) {
        return(htmltools::p("No indicator-level changes between the snapshots.",
                            class = "text-muted"))
      }
      cols <- intersect(
        c("indicator_id", "indicator_domain", "change", "direction",
          "result_from", "result_to", "severity_from", "severity_to"),
        names(diff)
      )
      render_evidence_table(as.data.frame(diff), columns = cols, max_rows = 100L)
    })

    invisible(NULL)
  })
}
