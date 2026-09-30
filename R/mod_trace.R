#' Traceability Module UI
#'
#' Displays traceability indicator scores and evidence coverage.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_trace_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::h4("Traceability Coverage"),
    shiny::uiOutput(ns("trace_summary")),
    htmltools::hr(),
    htmltools::h4("Indicator Scores"),
    shiny::uiOutput(ns("indicator_table")),
    htmltools::hr(),
    htmltools::h4("Result Distribution"),
    shiny::plotOutput(ns("trace_chart"), height = "280px"),
    htmltools::hr(),
    htmltools::h4("Change Impact Analysis"),
    htmltools::p(
      "Pick a source variable to see the ADaM variables that derive from it, ",
      "directly or through a chain. Uses a demo trace model built from the ",
      "'r4subdata' metadata.",
      class = "text-muted small"
    ),
    shiny::selectInput(ns("changed_var"), "Changed source variable",
                       choices = NULL),
    shiny::uiOutput(ns("impact_table"))
  )
}


#' Traceability Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_trace_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    trace_ev <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      ev[ev$indicator_domain == "trace", , drop = FALSE]
    })

    indicator_scores <- shiny::reactive({
      ev <- trace_ev()
      shiny::req(nrow(ev) > 0L)
      tryCatch(
        r4subtrace::trace_indicator_scores(ev),
        error = function(e) NULL
      )
    })

    output$trace_summary <- shiny::renderUI({
      ev <- trace_ev()
      if (nrow(ev) == 0L) {
        return(htmltools::p("No traceability evidence loaded.", class = "text-muted"))
      }
      n_pass <- sum(ev$result == "pass", na.rm = TRUE)
      n_fail <- sum(ev$result == "fail", na.rm = TRUE)
      n_warn <- sum(ev$result == "warn", na.rm = TRUE)
      pct <- if (nrow(ev) > 0L) round(n_pass / nrow(ev) * 100, 1) else 0

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        r4sub_value_box("Trace Indicators", nrow(ev), theme = "info"),
        r4sub_value_box("Passed", n_pass, theme = "success"),
        r4sub_value_box("Warnings", n_warn, theme = if (n_warn > 0) "warning" else "success"),
        r4sub_value_box("Failed", n_fail, theme = if (n_fail > 0) "danger" else "success")
      )
    })

    output$indicator_table <- shiny::renderUI({
      scores <- indicator_scores()
      if (is.null(scores) || nrow(scores) == 0L) {
        ev <- trace_ev()
        if (nrow(ev) == 0L) {
          return(htmltools::p("No traceability evidence.", class = "text-muted"))
        }
        cols <- intersect(
          c("indicator_id", "indicator_name", "severity", "result",
            "metric_value", "message", "location"),
          names(ev)
        )
        return(render_evidence_table(ev, columns = cols, max_rows = 50L))
      }
      render_evidence_table(scores, columns = names(scores), max_rows = 50L)
    })

    output$trace_chart <- shiny::renderPlot({
      ev <- trace_ev()
      shiny::req(nrow(ev) > 0L)

      results <- c("pass", "warn", "fail", "na")
      counts <- sapply(results, function(r) sum(ev$result == r, na.rm = TRUE))
      rc <- result_colors()

      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      par(mar = c(4, 4, 2, 2))
      barplot(
        counts,
        names.arg = results,
        col       = c(pass = "#27AE60", warn = "#F39C12",
                      fail = "#E74C3C", na = "#BDC3C7")[results],
        border    = NA,
        ylab      = "Count",
        main      = "Trace Results",
        las       = 1
      )
    })

    # Demo trace model from the r4subdata metadata (a suggested package).
    trace_model_demo <- shiny::reactive({
      if (!requireNamespace("r4subdata", quietly = TRUE)) return(NULL)
      tryCatch(
        suppressMessages(r4subtrace::build_trace_model(
          r4subdata::adam_metadata,
          r4subdata::sdtm_metadata,
          r4subdata::trace_mapping
        )),
        error = function(e) NULL
      )
    })

    # Populate the picker with source (SDTM) variables, which are the ones that
    # have downstream ADaM dependents.
    shiny::observe({
      tm <- trace_model_demo()
      if (is.null(tm)) return()
      nodes <- tm$nodes[tm$nodes$node_type == "variable" &
                          tm$nodes$role == "sdtm", , drop = FALSE]
      labs <- sort(unique(paste0(nodes$dataset, ".", nodes$variable)))
      if (length(labs) == 0L) return()
      sel <- if ("DM.AGE" %in% labs) "DM.AGE" else labs[1]
      shiny::updateSelectInput(session, "changed_var",
                               choices = labs, selected = sel)
    })

    impact_result <- shiny::reactive({
      tm <- trace_model_demo()
      shiny::req(tm, input$changed_var)
      tryCatch(
        suppressMessages(r4subtrace::trace_impact(tm, changed = input$changed_var)),
        error = function(e) NULL
      )
    })

    output$impact_table <- shiny::renderUI({
      tm <- trace_model_demo()
      if (is.null(tm)) {
        return(htmltools::p(
          "Change impact needs the r4subdata metadata package.",
          class = "text-muted"
        ))
      }
      imp <- impact_result()
      if (is.null(imp) || nrow(imp) == 0L) {
        return(htmltools::p(
          "No downstream variables derive from the selected variable.",
          class = "text-muted"
        ))
      }
      render_evidence_table(
        as.data.frame(imp),
        columns = c("dataset", "variable", "role", "depth", "path"),
        max_rows = 100L
      )
    })

    invisible(NULL)
  })
}
