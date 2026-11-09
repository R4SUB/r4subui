#' Evidence Explorer Module UI
#'
#' Displays the full evidence table with filtering by domain, severity,
#' and result.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_evidence_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::selectInput(ns("filter_domain"), "Domain",
                         choices = c("All", "quality", "trace", "risk", "usability"),
                         selected = "All"),
      shiny::selectInput(ns("filter_severity"), "Severity",
                         choices = c("All", "info", "low", "medium", "high", "critical"),
                         selected = "All"),
      shiny::selectInput(ns("filter_result"), "Result",
                         choices = c("All", "pass", "warn", "fail", "na"),
                         selected = "All"),
      shiny::selectInput(ns("filter_source"), "Source",
                         choices = c("All"),
                         selected = "All")
    ),
    htmltools::hr(),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::uiOutput(ns("count_total")),
      shiny::uiOutput(ns("count_pass")),
      shiny::uiOutput(ns("count_warn")),
      shiny::uiOutput(ns("count_fail"))
    ),
    htmltools::hr(),
    shiny::uiOutput(ns("drill_banner")),
    shiny::uiOutput(ns("evidence_table"))
  )
}


#' Evidence Explorer Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @param drill_rv An optional reactive returning an `indicator_id` to drill
#'   into (as produced by [mod_indicators_server()]), or `NULL` for no
#'   drill-down. When set, the evidence table is narrowed to that indicator via
#'   [filter_evidence_by_indicator()]. Defaults to a reactive that always
#'   returns `NULL`.
#' @return Invisible `NULL`.
#'
#' @export
mod_evidence_server <- function(id, evidence_rv,
                                drill_rv = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Active indicator drill-down (NULL = no drill-down).
    drill_indicator <- shiny::reactiveVal(NULL)

    shiny::observeEvent(drill_rv(), {
      drill_indicator(drill_rv())
    }, ignoreNULL = FALSE)

    # Update source choices when evidence changes
    shiny::observe({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)
      sources <- c("All", sort(unique(ev$source_name)))
      shiny::updateSelectInput(session, "filter_source", choices = sources)
    })

    # Filtered evidence
    filtered <- shiny::reactive({
      ev <- evidence_rv()
      shiny::req(ev, nrow(ev) > 0L)

      if (!is.null(input$filter_domain) && input$filter_domain != "All") {
        ev <- ev[ev$indicator_domain == input$filter_domain, , drop = FALSE]
      }
      if (!is.null(input$filter_severity) && input$filter_severity != "All") {
        ev <- ev[ev$severity == input$filter_severity, , drop = FALSE]
      }
      if (!is.null(input$filter_result) && input$filter_result != "All") {
        ev <- ev[ev$result == input$filter_result, , drop = FALSE]
      }
      if (!is.null(input$filter_source) && input$filter_source != "All") {
        ev <- ev[ev$source_name == input$filter_source, , drop = FALSE]
      }
      # Apply the indicator drill-down via the pure helper.
      ev <- filter_evidence_by_indicator(ev, drill_indicator())
      ev
    })

    # Banner showing (and clearing) an active indicator drill-down.
    output$drill_banner <- shiny::renderUI({
      ind <- drill_indicator()
      if (is.null(ind) || !nzchar(ind)) return(NULL)
      ns <- session$ns
      htmltools::div(
        class = "alert alert-info d-flex justify-content-between align-items-center",
        htmltools::span(
          "Showing evidence for indicator ",
          htmltools::tags$strong(ind), "."
        ),
        shiny::actionButton(ns("clear_drill"), "Clear",
                            class = "btn-sm btn-outline-secondary")
      )
    })

    shiny::observeEvent(input$clear_drill, {
      drill_indicator(NULL)
    })

    # Count boxes
    output$count_total <- shiny::renderUI({
      r4sub_value_box("Filtered Rows", nrow(filtered()), theme = "info")
    })
    output$count_pass <- shiny::renderUI({
      ev <- filtered()
      n <- sum(ev$result == "pass", na.rm = TRUE)
      r4sub_value_box("Pass", n, theme = "success")
    })
    output$count_warn <- shiny::renderUI({
      ev <- filtered()
      n <- sum(ev$result == "warn", na.rm = TRUE)
      r4sub_value_box("Warn", n, theme = "warning")
    })
    output$count_fail <- shiny::renderUI({
      ev <- filtered()
      n <- sum(ev$result == "fail", na.rm = TRUE)
      r4sub_value_box("Fail", n, theme = "danger")
    })

    # Evidence table
    output$evidence_table <- shiny::renderUI({
      render_evidence_table(filtered())
    })

    invisible(NULL)
  })
}
