#' Launch the R4SUB Dashboard
#'
#' Starts the interactive Shiny dashboard for exploring R4SUB evidence,
#' pillar scores, SCI, and sensitivity analysis.
#'
#' @param evidence An optional pre-loaded evidence data.frame. If `NULL`,
#'   the app shows an upload interface.
#' @param theme A `bslib::bs_theme` object. Default uses [r4sub_theme()].
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object (returned invisibly; runs interactively).
#'
#' @examples
#' \dontrun{
#' # With pre-loaded evidence
#' r4sub_app(evidence = my_evidence)
#'
#' # With upload interface
#' r4sub_app()
#' }
#'
#' @export
r4sub_app <- function(evidence = NULL, theme = r4sub_theme(), ...) {

  ui <- bslib::page_sidebar(
    title = "R4SUB Dashboard",
    theme = theme,
    sidebar = bslib::sidebar(
      title = "Data",
      shiny::fileInput("upload_evidence", "Upload Evidence (CSV/RDS)",
                       accept = c(".csv", ".rds")),
      shiny::actionButton("load_btn", "Load Data", class = "btn-primary w-100"),
      htmltools::hr(),
      shiny::uiOutput("data_status"),
      width = 280
    ),
    bslib::navset_tab(
      bslib::nav_panel("Overview", mod_overview_ui("overview")),
      bslib::nav_panel("Evidence", mod_evidence_ui("evidence")),
      bslib::nav_panel("Indicators", mod_indicators_ui("indicators")),
      bslib::nav_panel("Pillars", mod_pillar_detail_ui("pillars")),
      bslib::nav_panel("Sensitivity", mod_sensitivity_ui("sensitivity"))
    )
  )

  server <- function(input, output, session) {
    # Reactive evidence store
    ev_store <- shiny::reactiveVal(evidence)

    # Handle file upload
    shiny::observeEvent(input$load_btn, {
      file <- input$upload_evidence
      if (is.null(file)) {
        if (!is.null(evidence)) {
          ev_store(evidence)
          shiny::showNotification("Pre-loaded evidence activated.",
                                  type = "message")
        } else {
          shiny::showNotification("No file selected.", type = "warning")
        }
        return()
      }

      ext <- tolower(tools::file_ext(file$name))
      loaded <- tryCatch({
        if (ext == "rds") {
          readRDS(file$datapath)
        } else if (ext == "csv") {
          read.csv(file$datapath, stringsAsFactors = FALSE)
        } else {
          shiny::showNotification("Unsupported file type.", type = "error")
          return()
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message),
                                type = "error")
        NULL
      })

      if (is.null(loaded)) return()

      valid <- tryCatch({
        r4subcore::validate_evidence(loaded)
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste("Evidence validation failed:", e$message),
          type = "error"
        )
        FALSE
      })

      if (valid) {
        ev_store(loaded)
        shiny::showNotification(
          paste0("Loaded ", nrow(loaded), " evidence rows."),
          type = "message"
        )
      }
    })

    # Data status display
    output$data_status <- shiny::renderUI({
      ev <- ev_store()
      if (is.null(ev) || nrow(ev) == 0L) {
        return(htmltools::p("No data loaded.", class = "text-muted"))
      }

      n_rows   <- nrow(ev)
      n_sources <- length(unique(ev$source_name))
      n_domains <- length(unique(ev$indicator_domain))

      htmltools::tagList(
        htmltools::p(
          htmltools::tags$strong(n_rows), " evidence rows",
          class = "mb-1"
        ),
        htmltools::p(
          htmltools::tags$strong(n_sources), " source(s), ",
          htmltools::tags$strong(n_domains), " domain(s)",
          class = "mb-1 text-muted small"
        )
      )
    })

    # Wire up modules
    mod_overview_server("overview", ev_store)
    mod_evidence_server("evidence", ev_store)
    mod_indicators_server("indicators", ev_store)
    mod_pillar_detail_server("pillars", ev_store)
    mod_sensitivity_server("sensitivity", ev_store)
  }

  shiny::shinyApp(ui = ui, server = server, ...)
}
