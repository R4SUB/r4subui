#' Authority Profile Module UI
#'
#' Displays the regulatory authority profile selector, profile summary,
#' and requirement coverage table.
#'
#' @param id Module namespace ID.
#' @return A Shiny UI element.
#'
#' @export
mod_authority_ui <- function(id) {
  ns <- shiny::NS(id)

  authorities <- c("FDA", "EMA", "PMDA", "ANVISA", "Health Canada", "MHRA")
  sub_types   <- c("NDA", "BLA", "MAA", "JNDA", "SNDA", "IND")

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::selectInput(ns("authority"), "Authority",
                         choices = authorities, selected = "FDA"),
      shiny::selectInput(ns("sub_type"), "Submission Type",
                         choices = sub_types, selected = "NDA"),
      shiny::actionButton(ns("run_profile"), "Load Profile",
                          class = "btn-primary mt-4 w-100")
    ),
    htmltools::hr(),
    shiny::uiOutput(ns("profile_summary")),
    htmltools::hr(),
    htmltools::h4("Pillar Weights"),
    shiny::uiOutput(ns("weights_table")),
    htmltools::hr(),
    htmltools::h4("Requirement Coverage"),
    shiny::uiOutput(ns("coverage_table"))
  )
}


#' Authority Profile Module Server
#'
#' @param id Module namespace ID.
#' @param evidence_rv A reactive returning a validated evidence data.frame.
#' @return Invisible `NULL`.
#'
#' @export
mod_authority_server <- function(id, evidence_rv) {
  shiny::moduleServer(id, function(input, output, session) {

    profile_rv <- shiny::eventReactive(input$run_profile, {
      tryCatch(
        r4subprofile::submission_profile(
          authority       = input$authority,
          submission_type = input$sub_type
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Profile error:", e$message), type = "error"
          )
          NULL
        }
      )
    })

    output$profile_summary <- shiny::renderUI({
      prof <- profile_rv()
      if (is.null(prof)) {
        return(htmltools::p(
          "Select an authority and submission type, then click 'Load Profile'.",
          class = "text-muted"
        ))
      }

      summary_df <- tryCatch(
        r4subprofile::profile_summary(prof),
        error = function(e) NULL
      )

      if (is.null(summary_df)) {
        return(htmltools::p(
          paste0("Profile loaded: ", prof$authority, " ", prof$submission_type),
          class = "text-info"
        ))
      }

      htmltools::tagList(
        htmltools::h4(paste0(prof$authority, " \u2014 ", prof$submission_type, " Profile")),
        render_evidence_table(summary_df, columns = names(summary_df), max_rows = 20L)
      )
    })

    output$weights_table <- shiny::renderUI({
      prof <- profile_rv()
      if (is.null(prof)) {
        return(htmltools::p("No profile loaded.", class = "text-muted"))
      }

      weights <- prof$pillar_weights
      if (is.null(weights)) {
        return(htmltools::p("No pillar weights in profile.", class = "text-muted"))
      }

      wdf <- data.frame(
        pillar = names(weights),
        weight = unname(weights),
        weight_pct = paste0(round(unname(weights) * 100, 1), "%"),
        stringsAsFactors = FALSE
      )
      render_evidence_table(wdf, columns = names(wdf), max_rows = 10L)
    })

    output$coverage_table <- shiny::renderUI({
      prof <- profile_rv()
      ev   <- evidence_rv()
      if (is.null(prof)) {
        return(htmltools::p("No profile loaded.", class = "text-muted"))
      }
      if (is.null(ev) || nrow(ev) == 0L) {
        return(htmltools::p("No evidence loaded.", class = "text-muted"))
      }

      reqs <- prof$requirements
      if (is.null(reqs) || length(reqs) == 0L) {
        return(htmltools::p("No requirements defined in profile.", class = "text-muted"))
      }

      covered <- sapply(reqs, function(r) {
        any(ev$indicator_id == r, na.rm = TRUE)
      })

      cov_df <- data.frame(
        requirement = reqs,
        covered     = ifelse(covered, "yes", "no"),
        stringsAsFactors = FALSE
      )
      render_evidence_table(cov_df, columns = names(cov_df), max_rows = 50L)
    })

    invisible(NULL)
  })
}
