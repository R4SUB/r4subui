#' Render an Evidence Table as HTML
#'
#' Creates a formatted HTML table from an evidence data.frame, suitable for
#' display in a Shiny app or static report.
#'
#' @param evidence A validated evidence data.frame.
#' @param columns Character vector of column names to display. Default
#'   shows key columns.
#' @param max_rows Maximum number of rows to display. Default `500`.
#'
#' @return An HTML tags object (`shiny::tags$table`).
#'
#' @examples
#' \dontrun{
#' render_evidence_table(evidence)
#' }
#'
#' @export
render_evidence_table <- function(
    evidence,
    columns = c("indicator_id", "indicator_domain", "severity",
                "result", "message", "location", "metric_value"),
    max_rows = 500L
) {
  if (!is.data.frame(evidence) || nrow(evidence) == 0L) {
    return(htmltools::p("No evidence data to display.",
                        class = "text-muted"))
  }

  cols <- intersect(columns, names(evidence))
  if (length(cols) == 0L) {
    return(htmltools::p("No matching columns found.", class = "text-muted"))
  }

  ev <- evidence[seq_len(min(nrow(evidence), max_rows)), cols, drop = FALSE]

  # Build header
  header_cells <- lapply(cols, function(cn) {
    htmltools::tags$th(cn, scope = "col")
  })
  header_row <- htmltools::tags$tr(tagList(header_cells))

  # Build body rows
  body_rows <- lapply(seq_len(nrow(ev)), function(i) {
    cells <- lapply(cols, function(cn) {
      val <- as.character(ev[[cn]][i])
      if (is.na(val)) val <- ""

      # Color-code severity and result
      cell_class <- ""
      if (cn == "severity") {
        sev_cls <- c(info = "text-primary", low = "text-success",
                     medium = "text-warning", high = "text-danger",
                     critical = "fw-bold text-danger")
        cell_class <- unname(sev_cls[val])
        if (is.na(cell_class)) cell_class <- ""
      }
      if (cn == "result") {
        res_cls <- c(pass = "text-success", warn = "text-warning",
                     fail = "text-danger", na = "text-muted")
        cell_class <- unname(res_cls[val])
        if (is.na(cell_class)) cell_class <- ""
      }

      htmltools::tags$td(val, class = cell_class)
    })
    htmltools::tags$tr(tagList(cells))
  })

  truncated_msg <- NULL
  if (nrow(evidence) > max_rows) {
    truncated_msg <- htmltools::p(
      paste0("Showing ", max_rows, " of ", nrow(evidence), " rows."),
      class = "text-muted small mt-2"
    )
  }

  htmltools::tagList(
    htmltools::tags$div(
      class = "table-responsive",
      htmltools::tags$table(
        class = "table table-sm table-striped table-hover",
        htmltools::tags$thead(header_row),
        htmltools::tags$tbody(tagList(body_rows))
      )
    ),
    truncated_msg
  )
}


#' Create a Summary Value Box
#'
#' Creates a styled value box for displaying a single metric.
#'
#' @param title Character label.
#' @param value The value to display.
#' @param theme Character theme: `"primary"`, `"success"`, `"warning"`,
#'   `"danger"`, `"info"`. Default `"primary"`.
#' @param subtitle Optional subtitle text.
#'
#' @return A `bslib::value_box` element.
#'
#' @examples
#' \dontrun{
#' r4sub_value_box("SCI Score", 85.2, theme = "success")
#' }
#'
#' @export
r4sub_value_box <- function(title, value, theme = "primary", subtitle = NULL) {
  bslib::value_box(
    title    = title,
    value    = value,
    showcase = NULL,
    theme    = theme,
    p(subtitle)
  )
}
