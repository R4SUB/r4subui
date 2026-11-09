#' Filter Evidence Rows to a Single Indicator
#'
#' Returns the subset of an evidence data.frame belonging to one indicator.
#' This is the pure, non-reactive helper behind the dashboard's
#' indicator-to-evidence drill-down: selecting an indicator row in the
#' Indicators tab (from `r4subscore::sci_explain()$indicator_contributions`)
#' uses this function to narrow the Evidence tab to that indicator's rows.
#'
#' Because it is a plain data-frame-in, data-frame-out function with no reactive
#' or `shiny` dependencies, it is easy to unit test on its own. The reactive
#' wiring in [mod_evidence_server()] simply calls it.
#'
#' @param evidence A validated evidence data.frame, such as one produced by
#'   [generate_demo_evidence()]. Must contain an `indicator_id` column.
#' @param indicator_id A single indicator identifier to keep (for example
#'   `"Q-MISS-VAR"`). If `NULL`, `NA`, or empty, the evidence is returned
#'   unchanged, which lets callers treat "no selection" as "no filter".
#'
#' @return A data.frame with the same columns as `evidence`, containing only the
#'   rows whose `indicator_id` matches. When nothing matches, a zero-row
#'   data.frame with the original structure is returned.
#'
#' @examples
#' ev <- suppressMessages(generate_demo_evidence(40))
#' sub <- filter_evidence_by_indicator(ev, "Q-MISS-VAR")
#' nrow(sub)
#' unique(sub$indicator_id)
#'
#' # A NULL selection is a no-op and returns every row.
#' nrow(filter_evidence_by_indicator(ev, NULL)) == nrow(ev)
#'
#' @export
filter_evidence_by_indicator <- function(evidence, indicator_id) {
  if (!is.data.frame(evidence)) {
    stop("`evidence` must be a data.frame.", call. = FALSE)
  }
  if (!"indicator_id" %in% names(evidence)) {
    stop("`evidence` must contain an `indicator_id` column.", call. = FALSE)
  }

  # Treat an absent selection as "keep everything".
  if (is.null(indicator_id) || length(indicator_id) == 0L) {
    return(evidence)
  }
  if (length(indicator_id) > 1L) {
    stop("`indicator_id` must be a single value.", call. = FALSE)
  }
  if (is.na(indicator_id) || !nzchar(as.character(indicator_id))) {
    return(evidence)
  }

  keep <- !is.na(evidence$indicator_id) &
    evidence$indicator_id == as.character(indicator_id)
  evidence[keep, , drop = FALSE]
}
