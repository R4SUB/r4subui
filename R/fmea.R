#' Arrange an FMEA Risk Register for Display
#'
#' Orders a risk register produced by [r4subrisk::create_risk_register()] by its
#' Risk Priority Number (RPN) so the highest-priority risks appear first, and
#' keeps a fixed set of display columns. This is a pure, non-reactive helper
#' behind the dashboard's FMEA tab.
#'
#' It performs no scoring of its own: the `rpn`, `probability`, `impact`,
#' `detectability`, and `risk_level` values are exactly those computed by
#' 'r4subrisk'. The helper only reorders rows and selects columns, so it is easy
#' to unit test without launching the app.
#'
#' @param register A risk register data.frame, as returned by
#'   [r4subrisk::create_risk_register()]. Must contain an `rpn` column.
#' @param columns Character vector of columns to keep, in display order. Columns
#'   that are not present in `register` are dropped silently. Defaults to the
#'   FMEA breakdown (`probability`, `impact`, `detectability`) alongside the
#'   `rpn` and `risk_level`.
#' @param max_rows Maximum number of rows to return. Default `100`.
#'
#' @return A data.frame with the selected columns, ordered by descending `rpn`
#'   (ties keep their original order) and truncated to `max_rows` rows. Row names
#'   are reset.
#'
#' @examples
#' if (requireNamespace("r4subrisk", quietly = TRUE)) {
#'   ev <- suppressMessages(generate_demo_evidence(40, seed = 1))
#'   risks <- suppressMessages(r4subrisk::evidence_to_risks(ev))
#'   reg <- suppressMessages(r4subrisk::create_risk_register(risks))
#'   head(fmea_register_view(reg))
#' }
#'
#' @export
fmea_register_view <- function(
    register,
    columns = c("risk_id", "description", "category", "probability",
                "impact", "detectability", "rpn", "risk_level"),
    max_rows = 100L
) {
  if (!is.data.frame(register) || nrow(register) == 0L) {
    return(data.frame())
  }
  if (!"rpn" %in% names(register)) {
    stop("`register` must contain an `rpn` column.", call. = FALSE)
  }

  reg <- as.data.frame(register, stringsAsFactors = FALSE)
  # Highest RPN first; order() is stable, so equal-RPN rows keep their order.
  reg <- reg[order(-reg$rpn), , drop = FALSE]

  cols <- intersect(columns, names(reg))
  reg <- reg[seq_len(min(nrow(reg), max_rows)), cols, drop = FALSE]
  rownames(reg) <- NULL
  reg
}


#' Summarise an FMEA Pareto Analysis
#'
#' Reads the headline figures out of a Pareto analysis produced by
#' [r4subrisk::risk_pareto()] so they can drive the dashboard's FMEA summary
#' boxes. This is a pure, non-reactive helper.
#'
#' Every value it returns is read straight from the Pareto object (its rows and
#' its `total_rpn`, `n_vital_few`, and `cum_cutoff` attributes); the helper
#' computes no new risk scores. It is therefore easy to unit test on its own.
#'
#' @param pareto A `risk_pareto` object, as returned by
#'   [r4subrisk::risk_pareto()]. Expected to carry the columns `rpn` and
#'   `vital_few` and the attributes `total_rpn`, `n_vital_few`, and
#'   `cum_cutoff`.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`n_risks`}{Number of risks in the register (`nrow(pareto)`).}
#'     \item{`total_rpn`}{Total RPN across the register.}
#'     \item{`n_vital_few`}{Number of "vital few" risks that make up the
#'       cumulative share cut-off.}
#'     \item{`cum_cutoff`}{The cumulative-share cut-off used (for example `0.8`).}
#'     \item{`top_rpn`}{The largest single RPN in the register (`0` if empty).}
#'     \item{`vital_few_share`}{Fraction of all risks that are "vital few"
#'       (`0` if empty).}
#'   }
#'
#' @examples
#' if (requireNamespace("r4subrisk", quietly = TRUE)) {
#'   ev <- suppressMessages(generate_demo_evidence(40, seed = 1))
#'   risks <- suppressMessages(r4subrisk::evidence_to_risks(ev))
#'   reg <- suppressMessages(r4subrisk::create_risk_register(risks))
#'   fmea_pareto_summary(r4subrisk::risk_pareto(reg))
#' }
#'
#' @export
fmea_pareto_summary <- function(pareto) {
  if (!is.data.frame(pareto)) {
    stop("`pareto` must be a data.frame from r4subrisk::risk_pareto().",
         call. = FALSE)
  }
  n_risks <- nrow(pareto)

  total_rpn <- attr(pareto, "total_rpn")
  if (is.null(total_rpn)) {
    total_rpn <- if ("rpn" %in% names(pareto)) sum(pareto$rpn, na.rm = TRUE) else 0
  }

  n_vital_few <- attr(pareto, "n_vital_few")
  if (is.null(n_vital_few)) {
    n_vital_few <- if ("vital_few" %in% names(pareto)) {
      sum(pareto$vital_few, na.rm = TRUE)
    } else {
      0L
    }
  }

  cum_cutoff <- attr(pareto, "cum_cutoff")
  if (is.null(cum_cutoff)) cum_cutoff <- NA_real_

  top_rpn <- if (n_risks > 0L && "rpn" %in% names(pareto)) {
    max(pareto$rpn, na.rm = TRUE)
  } else {
    0
  }

  list(
    n_risks         = n_risks,
    total_rpn       = unname(total_rpn),
    n_vital_few     = unname(as.integer(n_vital_few)),
    cum_cutoff      = unname(cum_cutoff),
    top_rpn         = unname(top_rpn),
    vital_few_share = if (n_risks > 0L) as.integer(n_vital_few) / n_risks else 0
  )
}
