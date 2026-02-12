#' Default R4SUB Dashboard Theme
#'
#' Returns a `bslib::bs_theme()` for the R4SUB dashboard with a clean,
#' professional look.
#'
#' @param primary Primary accent color. Default `"#2C3E50"`.
#' @param success Success/pass color. Default `"#27AE60"`.
#' @param warning Warning color. Default `"#F39C12"`.
#' @param danger Danger/fail color. Default `"#E74C3C"`.
#' @param font_scale Numeric font scale factor. Default `1`.
#'
#' @return A `bslib::bs_theme` object.
#'
#' @examples
#' theme <- r4sub_theme()
#'
#' @export
r4sub_theme <- function(primary = "#2C3E50",
                        success = "#27AE60",
                        warning = "#F39C12",
                        danger  = "#E74C3C",
                        font_scale = 1) {
  bslib::bs_theme(
    version   = 5,
    bootswatch = "flatly",
    primary   = primary,
    success   = success,
    warning   = warning,
    danger    = danger,
    font_scale = font_scale,
    "enable-rounded" = TRUE
  )
}


#' Color palette for SCI bands
#' @noRd
band_colors <- function() {
  c(
    ready       = "#27AE60",
    minor_gaps  = "#F39C12",
    conditional = "#E67E22",
    high_risk   = "#E74C3C",
    unclassified = "#95A5A6"
  )
}


#' Color palette for risk levels
#' @noRd
risk_colors <- function() {
  c(
    critical = "#E74C3C",
    high     = "#E67E22",
    medium   = "#F39C12",
    low      = "#27AE60",
    unclassified = "#95A5A6"
  )
}


#' Color palette for result types
#' @noRd
result_colors <- function() {
  c(
    pass = "#27AE60",
    warn = "#F39C12",
    fail = "#E74C3C",
    na   = "#95A5A6"
  )
}


#' Color palette for severity levels
#' @noRd
severity_colors <- function() {
  c(
    info     = "#3498DB",
    low      = "#27AE60",
    medium   = "#F39C12",
    high     = "#E67E22",
    critical = "#E74C3C"
  )
}
