#' Compare Submission Readiness Between Two Evidence Snapshots
#'
#' Compares a baseline evidence snapshot against a current one and returns the
#' change in headline readiness: the Submission Confidence Index (SCI) delta,
#' the decision-band move, the per-pillar score deltas, and a row-level evidence
#' diff summary (how many indicators were added, removed, improved, or
#' regressed).
#'
#' This is the pure, non-reactive helper behind the dashboard's Compare tab. It
#' only surfaces values the engine packages already compute
#' ([r4subscore::compute_pillar_scores()], [r4subscore::compute_sci()], and
#' [r4subcore::evidence_diff()] / [r4subcore::evidence_diff_summary()]); it does
#' not define any new scoring or regulatory semantics. Because it is a plain
#' data-frame-in, list-out function with no `shiny` dependency, it is easy to
#' unit test on its own.
#'
#' @param baseline A validated evidence data.frame taken as the earlier
#'   ("before") snapshot, such as one produced by [generate_demo_evidence()].
#' @param current A validated evidence data.frame taken as the later ("after")
#'   snapshot. The diff is computed as `baseline` -> `current`, so an
#'   improvement in `current` shows as a positive `net_delta`.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`sci`}{A list with the `before` and `after` SCI values and their
#'       `delta` (`after - before`).}
#'     \item{`band`}{A list with the `before` and `after` decision bands and a
#'       logical `changed` flag.}
#'     \item{`n_critical`}{A list with the `before` and `after` critical-finding
#'       counts and their `delta`.}
#'     \item{`pillars`}{A data.frame with one row per pillar: `pillar`,
#'       `score_before`, `score_after`, `delta`, and `weight`, ordered by the
#'       size of the move (largest absolute delta first).}
#'     \item{`diff`}{The row-level [r4subcore::evidence_diff()] data.frame of
#'       changed, added, and removed indicators.}
#'     \item{`diff_summary`}{A named numeric vector of the
#'       [r4subcore::evidence_diff_summary()] metrics (for example `n_improved`,
#'       `n_regressed`, `net_delta`).}
#'   }
#'
#' @examples
#' before <- suppressMessages(generate_demo_evidence(60, seed = 1))
#' after  <- suppressMessages(generate_demo_evidence(60, seed = 2))
#' cmp <- compare_readiness(before, after)
#' cmp$sci$delta
#' cmp$pillars
#'
#' @export
compare_readiness <- function(baseline, current) {
  if (!is.data.frame(baseline) || nrow(baseline) == 0L) {
    stop("`baseline` must be a non-empty data.frame.", call. = FALSE)
  }
  if (!is.data.frame(current) || nrow(current) == 0L) {
    stop("`current` must be a non-empty data.frame.", call. = FALSE)
  }

  ps_before <- r4subscore::compute_pillar_scores(baseline)
  ps_after  <- r4subscore::compute_pillar_scores(current)
  sci_before <- r4subscore::compute_sci(ps_before)
  sci_after  <- r4subscore::compute_sci(ps_after)

  pb <- as.data.frame(ps_before)[, c("pillar", "pillar_score", "weight")]
  pa <- as.data.frame(ps_after)[, c("pillar", "pillar_score")]
  names(pb) <- c("pillar", "score_before", "weight")
  names(pa) <- c("pillar", "score_after")

  pillars <- merge(pb, pa, by = "pillar", all = TRUE)
  pillars$delta <- pillars$score_after - pillars$score_before
  pillars <- pillars[, c("pillar", "score_before", "score_after",
                         "delta", "weight")]
  # Largest movers first; NA deltas (a pillar present in only one snapshot)
  # sort last.
  ord <- order(-abs(pillars$delta), na.last = TRUE)
  pillars <- pillars[ord, , drop = FALSE]
  rownames(pillars) <- NULL

  diff <- r4subcore::evidence_diff(baseline, current)
  ds <- r4subcore::evidence_diff_summary(diff)
  diff_summary <- stats::setNames(ds$value, ds$metric)

  list(
    sci = list(
      before = sci_before$SCI,
      after  = sci_after$SCI,
      delta  = sci_after$SCI - sci_before$SCI
    ),
    band = list(
      before  = sci_before$band,
      after   = sci_after$band,
      changed = !identical(sci_before$band, sci_after$band)
    ),
    n_critical = list(
      before = sci_before$n_critical,
      after  = sci_after$n_critical,
      delta  = sci_after$n_critical - sci_before$n_critical
    ),
    pillars      = pillars,
    diff         = diff,
    diff_summary = diff_summary
  )
}
