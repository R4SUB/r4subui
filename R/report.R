#' Build a Readiness Report Snapshot
#'
#' Produces a timestamped Markdown snapshot of the current submission readiness
#' state from an evidence data.frame. The report captures the headline
#' Submission Confidence Index (SCI), the decision band, per-pillar
#' contributions, and the indicators driving the largest score loss.
#'
#' This is a pure, non-reactive helper: given the same evidence (and
#' `generated_at`) it always returns the same string, which makes it easy to
#' unit test. The dashboard's download control calls it from a
#' [shiny::downloadHandler()], but it can also be used on its own to generate a
#' standalone snapshot.
#'
#' @param evidence A validated evidence data.frame, such as one produced by
#'   [generate_demo_evidence()].
#' @param generated_at A `POSIXct`/`Date` (or any object accepted by
#'   [format()]) used as the report timestamp. Defaults to [Sys.time()]. Pass a
#'   fixed value for reproducible output.
#' @param top_n Maximum number of top loss indicators to list. Default `10`.
#'
#' @return A single character string containing the Markdown report.
#'
#' @examples
#' ev <- suppressMessages(generate_demo_evidence(30))
#' report <- build_readiness_report(ev, generated_at = as.POSIXct("2026-01-01"))
#' cat(substr(report, 1, 160))
#'
#' @export
build_readiness_report <- function(evidence,
                                   generated_at = Sys.time(),
                                   top_n = 10L) {
  if (!is.data.frame(evidence) || nrow(evidence) == 0L) {
    stop("`evidence` must be a non-empty data.frame.", call. = FALSE)
  }

  ps   <- r4subscore::compute_pillar_scores(evidence)
  res  <- r4subscore::compute_sci(ps)
  expl <- r4subscore::sci_explain(evidence)

  study_id <- "Not specified"
  if ("study_id" %in% names(evidence)) {
    ids <- unique(evidence$study_id[!is.na(evidence$study_id)])
    if (length(ids) > 0L) study_id <- paste(ids, collapse = ", ")
  }

  band_label <- .format_band(res$band)
  ts <- format(generated_at, "%Y-%m-%d %H:%M:%S")

  lines <- c(
    "# R4SUB Readiness Report",
    "",
    paste0("**Generated:** ", ts),
    paste0("**Study:** ", study_id),
    "",
    "## Headline",
    "",
    paste0("- **Submission Confidence Index (SCI):** ",
           round(res$SCI, 1), " / 100"),
    paste0("- **Decision band:** ", band_label),
    paste0("- **Critical findings:** ", res$n_critical),
    paste0("- **Gated:** ", if (isTRUE(res$gated)) "Yes" else "No"),
    paste0("- **Evidence rows:** ", nrow(evidence)),
    ""
  )

  if (isTRUE(res$gated) && !is.na(res$gate_reason)) {
    lines <- c(lines, paste0("> Gate reason: ", res$gate_reason), "")
  }

  # Per-pillar contributions
  lines <- c(lines, "## Per-pillar contributions", "")
  pc <- expl$pillar_contributions
  if (is.data.frame(pc) && nrow(pc) > 0L) {
    pc <- pc[order(-pc$loss), , drop = FALSE]
    rows <- lapply(seq_len(nrow(pc)), function(i) {
      c(.title_case(pc$pillar[i]),
        paste0(round(pc$pillar_score[i] * 100, 1), "%"),
        format(round(pc$weight[i], 2), nsmall = 2),
        as.character(round(pc$contribution[i], 2)),
        as.character(round(pc$loss[i], 2)))
    })
    lines <- c(lines, .md_table(
      header = c("Pillar", "Score", "Weight", "Contribution", "Loss"),
      rows = rows
    ), "")
  } else {
    lines <- c(lines, "_No pillar contributions available._", "")
  }

  # Top loss indicators
  lines <- c(lines, "## Top loss indicators", "")
  ic <- expl$indicator_contributions
  if (is.data.frame(ic) && nrow(ic) > 0L) {
    ic <- ic[order(-ic$loss), , drop = FALSE]
    ic <- ic[seq_len(min(nrow(ic), top_n)), , drop = FALSE]
    rows <- lapply(seq_len(nrow(ic)), function(i) {
      c(ic$indicator_id[i],
        ic$indicator_domain[i],
        paste0(round(ic$indicator_score[i] * 100, 1), "%"),
        as.character(round(ic$loss[i], 2)),
        paste0(round(ic$pct_of_total_loss[i], 1), "%"))
    })
    lines <- c(lines, .md_table(
      header = c("Indicator", "Domain", "Score", "Loss", "% of total loss"),
      rows = rows
    ), "")
  } else {
    lines <- c(lines, "_No indicator contributions available._", "")
  }

  # Footer
  lines <- c(
    lines,
    "---",
    "",
    paste0("_This report is a decision-support snapshot generated from the ",
           "current dashboard state. It is not an official regulatory ",
           "submission document._")
  )

  paste(lines, collapse = "\n")
}


#' Format an SCI band as a readable label
#' @noRd
.format_band <- function(band) {
  if (is.null(band) || is.na(band)) return("Unclassified")
  .title_case(gsub("_", " ", band))
}


#' Capitalise the first letter of a string
#' @noRd
.title_case <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}


#' Build a simple Markdown table from a header and list of row vectors
#' @noRd
.md_table <- function(header, rows) {
  head_line <- paste0("| ", paste(header, collapse = " | "), " |")
  sep_line  <- paste0("| ", paste(rep("---", length(header)), collapse = " | "),
                      " |")
  body <- vapply(rows, function(r) {
    paste0("| ", paste(r, collapse = " | "), " |")
  }, character(1))
  c(head_line, sep_line, body)
}
