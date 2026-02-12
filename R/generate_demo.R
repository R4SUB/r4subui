#' Generate Demo Evidence Data
#'
#' Creates a realistic demo evidence data.frame covering all four pillars,
#' multiple indicators, and mixed results. Useful for testing the dashboard.
#'
#' @param n_rows Approximate number of evidence rows to generate.
#'   Default `50`.
#' @param study_id Study identifier. Default `"DEMO-001"`.
#'
#' @return A validated evidence data.frame.
#'
#' @examples
#' ev <- generate_demo_evidence()
#' nrow(ev)
#'
#' @export
generate_demo_evidence <- function(n_rows = 50L, study_id = "DEMO-001") {
  ctx <- r4subcore::r4sub_run_context(study_id = study_id, environment = "DEV")

  # Define indicator pool
  indicators <- data.frame(
    indicator_id     = c("Q-MISS-VAR", "Q-TYPE-MISMATCH", "Q-LABEL-LEN",
                          "Q-DUP-RECORDS", "Q-FORMAT-ERR",
                          "T-ORPHAN-VAR", "T-TRACE-LEVEL", "T-AMBIGUOUS-MAP",
                          "T-DERIVATION-DOC",
                          "R-HIGH-RPN", "R-OPEN-RISK", "R-MITIGATION-GAP",
                          "U-DEFINE-COMPLETE", "U-REVIEWER-NOTE", "U-LABEL-QUALITY"),
    indicator_name   = c("Missing variable", "Type mismatch", "Label too long",
                          "Duplicate records", "Format error",
                          "Orphan ADaM variable", "Trace level check",
                          "Ambiguous mapping", "Derivation documentation",
                          "High RPN risk item", "Open risk", "Mitigation gap",
                          "Define.xml completeness", "Reviewer note",
                          "Label quality"),
    indicator_domain = c("quality", "quality", "quality", "quality", "quality",
                          "trace", "trace", "trace", "trace",
                          "risk", "risk", "risk",
                          "usability", "usability", "usability"),
    stringsAsFactors = FALSE
  )

  datasets <- c("ADSL", "ADAE", "ADLB", "ADCM", "ADTTE")
  severities <- c("info", "low", "medium", "high", "critical")
  results <- c("pass", "pass", "pass", "warn", "fail")  # bias toward pass

  set.seed(42)
  rows <- lapply(seq_len(n_rows), function(i) {
    ind_idx <- sample.int(nrow(indicators), 1)
    ds <- sample(datasets, 1)
    sev <- sample(severities, 1, prob = c(0.2, 0.25, 0.25, 0.2, 0.1))
    res <- sample(results, 1)

    data.frame(
      asset_type       = "dataset",
      asset_id         = ds,
      source_name      = "r4sub_demo",
      source_version   = "0.1.0",
      indicator_id     = indicators$indicator_id[ind_idx],
      indicator_name   = indicators$indicator_name[ind_idx],
      indicator_domain = indicators$indicator_domain[ind_idx],
      severity         = sev,
      result           = res,
      metric_value     = NA_real_,
      metric_unit      = NA_character_,
      message          = paste0(indicators$indicator_name[ind_idx],
                                " on ", ds),
      location         = paste0(ds, ":row_", sample(1:100, 1)),
      evidence_payload = "{}",
      stringsAsFactors = FALSE
    )
  })

  ev <- do.call(rbind, rows)
  r4subcore::as_evidence(ev, ctx = ctx)
}
