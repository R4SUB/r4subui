# Build a Readiness Report Snapshot

Produces a timestamped Markdown snapshot of the current submission
readiness state from an evidence data.frame. The report captures the
headline Submission Confidence Index (SCI), the decision band,
per-pillar contributions, and the indicators driving the largest score
loss.

## Usage

``` r
build_readiness_report(evidence, generated_at = Sys.time(), top_n = 10L)
```

## Arguments

- evidence:

  A validated evidence data.frame, such as one produced by
  [`generate_demo_evidence()`](https://r4sub.github.io/r4subui/reference/generate_demo_evidence.md).

- generated_at:

  A `POSIXct`/`Date` (or any object accepted by
  [`format()`](https://rdrr.io/r/base/format.html)) used as the report
  timestamp. Defaults to
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html). Pass a fixed
  value for reproducible output.

- top_n:

  Maximum number of top loss indicators to list. Default `10`.

## Value

A single character string containing the Markdown report.

## Details

This is a pure, non-reactive helper: given the same evidence (and
`generated_at`) it always returns the same string, which makes it easy
to unit test. The dashboard's download control calls it from a
[`shiny::downloadHandler()`](https://rdrr.io/pkg/shiny/man/downloadHandler.html),
but it can also be used on its own to generate a standalone snapshot.

## Examples

``` r
ev <- suppressMessages(generate_demo_evidence(30))
report <- build_readiness_report(ev, generated_at = as.POSIXct("2026-01-01"))
cat(substr(report, 1, 160))
#> # R4SUB Readiness Report
#> 
#> **Generated:** 2026-01-01 00:00:00
#> **Study:** DEMO-001
#> 
#> ## Headline
#> 
#> - **Submission Confidence Index (SCI):** 34.2 / 100
#> - **Decision 
```
