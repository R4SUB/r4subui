# Filter Evidence Rows to a Single Indicator

Returns the subset of an evidence data.frame belonging to one indicator.
This is the pure, non-reactive helper behind the dashboard's
indicator-to-evidence drill-down: selecting an indicator row in the
Indicators tab (from
`r4subscore::sci_explain()$indicator_contributions`) uses this function
to narrow the Evidence tab to that indicator's rows.

## Usage

``` r
filter_evidence_by_indicator(evidence, indicator_id)
```

## Arguments

- evidence:

  A validated evidence data.frame, such as one produced by
  [`generate_demo_evidence()`](https://r4sub.github.io/r4subui/reference/generate_demo_evidence.md).
  Must contain an `indicator_id` column.

- indicator_id:

  A single indicator identifier to keep (for example `"Q-MISS-VAR"`). If
  `NULL`, `NA`, or empty, the evidence is returned unchanged, which lets
  callers treat "no selection" as "no filter".

## Value

A data.frame with the same columns as `evidence`, containing only the
rows whose `indicator_id` matches. When nothing matches, a zero-row
data.frame with the original structure is returned.

## Details

Because it is a plain data-frame-in, data-frame-out function with no
reactive or `shiny` dependencies, it is easy to unit test on its own.
The reactive wiring in
[`mod_evidence_server()`](https://r4sub.github.io/r4subui/reference/mod_evidence_server.md)
simply calls it.

## Examples

``` r
ev <- suppressMessages(generate_demo_evidence(40))
sub <- filter_evidence_by_indicator(ev, "Q-MISS-VAR")
nrow(sub)
#> [1] 5
unique(sub$indicator_id)
#> [1] "Q-MISS-VAR"

# A NULL selection is a no-op and returns every row.
nrow(filter_evidence_by_indicator(ev, NULL)) == nrow(ev)
#> [1] TRUE
```
