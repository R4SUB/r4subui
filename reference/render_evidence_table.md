# Render an Evidence Table as HTML

Creates a formatted HTML table from an evidence data.frame, suitable for
display in a Shiny app or static report.

## Usage

``` r
render_evidence_table(
  evidence,
  columns = c("indicator_id", "indicator_domain", "severity", "result", "message",
    "location", "metric_value"),
  max_rows = 500L
)
```

## Arguments

- evidence:

  A validated evidence data.frame.

- columns:

  Character vector of column names to display. Default shows key
  columns.

- max_rows:

  Maximum number of rows to display. Default `500`.

## Value

An HTML tags object (`shiny::tags$table`).

## Examples

``` r
if (FALSE) { # \dontrun{
render_evidence_table(evidence)
} # }
```
