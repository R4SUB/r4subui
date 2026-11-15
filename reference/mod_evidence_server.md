# Evidence Explorer Module Server

Evidence Explorer Module Server

## Usage

``` r
mod_evidence_server(id, evidence_rv, drill_rv = shiny::reactive(NULL))
```

## Arguments

- id:

  Module namespace ID.

- evidence_rv:

  A reactive returning a validated evidence data.frame.

- drill_rv:

  An optional reactive returning an `indicator_id` to drill into (as
  produced by
  [`mod_indicators_server()`](https://r4sub.github.io/r4subui/reference/mod_indicators_server.md)),
  or `NULL` for no drill-down. When set, the evidence table is narrowed
  to that indicator via
  [`filter_evidence_by_indicator()`](https://r4sub.github.io/r4subui/reference/filter_evidence_by_indicator.md).
  Defaults to a reactive that always returns `NULL`.

## Value

Invisible `NULL`.
