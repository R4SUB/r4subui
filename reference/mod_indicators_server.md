# Indicators Module Server

Indicators Module Server

## Usage

``` r
mod_indicators_server(id, evidence_rv)
```

## Arguments

- id:

  Module namespace ID.

- evidence_rv:

  A reactive returning a validated evidence data.frame.

## Value

A reactive returning the `indicator_id` the user last chose to drill
into (a single string), or `NULL` before any selection. The app wires
this to the Evidence tab so the chosen indicator filters the evidence
rows.
