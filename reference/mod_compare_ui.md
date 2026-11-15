# Compare Module UI

Lets the reviewer upload a baseline evidence snapshot and compare the
current (loaded) evidence against it: the SCI and decision-band move,
the per-pillar score deltas, and a summary of which indicators improved
or regressed. All of the numbers come from the engine packages via
[`compare_readiness()`](https://r4sub.github.io/r4subui/reference/compare_readiness.md).

## Usage

``` r
mod_compare_ui(id)
```

## Arguments

- id:

  Module namespace ID.

## Value

A Shiny UI element.
