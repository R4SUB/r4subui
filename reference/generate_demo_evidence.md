# Generate Demo Evidence Data

Creates a realistic demo evidence data.frame covering all four pillars,
multiple indicators, and mixed results. Useful for testing the
dashboard.

## Usage

``` r
generate_demo_evidence(n_rows = 50L, study_id = "DEMO-001")
```

## Arguments

- n_rows:

  Approximate number of evidence rows to generate. Default `50`.

- study_id:

  Study identifier. Default `"DEMO-001"`.

## Value

A validated evidence data.frame.

## Examples

``` r
ev <- generate_demo_evidence()
#> ℹ Run context created: "R4S-20260316113000-wl4dieex"
#> ✔ Evidence table created: 50 rows
nrow(ev)
#> [1] 50
```
