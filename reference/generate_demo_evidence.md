# Generate Demo Evidence Data

Creates a realistic demo evidence data.frame covering all four pillars,
multiple indicators, and mixed results. Useful for testing the
dashboard.

## Usage

``` r
generate_demo_evidence(n_rows = 50L, study_id = "DEMO-001", seed = 42L)
```

## Arguments

- n_rows:

  Number of evidence rows to generate. Default `50`.

- study_id:

  Study identifier. Default `"DEMO-001"`.

- seed:

  Optional integer seed for reproducible output. The caller's
  random-number state is restored on exit, so setting it has no lasting
  side effect. Pass `NULL` to use the current RNG state. Default `42`.

## Value

A validated evidence data.frame.

## Examples

``` r
ev <- suppressMessages(generate_demo_evidence())
nrow(ev)
#> [1] 50
```
