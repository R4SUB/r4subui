# Summarise an FMEA Pareto Analysis

Reads the headline figures out of a Pareto analysis produced by
[`r4subrisk::risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.html)
so they can drive the dashboard's FMEA summary boxes. This is a pure,
non-reactive helper.

## Usage

``` r
fmea_pareto_summary(pareto)
```

## Arguments

- pareto:

  A `risk_pareto` object, as returned by
  [`r4subrisk::risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.html).
  Expected to carry the columns `rpn` and `vital_few` and the attributes
  `total_rpn`, `n_vital_few`, and `cum_cutoff`.

## Value

A named list with elements:

- `n_risks`:

  Number of risks in the register (`nrow(pareto)`).

- `total_rpn`:

  Total RPN across the register.

- `n_vital_few`:

  Number of "vital few" risks that make up the cumulative share cut-off.

- `cum_cutoff`:

  The cumulative-share cut-off used (for example `0.8`).

- `top_rpn`:

  The largest single RPN in the register (`0` if empty).

- `vital_few_share`:

  Fraction of all risks that are "vital few" (`0` if empty).

## Details

Every value it returns is read straight from the Pareto object (its rows
and its `total_rpn`, `n_vital_few`, and `cum_cutoff` attributes); the
helper computes no new risk scores. It is therefore easy to unit test on
its own.

## Examples

``` r
if (requireNamespace("r4subrisk", quietly = TRUE)) {
  ev <- suppressMessages(generate_demo_evidence(40, seed = 1))
  risks <- suppressMessages(r4subrisk::evidence_to_risks(ev))
  reg <- suppressMessages(r4subrisk::create_risk_register(risks))
  fmea_pareto_summary(r4subrisk::risk_pareto(reg))
}
#> $n_risks
#> [1] 11
#> 
#> $total_rpn
#> [1] 291
#> 
#> $n_vital_few
#> [1] 6
#> 
#> $cum_cutoff
#> [1] 0.8
#> 
#> $top_rpn
#> [1] 75
#> 
#> $vital_few_share
#> [1] 0.5454545
#> 
```
