# Arrange an FMEA Risk Register for Display

Orders a risk register produced by
[`r4subrisk::create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.html)
by its Risk Priority Number (RPN) so the highest-priority risks appear
first, and keeps a fixed set of display columns. This is a pure,
non-reactive helper behind the dashboard's FMEA tab.

## Usage

``` r
fmea_register_view(
  register,
  columns = c("risk_id", "description", "category", "probability", "impact",
    "detectability", "rpn", "risk_level"),
  max_rows = 100L
)
```

## Arguments

- register:

  A risk register data.frame, as returned by
  [`r4subrisk::create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.html).
  Must contain an `rpn` column.

- columns:

  Character vector of columns to keep, in display order. Columns that
  are not present in `register` are dropped silently. Defaults to the
  FMEA breakdown (`probability`, `impact`, `detectability`) alongside
  the `rpn` and `risk_level`.

- max_rows:

  Maximum number of rows to return. Default `100`.

## Value

A data.frame with the selected columns, ordered by descending `rpn`
(ties keep their original order) and truncated to `max_rows` rows. Row
names are reset.

## Details

It performs no scoring of its own: the `rpn`, `probability`, `impact`,
`detectability`, and `risk_level` values are exactly those computed by
'r4subrisk'. The helper only reorders rows and selects columns, so it is
easy to unit test without launching the app.

## Examples

``` r
if (requireNamespace("r4subrisk", quietly = TRUE)) {
  ev <- suppressMessages(generate_demo_evidence(40, seed = 1))
  risks <- suppressMessages(r4subrisk::evidence_to_risks(ev))
  reg <- suppressMessages(r4subrisk::create_risk_register(risks))
  head(fmea_register_view(reg))
}
#>                                 risk_id
#> 1 RISK-33c95a221ad36aa059fc5cefe6a0e589
#> 2 RISK-3efcfd9dfb1335bd6ac70b9e1bbb7953
#> 3 RISK-d49a1c47b93d84722ca25d7a29d0ca2f
#> 4 RISK-a3caac1c0850086021ff33f69c24b833
#> 5 RISK-bfb65252fafc62f5aedd151350aaf01f
#> 6 RISK-3f3a6830d06576e9110b3cda4e8e044d
#>                                         description category probability impact
#> 1   Ambiguous mapping - [Ambiguous mapping on ADAE]    trace           5      5
#> 2 High RPN risk item - [High RPN risk item on ADAE]     risk           4      4
#> 3        Mitigation gap - [Mitigation gap on ADTTE]     risk           4      4
#> 4            Format error - [Format error on ADTTE]  quality           3      3
#> 5                   Open risk - [Open risk on ADAE]     risk           3      3
#> 6   Ambiguous mapping - [Ambiguous mapping on ADSL]    trace           3      3
#>   detectability rpn risk_level
#> 1             3  75       high
#> 2             3  48       high
#> 3             3  48       high
#> 4             3  27     medium
#> 5             3  27     medium
#> 6             3  27     medium
```
