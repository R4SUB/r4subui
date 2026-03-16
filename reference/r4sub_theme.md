# Default R4SUB Dashboard Theme

Returns a
[`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
for the R4SUB dashboard with a clean, professional look.

## Usage

``` r
r4sub_theme(
  primary = "#2C3E50",
  success = "#27AE60",
  warning = "#F39C12",
  danger = "#E74C3C",
  font_scale = 1
)
```

## Arguments

- primary:

  Primary accent color. Default `"#2C3E50"`.

- success:

  Success/pass color. Default `"#27AE60"`.

- warning:

  Warning color. Default `"#F39C12"`.

- danger:

  Danger/fail color. Default `"#E74C3C"`.

- font_scale:

  Numeric font scale factor. Default `1`.

## Value

A
[`bslib::bs_theme`](https://rstudio.github.io/bslib/reference/bs_theme.html)
object.

## Examples

``` r
theme <- r4sub_theme()
```
