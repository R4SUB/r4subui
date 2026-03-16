# Create a Summary Value Box

Creates a styled value box for displaying a single metric.

## Usage

``` r
r4sub_value_box(title, value, theme = "primary", subtitle = NULL)
```

## Arguments

- title:

  Character label.

- value:

  The value to display.

- theme:

  Character theme: `"primary"`, `"success"`, `"warning"`, `"danger"`,
  `"info"`. Default `"primary"`.

- subtitle:

  Optional subtitle text.

## Value

A
[`bslib::value_box`](https://rstudio.github.io/bslib/reference/value_box.html)
element.

## Examples

``` r
if (FALSE) { # \dontrun{
r4sub_value_box("SCI Score", 85.2, theme = "success")
} # }
```
