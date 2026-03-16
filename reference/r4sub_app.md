# Launch the R4SUB Dashboard

Starts the interactive Shiny dashboard for exploring R4SUB evidence,
pillar scores, SCI, and sensitivity analysis.

## Usage

``` r
r4sub_app(evidence = NULL, theme = r4sub_theme(), ...)
```

## Arguments

- evidence:

  An optional pre-loaded evidence data.frame. If `NULL`, the app shows
  an upload interface.

- theme:

  A
  [`bslib::bs_theme`](https://rstudio.github.io/bslib/reference/bs_theme.html)
  object. Default uses
  [`r4sub_theme()`](https://r4sub.github.io/r4subui/reference/r4sub_theme.md).

- ...:

  Additional arguments passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

A Shiny app object (returned invisibly; runs interactively).

## Examples

``` r
if (FALSE) { # \dontrun{
# With pre-loaded evidence
r4sub_app(evidence = my_evidence)

# With upload interface
r4sub_app()
} # }
```
