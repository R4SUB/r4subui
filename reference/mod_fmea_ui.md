# FMEA Module UI

Displays the Failure Mode and Effects Analysis (FMEA) risk register
derived from the loaded evidence: a sortable register table ordered by
Risk Priority Number (RPN), and a Pareto view that highlights the "vital
few" risks that account for most of the total RPN. All of the numbers
come from 'r4subrisk' (via
[`r4subrisk::evidence_to_risks()`](https://r4sub.github.io/r4subrisk/reference/evidence_to_risks.html),
[`r4subrisk::create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.html),
and
[`r4subrisk::risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.html));
this tab only displays them.

## Usage

``` r
mod_fmea_ui(id)
```

## Arguments

- id:

  Module namespace ID.

## Value

A Shiny UI element.
