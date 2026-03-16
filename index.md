# r4subui

**r4subui** is the interactive Shiny dashboard for the R4SUB ecosystem.
It visualizes evidence tables, pillar scores, the Submission Confidence
Index (SCI), risk registers, traceability coverage, and regulatory
authority profiles — all in a single browser-based interface.

## Installation

``` r
install.packages("r4subui")
```

Development version:

``` r
pak::pak(c("R4SUB/r4subcore", "R4SUB/r4subscore", "R4SUB/r4subui"))
```

## Quick Start

``` r
library(r4subui)

# Launch with demo data
ev <- generate_demo_evidence(n_rows = 100)
r4sub_app(evidence = ev)

# Launch with upload interface
r4sub_app()
```

## Dashboard Tabs

| Tab              | Content                                                           |
|------------------|-------------------------------------------------------------------|
| **Overview**     | SCI score, decision band, pillar score cards, evidence summary    |
| **Evidence**     | Filterable evidence table (domain, severity, result, source)      |
| **Indicators**   | Per-indicator scores and SCI explainability (loss contributors)   |
| **Pillars**      | Pillar bar chart, contribution table, result distribution         |
| **Sensitivity**  | Interactive weight sliders and pre-defined weight scenarios       |
| **Risk**         | Severity distribution, risk register evidence table               |
| **Traceability** | Trace coverage summary, indicator scores, result distribution     |
| **Authority**    | Regulatory profile selector, pillar weights, requirement coverage |

## Using Modules in Custom Apps

All tabs are standalone Shiny modules and can be composed into your own
apps:

``` r
library(shiny)
library(r4subui)

ui <- fluidPage(
  mod_overview_ui("ov"),
  mod_risk_ui("rk")
)

server <- function(input, output, session) {
  ev_data <- reactiveVal(generate_demo_evidence())
  mod_overview_server("ov", ev_data)
  mod_risk_server("rk", ev_data)
}

shinyApp(ui, server)
```

## Key Functions

| Function                                                                                          | Purpose                      |
|---------------------------------------------------------------------------------------------------|------------------------------|
| [`r4sub_app()`](https://r4sub.github.io/r4subui/reference/r4sub_app.md)                           | Launch the full dashboard    |
| [`r4sub_theme()`](https://r4sub.github.io/r4subui/reference/r4sub_theme.md)                       | Default bslib theme          |
| [`r4sub_value_box()`](https://r4sub.github.io/r4subui/reference/r4sub_value_box.md)               | Styled metric card           |
| [`render_evidence_table()`](https://r4sub.github.io/r4subui/reference/render_evidence_table.md)   | HTML evidence table renderer |
| [`generate_demo_evidence()`](https://r4sub.github.io/r4subui/reference/generate_demo_evidence.md) | Demo data generator          |
| `mod_overview_ui/server()`                                                                        | Overview tab module          |
| `mod_evidence_ui/server()`                                                                        | Evidence explorer module     |
| `mod_indicators_ui/server()`                                                                      | Indicator scores module      |
| `mod_pillar_detail_ui/server()`                                                                   | Pillar detail module         |
| `mod_sensitivity_ui/server()`                                                                     | Sensitivity analysis module  |
| `mod_risk_ui/server()`                                                                            | Risk register module         |
| `mod_trace_ui/server()`                                                                           | Traceability coverage module |
| `mod_authority_ui/server()`                                                                       | Authority profile module     |

## License

MIT
