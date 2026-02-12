# r4subui

**r4subui** is the interactive Shiny dashboard for the R4SUB ecosystem.

It visualizes evidence tables, pillar scores, the Submission Confidence Index (SCI), indicator contributions, and sensitivity analysis -- all in a single browser-based interface.

## Installation

```r
pak::pak(c("R4SUB/r4subcore", "R4SUB/r4subscore", "R4SUB/r4subui"))
```

## Quick Start

```r
library(r4subui)

# Launch with demo data
ev <- generate_demo_evidence(n_rows = 100)
r4sub_app(evidence = ev)

# Or launch with upload interface
r4sub_app()
```

## Dashboard Tabs

| Tab | Content |
|---|---|
| **Overview** | SCI score, decision band, pillar score cards, evidence summary |
| **Evidence** | Filterable evidence table (by domain, severity, result, source) |
| **Indicators** | Per-indicator scores + SCI explainability (loss contributors) |
| **Pillars** | Pillar bar chart, contribution table, result distribution |
| **Sensitivity** | Interactive weight sliders + pre-defined weight scenarios |

## Key Features

- **Upload or pre-load**: CSV/RDS upload or pass evidence directly from R
- **Real-time SCI**: adjust pillar weights and see SCI change instantly
- **Color-coded**: severity and result cells are color-coded throughout
- **Modular**: each tab is a standalone Shiny module, reusable in custom apps
- **Themeable**: uses `bslib` with a configurable `r4sub_theme()`

## Using Modules in Custom Apps

All modules are exported and can be composed into your own Shiny apps:

```r
library(shiny)
library(r4subui)

ui <- fluidPage(
  mod_overview_ui("my_overview")
)

server <- function(input, output, session) {
  ev_data <- reactiveVal(generate_demo_evidence())
  mod_overview_server("my_overview", ev_data)
}

shinyApp(ui, server)
```

## Exported Functions

| Function | Purpose |
|---|---|
| `r4sub_app()` | Launch the full dashboard |
| `r4sub_theme()` | Default bslib theme |
| `r4sub_value_box()` | Styled metric card |
| `render_evidence_table()` | HTML evidence table renderer |
| `generate_demo_evidence()` | Demo data generator |
| `mod_overview_ui/server()` | Overview module |
| `mod_evidence_ui/server()` | Evidence explorer module |
| `mod_indicators_ui/server()` | Indicator scores module |
| `mod_pillar_detail_ui/server()` | Pillar detail module |
| `mod_sensitivity_ui/server()` | Sensitivity analysis module |

## License

MIT
