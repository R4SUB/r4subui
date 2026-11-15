# Dashboard Tour

`r4subui` provides an eleven-tab Shiny dashboard. This vignette shows
the output of each tab using the built-in demo data.

``` r

library(r4subui)
library(r4subcore)
library(r4subscore)
library(r4subrisk)
library(r4subtrace)
library(r4subprofile)
library(r4subdata)

data(evidence_pharma)
ev <- evidence_pharma

# Pre-compute shared objects
pillar_scores <- compute_pillar_scores(ev)
sci           <- compute_sci(pillar_scores)

cols <- c(quality = "#2C6DB5", trace = "#27AE60",
          risk = "#E74C3C", usability = "#F39C12")
```

------------------------------------------------------------------------

## Tab 1 - Overview

SCI score, decision band, and per-pillar score cards.

``` r

scores_vec <- setNames(pillar_scores$pillar_score, pillar_scores$pillar)

band_col <- c(ready = "#27AE60", minor_gaps = "#F39C12",
              conditional = "#E67E22", high_risk = "#E74C3C")[[sci$band]]

# Save the caller's graphics parameters; restored in the final chunk.
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(4, 5, 3, 1))

barplot(sci$SCI, col = band_col, border = NA, xlim = c(0, 100),
        horiz = TRUE, axes = FALSE,
        main = paste0("SCI  =  ", round(sci$SCI, 1)))
axis(1, at = c(0, 50, 70, 85, 100))
abline(v = c(70, 85), lty = 2, col = "#888888")
mtext(sci$band, side = 1, line = 2.5, col = band_col, font = 2)

barplot(scores_vec[names(cols)], col = cols[names(scores_vec)],
        border = NA, ylim = c(0, 1), las = 1,
        ylab = "Score", main = "Pillar Scores")
abline(h = 0.85, lty = 2, col = "#555555")
```

![Overview: SCI and pillar
scores](dashboard-tour_files/figure-html/overview-1.png)

Overview: SCI and pillar scores

------------------------------------------------------------------------

## Tab 2 - Readiness Targets

Set a target SCI and see the gap to it, plus the pillar that would lift
the score the most (the biggest lever).

``` r

target  <- 85
gap     <- sci_gap_to_target(sci, target)
levers  <- gap$pillars  # pillar, current, weight, max_lift (ordered by max_lift)

par(mar = c(4, 7, 3, 2))
barplot(rev(setNames(levers$max_lift, levers$pillar)),
        horiz = TRUE, las = 1, col = rev(cols[levers$pillar]), border = NA,
        xlab = "SCI points available",
        main = paste0("Biggest Levers  |  Gap to SCI ", target,
                      " = ", round(gap$gap, 1)))
```

![Targets: gap to a target SCI and biggest
levers](dashboard-tour_files/figure-html/targets-1.png)

Targets: gap to a target SCI and biggest levers

------------------------------------------------------------------------

## Tab 3 - Evidence Explorer

Filterable table of all evidence rows.

``` r

with(ev, table(indicator_domain, result))
#>                 result
#> indicator_domain fail na pass warn
#>        quality     26 14   55   25
#>        risk         6  2   24    7
#>        trace        9  1   29    8
#>        usability    8  3   26    7
```

``` r

domains     <- c("quality", "trace", "risk", "usability")
results     <- c("pass", "warn", "fail")
result_cols <- c(pass = "#27AE60", warn = "#F39C12", fail = "#E74C3C")

mat <- sapply(results, function(r)
  sapply(domains, function(d)
    sum(ev$indicator_domain == d & ev$result == r, na.rm = TRUE)
  )
)

par(mar = c(4, 7, 3, 6))
barplot(t(mat), beside = TRUE, col = result_cols,
        names.arg = domains, las = 1, border = NA,
        ylab = "Count", main = "Evidence Results by Pillar")
legend("topright", legend = results, fill = result_cols,
       border = NA, bty = "n", inset = c(-0.18, 0), xpd = TRUE)
```

![Evidence: result distribution by
pillar](dashboard-tour_files/figure-html/evidence-chart-1.png)

Evidence: result distribution by pillar

------------------------------------------------------------------------

## Tab 4 - Indicator Scores

Per-indicator scores and SCI loss contributors.

``` r

expl     <- sci_explain(ev)
top_loss <- head(expl$indicator_contributions, 10)

par(mar = c(4, 14, 3, 2))
barplot(rev(top_loss$loss),
        names.arg = rev(top_loss$indicator_name),
        horiz = TRUE, las = 1, col = "#E74C3C", border = NA,
        xlab = "SCI loss", main = "Top Loss Contributors")
```

![Top 10 indicators by SCI loss
contribution](dashboard-tour_files/figure-html/indicators-1.png)

Top 10 indicators by SCI loss contribution

------------------------------------------------------------------------

## Tab 5 - Pillar Detail

Pillar scores and their contribution to the SCI.

``` r

contrib <- expl$pillar_contributions  # has pillar, pillar_score, weight, contribution, loss

par(mfrow = c(1, 2), mar = c(4, 6, 3, 1))
barplot(setNames(contrib$pillar_score, contrib$pillar),
        col = cols[contrib$pillar], border = NA,
        ylim = c(0, 1), las = 1,
        ylab = "Score", main = "Pillar Scores")

barplot(setNames(contrib$contribution, contrib$pillar),
        col = cols[contrib$pillar], border = NA, las = 1,
        ylab = "Contribution to SCI", main = "SCI Contribution")
```

![Pillar score and SCI
contribution](dashboard-tour_files/figure-html/pillar-detail-1.png)

Pillar score and SCI contribution

------------------------------------------------------------------------

## Tab 6 - Sensitivity Analysis

SCI stability under alternative pillar weight scenarios.

``` r

weight_grid <- data.frame(
  quality   = c(0.35, 0.50, 0.25, 0.25),
  trace     = c(0.25, 0.20, 0.40, 0.25),
  risk      = c(0.25, 0.20, 0.25, 0.25),
  usability = c(0.15, 0.10, 0.10, 0.25)
)
scenario_labels <- c("Default", "Quality-heavy", "Trace-heavy", "Equal")

sens <- sci_sensitivity_analysis(ev, weight_grid)

par(mar = c(4, 11, 3, 2))
barplot(setNames(sens$SCI, scenario_labels),
        horiz = TRUE, las = 1, col = "#2C6DB5", border = NA,
        xlim = c(0, 100), xlab = "SCI",
        main = "SCI by Weight Scenario")
abline(v = c(70, 85), lty = 2, col = "#888888")
```

![SCI sensitivity to weight
variations](dashboard-tour_files/figure-html/sensitivity-1.png)

SCI sensitivity to weight variations

------------------------------------------------------------------------

## Tab 7 - Risk Register

``` r

data(risk_register_pharma)
rr          <- create_risk_register(risk_register_pharma)
risk_sc     <- compute_risk_scores(rr)

rd        <- risk_sc$risk_distribution
risk_cols <- c(low = "#27AE60", medium = "#F39C12",
               high = "#E67E22", critical = "#E74C3C")
rd_named  <- setNames(rd$n, rd$risk_level)
rd_plot   <- rd_named[intersect(names(risk_cols), names(rd_named))]

par(mar = c(4, 5, 3, 2))
barplot(rd_plot, col = risk_cols[names(rd_plot)], border = NA,
        ylab = "Count", las = 1,
        main = paste0("Risk Distribution  |  Mean RPN = ", risk_sc$mean_rpn))
```

![Risk distribution by severity
level](dashboard-tour_files/figure-html/risk-1.png)

Risk distribution by severity level

------------------------------------------------------------------------

## Tab 8 - FMEA (Risk Pareto)

``` r

pareto <- risk_pareto(rr)

par(mar = c(4, 5, 3, 5))
bp <- barplot(pareto$rpn, names.arg = pareto$rank, border = NA,
              col = ifelse(pareto$vital_few, "#2C6DB5", "#AEB6BF"),
              ylab = "RPN", xlab = "Risk (ranked by RPN)", las = 1,
              ylim = c(0, max(pareto$rpn) * 1.1),
              main = paste0("Risk Pareto  |  vital few = ",
                            attr(pareto, "n_vital_few")))
lines(bp, pareto$cum_share * max(pareto$rpn) * 1.1, type = "o",
      pch = 16, col = "#E74C3C", lwd = 2)
```

![FMEA: Pareto of risk priority
numbers](dashboard-tour_files/figure-html/fmea-1.png)

FMEA: Pareto of risk priority numbers

The register, RPN, and Pareto shares are all produced by `r4subrisk`;
the FMEA tab only orders and displays them.

------------------------------------------------------------------------

## Tab 9 - Traceability

``` r

data(adam_metadata); data(sdtm_metadata); data(trace_mapping)
ctx      <- r4sub_run_context(study_id = "CDISCPILOT01", environment = "DEV")
#> ℹ Run context created: "R4S-20261115073107-wl4dieex"
tm       <- build_trace_model(adam_metadata, sdtm_metadata, trace_mapping)
ev_trace <- trace_model_to_evidence(tm, ctx = ctx)
#> ✔ Evidence table created: 47 rows

res_counts <- table(ev_trace$result)
trace_cols <- c(pass = "#27AE60", warn = "#F39C12",
                fail = "#E74C3C", na = "#BDC3C7")
rc <- res_counts[intersect(names(trace_cols), names(res_counts))]

par(mar = c(4, 5, 3, 2))
barplot(rc, col = trace_cols[names(rc)], border = NA,
        ylab = "Count", las = 1,
        main = paste0("Trace Results  |  ", nrow(ev_trace), " indicators"))
```

![Traceability: result
distribution](dashboard-tour_files/figure-html/trace-1.png)

Traceability: result distribution

------------------------------------------------------------------------

## Tab 10 - Authority Profile

``` r

profiles <- list(
  FDA  = submission_profile("FDA",  "NDA"),
  EMA  = submission_profile("EMA",  "MAA"),
  PMDA = submission_profile("PMDA", "NDA_JP")
)

weight_mat <- sapply(profiles, function(p) p$pillar_weights)
pillars    <- rownames(weight_mat)

par(mar = c(4, 5, 3, 6))
barplot(weight_mat, beside = TRUE,
        col = cols[pillars], border = NA, ylim = c(0, 0.5),
        ylab = "Weight", las = 1,
        main = "Pillar Weights by Regulatory Authority")
legend("topright", legend = pillars, fill = cols[pillars],
       border = NA, bty = "n", inset = c(-0.22, 0), xpd = TRUE)
```

![Pillar weights by regulatory
authority](dashboard-tour_files/figure-html/authority-1.png)

Pillar weights by regulatory authority

------------------------------------------------------------------------

## Tab 11 - Compare

Diffs the current evidence against a baseline snapshot.
[`compare_readiness()`](https://r4sub.github.io/r4subui/reference/compare_readiness.md)
reuses the engine’s own scoring and evidence-diff functions, so the
Compare tab only reports values the ecosystem already computes.

``` r

before <- generate_demo_evidence(60, seed = 1)
#> ℹ Run context created: "R4S-20261115073107-da7wnr6u"
#> ✔ Evidence table created: 60 rows
after  <- generate_demo_evidence(60, seed = 2)
#> ℹ Run context created: "R4S-20261115073107-uoff5hq2"
#> ✔ Evidence table created: 60 rows

cmp <- compare_readiness(before, after)

# Headline SCI and decision-band move.
c(before = cmp$sci$before, after = cmp$sci$after, delta = cmp$sci$delta)
#> before  after  delta 
#>   44.9   32.6  -12.3

# Per-pillar score change (largest movers first).
cmp$pillars
#>      pillar score_before score_after       delta weight
#> 1   quality    0.4916667   0.2800000 -0.21166667   0.35
#> 2 usability    0.5173611   0.3458333 -0.17152778   0.15
#> 3     trace    0.4562500   0.3854167 -0.07083333   0.25
#> 4      risk    0.3395833   0.3174603 -0.02212302   0.25

# How many indicators improved or regressed.
cmp$diff_summary[c("n_improved", "n_regressed", "net_delta")]
#>  n_improved n_regressed   net_delta 
#>      24.000      39.000      -4.125
```

------------------------------------------------------------------------

## Launch the live dashboard

``` r

r4sub_app(evidence = evidence_pharma)
```
