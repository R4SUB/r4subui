# Changelog

## r4subui (development version)

- Add an FMEA tab that renders the ‘r4subrisk’ risk register and its
  Pareto analysis for the loaded evidence: a register table sorted by
  Risk Priority Number (RPN), summary boxes for the total RPN and the
  “vital few” risks, and a Pareto chart of RPN with the cumulative-share
  line. The register, RPN, and Pareto are all computed by ‘r4subrisk’
  ([`evidence_to_risks()`](https://r4sub.github.io/r4subrisk/reference/evidence_to_risks.html),
  [`create_risk_register()`](https://r4sub.github.io/r4subrisk/reference/create_risk_register.html),
  and
  [`risk_pareto()`](https://r4sub.github.io/r4subrisk/reference/risk_pareto.html));
  the tab only displays them. The display arrangement is factored into
  two pure, exported helpers,
  [`fmea_register_view()`](https://r4sub.github.io/r4subui/reference/fmea_register_view.md)
  and
  [`fmea_pareto_summary()`](https://r4sub.github.io/r4subui/reference/fmea_pareto_summary.md),
  so it is unit-testable independently of the app.
- Add a Compare tab that diffs the current evidence against an uploaded
  baseline snapshot: it shows the SCI and decision-band move, the
  per-pillar score deltas, and how many indicators improved or
  regressed. The comparison is produced by the new pure, exported helper
  [`compare_readiness()`](https://r4sub.github.io/r4subui/reference/compare_readiness.md),
  which only surfaces values the engine packages already compute
  ([`compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.html),
  [`compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.html),
  [`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.html),
  and
  [`evidence_diff_summary()`](https://r4sub.github.io/r4subcore/reference/evidence_diff_summary.html)),
  so it is unit-testable independently of the app.
- Add an indicator-to-evidence drill-down: pick a contributing indicator
  in the Indicators tab and open its rows in the Evidence tab. The
  filtering is done by the new pure, exported helper
  [`filter_evidence_by_indicator()`](https://r4sub.github.io/r4subui/reference/filter_evidence_by_indicator.md),
  so it is unit-testable independently of the app.
- Add a readiness report download to the Overview tab: a timestamped
  Markdown snapshot of the current SCI, decision band, per-pillar
  contributions, and top loss indicators. The report body is produced by
  the new pure, exported helper
  [`build_readiness_report()`](https://r4sub.github.io/r4subui/reference/build_readiness_report.md),
  so it is unit-testable independently of the app.

## r4subui 0.2.1

- Metadata and documentation polish for CRAN submission; no user-facing
  changes.
- Document the Targets tab (added in 0.2.0) in the README tab table and
  the dashboard tour vignette, so the docs describe all nine dashboard
  tabs.

## r4subui 0.2.0

- Add a Targets panel that sets a target SCI and shows the gap to it
  plus the pillar that would move the score the most, using
  [`sci_targets()`](https://r4sub.github.io/r4subscore/reference/sci_targets.html)
  and
  [`sci_gap_to_target()`](https://r4sub.github.io/r4subscore/reference/sci_gap_to_target.html).
- Add a Monte Carlo risk simulation to the Risk tab: derives a register
  from the evidence and reports the distribution of total RPN and each
  risk’s probability of being critical, using
  [`risk_monte_carlo()`](https://r4sub.github.io/r4subrisk/reference/risk_monte_carlo.html).
- Add change impact analysis to the Traceability tab: pick a source
  variable and see the ADaM variables that derive from it, using
  [`trace_impact()`](https://r4sub.github.io/r4subtrace/reference/trace_impact.html)
  on a demo trace model built from the ‘r4subdata’ metadata.
- Add an authority comparison to the Authority tab that shows how pillar
  weights and the readiness bar differ across regulators, using
  [`compare_authorities()`](https://r4sub.github.io/r4subprofile/reference/compare_authorities.html).

## r4subui 0.1.0

- Initial release.
- Expanded “R4SUB” as “Ready for Submission” in the package DESCRIPTION,
  for consistency with the rest of the ecosystem.
