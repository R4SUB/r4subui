# Changelog

## r4subui 0.2.1

- Metadata and documentation polish for CRAN submission; no user-facing
  changes.

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
