# r4subui 0.2.0

- Add a Targets panel that sets a target SCI and shows the gap to it plus the
  pillar that would move the score the most, using `sci_targets()` and
  `sci_gap_to_target()`.
- Add a Monte Carlo risk simulation to the Risk tab: derives a register from the
  evidence and reports the distribution of total RPN and each risk's probability
  of being critical, using `risk_monte_carlo()`.
- Add change impact analysis to the Traceability tab: pick a source variable and
  see the ADaM variables that derive from it, using `trace_impact()` on a demo
  trace model built from the 'r4subdata' metadata.
- Add an authority comparison to the Authority tab that shows how pillar weights
  and the readiness bar differ across regulators, using `compare_authorities()`.

# r4subui 0.1.0

- Initial release.
- Expanded "R4SUB" as "Ready for Submission" in the package DESCRIPTION, for
  consistency with the rest of the ecosystem.
