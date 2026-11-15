# Compare Submission Readiness Between Two Evidence Snapshots

Compares a baseline evidence snapshot against a current one and returns
the change in headline readiness: the Submission Confidence Index (SCI)
delta, the decision-band move, the per-pillar score deltas, and a
row-level evidence diff summary (how many indicators were added,
removed, improved, or regressed).

## Usage

``` r
compare_readiness(baseline, current)
```

## Arguments

- baseline:

  A validated evidence data.frame taken as the earlier ("before")
  snapshot, such as one produced by
  [`generate_demo_evidence()`](https://r4sub.github.io/r4subui/reference/generate_demo_evidence.md).

- current:

  A validated evidence data.frame taken as the later ("after") snapshot.
  The diff is computed as `baseline` -\> `current`, so an improvement in
  `current` shows as a positive `net_delta`.

## Value

A named list with elements:

- `sci`:

  A list with the `before` and `after` SCI values and their `delta`
  (`after - before`).

- `band`:

  A list with the `before` and `after` decision bands and a logical
  `changed` flag.

- `n_critical`:

  A list with the `before` and `after` critical-finding counts and their
  `delta`.

- `pillars`:

  A data.frame with one row per pillar: `pillar`, `score_before`,
  `score_after`, `delta`, and `weight`, ordered by the size of the move
  (largest absolute delta first).

- `diff`:

  The row-level
  [`r4subcore::evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.html)
  data.frame of changed, added, and removed indicators.

- `diff_summary`:

  A named numeric vector of the
  [`r4subcore::evidence_diff_summary()`](https://r4sub.github.io/r4subcore/reference/evidence_diff_summary.html)
  metrics (for example `n_improved`, `n_regressed`, `net_delta`).

## Details

This is the pure, non-reactive helper behind the dashboard's Compare
tab. It only surfaces values the engine packages already compute
([`r4subscore::compute_pillar_scores()`](https://r4sub.github.io/r4subscore/reference/compute_pillar_scores.html),
[`r4subscore::compute_sci()`](https://r4sub.github.io/r4subscore/reference/compute_sci.html),
and
[`r4subcore::evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.html)
/
[`r4subcore::evidence_diff_summary()`](https://r4sub.github.io/r4subcore/reference/evidence_diff_summary.html));
it does not define any new scoring or regulatory semantics. Because it
is a plain data-frame-in, list-out function with no `shiny` dependency,
it is easy to unit test on its own.

## Examples

``` r
before <- suppressMessages(generate_demo_evidence(60, seed = 1))
after  <- suppressMessages(generate_demo_evidence(60, seed = 2))
cmp <- compare_readiness(before, after)
cmp$sci$delta
#> [1] -12.3
cmp$pillars
#>      pillar score_before score_after       delta weight
#> 1   quality    0.4916667   0.2800000 -0.21166667   0.35
#> 2 usability    0.5173611   0.3458333 -0.17152778   0.15
#> 3     trace    0.4562500   0.3854167 -0.07083333   0.25
#> 4      risk    0.3395833   0.3174603 -0.02212302   0.25
```
