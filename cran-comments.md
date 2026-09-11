## Resubmission

This is a resubmission addressing the review comments:

* Software names in the Description are now single-quoted ('shiny').
* The `r4sub_app()` example, which launches an interactive dashboard, is now
  wrapped in `if (interactive()) { ... }` instead of `\dontrun{}`.
* Functions that change graphical parameters now save and restore them with an
  immediate `on.exit(par(oldpar))` (R/mod_pillar_detail.R, R/mod_risk.R,
  R/mod_trace.R). The vignette that changes `par()` now restores it as well.

## Submission notes

This is the first submission of r4subui. It is part of the R4SUB (Ready for
Submission) ecosystem and provides an interactive 'shiny' dashboard for
visualizing evidence, pillar scores, the Submission Confidence Index (SCI),
risk registers, traceability coverage, and regulatory authority profiles.

All packages listed in Imports are available on CRAN: bslib, graphics,
htmltools, r4subcore, r4subprofile, r4subscore, r4subtrace, shiny, utils. The
suggested r4subdata, r4subrisk, knitr, rmarkdown, and testthat are also on
CRAN. The vignette and examples that use suggested packages are guarded with
requireNamespace().

## Test environments

* local: Windows 11 x64, R 4.5.x
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the standard "New submission" note.

## Downstream dependencies

There are no reverse dependencies on CRAN at this time.
