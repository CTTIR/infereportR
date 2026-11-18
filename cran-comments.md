## Submission

This is a new submission: infereportR 0.2.0. Version 0.1.0 was prepared for
CRAN but not submitted; 0.2.0 adds the figure layer of the former package
inferplotR, which is not submitted separately.

It imports infercalcR, which is submitted to CRAN first; this package can
only be submitted once infercalcR is available on CRAN.

## Test environments

* Local: Linux container, R 4.6.2 (x86_64-pc-linux-gnu), ggplot2 4.0.3,
  `R CMD check --as-cran` on the built tarball with the vignettes, and the
  CRAN incoming feasibility check enabled.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release,
  R oldrel-1), plus ubuntu-latest R 4.1 with hard dependencies only, where
  the vignettes are skipped because knitr is not installed.

## R CMD check results

0 errors | 0 warnings | 0 notes locally without the incoming check. With the
CRAN incoming feasibility check, the only items are:

* New submission.
* Strong dependency not in a mainstream repository: infercalcR. It is
  submitted to CRAN first.

## Notes

* Pattern fills use grid patterns (R >= 4.1.0) and need a pattern-capable
  device such as `cairo_pdf()`, `pdf()`, `svg()` or 'ragg'; the tests skip a
  device that is not available.
* Examples, tests and vignettes run offline, use data sets shipped with R
  or simulated in the package, and write only to `tempdir()`.
