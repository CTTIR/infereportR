## Submission

This is a new submission: infereportR 0.1.0.

It imports infercalcR, which is submitted to CRAN first; this package can
only be submitted once infercalcR is available on CRAN.

## Test environments

* Local: macOS 27 on Apple silicon, R 4.6.1 (aarch64-apple-darwin),
  `R CMD check --as-cran` on the built tarball, including the PDF manual
  (TinyTeX) and the HTML manual validated with HTML Tidy 5.8.0, with
  `NOT_CRAN` unset and infercalcR 0.1.0 installed from its source package.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release,
  R oldrel-1), plus ubuntu-latest R 4.1 with hard dependencies only, where
  the vignettes are skipped because knitr is not installed.

## R CMD check results

Locally: 0 errors | 1 warning | 0 notes. The warning is the CRAN incoming
feasibility check and lists issues that must be resolved before submission:

* New submission.
* Strong dependency not in a mainstream repository: infercalcR. It is
  submitted to CRAN first.

## Notes

* 'ggplot2' is optional (Suggests) and used conditionally.
* The three vignettes use data sets shipped with R, run offline in a few
  seconds and evaluate their 'ggplot2' chunks only when that package is
  installed.
