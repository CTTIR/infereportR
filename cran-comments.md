## Submission

This is a new submission: inferplotR 0.1.0.

It imports infercalcR and infereportR, which are submitted to CRAN first; this
package can only be submitted once both are available on CRAN.

## Test environments

* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release,
  R oldrel-1), plus ubuntu-latest R 4.1 with hard dependencies only, where
  the vignettes are skipped because knitr is not installed.

## R CMD check results

0 errors | 0 warnings | 0 notes, apart from the CRAN incoming feasibility
check, which lists:

* New submission.
* Strong dependencies not in a mainstream repository: infercalcR,
  infereportR. They are submitted to CRAN first.
