## Submission

This is a new submission: infereportR 0.1.0.

It imports infercalcR, which is submitted to CRAN first; this package can
only be submitted once infercalcR is available on CRAN.

## Test environments

* Local: macOS 27 on Apple silicon, R 4.6.1 (aarch64-apple-darwin),
  `R CMD check --as-cran --no-manual` on the built tarball with `NOT_CRAN`
  unset and infercalcR 0.1.0 installed from its source package. LaTeX is
  not installed on this machine, so the PDF manual was not built locally.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release,
  R oldrel-1), plus ubuntu-latest R 4.1 with hard dependencies only.

## R CMD check results

Locally: 0 errors | 1 warning | 0 notes. The warning is the CRAN incoming
feasibility check and lists issues that must be resolved before submission:

* New submission.
* Strong dependency not in a mainstream repository: infercalcR. It is
  submitted to CRAN first.
* Suggests not in a mainstream repository: DescrTab2 (archived on CRAN).
* The repository URLs in DESCRIPTION return 404 while the GitHub
  repository is private.

## Notes

* 'ggplot2' and 'DescrTab2' are optional (Suggests) and used conditionally.
  'DescrTab2' was archived on CRAN on 2025-10-18; the optional adapter is
  verified for its upstream version 2.1.26, and its examples and tests run
  only when that version is installed.
