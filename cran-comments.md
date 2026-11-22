## Submission

This is a new submission: infereportR 0.3.0.

* Order: infereportR imports infercalcR, which is submitted to CRAN first.
  This package is submitted only after infercalcR has been accepted, so the
  incoming note on the strong dependency below will no longer apply.
* Version 0.1.0 was prepared for CRAN but never submitted. 0.2.0 adds the
  figure layer that was formerly the separate package inferplotR; inferplotR
  was never on CRAN and is not submitted separately. 0.3.0 adds slide decks
  and documents with the bundled Bleisatz Beamer theme and document class.
* Licenses: the package is MIT. The two bundled files
  inst/bleisatz/beamerthemeBleisatz.sty and inst/bleisatz/bleisatz.cls are
  included unmodified under the LaTeX Project Public License 1.3c; this is
  stated in the Copyright field of DESCRIPTION and in LICENSE.md.
* No LaTeX is needed by examples, tests or vignettes; compiling is optional
  and skipped when no LaTeX is installed.

## Test environments

* Local: Linux container, Ubuntu 26.04.1, R 4.6.2 (x86_64-pc-linux-gnu),
  ggplot2 4.0.3. `R CMD check --as-cran` on the built tarball with the
  vignettes, `NOT_CRAN` unset, the CRAN incoming feasibility check enabled
  and R-devel-like check settings (`_R_CHECK_LENGTH_1_CONDITION_`,
  `_R_CHECK_LENGTH_1_LOGIC2_`, `_R_CHECK_THINGS_IN_TEMP_DIR_`,
  `_R_CHECK_THINGS_IN_CHECK_DIR_`, `_R_CHECK_RD_VALIDATE_RD2HTML_`,
  `_R_CHECK_SUGGESTS_ONLY_`).
* Local, C locale: the same check with `LANG=C LC_ALL=C` (session charset
  ASCII), to cover non-ASCII strings in a non-UTF-8 locale.
* Local, without Suggests: the same check with a library that lacks ragg,
  systemfonts and the packages that depend on them, and
  `_R_CHECK_FORCE_SUGGESTS_=false`; tests and examples that need them skip.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release,
  R oldrel-1), plus ubuntu-latest R 4.1 with hard dependencies only, where
  the vignettes are skipped because knitr is not installed.
* win-builder: to be run before submission.

## R CMD check results

0 errors | 0 warnings | 0 notes locally without the incoming check, in the
UTF-8 locale. With the CRAN incoming feasibility check, the only items are:

* New submission.
* Strong dependencies not in the CRAN or BioC software repositories:
  infercalcR. It is submitted to CRAN first (see above).

Locally the incoming check also says "No suitable spell-checker program
found", because the container has no spell checker; the technical terms are
listed in `inst/WORDLIST`.

The C-locale run and the run without Suggests also pass every test
(0 failures in each). The local runs list example timings above a lowered
threshold of 2 s or, in the run without Suggests, which shared the host
with two other checks, one example above 5 s elapsed
(`figure_correlation_matrix()`, 4.2 s CPU); these timing notes come from the
load on the host, see "Check time".

## Check time

Measured on a shared 8-core Linux host under load (load average about 12),
`NOT_CRAN` unset:

* Whole `R CMD check --as-cran` with the vignettes: 18 min wall time on
  that host; the timed steps took 846 s CPU (installation 49 s, R code
  checks 92 s, examples 65 s and 97 s with `--run-donttest`, tests 250 s,
  vignette rebuild 216 s). The host was shared with other jobs; a quiet
  check machine should need clearly less.
* No example outside `\donttest{}` takes more than 3 s CPU on that host
  (the slowest, `scale_fill_hatch()`, takes 2.6 s). The slowest
  `\donttest{}` example is `figure_gallery()` (30 s), which writes the twelve
  gallery figures.
* The figure tests run a fast subset on CRAN (98 `test_that()` blocks skip
  there); the full set runs with `NOT_CRAN=true`. Compared with the first
  local CRAN-like check of 0.2.0, test CPU fell from 287 s to 250 s and the
  vignette rebuild from 260 s to 216 s.

## Notes

* Pattern fills use grid patterns (R >= 4.1.0) and need a pattern-capable
  device such as `cairo_pdf()`, `pdf()`, `svg()` or 'ragg'; the tests skip a
  device that is not available.
* Examples, tests and vignettes run offline, use data sets shipped with R
  or simulated in the package, and write only to `tempdir()`.
  `save_figure_set()` has no default directory, so it never writes to the
  working directory unless asked to.
