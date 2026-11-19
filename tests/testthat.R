# testthat is suggested, not imported: the hard-dependencies CI lane checks
# the package without it, so the tests run only where it is installed.
# The combinatorial figure tests (every mode, style, theme choice and gallery
# figure) call skip_on_cran(); they run where NOT_CRAN is "true" (the quality
# gate, CI and devtools), and CRAN runs one fast test or more per file.
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(infereportR)
  test_check("infereportR")
}
