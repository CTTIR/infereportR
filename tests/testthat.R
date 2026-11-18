# testthat is suggested, not imported: the hard-dependencies CI lane checks
# the package without it, so the tests run only where it is installed.
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(inferplotR)
  test_check("inferplotR")
}
