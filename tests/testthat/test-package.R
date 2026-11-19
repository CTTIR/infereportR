test_that("the package loads and has a version", {
  expect_true(isNamespaceLoaded("infereportR"))
  expect_s3_class(utils::packageVersion("infereportR"), "package_version")
})
