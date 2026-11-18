test_that("the package loads and has a version", {
  expect_true(isNamespaceLoaded("inferplotR"))
  expect_s3_class(utils::packageVersion("inferplotR"), "package_version")
})
