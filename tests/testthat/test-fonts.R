test_that("figure_font returns a single family name", {
  f <- figure_font(reset = TRUE)
  expect_type(f, "character")
  expect_length(f, 1L)
  expect_true(f %in% c("IBM Plex Sans", "sans"))
  expect_true(figure_font("mono") %in% c("IBM Plex Mono", "mono"))
})

test_that("figure_font falls back to generic families and caches", {
  # Reset after the mocks are gone, so no mocked family stays in the cache.
  withr::defer(figure_font(reset = TRUE))
  local_mocked_bindings(.ip_has_font = function(family) FALSE)
  expect_identical(figure_font("sans", reset = TRUE), "sans")
  expect_identical(figure_font("mono"), "mono")
  local_mocked_bindings(.ip_has_font = function(family) TRUE)
  expect_identical(figure_font("sans"), "sans")
  expect_identical(figure_font("sans", reset = TRUE), "IBM Plex Sans")
  expect_identical(figure_font("mono"), "IBM Plex Mono")
})

test_that("figure_font validates its arguments", {
  expect_error(figure_font("serif"), class = "inferplotR_invalid_input")
  expect_error(figure_font(reset = NA), class = "inferplotR_invalid_input")
})

test_that(".ip_has_font never errors", {
  expect_type(.ip_has_font("No Such Font Family 123"), "logical")
  expect_false(.ip_has_font("No Such Font Family 123"))
})
