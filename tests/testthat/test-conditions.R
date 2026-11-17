test_that(".ip_abort raises classed errors and keeps the parent", {
  parent <- simpleError("inner")
  e <- tryCatch(.ip_abort("x must be valid", "too_small", parent = parent),
                error = function(e) e)
  expect_s3_class(e, "inferplotR_too_small")
  expect_s3_class(e, "inferplotR_error")
  expect_s3_class(e, "error")
  expect_identical(e$parent, parent)
  expect_null(e$call)
  expect_error(.ip_abort("bad"), class = "inferplotR_invalid_input")
})

test_that(".ip_warn raises classed warnings", {
  w <- tryCatch(.ip_warn("small text", "apa_text_size"), warning = function(w) w)
  expect_s3_class(w, "inferplotR_apa_text_size")
  expect_s3_class(w, "inferplotR_warning")
  expect_s3_class(w, "warning")
})
