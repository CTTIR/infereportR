test_that(".ir_abort raises classed errors and keeps the parent", {
  parent <- simpleError("inner")
  e <- tryCatch(.ir_abort("x must be valid", "too_small", parent = parent),
                error = function(e) e)
  expect_s3_class(e, "infereportR_too_small")
  expect_s3_class(e, "infereportR_error")
  expect_s3_class(e, "error")
  expect_identical(e$parent, parent)
  expect_null(e$call)
  expect_error(.ir_abort("bad"), class = "infereportR_invalid_input")
})

test_that(".ir_warn raises classed warnings", {
  w <- tryCatch(.ir_warn("small text", "apa_text_size"), warning = function(w) w)
  expect_s3_class(w, "infereportR_apa_text_size")
  expect_s3_class(w, "infereportR_warning")
  expect_s3_class(w, "warning")
})
