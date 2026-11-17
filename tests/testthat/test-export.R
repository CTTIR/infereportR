png_width <- function(file) {
  header <- readBin(file, "raw", 24L)
  sum(as.integer(header[17:20]) * 256^(3:0))
}

png_height <- function(file) {
  header <- readBin(file, "raw", 24L)
  sum(as.integer(header[21:24]) * 256^(3:0))
}

small_plot <- function() {
  ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 1, 3)), ggplot2::aes(x, y)) +
    ggplot2::geom_line()
}

test_that("figure_size returns every preset exactly", {
  for (i in seq_len(nrow(.ip_presets))) {
    expect_identical(
      figure_size(.ip_presets$preset[[i]]),
      list(width = .ip_presets$width[[i]], height = .ip_presets$height[[i]], units = "mm")
    )
  }
  expect_identical(figure_size(), list(width = 85, height = 70, units = "mm"))
})

test_that("slide presets are 134, 86 and 64 mm wide and 44 mm high", {
  expect_identical(figure_size("slide_full"), list(width = 134, height = 44, units = "mm"))
  expect_identical(figure_size("slide_side"), list(width = 86, height = 44, units = "mm"))
  expect_identical(figure_size("slide_half"), list(width = 64, height = 44, units = "mm"))
  expect_identical(figure_size("journal_page"), list(width = 174, height = 100, units = "mm"))
})

test_that("save_figure writes a slide preset at 44 mm height", {
  skip_if_not(isTRUE(capabilities("cairo")) || requireNamespace("ragg", quietly = TRUE))
  f <- withr::local_tempfile(fileext = ".png")
  save_figure(small_plot(), f, preset = "slide_side", dpi = 100)
  expect_equal(png_width(f), 86 / 25.4 * 100, tolerance = 1.5 / 339)
  expect_equal(png_height(f), 44 / 25.4 * 100, tolerance = 1.5 / 173)
})

test_that("figure_size rejects unknown presets", {
  expect_error(figure_size("a4"), class = "inferplotR_invalid_input")
  expect_error(figure_size(NA_character_), class = "inferplotR_invalid_input")
  expect_error(figure_size(1), class = "inferplotR_invalid_input")
})

test_that("save_figure writes pdf", {
  skip_if_not(capabilities("cairo"))
  f <- withr::local_tempfile(fileext = ".pdf")
  expect_invisible(out <- save_figure(small_plot(), f))
  expect_identical(out, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("save_figure writes png and svg", {
  f_png <- withr::local_tempfile(fileext = ".png")
  expect_invisible(out <- save_figure(small_plot(), f_png, dpi = 100))
  expect_identical(out, f_png)
  expect_gt(file.size(f_png), 0)
  expect_equal(png_width(f_png), 85 / 25.4 * 100, tolerance = 1.5 / 335)

  skip_if_not(capabilities("cairo"))
  f_svg <- withr::local_tempfile(fileext = ".SVG")
  expect_invisible(save_figure(small_plot(), f_svg, preset = "slide_half"))
  expect_gt(file.size(f_svg), 0)
  expect_true(any(grepl("<svg", readLines(f_svg, warn = FALSE), fixed = TRUE)))
})

test_that("save_figure writes tiff", {
  skip_if_not(isTRUE(capabilities("cairo")) || requireNamespace("ragg", quietly = TRUE))
  f <- withr::local_tempfile(fileext = ".tiff")
  save_figure(small_plot(), f, dpi = 72)
  expect_gt(file.size(f), 0)
})

test_that("explicit width and height override the preset", {
  f <- withr::local_tempfile(fileext = ".png")
  save_figure(small_plot(), f, preset = "journal_page", width = 50, height = 40, dpi = 100)
  w <- png_width(f)
  expect_gte(w, 196)
  expect_lte(w, 198)
})

test_that("missing Cairo raises device_unsupported for pdf", {
  local_mocked_bindings(.ip_cairo_available = function() FALSE)
  f <- withr::local_tempfile(fileext = ".pdf")
  expect_error(save_figure(small_plot(), f), class = "inferplotR_device_unsupported")
  expect_false(file.exists(f))
  local_mocked_bindings(.ip_ragg_available = function() FALSE)
  f_png <- withr::local_tempfile(fileext = ".png")
  expect_error(save_figure(small_plot(), f_png), class = "inferplotR_device_unsupported")
})

test_that("bad file, plot and size arguments raise invalid_input", {
  p <- small_plot()
  expect_error(save_figure(p, "x.jpeg"), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, ""), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, NA), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, NA_character_), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, "pdf"), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, c("a.pdf", "b.pdf")), class = "inferplotR_invalid_input")
  expect_error(save_figure(1, "x.pdf"), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, "x.pdf", preset = "a4"), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, "x.pdf", width = -1), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, "x.pdf", height = "40"), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, "x.pdf", dpi = 0), class = "inferplotR_invalid_input")
  expect_error(save_figure(p, "x.pdf", bg = NA), class = "inferplotR_invalid_input")
})
