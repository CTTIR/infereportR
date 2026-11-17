cf_plot <- function() {
  d <- data.frame(hour = c(0, 2, 4, 8, 12, 24), level = c(9, 14, 19, 17, 13, 10))
  ggplot2::ggplot(d, ggplot2::aes(hour, level)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Level over time", x = "Hours", y = "Level")
}

cf_dims <- function(fig) {
  lim <- fig$coordinates$limits
  c(ncol = diff(lim$x), nrow = diff(lim$y))
}

cf_margin_mm <- function(fig) {
  as.numeric(grid::convertUnit(fig$theme$plot.margin, "mm", valueOnly = TRUE))
}

cf_fill <- function(fig, element) ggplot2::calc_element(element, fig$theme)$fill

# Non-blank rows of the figure, bottom last, as strings.
cf_rows <- function(fig) {
  d <- fig$data
  rows <- sort(unique(d$row))
  vapply(rows, function(r) {
    s <- d[d$row == r, , drop = FALSE]
    line <- rep(" ", max(s$col))
    line[s$col] <- s$glyph
    sub(" +$", "", paste(line, collapse = ""))
  }, "")
}

test_that("the C64 tokens are the Pepto values", {
  expect_identical(toupper(.ip_c64[["background"]]), "#352879")
  expect_identical(toupper(.ip_c64[["foreground"]]), "#6C5EB5")
})

test_that("char_figure() returns a ggplot of one text layer with one row per non-blank cell", {
  p <- cf_plot()
  g <- char_grid(p)
  fig <- char_figure(p)
  expect_true(inherits(fig, "ggplot"))
  expect_length(fig$layers, 1L)
  expect_true(inherits(fig$layers[[1]]$geom, "GeomText"))
  expect_identical(names(fig$data), c("col", "row", "glyph"))
  expect_identical(nrow(fig$data), sum(g$chars != " "))
  expect_identical(fig$data$glyph, g$chars[cbind(fig$data$row, fig$data$col)])
  expect_false(any(fig$data$glyph == " "))
})

test_that("the bw figure is ink on paper without margin", {
  fig <- char_figure(cf_plot())
  built <- layer_data_of(fig)
  expect_identical(unique(built$colour), .ip_ink)
  expect_identical(cf_fill(fig, "panel.background"), .ip_paper)
  expect_identical(cf_fill(fig, "plot.background"), .ip_paper)
  expect_equal(cf_margin_mm(fig), rep(0, 4))
})

test_that("the c64 figure is light blue on blue inside a light-blue border", {
  fig <- char_figure(cf_plot(), style = "c64", width_mm = 100)
  built <- layer_data_of(fig)
  expect_identical(toupper(unique(built$colour)), "#6C5EB5")
  expect_identical(toupper(cf_fill(fig, "panel.background")), "#352879")
  expect_identical(toupper(cf_fill(fig, "plot.background")), "#6C5EB5")
  expect_identical(unique(built$colour), .ip_c64[["foreground"]])
  expect_identical(cf_fill(fig, "panel.background"), .ip_c64[["background"]])
  expect_equal(cf_margin_mm(fig), rep(8, 4))
})

test_that("text size fills one cell per glyph", {
  fig <- char_figure(cf_plot(), width_mm = 128, char_width = 0.5)
  expect_equal(unique(layer_data_of(fig)$size), (128 / 64) / 0.5)
})

test_that("coordinate limits span the grid at the cell ratio", {
  fig <- char_figure(cf_plot(), width = 30, height = 12, cell_ratio = 1.5)
  expect_true(inherits(fig$coordinates, "CoordCartesian"))
  expect_equal(fig$coordinates$ratio, 1.5)
  expect_equal(fig$coordinates$limits$x, c(0.5, 30.5))
  expect_equal(fig$coordinates$limits$y, c(-12.5, -0.5))
})

test_that("ready defaults to TRUE for c64 and FALSE for bw", {
  p <- cf_plot()
  bw <- char_figure(p)
  c64 <- char_figure(p, style = "c64")
  expect_identical(cf_dims(bw)[["nrow"]], 20)
  expect_identical(cf_dims(c64)[["nrow"]], 27)
  expect_false(identical(utils::tail(cf_rows(bw), 1L), "_"))
  expect_identical(cf_dims(char_figure(p, style = "c64", ready = FALSE))[["nrow"]], 25)
})

test_that("ready = TRUE ends with READY. and the cursor", {
  p <- cf_plot()
  fig <- char_figure(p, ready = TRUE)
  rows <- cf_rows(fig)
  expect_identical(utils::tail(rows, 2L), c("READY.", "_"))
  expect_identical(max(fig$data$row), 22L)
  uni <- char_figure(p, style = "c64", charset = "unicode")
  expect_identical(utils::tail(cf_rows(uni), 2L), c("READY.", "█"))
})

test_that("c64 defaults are a 40 x 25 grid in upper case", {
  p <- cf_plot()
  fig <- char_figure(p, style = "c64", ready = FALSE)
  expect_identical(cf_dims(fig), c(ncol = 40, nrow = 25))
  expect_identical(cf_rows(fig)[[1]], "LEVEL OVER TIME")
  expect_identical(cf_dims(char_figure(p)), c(ncol = 64, nrow = 20))
  expect_identical(cf_rows(char_figure(p))[[1]], "Level over time")
})

test_that("a chargrid is drawn as is and rejects grid arguments", {
  g <- char_grid(cf_plot(), width = 32, height = 12)
  fig <- char_figure(g)
  expect_identical(cf_dims(fig), c(ncol = 32, nrow = 12))
  expect_error(char_figure(g, width = 40), class = "inferplotR_invalid_input")
  expect_error(char_figure(g, height = 20), class = "inferplotR_invalid_input")
  expect_error(char_figure(g, charset = "unicode"), class = "inferplotR_invalid_input")
  expect_error(char_figure(g, case = "upper"), class = "inferplotR_invalid_input")
})

test_that("char_figure() validates its arguments", {
  p <- cf_plot()
  expect_error(char_figure(1:3), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, style = "amiga"), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, width_mm = 0), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, cell_ratio = -1), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, char_width = NA), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, ready = "yes"), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, family = ""), class = "inferplotR_invalid_input")
  expect_error(char_size(p, width_mm = -5), class = "inferplotR_invalid_input")
  expect_error(char_size("x"), class = "inferplotR_invalid_input")
})

test_that("char_figure() passes skipped-layer warnings on", {
  p <- ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) + ggplot2::geom_boxplot()
  expect_warning(char_figure(p), class = "inferplotR_skipped_layer")
  expect_no_warning(char_size(p))
})

test_that("char_size() agrees across a ggplot, a chargrid and a figure (bw)", {
  p <- cf_plot()
  expected <- 20 * (134 / 64) * 2
  s_plot <- char_size(p)
  s_grid <- char_size(char_grid(p))
  s_fig <- char_size(char_figure(p))
  expect_equal(s_plot, list(width = 134, height = expected, units = "mm"))
  expect_equal(s_grid, s_plot)
  expect_equal(s_fig, s_plot)
})

test_that("char_size() agrees across a ggplot, a chargrid and a figure (c64)", {
  p <- cf_plot()
  w <- 100
  ratio <- 1.5
  border <- 0.08 * w
  expected <- list(width = w + 2 * border, height = 27 * (w / 40) * ratio + 2 * border,
                   units = "mm")
  s_plot <- char_size(p, width_mm = w, cell_ratio = ratio, style = "c64")
  s_grid <- char_size(char_grid(p, width = 40, height = 25), width_mm = w,
                      cell_ratio = ratio, style = "c64")
  s_fig <- char_size(char_figure(p, style = "c64", width_mm = w, cell_ratio = ratio),
                     width_mm = w, cell_ratio = ratio, style = "c64")
  expect_equal(s_plot, expected)
  expect_equal(s_grid, expected)
  expect_equal(s_fig, expected)
})

test_that("a figure without READY rows is sized from its own limits", {
  fig <- char_figure(cf_plot(), style = "c64", ready = FALSE)
  s <- char_size(fig, style = "c64")
  expect_equal(s$height, 25 * (134 / 40) * 2 + 2 * 0.08 * 134)
})

test_that("a figure saved at char_size() is written", {
  skip_if_not(cairo_pdf_works(), "cairo_pdf() writes no file here")
  p <- cf_plot()
  s <- char_size(p)
  tmp_pdf <- withr::local_tempfile(fileext = ".pdf")
  save_figure(char_figure(p), tmp_pdf, width = s$width, height = s$height)
  expect_true(file.exists(tmp_pdf))
  expect_gt(file.size(tmp_pdf), 0)
})

test_that("both styles draw without warnings", {
  p <- cf_plot()
  expect_no_warning(draws_silently(char_figure(p)))
  expect_no_warning(draws_silently(char_figure(p, style = "c64")))
})

test_that("the bw figure passes the two_values check", {
  res <- check_figure(char_figure(cf_plot()))
  expect_identical(res$status[res$check == "two_values"], "pass")
})
