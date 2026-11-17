bad <- "inferplotR_invalid_input"

bar_plot <- function(...) {
  d <- data.frame(g = c("a", "b", "c"), v = c(3, 5, 2))
  ggplot2::ggplot(d, ggplot2::aes(g, v, fill = g)) +
    ggplot2::geom_col(colour = .ip_ink) +
    scale_fill_hatch(...)
}

test_that("solid and open give the two colour tokens", {
  expect_identical(hatch_pattern("solid"), .ip_ink)
  expect_identical(hatch_pattern("open"), .ip_paper)
})

test_that("every other type is a grid tiling pattern", {
  for (type in setdiff(.ip_hatch_types, c("solid", "open"))) {
    p <- hatch_pattern(type)
    expect_s3_class(p, "GridPattern")
    expect_s3_class(p, "GridTilingPattern")
  }
  expect_s3_class(hatch_pattern("hatch", spacing = 0.5, linewidth = 2), "GridPattern")
  expect_s3_class(hatch_pattern("dots", spacing = 10, linewidth = 0.1), "GridPattern")
})

test_that("tile geometry follows the specification", {
  diag <- hatch_pattern("hatch", spacing = 2)
  expect_equal(grid::convertUnit(diag$width, "mm", valueOnly = TRUE), 2 * sqrt(2))
  straight <- hatch_pattern("horizontal", spacing = 2)
  expect_equal(grid::convertUnit(straight$width, "mm", valueOnly = TRUE), 2)
  expect_equal(grid::convertUnit(hatch_pattern("dots", spacing = 3)$height, "mm",
                                 valueOnly = TRUE), 3)
  segs <- .ip_hatch_segments("crosshatch")
  expect_length(segs$x0, 6L)
  expect_identical(.ip_hatch_segments("backhatch")$x0, 1 - .ip_hatch_segments("hatch")$x0)
})

test_that("default spacing is the 1.4 mm slide token", {
  expect_identical(.ip_hatch_spacing, 1.4)
  expect_identical(formals(hatch_pattern)$spacing, quote(.ip_hatch_spacing))
  expect_identical(formals(scale_fill_hatch)$spacing, quote(.ip_hatch_spacing))
  mm <- function(p) grid::convertUnit(p$width, "mm", valueOnly = TRUE)
  expect_equal(mm(hatch_pattern("horizontal")), 1.4)
  expect_equal(mm(hatch_pattern("hatch")), 1.4 * sqrt(2))
  expect_equal(mm(scale_fill_hatch()$palette(3)[[3]]), 1.4 * sqrt(2))
  # Paper figures may pass the former 1.6 mm.
  expect_equal(mm(hatch_pattern("vertical", spacing = 1.6)), 1.6)
  expect_equal(mm(scale_fill_hatch(spacing = 1.6)$palette(3)[[3]]), 1.6 * sqrt(2))
})

test_that("scale_fill_hatch palette returns the documented list", {
  pal <- scale_fill_hatch()$palette
  fills <- pal(6)
  expect_type(fills, "list")
  expect_length(fills, 6L)
  expect_identical(fills[[1]], .ip_ink)
  expect_identical(fills[[2]], .ip_paper)
  for (i in 3:6) expect_s3_class(fills[[i]], "GridPattern")
  expect_error(pal(7), class = "inferplotR_too_many_groups")
  custom <- scale_fill_hatch(patterns = c("vertical", "open"))$palette(2)
  expect_s3_class(custom[[1]], "GridPattern")
  expect_identical(custom[[2]], .ip_paper)
})

test_that("scale_fill_hatch fills a 3-bar plot with a list column", {
  p <- bar_plot(name = "Group")
  b <- ggplot2::ggplot_build(p)
  fill <- b$data[[1]]$fill
  expect_type(fill, "list")
  expect_length(fill, 3L)
  expect_identical(fill[[1]], .ip_ink)
  expect_identical(fill[[2]], .ip_paper)
  expect_s3_class(fill[[3]], "GridPattern")
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("a hatched plot renders to PDF with cairo", {
  skip_if_not(capabilities("cairo"))
  file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(file, width = 3, height = 2.5)
  dev <- grDevices::dev.cur()
  withr::defer(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev))
  print(bar_plot())
  grDevices::dev.off(dev)
  expect_true(file.exists(file))
  expect_gt(file.size(file), 1024)
})

test_that("a hatched plot renders to PNG with ragg", {
  skip_if_not_installed("ragg")
  file <- withr::local_tempfile(fileext = ".png")
  ragg::agg_png(file, width = 3, height = 2.5, units = "in", res = 150)
  dev <- grDevices::dev.cur()
  withr::defer(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev))
  print(bar_plot())
  grDevices::dev.off(dev)
  expect_true(file.exists(file))
  expect_gt(file.size(file), 1024)
})

test_that("invalid hatch arguments raise inferplotR_invalid_input", {
  expect_error(hatch_pattern("zigzag"), class = bad)
  expect_error(hatch_pattern(), class = bad)
  expect_error(hatch_pattern(c("hatch", "dots")), class = bad)
  expect_error(hatch_pattern("hatch", spacing = 0), class = bad)
  expect_error(hatch_pattern("hatch", spacing = 11), class = bad)
  expect_error(hatch_pattern("hatch", spacing = NA_real_), class = bad)
  expect_error(hatch_pattern("hatch", linewidth = -1), class = bad)
  expect_error(hatch_pattern("hatch", linewidth = 3), class = bad)
  expect_error(scale_fill_hatch(patterns = character()), class = bad)
  expect_error(scale_fill_hatch(patterns = c("hatch", "zigzag")), class = bad)
  expect_error(scale_fill_hatch(patterns = NA_character_), class = bad)
  expect_error(scale_fill_hatch(spacing = 0), class = bad)
  expect_error(scale_fill_hatch(linewidth = -1), class = bad)
})
