bad <- "inferplotR_invalid_input"
many <- "inferplotR_too_many_groups"

test_that(".ip_palette returns prefixes and refuses to recycle", {
  for (v in list(c("a", "b", "c"), c(16, 1, 15), list("x", 2, NULL))) {
    pal <- .ip_palette(v, "things")
    expect_identical(pal(2), v[1:2])
    expect_identical(pal(3), v)
    expect_length(pal(0), 0L)
    expect_error(pal(4), class = many)
  }
  expect_type(.ip_palette(list(1, 2), "things")(1), "list")
})

test_that("linetype palette gives the documented sequence", {
  pal <- scale_linetype_figure()$palette
  expect_identical(pal(6), c("solid", "22", "13", "1343", "73", "2262"))
  expect_identical(pal(2), c("solid", "22"))
  expect_error(pal(7), class = many)
  custom <- scale_linetype_figure(values = c("solid", "13"))$palette
  expect_identical(custom(2), c("solid", "13"))
  expect_error(custom(3), class = many)
})

test_that("shape palette gives the documented sequence", {
  pal <- scale_shape_figure()$palette
  expect_identical(pal(8), c(16, 1, 15, 0, 17, 2, 18, 5))
  expect_error(pal(9), class = many)
  expect_identical(scale_shape_figure(values = c(1, 2))$palette(2), c(1, 2))
})

test_that("ink palette maps every level to ink", {
  pal <- scale_colour_ink()$palette
  expect_identical(pal(3), rep(.ip_ink, 3))
  expect_identical(pal(20), rep(.ip_ink, 20))
  expect_identical(scale_color_ink, scale_colour_ink)
  expect_identical(scale_colour_ink(aesthetics = c("colour", "fill"))$aesthetics,
                   c("colour", "fill"))
})

test_that("scales pass ... through to discrete_scale", {
  s <- scale_linetype_figure(name = "Group", labels = c("One", "Two"))
  expect_identical(s$name, "Group")
  expect_identical(s$labels, c("One", "Two"))
  expect_identical(scale_shape_figure(name = "Arm")$name, "Arm")
  expect_identical(scale_colour_ink(name = "Arm")$name, "Arm")
})

test_that("ink, linetype and shape scales combine on a 2-group line plot", {
  d <- data.frame(x = rep(1:4, 2), y = c(1:4, 4:1), g = rep(c("A", "B"), each = 4))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g, linetype = g, shape = g)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    scale_linetype_figure() + scale_shape_figure() + scale_colour_ink()
  b <- ggplot2::ggplot_build(p)
  lines <- b$data[[1]]
  points <- b$data[[2]]
  expect_true(all(lines$colour == .ip_ink))
  expect_true(all(points$colour == .ip_ink))
  expect_setequal(unique(lines$linetype), c("solid", "22"))
  expect_identical(lines$linetype[lines$group == 1][1], "solid")
  expect_identical(lines$linetype[lines$group == 2][1], "22")
  expect_setequal(unique(points$shape), c(16, 1))
  expect_identical(points$shape[points$group == 1][1], 16)
  expect_identical(points$shape[points$group == 2][1], 1)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("too many groups fails at build time with a classed error", {
  d <- data.frame(x = rep(1:2, 7), y = 1:14, g = rep(letters[1:7], each = 2))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, linetype = g)) +
    ggplot2::geom_line() + scale_linetype_figure()
  expect_error(ggplot2::ggplot_build(p), class = many)
})

test_that("invalid scale arguments raise inferplotR_invalid_input", {
  for (v in list(character(), NA_character_, c("solid", NA), "", list("solid"), TRUE)) {
    expect_error(scale_linetype_figure(values = v), class = bad)
    expect_error(scale_shape_figure(values = v), class = bad)
  }
  for (a in list(character(), NA_character_, 1, "")) {
    expect_error(scale_colour_ink(aesthetics = a), class = bad)
  }
})
