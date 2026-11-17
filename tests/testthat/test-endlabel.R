two_lines <- function(y_last_a = 5, y_b = NULL) {
  d <- data.frame(
    x = rep(1:5, 2),
    y = c(1, 2, 3, 4, y_last_a, 2, 3, 4, 5, 6),
    g = rep(c("a", "b"), each = 5),
    stringsAsFactors = FALSE
  )
  if (!is.null(y_b)) d$y[d$g == "b"] <- y_b
  d
}

endlabel_data <- function(d, ...) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = g)) +
    ggplot2::geom_line() +
    geom_endlabel(ggplot2::aes(label = g), ...)
  ggplot2::layer_data(p, 2L)
}

test_that("each group is labelled at its largest x", {
  ld <- endlabel_data(two_lines())
  expect_identical(nrow(ld), 2L)
  expect_equal(ld$x, c(5, 5))
  expect_setequal(ld$label, c("a", "b"))
  expect_equal(ld$y[ld$label == "b"], 6)
})

test_that("a missing last point moves the label to the last finite point", {
  ld <- endlabel_data(two_lines(y_last_a = NA), na.rm = TRUE)
  expect_identical(nrow(ld), 2L)
  expect_equal(ld$x[ld$label == "a"], 4)
  expect_equal(ld$y[ld$label == "a"], 4)
  expect_equal(ld$x[ld$label == "b"], 5)
})

test_that("a group without finite rows yields no row and no error", {
  d <- two_lines(y_b = NA_real_)
  expect_no_error(ld <- endlabel_data(d, na.rm = TRUE))
  expect_identical(nrow(ld), 1L)
  expect_identical(ld$label, "a")
})

test_that("the stat itself drops non-finite rows and breaks ties by first row", {
  d <- data.frame(x = c(1, 3, 3, Inf), y = c(1, 2, 5, 1), label = c("p", "q", "r", "s"),
                  stringsAsFactors = FALSE)
  out <- StatEndlabel$compute_group(d, scales = list())
  expect_identical(nrow(out), 1L)
  expect_identical(out$label, "q")

  empty <- data.frame(x = c(NA, 1), y = c(1, NaN), label = c("a", "b"))
  out0 <- StatEndlabel$compute_group(empty, scales = list())
  expect_s3_class(out0, "data.frame")
  expect_identical(nrow(out0), 0L)
  expect_identical(names(out0), names(empty))
})

test_that("the label colour is the ink token", {
  ld <- endlabel_data(two_lines())
  expect_true(all(ld$colour == .ip_ink))
  ld2 <- endlabel_data(two_lines(), colour = "red")
  expect_true(all(ld2$colour == .ip_ink))
})

test_that("defaults: hjust, size, family, stat and geom", {
  layer <- geom_endlabel(ggplot2::aes(label = g))
  expect_s3_class(layer$stat, "StatEndlabel")
  expect_s3_class(layer$geom, "GeomEndlabel")
  expect_s3_class(layer$geom, "GeomText")
  expect_equal(layer$aes_params$hjust, 0)
  expect_equal(layer$aes_params$size, 8 / ggplot2::.pt)
  expect_equal(layer$aes_params$size, .ip_label_size_pt / ggplot2::.pt)
  expect_identical(layer$aes_params$family, figure_font("sans"))
  expect_false(layer$show.legend)
  custom <- geom_endlabel(size = 3, family = "serif", hjust = 0.5)
  expect_equal(custom$aes_params$size, 3)
  expect_identical(custom$aes_params$family, "serif")
  expect_equal(custom$aes_params$hjust, 0.5)
})

# Builds the panel grob of the end-label layer and lays it out under a viewport
# of known size on a ragg device. Returns the text grob and the font settings.
endlabel_text <- function(d, width = 60, height = 40) {
  skip_if_not_installed("ragg")
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = g)) +
    ggplot2::geom_line() +
    geom_endlabel(ggplot2::aes(label = g))
  b <- ggplot2::ggplot_build(p)
  g <- GeomEndlabel$draw_panel(b$data[[2]], b$layout$panel_params[[1]], b$layout$coord)
  f <- withr::local_tempfile(fileext = ".png")
  ragg::agg_png(f, width = 100, height = 100, units = "mm", res = 72)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::pushViewport(grid::viewport(width = grid::unit(width, "mm"),
                                    height = grid::unit(height, "mm")))
  laid <- grid::makeContent(g)
  txt <- grid::getGrob(laid, "labels")
  h <- .ip_text_height_npc(g$fontsize[[1]], g$fontfamily[[1]])
  x_mm <- grid::convertX(txt$x, "mm", valueOnly = TRUE)
  grid::popViewport()
  list(grob = g, text = txt, height = h, x_mm = x_mm, width = width)
}

test_that("two series ending at the same y get labels at least the gap apart", {
  d <- two_lines(y_b = c(2, 3, 4, 4.5, 5))
  out <- endlabel_text(d)
  y <- as.numeric(out$text$y)
  expect_length(y, 2L)
  expect_gt(out$height, 0)
  expect_gte(abs(diff(y)), .ip_endlabel_gap * out$height - 1e-9)
  # The pair keeps its mean position.
  expect_equal(mean(y), mean(out$grob$y))
  expect_equal(out$grob$y[[1]], out$grob$y[[2]])
})

test_that("labels already far apart stay where they are", {
  d <- two_lines(y_last_a = 1)
  out <- endlabel_text(d)
  expect_equal(as.numeric(out$text$y), out$grob$y)
})

test_that("labels start at x + nudge with hjust = 0 in the ink colour", {
  out <- endlabel_text(two_lines())
  expect_equal(out$text$hjust, c(0, 0))
  expect_equal(out$x_mm, out$grob$x * out$width + .ip_endlabel_nudge)
  expect_identical(out$text$gp$col, rep(.ip_ink, 2L))
  expect_equal(out$text$gp$fontsize, rep(.ip_label_size_pt, 2L))
})

test_that(".ip_spread pushes apart, keeps order and mean", {
  expect_identical(.ip_spread(0.5, 0.1), 0.5)
  y <- c(0.5, 0.5, 0.52)
  s <- .ip_spread(y, 0.1)
  expect_equal(mean(s), mean(y))
  expect_true(all(diff(sort(s)) >= 0.1 - 1e-12))
  expect_identical(order(s), order(y))
  expect_equal(.ip_spread(c(0.9, 0.1), 0.1), c(0.9, 0.1))
})

test_that("a plot with end labels draws, including an empty panel", {
  d <- two_lines()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = g)) +
    ggplot2::geom_line() +
    geom_endlabel(ggplot2::aes(label = g)) +
    ggplot2::coord_cartesian(clip = "off")
  expect_s3_class(draws_silently(p), "gtable")
  skip_if_not_installed("ragg")
  f <- withr::local_tempfile(fileext = ".png")
  ragg::agg_png(f, width = 80, height = 44, units = "mm", res = 100)
  expect_no_error(print(p))
  grDevices::dev.off()
  expect_gt(file.size(f), 0)
  b <- ggplot2::ggplot_build(p)
  empty <- GeomEndlabel$draw_panel(b$data[[2]][0, ], b$layout$panel_params[[1]],
                                   b$layout$coord)
  expect_s3_class(empty, "null")
})

test_that("bad arguments raise invalid_input", {
  expect_error(geom_endlabel(hjust = "a"), class = "inferplotR_invalid_input")
  expect_error(geom_endlabel(size = -1), class = "inferplotR_invalid_input")
  expect_error(geom_endlabel(size = 0), class = "inferplotR_invalid_input")
  expect_error(geom_endlabel(family = NA_character_), class = "inferplotR_invalid_input")
  expect_error(geom_endlabel(na.rm = NA), class = "inferplotR_invalid_input")
  expect_error(geom_endlabel(show.legend = "no"), class = "inferplotR_invalid_input")
  expect_error(geom_endlabel(inherit.aes = 1), class = "inferplotR_invalid_input")
})
