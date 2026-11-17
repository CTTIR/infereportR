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

test_that("defaults: hjust, size, family and stat", {
  layer <- geom_endlabel(ggplot2::aes(label = g))
  expect_s3_class(layer$stat, "StatEndlabel")
  expect_s3_class(layer$geom, "GeomText")
  expect_equal(layer$aes_params$hjust, -0.15)
  expect_equal(layer$aes_params$size, 9 / ggplot2::.pt)
  expect_identical(layer$aes_params$family, figure_font("sans"))
  expect_false(layer$show.legend)
  custom <- geom_endlabel(size = 3, family = "serif", hjust = 0)
  expect_equal(custom$aes_params$size, 3)
  expect_identical(custom$aes_params$family, "serif")
  expect_equal(custom$aes_params$hjust, 0)
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
