test_that("GeomBracket draws a bar, two tips and a label", {
  d <- data.frame(x = 1, xend = 2, y = 5, label = "p = 0.1", tip = 0.2, PANEL = 1L,
                  group = 1L, colour = .ip_ink, linewidth = 0.3, size = 3, family = "",
                  alpha = NA)
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point()
  b <- ggplot2::ggplot_build(p)
  g <- GeomBracket$draw_panel(d, b$layout$panel_params[[1]], b$layout$coord)
  expect_s3_class(g, "gTree")
  kinds <- unname(vapply(g$children, function(k) class(k)[1], ""))
  expect_identical(kinds, c("segments", "segments", "segments", "text"))
  expect_s3_class(GeomBracket$draw_panel(d[0, ], b$layout$panel_params[[1]], b$layout$coord),
                  "null")
  expect_identical(GeomBracket$required_aes, c("x", "xend", "y", "label"))
})

test_that("GeomBracket draws nothing for headroom rows", {
  d <- data.frame(x = c(1, 1), xend = c(2, 1), y = c(5, 6), label = c("p = 0.1", ""),
                  tip = c(0.2, 0), headroom = c(FALSE, TRUE), PANEL = 1L, group = 1:2,
                  colour = .ip_ink, linewidth = 0.3, size = 3, family = "", alpha = NA)
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point()
  b <- ggplot2::ggplot_build(p)
  g <- GeomBracket$draw_panel(d, b$layout$panel_params[[1]], b$layout$coord)
  expect_identical(length(g$children[[4]]$label), 1L)
  expect_identical(length(g$children[[1]]$x0), 1L)
  expect_s3_class(GeomBracket$draw_panel(d[2, ], b$layout$panel_params[[1]], b$layout$coord),
                  "null")
})
