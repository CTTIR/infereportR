test_that("stat_omnibus labels a direct anova_one_way run", {
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point() +
    stat_omnibus(method = "anova_one_way")
  d <- layer_data_of(p, 2L)
  expect_identical(nrow(d), 1L)
  direct <- infercalcR::run_inference(infercalcR::method_spec("anova_one_way"),
                                      x = pg$weight, group = pg$group)
  shown <- report_table(direct)
  expect_true(grepl(shown$statistic_display, d$label, fixed = TRUE))
  expect_true(grepl(shown$p_display, d$label, fixed = TRUE))
  expect_equal(infercalcR::result_data(d$.ir_result[[1]])$statistic,
               infercalcR::result_data(direct)$statistic)
  expect_identical(d$.ir_result_id, "infereportR:omnibus:anova_one_way:panel1")
  expect_identical(d$colour, .ir_ink)
  expect_no_warning(draws_silently(p))
})

test_that("other omnibus methods and arguments work", {
  for (m in c("anova_one_way_welch", "kruskal_wallis")) {
    d <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
                         ggplot2::geom_point() + stat_omnibus(method = m), 2L)
    expect_identical(infercalcR::result_metadata(d$.ir_result[[1]])$method_id, m)
  }
  bad <- "infereportR_invalid_input"
  expect_error(stat_omnibus(), class = bad)
  expect_error(stat_omnibus(method = "t_test"), class = "infereportR_unsupported")
  expect_error(stat_omnibus(method = "anova_one_way", label_x = NA), class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", hjust = "a"), class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", method_args = list(group = 1)),
               class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", text_size = -1), class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", family = 2), class = bad)
})

test_that("an omnibus label at the panel edge keeps a gap from the axis", {
  base <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point()
  edge <- ggplot2::layer_grob(base + stat_omnibus(method = "anova_one_way"), 2L)[[1]]
  inside <- ggplot2::layer_grob(base + stat_omnibus(method = "anova_one_way", label_x = 2),
                                2L)[[1]]
  expect_s3_class(edge, "text")
  expect_identical(grid::unitType(edge$x), "sum")
  expect_identical(grid::unitType(inside$x), "native")
  right <- ggplot2::layer_grob(base + stat_omnibus(method = "anova_one_way", label_x = Inf,
                                                   hjust = 1), 2L)[[1]]
  expect_identical(grid::unitType(right$x), "sum")
})

test_that("a panel without finite values gives an empty omnibus layer", {
  d <- data.frame(g = factor(c("a", "a", "b", "b")), y = c(Inf, -Inf, Inf, NaN))
  p <- ggplot2::ggplot(d, ggplot2::aes(g, y)) + ggplot2::geom_point() +
    stat_omnibus(method = "anova_one_way")
  expect_identical(nrow(layer_data_of(p, 2L)), 0L)
  empty <- .ir_text_empty()
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), c("x", "y", "label", "group", ".ir_result", ".ir_result_id"))
})

test_that("the omnibus label names the test statistic", {
  symbols <- c(anova_one_way = "F = ", anova_one_way_welch = "F = ", kruskal_wallis = "H = ")
  for (m in names(symbols)) {
    d <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
                         ggplot2::geom_point() + stat_omnibus(method = m), 2L)
    shown <- report_table(d$.ir_result[[1]])
    expect_identical(d$label, paste0(shown$estimand, ": ", symbols[[m]],
                                     shown$statistic_display, ", ", shown$p_display))
  }
})

test_that("corner labels are inset from the panel edges", {
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point() +
    stat_omnibus(method = "anova_one_way")
  b <- ggplot2::ggplot_build(p)
  grob <- b$plot$layers[[2]]$geom$draw_panel(
    b$data[[2]], b$layout$panel_params[[1]], b$layout$coord)
  expect_s3_class(grob, "text")
  # Resolve the units in a panel-like viewport whose native scale is 0 to 1.
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 4, height = 3)
  grid::pushViewport(grid::viewport(xscale = c(0, 1), yscale = c(0, 1)))
  x_mm <- grid::convertX(grob$x, "mm", valueOnly = TRUE)
  y_mm <- grid::convertY(grob$y, "mm", valueOnly = TRUE)
  top_mm <- grid::convertY(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  grDevices::dev.off()
  expect_equal(x_mm, .ir_edge_inset)
  expect_equal(y_mm, top_mm - .ir_edge_inset)
  q <- ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
    stat_correlation(method = "correlation_pearson", label_x = 3, label_y = 30)
  bq <- ggplot2::ggplot_build(q)
  plain <- bq$plot$layers[[2]]$geom$draw_panel(
    bq$data[[2]], bq$layout$panel_params[[1]], bq$layout$coord)
  ref <- ggplot2::GeomText$draw_panel(bq$data[[2]], bq$layout$panel_params[[1]],
                                      bq$layout$coord)
  expect_identical(plain$x, ref$x)
  expect_no_warning(draws_silently(p))
  expect_no_warning(draws_silently(q))
})
