gallery_forest_results <- function() {
  fp <- .ip_gallery_data()$fp
  spec <- infercalcR::method_spec("t_means_two_independent_welch")
  lapply(.ip_gallery_subsets(fp), function(s) {
    infercalcR::run_inference(spec, x = fp$il6[s & fp$late == 1], y = fp$il6[s & fp$late == 0])
  })
}

parse_forest_oracle <- function(v) {
  list(n = as.numeric(sub("^n=([0-9]+) .*$", "\\1", v)),
       est = as.numeric(sub("^.* est=([-0-9.]+) .*$", "\\1", v)),
       text = sub("^.* text=", "", v))
}

test_that("figure_forest reproduces the 7 gallery rows (figure 06)", {
  res <- gallery_forest_results()
  p <- figure_forest(res, xlab = "IL-6 difference, late minus early peak (pg/ml)")
  b <- ggplot2::ggplot_build(p)
  labels <- names(res)
  # Layers: null line, CI segments, markers, label, n, estimate text, header, rule.
  marks <- b$data[[3]]
  est_text <- b$data[[6]]
  label_layer <- b$data[[4]]
  n_layer <- b$data[[5]]
  expect_identical(nrow(marks), 7L)
  for (i in seq_along(labels)) {
    o <- parse_forest_oracle(gallery_value("06", paste0("row:", labels[i])))
    expect_identical(est_text$label[i], o$text)
    expect_equal(round(marks$x[i], 6), o$est)
    expect_identical(marks$size[i] > 0, TRUE)
    expect_identical(n_layer$label[i], format(o$n))
  }
  expect_identical(label_layer$fontface, c(2L, rep(1L, 6)))
  expect_identical(label_layer$label, labels)
  # The overall row is on top, above the rule; the others follow in order.
  expect_true(all(marks$y[1] > marks$y[-1]))
  expect_identical(order(-marks$y[-1]), 1:6)
  expect_length(inference_results(p), 7L)
  expect_identical(nrow(figure_report(p)), 7L)
  expect_identical(b$data[[1]]$xintercept, 0)
})

test_that("figure_forest lays text columns out from measured widths", {
  res <- gallery_forest_results()
  p <- figure_forest(res)
  b <- ggplot2::ggplot_build(p)
  xr <- b$layout$panel_params[[1]]$x.range
  label_x <- unique(b$data[[4]]$x)
  n_x <- unique(b$data[[5]]$x)
  text_x <- unique(b$data[[6]]$x)
  expect_lt(label_x, n_x)
  expect_lt(n_x, xr[1])
  expect_gt(text_x, xr[2])
  margin <- as.numeric(p$theme$plot.margin)
  # Longer labels need a wider left margin.
  long <- res
  # Long enough to widen the column, short enough for any common sans font.
  names(long)[2] <- "A much longer subgroup label"
  q <- figure_forest(long)
  expect_gt(as.numeric(q$theme$plot.margin)[4], margin[4])
  # The panel fills what is left of the slot.
  expect_equal(sum(margin[c(2, 4)]) < 134, TRUE)
  expect_no_warning(draws_silently(p))
  expect_identical(check_figure(p)$status[check_figure(p)$check == "two_values"], "pass")
  for (mode in c("light", "dark")) for (tr in c(FALSE, TRUE)) {
    pm <- figure_forest(res, mode = mode, transparent = tr)
    expect_identical(check_figure(pm)$status[check_figure(pm)$check == "two_values"], "pass")
    expect_identical(unique(ggplot2::layer_data(pm, 3)$colour), .ip_mode(mode)$ink)
  }
})

test_that("figure_forest checks its input", {
  res <- gallery_forest_results()
  expect_error(figure_forest(list()), class = "inferplotR_invalid_input")
  expect_error(figure_forest(unname(res)), class = "inferplotR_invalid_input")
  expect_error(figure_forest(list(a = 1)), class = "inferplotR_invalid_input")
  expect_error(figure_forest(res, n = 1:2), class = "inferplotR_invalid_input")
  expect_error(figure_forest(res, overall = 9L), class = "inferplotR_invalid_input")
  expect_error(figure_forest(res, row = "nope"), class = "inferplotR_invalid_input")
  expect_error(figure_forest(res, mode = "sepia"), class = "inferplotR_invalid_input")
  expect_error(figure_forest(res, slot = "poster"), class = "inferplotR_invalid_input")
  two <- infercalcR::run_inference(infercalcR::method_spec("anova_one_way"),
                                   x = PlantGrowth$weight, group = PlantGrowth$group)
  if (nrow(infercalcR::result_data(two)) != 1L) {
    expect_error(figure_forest(list(a = two)), class = "inferplotR_invalid_input")
  }
  p <- figure_forest(res[2:3], overall = NULL, header = NULL, n = c(10, 20))
  expect_identical(ggplot2::layer_data(p, 5)$label, c("10", "20"))
  expect_length(inference_results(p), 2L)
  expect_no_warning(draws_silently(p))
})

test_that("figure_forest(xlim) sets the axis range and keeps the text columns outside", {
  res <- gallery_forest_results()
  p <- figure_forest(res, xlim = c(-30, 60))
  b <- ggplot2::ggplot_build(p)
  xr <- b$layout$panel_params[[1]]$x.range
  expect_equal(xr, c(-30, 60))
  expect_lt(max(unique(b$data[[5]]$x)), -30)
  expect_lt(unique(b$data[[4]]$x), unique(b$data[[5]]$x))
  expect_gt(unique(b$data[[6]]$x), 60)
  expect_true(unique(b$data[[8]]$xend) > 60)
  expect_no_warning(draws_silently(p))
  # Without xlim the range follows the data, so it differs.
  auto <- ggplot2::ggplot_build(figure_forest(res))$layout$panel_params[[1]]$x.range
  expect_false(isTRUE(all.equal(auto, c(-30, 60))))
  # A narrow range cuts intervals at the limits and hides what lies outside.
  q <- figure_forest(res, xlim = c(15, 30))
  bq <- ggplot2::ggplot_build(q)
  # The null line (0) lies outside, so the CI segments are the first layer.
  expect_false(inherits(q$layers[[1]]$geom, "GeomVline"))
  seg <- bq$data[[1]]
  expect_true(all(seg$x >= 15 & seg$xend <= 30, na.rm = TRUE))
  marks <- bq$data[[2]]
  expect_true(all(is.na(marks$x) | (marks$x >= 15 & marks$x <= 30)))
  expect_true(anyNA(marks$x))
  expect_length(inference_results(q), 7L)
  expect_false(any(vapply(bq$data, function(d) "xintercept" %in% names(d), logical(1))))
  expect_no_warning(draws_silently(q))
  for (bad in list(1, c(2, 1), c(0, Inf), c("a", "b"), c(NA, 1))) {
    expect_error(figure_forest(res, xlim = bad), class = "inferplotR_invalid_input")
  }
})
