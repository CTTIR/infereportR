test_that("stat_correlation equals a direct run with the id column", {
  p <- ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
    stat_correlation(method = "correlation_pearson")
  d <- layer_data_of(p, 2L)
  direct <- infercalcR::run_inference(infercalcR::method_spec("correlation_pearson"),
    data = data.frame(id = seq_len(nrow(mt)), x = mt$wt, y = mt$mpg))
  got <- infercalcR::result_data(d$.ip_result[[1]])
  want <- infercalcR::result_data(direct)
  expect_equal(got[setdiff(names(got), "row_id")], want[setdiff(names(want), "row_id")])
  expect_true(startsWith(d$label, "r = "))
  shown <- infereportR::report_table(direct)
  expect_identical(d$label, paste0("r = ", shown$estimate_display, " ", shown$interval_display,
                                   ", ", shown$p_display))
  expect_no_warning(draws_silently(p))
})

test_that("symbols follow the method", {
  lab <- function(m) {
    layer_data_of(ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) +
                    stat_correlation(method = m, label_x = 3, label_y = 30))$label
  }
  expect_true(startsWith(lab("correlation_spearman"), "rho = "))
  expect_true(startsWith(lab("correlation_kendall"), "tau = "))
})

test_that("fit = TRUE draws predict_inference values from infercalcR", {
  layers <- stat_correlation(method = "correlation_pearson", fit = TRUE)
  expect_type(layers, "list")
  expect_length(layers, 2L)
  p <- ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() + layers
  fit <- layer_data_of(p, 2L)
  model <- infercalcR::run_inference(infercalcR::method_spec("prediction_model_linear"),
    data = data.frame(id = seq_len(nrow(mt)), x = mt$wt, y = mt$mpg), predictors = "x",
    independent_units = TRUE)
  pred <- infercalcR::result_data(infercalcR::predict_inference(
    model, data.frame(x = fit$x), interval = "mean"))
  expect_equal(fit$y, pred$estimate)
  expect_equal(fit$ymin, pred$conf.low)
  expect_equal(fit$ymax, pred$conf.high)
  expect_length(inference_results(p), 2L)
  expect_no_warning(draws_silently(p))
})

test_that("stat_correlation needs continuous axes and explicit methods", {
  expect_error(stat_correlation(), class = "inferplotR_invalid_input")
  expect_error(stat_correlation(method = "anova_one_way"), class = "inferplotR_unsupported")
  expect_error(stat_correlation(method = "correlation_pearson", fit = NA),
               class = "inferplotR_invalid_input")
  expect_error(stat_correlation(method = "correlation_pearson", conf.level = 0),
               class = "inferplotR_invalid_input")
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
                                       stat_correlation(method = "correlation_pearson")),
               class = "inferplotR_unsupported")
})

test_that("labels leave out a missing interval; the report keeps it", {
  for (m in c("correlation_spearman", "correlation_kendall")) {
    p <- ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
      stat_correlation(method = m)
    d <- layer_data_of(p, 2L)
    res <- infercalcR::result_data(d$.ip_result[[1]])
    expect_true(is.na(res$conf.low) && is.na(res$conf.high))
    shown <- infereportR::report_table(d$.ip_result[[1]])
    sym <- .ip_correlation_symbol[[m]]
    expect_identical(d$label, paste0(sym, shown$estimate_display, ", ", shown$p_display))
    expect_false(grepl(shown$interval_display, d$label, fixed = TRUE))
    rep <- figure_report(p)
    expect_identical(rep$interval_display, shown$interval_display)
  }
})

test_that("correlation and omnibus labels default to the in-plot label size", {
  d <- layer_data_of(ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
                       stat_correlation(method = "correlation_pearson"), 2L)
  expect_equal(d$size * ggplot2::.pt, .ip_label_size_pt)
  o <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point() +
                       stat_omnibus(method = "anova_one_way"), 2L)
  expect_equal(o$size * ggplot2::.pt, .ip_label_size_pt)
})

test_that("label = \"estimate_ci_p_n\" sets two lines with n from infereportR", {
  p <- ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
    stat_correlation(method = "correlation_pearson", label = "estimate_ci_p_n")
  d <- layer_data_of(p, 2L)
  res <- d$.ip_result[[1]]
  shown <- infereportR::report_table(res)
  n <- infercalcR::result_data(res)$n
  expect_equal(n, 32)
  expect_identical(d$label, paste0("r = ", shown$estimate_display, " ", shown$interval_display,
                                   "\n", shown$p_display, ", n = 32"))
  # The count is written by infereportR and has no decimals.
  expect_identical(.ip_format_n(n), "32")
  expect_identical(.ip_format_n(80), "80")
  expect_equal(d$lineheight, 0.95)
  expect_no_warning(draws_silently(p))
  # The default stays one line; lineheight given by the user wins.
  one <- layer_data_of(ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
                         stat_correlation(method = "correlation_pearson"), 2L)
  expect_false(grepl("\n", one$label, fixed = TRUE))
  own <- layer_data_of(ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
                         stat_correlation(method = "correlation_pearson",
                                          label = "estimate_ci_p_n", lineheight = 1.2), 2L)
  expect_equal(own$lineheight, 1.2)
  expect_error(stat_correlation(method = "correlation_pearson", label = "n"),
               class = "inferplotR_invalid_input")
})

test_that("the two-line label leaves out a missing interval (D7)", {
  for (m in c("correlation_spearman", "correlation_kendall")) {
    d <- layer_data_of(ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
                         stat_correlation(method = m, label = "estimate_ci_p_n"), 2L)
    shown <- infereportR::report_table(d$.ip_result[[1]])
    expect_identical(d$label, paste0(.ip_correlation_symbol[[m]], shown$estimate_display,
                                     "\n", shown$p_display, ", n = 32"))
  }
})

test_that("small panels fail as documented: 4 pairs per correlation, 20 for the fit", {
  small <- mt[1:10, ]
  base <- ggplot2::ggplot(small, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  d <- layer_data_of(base + stat_correlation(method = "correlation_pearson",
                                             label = "estimate_ci_p_n"), 2L)
  expect_true(endsWith(d$label, "n = 10"))
  expect_error(ggplot2::ggplot_build(base + stat_correlation(method = "correlation_pearson",
                                                             fit = TRUE)),
               class = "inferplotR_calculation_failed")
  tiny <- ggplot2::ggplot(mt[1:3, ], ggplot2::aes(wt, mpg)) + ggplot2::geom_point() +
    stat_correlation(method = "correlation_pearson")
  expect_error(ggplot2::ggplot_build(tiny), class = "inferplotR_calculation_failed")
})
