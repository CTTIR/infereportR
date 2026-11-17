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
