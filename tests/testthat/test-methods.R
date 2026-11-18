test_that("method sets match the specification", {
  expect_identical(.ir_methods$means, "t_mean_one_sample")
  expect_setequal(.ir_methods$compare,
                  c("t_means_two_independent_welch", "t_means_two_independent_student",
                    "rank_sum_mann_whitney", "brunner_munzel"))
  expect_setequal(.ir_methods$omnibus, c("anova_one_way", "anova_one_way_welch", "kruskal_wallis"))
  expect_setequal(.ir_methods$correlation,
                  c("correlation_pearson", "correlation_spearman", "correlation_kendall"))
  expect_true(all(unlist(.ir_methods) %in% infercalcR::method_catalogue()$method_id))
})

test_that(".ir_check_method requires an explicit supported method", {
  expect_error(.ir_check_method(NULL, "compare"), class = "infereportR_invalid_input")
  expect_error(.ir_check_method("", "compare"), class = "infereportR_invalid_input")
  expect_error(.ir_check_method(1, "compare"), class = "infereportR_invalid_input")
  expect_error(.ir_check_method("t_test", "compare"), class = "infereportR_unsupported")
  expect_error(.ir_check_method("anova_one_way", "compare"), class = "infereportR_unsupported")
  expect_identical(.ir_check_method("kruskal_wallis", "omnibus"), "kruskal_wallis")
})

test_that(".ir_spec passes only the options that were set", {
  s <- .ir_spec("t_means_two_independent_welch")
  d <- infercalcR::method_spec("t_means_two_independent_welch")
  expect_identical(s, d)
  s2 <- .ir_spec("t_means_two_independent_welch", alternative = "less", conf.level = 0.9)
  expect_identical(s2$alternative, "less")
  expect_identical(s2$conf.level, 0.9)
  expect_error(.ir_spec("t_means_two_independent_welch", conf.level = 2),
               class = "infereportR_calculation_failed")
})

test_that(".ir_run returns valid infercalcR results", {
  r <- .ir_run(.ir_spec("t_means_two_independent_welch"),
               x = c(5.1, 4.8, 6.0, 5.5), y = c(4.2, 4.0, 4.9, 4.4), result_id = "test:welch")
  expect_s3_class(r, "inference_result")
  expect_silent(infercalcR::validate_inference(r))
  expect_identical(infercalcR::result_metadata(r)$result_id, "test:welch")
})

test_that(".ir_run wraps infercalcR errors with the parent attached", {
  e <- tryCatch(.ir_run(.ir_spec("t_means_two_independent_welch"),
                        x = numeric(), y = 1:3, result_id = "bad"),
                error = function(e) e)
  expect_s3_class(e, "infereportR_calculation_failed")
  expect_false(is.null(e$parent))
})

test_that(".ir_display returns infereportR display columns", {
  r <- .ir_run(.ir_spec("t_means_two_independent_welch"),
               x = c(5.1, 4.8, 6.0, 5.5), y = c(4.2, 4.0, 4.9, 4.4), result_id = "test:welch")
  d <- .ir_display(r)
  expect_s3_class(d, "data.frame")
  expect_true(all(c("p_selected", "p_selection", "result_id", "method_id", "variant_id",
                    "estimate_display", "interval_display", "statistic_display",
                    "p_display", "confidence_display") %in% names(d)))
  expect_identical(d$p_display, infereportR::report_table(r)$p_display)
  expect_error(.ir_display(r, p = "other"), class = "infereportR_invalid_input")
  expect_error(.ir_display(list()), class = "infereportR_calculation_failed")
})
