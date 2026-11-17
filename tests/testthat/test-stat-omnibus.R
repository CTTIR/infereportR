test_that("stat_omnibus labels a direct anova_one_way run", {
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point() +
    stat_omnibus(method = "anova_one_way")
  d <- layer_data_of(p, 2L)
  expect_identical(nrow(d), 1L)
  direct <- infercalcR::run_inference(infercalcR::method_spec("anova_one_way"),
                                      x = pg$weight, group = pg$group)
  shown <- infereportR::report_table(direct)
  expect_true(grepl(shown$statistic_display, d$label, fixed = TRUE))
  expect_true(grepl(shown$p_display, d$label, fixed = TRUE))
  expect_equal(infercalcR::result_data(d$.ip_result[[1]])$statistic,
               infercalcR::result_data(direct)$statistic)
  expect_identical(d$.ip_result_id, "inferplotR:omnibus:anova_one_way:panel1")
  expect_identical(d$colour, .ip_ink)
  expect_no_warning(draws_silently(p))
})

test_that("other omnibus methods and arguments work", {
  for (m in c("anova_one_way_welch", "kruskal_wallis")) {
    d <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
                         ggplot2::geom_point() + stat_omnibus(method = m), 2L)
    expect_identical(infercalcR::result_metadata(d$.ip_result[[1]])$method_id, m)
  }
  bad <- "inferplotR_invalid_input"
  expect_error(stat_omnibus(), class = bad)
  expect_error(stat_omnibus(method = "t_test"), class = "inferplotR_unsupported")
  expect_error(stat_omnibus(method = "anova_one_way", label_x = NA), class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", hjust = "a"), class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", method_args = list(group = 1)),
               class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", text_size = -1), class = bad)
  expect_error(stat_omnibus(method = "anova_one_way", family = 2), class = bad)
})
