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
