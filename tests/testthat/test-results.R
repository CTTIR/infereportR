test_that("S3 result objects survive ggplot_build", {
  r <- infercalcR::run_inference(infercalcR::method_spec("t_mean_one_sample"),
                                 x = pg_values("ctrl"), mu0 = 0, result_id = "probe")
  StatProbe <- ggplot2::ggproto("StatProbe", ggplot2::Stat, required_aes = c("x", "y"),
    compute_panel = function(data, scales) {
      out <- data.frame(x = 1, y = 5, label = "probe", group = 1L, PANEL = data$PANEL[1])
      out$.ip_result <- list(r)
      out$.ip_result_id <- "probe"
      out
    })
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
    ggplot2::layer(stat = StatProbe, geom = "text", position = "identity")
  d <- layer_data_of(p)
  expect_identical(d$.ip_result[[1]], r)
  expect_no_warning(draws_silently(p))
})

test_that("inference_results collects results in layer then panel order", {
  p <- ggplot2::ggplot(pg_facets, ggplot2::aes(group, weight)) +
    ggplot2::facet_wrap(~panel) +
    stat_omnibus(method = "anova_one_way") +
    stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  res <- inference_results(p)
  expect_named(res, c("inferplotR:omnibus:anova_one_way:panel1",
                      "inferplotR:omnibus:anova_one_way:panel2",
                      "inferplotR:compare:t_means_two_independent_welch:panel1",
                      "inferplotR:compare:t_means_two_independent_welch:panel2"))
  for (r in res) {
    expect_silent(infercalcR::validate_inference(r))
    expect_type(infereportR::report_text(r), "character")
  }
  first <- pg_facets[pg_facets$panel == "first", ]
  direct <- infercalcR::run_inference(infercalcR::method_spec("anova_one_way"),
                                      x = first$weight, group = first$group)
  expect_equal(infercalcR::result_data(res[[1]])$p.value,
               infercalcR::result_data(direct)$p.value)
})

test_that("identical layers give one result", {
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point() +
    stat_omnibus(method = "anova_one_way") + stat_omnibus(method = "anova_one_way")
  expect_length(inference_results(p), 1L)
})

test_that("plots without inference layers give empty, type-stable results", {
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point()
  expect_identical(inference_results(p), list())
  fr <- figure_report(p)
  expect_s3_class(fr, "data.frame")
  expect_identical(nrow(fr), 0L)
  r <- infercalcR::run_inference(infercalcR::method_spec(welch), x = 1:4, y = 2:6)
  expect_identical(names(fr), c("figure_layer", "figure_panel",
                                names(infereportR::report_table(r))))
  expect_type(fr$figure_layer, "integer")
  expect_type(fr$figure_panel, "character")
})

test_that("figure_report binds report_table rows", {
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
    ggplot2::geom_point() +
    stat_means() +
    stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  fr <- figure_report(p)
  expect_identical(nrow(fr), 3L + 2L)
  expect_identical(fr$figure_layer, c(2L, 2L, 2L, 3L, 3L))
  fam <- fr[fr$figure_layer == 3L, ]
  expect_identical(fam$p_display,
                   infereportR::report_table(manual_family(), p = "adjusted")$p_display)
  raw <- figure_report(p, p = "raw")
  expect_identical(raw$p_selection[raw$figure_layer == 3L], rep("raw", 2))
  expect_error(figure_report(p, p = "x"), class = "inferplotR_invalid_input")
  expect_error(figure_report(1), class = "inferplotR_invalid_input")
  expect_error(figure_report(p, p = "adjusted"), class = "inferplotR_calculation_failed")
})
