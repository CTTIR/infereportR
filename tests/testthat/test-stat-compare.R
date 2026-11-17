base_plot <- function() ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point()

test_that("stat_compare equals the manual infercalcR family", {
  p <- base_plot() + stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  d <- layer_data_of(p, 2L)
  fam <- manual_family()
  got <- infercalcR::result_data(d$.ip_result[[1]])
  want <- infercalcR::result_data(fam)
  expect_equal(got$p.adjusted, want$p.adjusted)
  expect_equal(got$estimate, want$estimate)
  expect_identical(got$row_id, c("trt1-ctrl", "trt2-ctrl"))
  expect_identical(got$variable_id, rep("weight", 2))
  expect_identical(d$label, infereportR::report_table(fam, p = "adjusted")$p_display)
  expect_no_warning(draws_silently(p))
})

test_that("pairs are oriented as B minus A", {
  p <- base_plot() + stat_compare(method = welch, comparisons = list(c("ctrl", "trt1")))
  r <- infercalcR::result_data(layer_data_of(p, 2L)$.ip_result[[1]])
  expect_identical(r$contrast, "trt1 minus ctrl")
  expect_equal(r$estimate, mean(pg_values("trt1")) - mean(pg_values("ctrl")))
  expect_true(is.na(r$p.adjusted))
})

test_that("the procedure and the adjustment must be explicit", {
  bad <- "inferplotR_invalid_input"
  expect_error(stat_compare(adjust = "none"), class = bad)
  expect_error(stat_compare(method = "t_test", adjust = "none"),
               class = "inferplotR_unsupported")
  expect_error(stat_compare(method = welch,
                            comparisons = list(c("ctrl", "trt1"), c("ctrl", "trt2"))),
               class = bad)
  expect_error(ggplot2::ggplot_build(base_plot() + stat_compare(method = welch)), class = bad)
  expect_error(ggplot2::ggplot_build(base_plot() + stat_compare(method = welch,
                                                                reference = "ctrl")),
               class = bad)
  d <- layer_data_of(base_plot() + stat_compare(method = welch, adjust = "none"), 2L)
  expect_identical(nrow(d), 3L)
  r <- infercalcR::result_data(d$.ip_result[[1]])
  expect_identical(r$row_id, c("trt1-ctrl", "trt2-ctrl", "trt2-trt1"))
  expect_true(all(is.na(r$p.adjusted)))
})

test_that("brackets stack above the data inside the y range", {
  d <- layer_data_of(base_plot() + stat_compare(method = welch, adjust = "bonferroni"), 2L)
  top <- max(pg$weight)
  rng <- diff(range(pg$weight))
  expect_equal(d$y, top + seq_len(3) * 0.09 * rng)
  expect_equal(d$yend, d$y)
  expect_equal(d$tip, rep(0.02 * rng, 3))
  expect_identical(as.numeric(d$x), c(1, 1, 2))
  expect_identical(as.numeric(d$xend), c(2, 3, 3))
  yr <- ggplot2::ggplot_build(base_plot() + stat_compare(method = welch, adjust = "holm"))$
    layout$panel_params[[1]]$y.range
  expect_true(max(d$y) < yr[2])
})

test_that("label types use infereportR strings and never 'ns'", {
  fam <- manual_family()
  shown <- infereportR::report_table(fam, p = "adjusted")
  lab <- function(type) {
    layer_data_of(base_plot() + stat_compare(method = welch, reference = "ctrl",
                                             adjust = "holm", label = type), 2L)$label
  }
  expect_identical(lab("estimate_ci"), paste(shown$estimate_display, shown$interval_display))
  expect_identical(lab("estimate_ci_p"), paste0(shown$estimate_display, " ",
                                                shown$interval_display, ", ",
                                                shown$p_display))
  expect_identical(lab("stars"), shown$p_display)
  expect_identical(.ip_compare_labels(data.frame(p_selected = c(0.0001, 0.005, 0.02, 0.3, NA),
                                                 p_display = c("a", "b", "c", "p = 0.3", "u")),
                                      "stars"),
                   c("***", "**", "*", "p = 0.3", "u"))
})

test_that("facets give one family per panel", {
  p <- ggplot2::ggplot(pg_facets, ggplot2::aes(group, weight)) + ggplot2::facet_wrap(~panel) +
    stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  d <- layer_data_of(p)
  expect_identical(unique(d$.ip_result_id),
                   c("inferplotR:compare:t_means_two_independent_welch:panel1",
                     "inferplotR:compare:t_means_two_independent_welch:panel2"))
})

test_that("method_args pass through and reserved names are rejected", {
  p <- base_plot() + stat_compare(method = "rank_sum_mann_whitney", reference = "ctrl",
                                  adjust = "holm", method_args = list(exact = FALSE))
  expect_s3_class(layer_data_of(p, 2L)$.ip_result[[1]], "inference_result")
  expect_error(stat_compare(method = welch, adjust = "none", method_args = list(x = 1)),
               class = "inferplotR_invalid_input")
  expect_error(stat_compare(method = welch, adjust = "none", method_args = list(1)),
               class = "inferplotR_invalid_input")
  expect_error(stat_compare(method = welch, adjust = "none", method_args = 1),
               class = "inferplotR_invalid_input")
})

test_that("invalid comparison arguments are rejected", {
  bad <- "inferplotR_invalid_input"
  expect_error(stat_compare(method = welch, comparisons = list(c("a", "b")), reference = "a",
                            adjust = "none"), class = bad)
  expect_error(stat_compare(method = welch, comparisons = list("a"), adjust = "none"),
               class = bad)
  expect_error(stat_compare(method = welch, adjust = "sometimes"), class = bad)
  expect_error(stat_compare(method = welch, adjust = "none", label = "x"), class = bad)
  expect_error(stat_compare(method = welch, adjust = "none", step = -1), class = bad)
  expect_error(stat_compare(method = welch, adjust = "none", alternative = "up"), class = bad)
  expect_error(ggplot2::ggplot_build(base_plot() + stat_compare(
    method = welch, comparisons = list(c("ctrl", "zzz")))), class = bad)
  expect_error(ggplot2::ggplot_build(base_plot() + stat_compare(
    method = welch, reference = "zzz", adjust = "none")), class = bad)
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) +
                                       stat_compare(method = welch, adjust = "none")),
               class = "inferplotR_unsupported")
})

test_that("infercalcR failures surface as calculation_failed with a parent", {
  d <- pg
  d$group <- factor(d$group, levels = c(levels(pg$group), "empty"))
  p <- ggplot2::ggplot(d, ggplot2::aes(group, weight)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    stat_compare(method = welch, comparisons = list(c("ctrl", "empty")))
  e <- tryCatch(ggplot2::ggplot_build(p), error = function(e) e)
  # ggplot2 reports layer errors with the original condition as parent.
  while (!inherits(e, "inferplotR_calculation_failed") && !is.null(e$parent)) e <- e$parent
  expect_s3_class(e, "inferplotR_calculation_failed")
  expect_false(is.null(e$parent))
})

test_that("precomputed results draw with infereportR::stat_inference", {
  fam <- manual_family()
  pos <- data.frame(row_id = c("trt1-ctrl", "trt2-ctrl"), group1 = c("ctrl", "ctrl"),
                    group2 = c("trt1", "trt2"), y_position = c(6.6, 6.9))
  p <- base_plot() +
    infereportR::stat_inference(fam, positions = pos, groups = levels(pg$group))
  expect_no_warning(draws_silently(p))
})
