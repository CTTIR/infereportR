base_plot <- function() ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point()

test_that("stat_compare equals the manual infercalcR family", {
  p <- base_plot() + stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  d <- layer_data_of(p, 2L)
  fam <- manual_family()
  got <- infercalcR::result_data(d$.ir_result[[1]])
  want <- infercalcR::result_data(fam)
  expect_equal(got$p.adjusted, want$p.adjusted)
  expect_equal(got$estimate, want$estimate)
  expect_identical(got$row_id, c("trt1-ctrl", "trt2-ctrl"))
  expect_identical(got$variable_id, rep("weight", 2))
  expect_identical(d$label[!d$headroom],
                   report_table(fam, p = "adjusted")$p_display)
  expect_no_warning(draws_silently(p))
})

test_that("pairs are oriented as B minus A", {
  p <- base_plot() + stat_compare(method = welch, comparisons = list(c("ctrl", "trt1")))
  r <- infercalcR::result_data(layer_data_of(p, 2L)$.ir_result[[1]])
  expect_identical(r$contrast, "trt1 minus ctrl")
  expect_equal(r$estimate, mean(pg_values("trt1")) - mean(pg_values("ctrl")))
  expect_true(is.na(r$p.adjusted))
})

test_that("the procedure and the adjustment must be explicit", {
  bad <- "infereportR_invalid_input"
  expect_error(stat_compare(adjust = "none"), class = bad)
  expect_error(stat_compare(method = "t_test", adjust = "none"),
               class = "infereportR_unsupported")
  expect_error(stat_compare(method = welch,
                            comparisons = list(c("ctrl", "trt1"), c("ctrl", "trt2"))),
               class = bad)
  expect_error(ggplot2::ggplot_build(base_plot() + stat_compare(method = welch)), class = bad)
  expect_error(ggplot2::ggplot_build(base_plot() + stat_compare(method = welch,
                                                                reference = "ctrl")),
               class = bad)
  d <- layer_data_of(base_plot() + stat_compare(method = welch, adjust = "none"), 2L)
  expect_identical(sum(!d$headroom), 3L)
  r <- infercalcR::result_data(d$.ir_result[[1]])
  expect_identical(r$row_id, c("trt1-ctrl", "trt2-ctrl", "trt2-trt1"))
  expect_true(all(is.na(r$p.adjusted)))
})

test_that("brackets stack at first + (i - 1) * step with one headroom row", {
  skip_on_cran()
  p <- base_plot() + stat_compare(method = welch, adjust = "bonferroni")
  d <- layer_data_of(p, 2L)
  top <- max(pg$weight)
  rng <- diff(range(pg$weight))
  bars <- d[!d$headroom, ]
  expect_identical(nrow(bars), 3L)
  expect_equal(bars$y, top + (.ir_bracket_first + (0:2) * .ir_bracket_step) * rng)
  expect_equal(bars$yend, bars$y)
  expect_equal(bars$tip, rep(0.02 * rng, 3))
  expect_identical(as.numeric(bars$x), c(1, 1, 2))
  expect_identical(as.numeric(bars$xend), c(2, 3, 3))
  room <- d[d$headroom, ]
  expect_identical(nrow(room), 1L)
  expect_equal(room$y, top + (.ir_bracket_first + 3 * .ir_bracket_step) * rng)
  yr <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range
  expect_true(room$y <= yr[2])
  expect_false("ymax" %in% names(d))
  expect_no_warning(draws_silently(p))
})

test_that("first and step defaults are the tokens and stay adjustable", {
  f <- formals(stat_compare)
  expect_identical(c(f$first, f$step), c(.ir_bracket_first, .ir_bracket_step))
  d <- layer_data_of(base_plot() + stat_compare(method = welch, reference = "ctrl",
                                                adjust = "holm", first = 0.05, step = 0.1), 2L)
  top <- max(pg$weight)
  rng <- diff(range(pg$weight))
  expect_equal(d$y, top + c(0.05, 0.15, 0.25) * rng)
  expect_identical(d$headroom, c(FALSE, FALSE, TRUE))
  expect_error(stat_compare(method = welch, adjust = "none", first = -0.1),
               class = "infereportR_invalid_input")
})

test_that("bracket labels default to the in-plot label size", {
  skip_on_cran()
  d <- layer_data_of(base_plot() + stat_compare(method = welch, reference = "ctrl",
                                                adjust = "holm"), 2L)
  expect_equal(unique(d$size) * ggplot2::.pt, .ir_label_size_pt)
  d2 <- layer_data_of(base_plot() + stat_compare(method = welch, reference = "ctrl",
                                                 adjust = "holm", text_size = 3), 2L)
  expect_equal(unique(d2$size), 3)
})

test_that("each panel gets its own headroom row", {
  skip_on_cran()
  p <- ggplot2::ggplot(pg_facets, ggplot2::aes(group, weight)) + ggplot2::facet_wrap(~panel) +
    stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  d <- layer_data_of(p)
  expect_identical(as.integer(table(d$PANEL[d$headroom])), c(1L, 1L))
  expect_no_warning(draws_silently(p))
})

test_that("label types use infereportR strings and never 'ns'", {
  skip_on_cran()
  fam <- manual_family()
  shown <- report_table(fam, p = "adjusted")
  lab <- function(type) {
    d <- layer_data_of(base_plot() + stat_compare(method = welch, reference = "ctrl",
                                                  adjust = "holm", label = type), 2L)
    d$label[!d$headroom]
  }
  expect_identical(lab("estimate_ci"), paste(shown$estimate_display, shown$interval_display))
  expect_identical(lab("estimate_ci_p"), paste0(shown$estimate_display, " ",
                                                shown$interval_display, ", ",
                                                shown$p_display))
  expect_identical(lab("stars"), shown$p_display)
  expect_identical(.ir_compare_labels(data.frame(p_selected = c(0.0001, 0.005, 0.02, 0.3, NA),
                                                 p_display = c("a", "b", "c", "p = 0.3", "u")),
                                      "stars"),
                   c("***", "**", "*", "p = 0.3", "u"))
})

test_that("facets give one family per panel", {
  p <- ggplot2::ggplot(pg_facets, ggplot2::aes(group, weight)) + ggplot2::facet_wrap(~panel) +
    stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  d <- layer_data_of(p)
  expect_identical(unique(d$.ir_result_id),
                   c("infereportR:compare:t_means_two_independent_welch:panel1",
                     "infereportR:compare:t_means_two_independent_welch:panel2"))
})

test_that("method_args pass through and reserved names are rejected", {
  p <- base_plot() + stat_compare(method = "rank_sum_mann_whitney", reference = "ctrl",
                                  adjust = "holm", method_args = list(exact = FALSE))
  expect_s3_class(layer_data_of(p, 2L)$.ir_result[[1]], "inference_result")
  expect_error(stat_compare(method = welch, adjust = "none", method_args = list(x = 1)),
               class = "infereportR_invalid_input")
  expect_error(stat_compare(method = welch, adjust = "none", method_args = list(1)),
               class = "infereportR_invalid_input")
  expect_error(stat_compare(method = welch, adjust = "none", method_args = 1),
               class = "infereportR_invalid_input")
})

test_that("invalid comparison arguments are rejected", {
  bad <- "infereportR_invalid_input"
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
               class = "infereportR_unsupported")
})

test_that("infercalcR failures surface as calculation_failed with a parent", {
  # One observation is too few for the Welch test; infercalcR stops.
  d <- rbind(pg, data.frame(weight = 5, group = "single"))
  p <- ggplot2::ggplot(d, ggplot2::aes(group, weight)) +
    stat_compare(method = welch, comparisons = list(c("ctrl", "single")))
  e <- tryCatch(ggplot2::ggplot_build(p), error = function(e) e)
  # ggplot2 reports layer errors with the original condition as parent.
  while (!inherits(e, "infereportR_calculation_failed") && !is.null(e$parent)) e <- e$parent
  expect_s3_class(e, "infereportR_calculation_failed")
  expect_false(is.null(e$parent))
})

test_that("precomputed results draw with stat_inference", {
  skip_on_cran()
  fam <- manual_family()
  pos <- data.frame(row_id = c("trt1-ctrl", "trt2-ctrl"), group1 = c("ctrl", "ctrl"),
                    group2 = c("trt1", "trt2"), y_position = c(6.6, 6.9))
  p <- base_plot() +
    stat_inference(fam, positions = pos, groups = levels(pg$group))
  expect_no_warning(draws_silently(p))
})

test_that("headroom sets the invisible row in steps above the top bracket", {
  rng <- diff(range(pg$weight))
  top <- max(pg$weight)
  d <- layer_data_of(base_plot() + stat_compare(method = welch, adjust = "holm",
                                                headroom = 2), 2L)
  expect_equal(d$y[d$headroom], top + (.ir_bracket_first + 4 * .ir_bracket_step) * rng)
  expect_identical(sum(!d$headroom), 3L)
  expect_error(stat_compare(method = welch, adjust = "holm", headroom = 0.5),
               class = "infereportR_invalid_input")
  expect_error(stat_compare(method = welch, adjust = "holm", headroom = NA),
               class = "infereportR_invalid_input")
})

test_that("a scale transformation changes the analyzed values, a coordinate transformation does not", {
  plot <- base_plot() + stat_compare(method = welch, reference = "ctrl", adjust = "holm")
  raw <- figure_table(plot)
  spec <- infercalcR::method_spec(welch)
  on_log <- infercalcR::result_data(infercalcR::run_inference(
    spec, x = log10(pg_values("trt1")), y = log10(pg_values("ctrl")), result_id = "log"))
  logged <- figure_table(plot + ggplot2::scale_y_log10())
  expect_equal(logged$estimate[[1]], on_log$estimate[[1]])
  expect_equal(logged$p.value[[1]], on_log$p.value[[1]])
  expect_false(isTRUE(all.equal(logged$p.value, raw$p.value)))
  ns <- asNamespace("ggplot2")
  coord <- if (exists("coord_transform", envir = ns, inherits = FALSE)) {
    get("coord_transform", envir = ns)(y = "log10")
  } else {
    ggplot2::coord_trans(y = "log10")
  }
  kept <- figure_table(plot + coord)
  expect_equal(kept$estimate, raw$estimate)
  expect_equal(kept$p.value, raw$p.value)
})

test_that("a level missing from one facet skips its pairs in that panel only", {
  d <- data.frame(
    f = rep(c("P", "Q"), each = 9),
    g = c(rep(c("a", "b", "c"), each = 3), rep(c("a", "b"), c(5, 4))),
    y = c(1, 2, 4, 3, 5, 6, 6, 8, 9, 2, 3, 5, 4, 6, 5, 7, 8, 9)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(g, y)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~f) +
    stat_compare(method = welch, reference = "a", adjust = "holm")
  res <- inference_results(p)
  expect_length(res, 2L)
  rows <- lapply(res, function(r) infercalcR::result_data(r)$row_id)
  expect_identical(unname(rows), list(c("b-a", "c-a"), "b-a"))
  q <- ggplot2::ggplot(d, ggplot2::aes(g, y)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~f) +
    stat_compare(method = welch, comparisons = list(c("a", "c")))
  d2 <- layer_data_of(q, 2L)
  expect_identical(sort(unique(as.character(d2$PANEL))), "1")
  expect_no_warning(draws_silently(p))
})
