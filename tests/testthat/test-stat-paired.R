lactate_long <- function(pl = gallery_lactate()) {
  data.frame(id = rep(pl$id, 2), value = c(pl$admission, pl$h24),
             time = factor(rep(c("Admission", "24 h"), each = nrow(pl)),
                           levels = c("Admission", "24 h")))
}

paired_plot <- function(d = lactate_long(), ...) {
  ggplot2::ggplot(d, ggplot2::aes(time, value, id = id)) +
    stat_paired(...) +
    ggplot2::labs(x = NULL, y = "Lactate (mmol/l)") +
    theme_figure("slide")
}

test_that("stat_paired reproduces the gallery oracle (figure 09), after minus before", {
  p <- paired_plot(method = "t_means_paired", unit = "mmol/l")
  d <- layer_data_of(p)
  res <- inference_results(p)
  expect_length(res, 1L)
  rd <- infercalcR::result_data(res[[1]])
  pl <- gallery_lactate()
  expect_equal(rd$estimate, mean(pl$h24 - pl$admission))
  expect_lt(rd$estimate, 0)
  shown <- infereportR::report_table(res[[1]])
  expect_identical(paste(shown$estimate_display, shown$interval_display, shown$p_display),
                   gallery_value("09", "paired"))
  label <- d$label[d$part == "label"]
  expect_identical(label, paste("Mean change", "-1.67 mmol/l", "[-2.25, -1.1]", "p < 0.001",
                                sep = "\n"))
  # The heavy line joins the level means; the label sits at the second mean.
  means <- d[d$part == "mean", ]
  expect_equal(means$y, c(mean(pl$admission), mean(pl$h24)))
  expect_equal(d$y[d$part == "label"], mean(pl$h24))
  expect_equal(sum(d$part == "unit"), 28L)
  direct <- infercalcR::run_inference(infercalcR::method_spec("t_means_paired"),
    data = data.frame(id = pl$id, x = pl$h24, y = pl$admission))
  want <- infercalcR::result_data(direct)
  expect_equal(rd[c("estimate", "conf.low", "conf.high", "p.value", "n")],
               want[c("estimate", "conf.low", "conf.high", "p.value", "n")])
})

test_that("stat_paired needs a method, two x levels and drops incomplete ids", {
  expect_error(stat_paired(), class = "inferplotR_invalid_input")
  expect_error(stat_paired(method = "t_means_two_independent_welch"),
               class = "inferplotR_unsupported")
  three <- lactate_long()
  three$time <- factor(rep(c("a", "b", "c"), length.out = nrow(three)))
  expect_error(ggplot2::ggplot_build(paired_plot(three, method = "t_means_paired")),
               class = "inferplotR_invalid_input")
  cont <- lactate_long()
  cont$time <- as.integer(cont$time)
  expect_error(ggplot2::ggplot_build(
    ggplot2::ggplot(cont, ggplot2::aes(time, value, id = id)) +
      stat_paired(method = "t_means_paired")), class = "inferplotR_invalid_input")
  d <- lactate_long()
  d$value[d$id == 3 & d$time == "24 h"] <- NA
  d <- d[!(d$id == 5 & d$time == "Admission"), ]
  d$value[d$id == 7 & d$time == "Admission"] <- Inf
  res <- inference_results(paired_plot(d, method = "t_means_paired"))
  expect_equal(infercalcR::result_data(res[[1]])$n, 11)
  dup <- rbind(lactate_long(), lactate_long()[1, ])
  expect_error(ggplot2::ggplot_build(paired_plot(dup, method = "t_means_paired")),
               class = "inferplotR_invalid_input")
})

test_that("stat_paired runs the signed-rank method and reports infercalcR failures", {
  # The gallery data has tied differences: the exact test refuses them.
  expect_error(ggplot2::ggplot_build(paired_plot(method = "signed_rank_wilcoxon")),
               class = "inferplotR_calculation_failed")
  p <- paired_plot(method = "signed_rank_wilcoxon", method_args = list(exact = FALSE))
  d <- layer_data_of(p)
  expect_true(startsWith(d$label[d$part == "label"], "Pseudomedian change\n"))
  expect_identical(figure_report(p)$method_id, "signed_rank_wilcoxon")
  expect_error(stat_paired(method = "t_means_paired", method_args = list(x = 1)),
               class = "inferplotR_invalid_input")
})

test_that("stat_paired draws one grob tree, silently, in two values", {
  p <- paired_plot(method = "t_means_paired", unit = "mmol/l")
  expect_no_warning(g <- draws_silently(p))
  expect_s3_class(g, "gtable")
  b <- ggplot2::ggplot_build(p)
  grob <- b$plot$layers[[1]]$geom$draw_panel(b$data[[1]], b$layout$panel_params[[1]],
                                             b$plot$coordinates)
  expect_s3_class(grob, "gTree")
  expect_setequal(names(grob$children),
                  c("lines", "points", "mean_line", "mean_points", "label"))
  expect_identical(check_figure(p)$status[check_figure(p)$check == "two_values"], "pass")
  none <- layer_data_of(paired_plot(method = "t_means_paired", label = "none"))
  expect_identical(none$label[none$part == "label"], "")
  dark <- layer_data_of(paired_plot(method = "t_means_paired", mode = "dark"))
  expect_identical(unique(dark$colour), .ip_mode("dark")$ink)
})
