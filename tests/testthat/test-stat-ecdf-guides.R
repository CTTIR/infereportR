ecdf_plot <- function(...) {
  cohort <- gallery_cohort()
  ttp <- data.frame(severity = cohort$severity, peak = cohort$peak)
  ggplot2::ggplot(ttp, ggplot2::aes(peak, linetype = severity)) +
    ggplot2::stat_ecdf(geom = "step", pad = FALSE) +
    stat_ecdf_guides(...) +
    scale_linetype_figure() +
    ggplot2::labs(x = "Hours to C5a peak (h)", y = "Cumulative proportion") +
    theme_figure("slide", legend = "none")
}

test_that("drop lines sit at the type-1 medians of the gallery (figure 05)", {
  p <- ecdf_plot(curve_labels = c(0.85, 0.35))
  d <- layer_data_of(p, 2L)
  drop <- d[d$part == "drop", ]
  cohort <- gallery_cohort()
  want <- vapply(levels(cohort$severity), function(g) {
    stats::quantile(cohort$peak[cohort$severity == g], 0.5, type = 1, names = FALSE)
  }, numeric(1))
  expect_equal(drop$x, unname(want))
  oracle <- strsplit(strsplit(gallery_value("05", "median_peak_h"), "; ")[[1]], "=")
  expect_equal(drop$x, as.numeric(vapply(oracle, `[`, "", 2)), tolerance = 1e-4)
  expect_equal(drop$y, c(0.5, 0.5))
  guide <- d[d$part == "guide", ]
  expect_identical(guide$label, "median")
  expect_equal(guide$y, 0.5)
  curve <- d[d$part == "curve", ]
  expect_identical(curve$label, levels(cohort$severity))
  expect_identical(curve$hjust, c(1, 0))
  expect_equal(curve$y, c(0.85, 0.35))
  expect_no_warning(draws_silently(p))
  expect_identical(check_figure(p)$status[check_figure(p)$check == "two_values"], "pass")
})

test_that("stat_ecdf_guides handles several probs and checks its input", {
  d <- layer_data_of(ecdf_plot(probs = c(0.25, 0.75), guide_label = ""), 2L)
  expect_identical(sum(d$part == "drop"), 4L)
  expect_identical(sum(d$part == "curve"), 0L)
  expect_error(stat_ecdf_guides(probs = 1), class = "inferplotR_invalid_input")
  expect_error(stat_ecdf_guides(curve_labels = "a"), class = "inferplotR_invalid_input")
  expect_error(stat_ecdf_guides(mode = "sepia"), class = "inferplotR_invalid_input")
  expect_error(ggplot2::ggplot_build(ecdf_plot(curve_labels = 0.5)),
               class = "inferplotR_invalid_input")
  dark <- layer_data_of(ecdf_plot(mode = "dark"), 2L)
  expect_identical(unique(dark$colour), .ip_mode("dark")$ink)
})
