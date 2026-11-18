outcome_counts <- function() {
  groups <- c("ISS < 16", "ISS 16-24", "ISS \u2265 25")
  data.frame(
    group = factor(rep(groups, each = 3), levels = groups),
    outcome = factor(rep(c("No organ failure", "Single organ", "Multiple organs"), 3),
                     levels = c("No organ failure", "Single organ", "Multiple organs")),
    n = c(52, 9, 3, 47, 15, 9, 24, 17, 16))
}

stack_plot <- function(position = "fill", ...) {
  ggplot2::ggplot(outcome_counts(), ggplot2::aes(group, n, fill = outcome)) +
    ggplot2::geom_col(position = position, width = 0.62, colour = "black") +
    stat_stacklabel(position = position, ...) +
    scale_fill_hatch(patterns = c("open", "hatch", "solid")) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Injury Severity Score", y = "Patients (proportion)") +
    theme_figure("slide", legend = "none")
}

test_that("labels name the levels at the segment centres of the last bar", {
  p <- stack_plot()
  b <- ggplot2::ggplot_build(p)
  bars <- b$data[[1]]
  labs <- b$data[[2]]
  expect_identical(nrow(labs), 3L)
  expect_setequal(labs$label, levels(outcome_counts()$outcome))
  last <- bars[bars$x == 3, ]
  centre <- (last$ymin + last$ymax) / 2
  expect_equal(labs$y[order(labs$group)], centre[order(last$group)])
  expect_equal(unique(labs$x), 3 + 0.38)
  expect_identical(unique(labs$hjust), 0)
  expect_no_warning(draws_silently(p))
  expect_identical(check_figure(p)$status[check_figure(p)$check == "two_values"], "pass")
})

test_that("side and stack positions follow the bars", {
  p <- stack_plot(position = "stack", side = "left", offset = 0.5)
  b <- ggplot2::ggplot_build(p)
  bars <- b$data[[1]]
  labs <- b$data[[2]]
  first <- bars[bars$x == 1, ]
  expect_equal(labs$y[order(labs$group)],
               ((first$ymin + first$ymax) / 2)[order(first$group)])
  expect_equal(unique(labs$x), 0.5)
  expect_identical(unique(labs$hjust), 1)
  dark <- ggplot2::layer_data(stack_plot(mode = "dark"), 2)
  expect_identical(unique(dark$colour), .ip_mode("dark")$ink)
  expect_error(stat_stacklabel(side = "top"), class = "inferplotR_invalid_input")
  expect_error(stat_stacklabel(position = "dodge"), class = "inferplotR_invalid_input")
  expect_error(stat_stacklabel(offset = "a"), class = "inferplotR_invalid_input")
})
