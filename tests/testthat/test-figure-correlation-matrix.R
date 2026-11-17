markers_frame <- function() as.data.frame(gallery_markers())

test_that("figure_correlation_matrix reproduces the Spearman oracle (figure 08)", {
  mk <- markers_frame()
  p <- figure_correlation_matrix(mk, method = "correlation_spearman")
  res <- inference_results(p)
  expect_length(res, 10L)
  vars <- colnames(mk)
  pairs <- utils::combn(5, 2)
  b <- ggplot2::ggplot_build(p)
  values <- b$data[[2]]
  for (k in seq_len(ncol(pairs))) {
    key <- paste0("rho:", vars[pairs[1, k]], "-", vars[pairs[2, k]])
    want <- strsplit(gallery_value("08", key), " display=", fixed = TRUE)[[1]]
    rd <- infercalcR::result_data(res[[k]])
    expect_equal(round(rd$estimate, 6), as.numeric(want[1]))
    expect_identical(values$label[k], want[2])
  }
  # Circles: filled for positive, open for negative; area follows |rho|.
  circles <- b$data[[1]]
  est <- unname(vapply(res, function(r) infercalcR::result_data(r)$estimate, numeric(1)))
  expect_identical(circles$shape, ifelse(est > 0, 16, 1))
  expect_identical(sum(circles$shape == 1), 1L)
  expect_true(all(diff(circles$size[order(abs(est))]) >= 0))
  # Circles above the diagonal, values mirrored below it.
  expect_true(all(circles$x > 6 - circles$y))
  expect_true(all(values$x < 6 - values$y))
  expect_identical(b$data[[3]]$label, vars)
  expect_no_warning(draws_silently(p))
  expect_identical(check_figure(p)$status[check_figure(p)$check == "two_values"], "pass")
})

test_that("a pair without a result draws a cross and keeps its status", {
  d <- data.frame(a = rnorm(30), b = rnorm(30), k = rep(1, 30))
  p <- figure_correlation_matrix(d, method = "correlation_pearson", labels = c("A", "B", "K"))
  b <- ggplot2::ggplot_build(p)
  crosses <- b$data[[2]]
  expect_identical(nrow(crosses), 2L)
  expect_true(all(crosses$label %in% c("\u00d7", "x")))
  rep <- figure_report(p)
  expect_identical(nrow(rep), 3L)
  expect_identical(sum(rep$status != "ok"), 2L)
  expect_identical(b$data[[4]]$label, c("A", "B", "K"))
  expect_no_warning(draws_silently(p))
  for (mode in c("light", "dark")) for (tr in c(FALSE, TRUE)) {
    pm <- figure_correlation_matrix(d[1:2], method = "correlation_pearson", mode = mode,
                                    transparent = tr)
    expect_identical(check_figure(pm)$status[check_figure(pm)$check == "two_values"], "pass")
  }
})

test_that("figure_correlation_matrix checks its input", {
  mk <- markers_frame()
  expect_error(figure_correlation_matrix(mk), class = "inferplotR_invalid_input")
  expect_error(figure_correlation_matrix(mk, method = "t_means_paired"),
               class = "inferplotR_unsupported")
  expect_error(figure_correlation_matrix(mk, vars = "C3a", method = "correlation_kendall"),
               class = "inferplotR_invalid_input")
  expect_error(figure_correlation_matrix(mk, vars = c("C3a", "nope"),
                                         method = "correlation_kendall"),
               class = "inferplotR_invalid_input")
  expect_error(figure_correlation_matrix(as.matrix(mk), method = "correlation_kendall"),
               class = "inferplotR_invalid_input")
  expect_error(figure_correlation_matrix(mk, method = "correlation_kendall", labels = "a"),
               class = "inferplotR_invalid_input")
  p <- figure_correlation_matrix(mk, vars = c("C3a", "C5a"), method = "correlation_kendall")
  expect_length(inference_results(p), 1L)
})
