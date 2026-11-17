pg_plot <- function() ggplot2::ggplot(pg, ggplot2::aes(group, weight))

stat_classes <- function(x) {
  layers <- Filter(function(e) inherits(e, "LayerInstance"), x)
  vapply(layers, function(l) class(l$stat)[1L], character(1))
}

test_that("the full bundle builds, draws and carries every result", {
  skip_if_not_installed("ragg")
  p <- pg_plot() + inferplot(compare = welch, reference = "ctrl", adjust = "holm",
                             omnibus = "anova_one_way")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_no_warning(draws_silently(p))
  res <- inference_results(p)
  ids <- names(res)
  expect_identical(sum(startsWith(ids, "inferplotR:omnibus:anova_one_way:")), 1L)
  expect_identical(sum(startsWith(ids, paste0("inferplotR:compare:", welch, ":"))), 1L)
  expect_identical(sum(startsWith(ids, "inferplotR:means:t_mean_one_sample:")), 3L)
  expect_length(res, 5L)
  fam <- res[[paste0("inferplotR:compare:", welch, ":panel1")]]
  expect_identical(nrow(infercalcR::result_data(fam)), 2L)
})

test_that("inferplot returns a plain list that ggplot2 adds element by element", {
  x <- inferplot()
  expect_identical(class(x), "list")
  p <- pg_plot() + x
  expect_true(inherits(p, "ggplot"))
  expect_length(p$layers, 2L)
  expect_true(isTRUE(attr(p$theme, "complete")))
  aes_scaled <- unlist(lapply(p$scales$scales, function(s) s$aesthetics))
  expect_true(all(c("colour", "linetype", "shape", "fill") %in% aes_scaled))
})

test_that("defaults add theme, scales, points and means but never a test", {
  x <- inferplot()
  expect_length(x, 7L)
  expect_true(inherits(x[[1]], "theme"))
  expect_true(all(vapply(x[2:5], function(s) inherits(s, "Scale"), logical(1))))
  expect_identical(unname(stat_classes(x)), c("StatIdentity", "StatMeans"))
  expect_true(inherits(x[[6]]$geom, "GeomPoint"))
  res <- inference_results(pg_plot() + x)
  expect_length(res, 3L)
  expect_true(all(startsWith(names(res), "inferplotR:means:")))

  expect_identical(unname(stat_classes(inferplot(means = FALSE, points = "none"))),
                   character())
  plain <- inferplot(points = "plain")
  expect_true(inherits(plain[[6]]$position, "PositionIdentity"))
  expect_true(inherits(x[[6]]$position, "PositionJitter"))
  expect_identical(unname(stat_classes(
    inferplot(means = FALSE, correlation = "correlation_pearson", points = "plain"))),
    c("StatIdentity", "StatCorrelation"))
})

test_that("the adjustment rule of stat_compare applies", {
  bad <- "inferplotR_invalid_input"
  pairs <- list(c("ctrl", "trt1"), c("ctrl", "trt2"), c("trt1", "trt2"))
  expect_error(inferplot(compare = welch, comparisons = pairs), class = bad)
  expect_error(ggplot2::ggplot_build(pg_plot() + inferplot(compare = welch)), class = bad)
  expect_error(ggplot2::ggplot_build(pg_plot() + inferplot(compare = welch,
                                                           reference = "ctrl")),
               class = bad)
  ok <- pg_plot() + inferplot(compare = welch, comparisons = pairs, adjust = "none")
  expect_length(inference_results(ok), 4L)
})

test_that("invalid arguments are rejected before any layer is built", {
  bad <- "inferplotR_invalid_input"
  expect_error(inferplot(target = "poster"), class = bad)
  expect_error(inferplot(points = "dots"), class = bad)
  expect_error(inferplot(means = NA), class = bad)
  expect_error(inferplot(conf.level = 1.5), class = bad)
  expect_error(inferplot(conf.level = NULL), class = bad)
  expect_error(inferplot(legend = "top"), class = bad)
  expect_error(inferplot(label = "q"), class = bad)
  expect_error(inferplot(base_size = "9"), class = bad)
  expect_error(inferplot(base_size = -1), class = bad)
  expect_error(inferplot(compare = NA_character_), class = bad)
  expect_error(inferplot(colour = "red"), class = bad)
  expect_error(inferplot(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), class = bad)
  expect_error(inferplot(adjust = "holm"), class = bad)
  expect_error(inferplot(reference = "ctrl"), class = bad)
  expect_error(inferplot(compare = "t_test"), class = "inferplotR_unsupported")
  expect_error(inferplot(omnibus = "chisq"), class = "inferplotR_unsupported")
  expect_error(inferplot(correlation = "anova_one_way"), class = "inferplotR_unsupported")
  expect_warning(inferplot(base_size = 7), class = "inferplotR_apa_text_size")
})

test_that("the bundle equals the components added one by one", {
  bundle <- pg_plot() + inferplot(compare = welch, reference = "ctrl", adjust = "holm",
                                  omnibus = "anova_one_way")
  by_hand <- pg_plot() +
    theme_figure("paper", base_size = 9, legend = "inside") +
    scale_colour_ink() + scale_linetype_figure() + scale_shape_figure() +
    scale_fill_hatch() +
    ggplot2::geom_point(shape = 1, colour = .ip_ink, size = 1.2,
                        position = ggplot2::position_jitter(width = 0.08, height = 0,
                                                            seed = 1)) +
    stat_means(conf.level = 0.95, offset = .ip_means_offset) +
    stat_omnibus(method = "anova_one_way") +
    stat_compare(method = welch, reference = "ctrl", adjust = "holm", label = "p")
  expect_equal(ggplot2::ggplot_build(bundle)$data, ggplot2::ggplot_build(by_hand)$data)
})

test_that("jittered points are reproducible", {
  a <- layer_data_of(pg_plot() + inferplot(means = FALSE))
  b <- layer_data_of(pg_plot() + inferplot(means = FALSE))
  expect_identical(a$x, b$x)
  expect_false(isTRUE(all.equal(a$x, round(a$x))))
})

test_that("means sit beside jittered points and on plain ones", {
  jit <- ggplot2::ggplot_build(pg_plot() + inferplot())
  pts <- jit$data[[1]]
  mns <- jit$data[[2]]
  expect_equal(as.numeric(mns$x), 1:3 + .ip_means_offset)
  # No raw point reaches the column of its mean.
  for (g in 1:3) {
    xs <- pts$x[round(pts$x) == g]
    expect_true(max(xs) < mns$x[g] - 0.1)
  }
  plain <- layer_data_of(pg_plot() + inferplot(points = "plain"), 2L)
  expect_equal(as.numeric(plain$x), 1:3)
  none <- layer_data_of(pg_plot() + inferplot(points = "none"))
  expect_equal(as.numeric(none$x), 1:3)
  expect_no_warning(draws_silently(pg_plot() + inferplot()))
})
