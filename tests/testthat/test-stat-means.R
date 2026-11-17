test_that("stat_means numbers equal direct infercalcR runs", {
  d <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + stat_means())
  expect_identical(nrow(d), 3L)
  for (i in 1:3) {
    level <- levels(pg$group)[i]
    direct <- infercalcR::result_data(infercalcR::run_inference(
      infercalcR::method_spec("t_mean_one_sample"), x = pg_values(level), mu0 = 0))
    expect_equal(d$y[i], direct$estimate)
    expect_equal(d$ymin[i], direct$conf.low)
    expect_equal(d$ymax[i], direct$conf.high)
    expect_equal(d$n[i], direct$n)
    expect_identical(d$.ip_result_id[i],
                     paste0("inferplotR:means:t_mean_one_sample:panel1:", level))
  }
  expect_true(all(d$colour == .ip_ink))
})

test_that("stat_means labels are infereportR strings", {
  d <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
                       stat_means(label = "estimate_ci"))
  shown <- infereportR::report_table(d$.ip_result[[1]])
  expect_identical(d$label[1], paste(shown$estimate_display, shown$interval_display))
  d0 <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + stat_means())
  expect_identical(d0$label, rep("", 3))
  d1 <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight)) +
                        stat_means(label = "estimate"))
  expect_identical(d1$label[1], shown$estimate_display)
})

test_that("a group with one observation keeps its row with a status", {
  d <- rbind(pg, data.frame(weight = 4, group = "single"))
  d$group <- factor(d$group, levels = c(levels(pg$group), "single"))
  p <- ggplot2::ggplot(d, ggplot2::aes(group, weight)) +
    stat_means(label = "estimate_ci", na.rm = TRUE)
  out <- layer_data_of(p)
  expect_identical(nrow(out), 4L)
  expect_true(is.na(out$y[4]))
  r <- out$.ip_result[[4]]
  expect_identical(infercalcR::result_data(r)$status, "undefined")
  shown <- infereportR::report_table(r)
  expect_identical(out$label[4], paste(shown$estimate_display, shown$interval_display))
  expect_false(grepl("ns", out$label[4], fixed = TRUE))
  expect_no_warning(draws_silently(p))
})

test_that("stat_means works per panel and per group", {
  p <- ggplot2::ggplot(pg_facets, ggplot2::aes(group, weight)) +
    ggplot2::facet_wrap(~panel) + stat_means()
  expect_length(inference_results(p), 6L)
  pd <- within(pg, sub <- rep(c("a", "b"), 15))
  d <- layer_data_of(ggplot2::ggplot(pd, ggplot2::aes(group, weight, group = interaction(group, sub))) +
                       stat_means())
  expect_identical(nrow(d), 6L)
  expect_false(anyDuplicated(d$.ip_result_id) > 0L)
})

test_that("stat_means validates its arguments", {
  bad <- "inferplotR_invalid_input"
  expect_error(stat_means(method = "t_means_two_independent_welch"),
               class = "inferplotR_unsupported")
  expect_error(stat_means(conf.level = 1.2), class = bad)
  expect_error(stat_means(conf.level = NULL), class = bad)
  expect_error(stat_means(label = "p"), class = bad)
  expect_error(stat_means(na.rm = NA), class = bad)
})

test_that("constant aesthetics such as shape are kept", {
  d <- layer_data_of(ggplot2::ggplot(pg, ggplot2::aes(group, weight, shape = group)) +
                       stat_means())
  expect_length(unique(d$shape), 3L)
  expect_false(anyNA(d$shape))
})

test_that("offset shifts the drawing on a discrete x only", {
  base <- ggplot2::ggplot(pg, ggplot2::aes(group, weight))
  d0 <- layer_data_of(base + stat_means())
  d <- layer_data_of(base + stat_means(offset = .ip_means_offset))
  expect_equal(as.numeric(d$x), as.numeric(d0$x) + .ip_means_offset)
  expect_equal(d$y, d0$y)
  expect_identical(d$.ip_result_id, d0$.ip_result_id)
  expect_identical(formals(stat_means)$offset, 0)
  cont <- data.frame(x = rep(1:2, each = 5), y = c(1:5, 3:7))
  dc <- layer_data_of(ggplot2::ggplot(cont, ggplot2::aes(x, y, group = x)) +
                        stat_means(offset = 0.3))
  expect_equal(dc$x, c(1, 2))
  expect_error(stat_means(offset = NA), class = "inferplotR_invalid_input")
  expect_error(stat_means(offset = c(1, 2)), class = "inferplotR_invalid_input")
  expect_no_warning(draws_silently(base + ggplot2::geom_point() +
                                     stat_means(offset = .ip_means_offset)))
})

test_that("means draw as a filled 2.1 mm point with a 1.1 pt interval", {
  base <- ggplot2::ggplot(pg, ggplot2::aes(group, weight))
  d <- layer_data_of(base + stat_means())
  expect_equal(unique(d$size) * 4, 2.1)
  expect_equal(unique(d$linewidth), .ip_lw(.ip_rule_heavy))
  expect_identical(unique(d$shape), 19)
  user <- layer_data_of(base + stat_means(size = 1, linewidth = 0.2, shape = 15))
  expect_identical(unique(user$size), 1)
  expect_identical(unique(user$linewidth), 0.2)
  expect_identical(unique(user$shape), 15)
  other <- layer_data_of(base + stat_means(geom = "point"))
  expect_false(isTRUE(all.equal(unique(other$size) * 4, 2.1)))
})
