checks <- c("text_size", "font_family", "two_values", "axis_labels", "embedded_title",
            "legend_case")

pg_caps <- within(PlantGrowth, {
  group <- factor(group, labels = c("Control", "Treatment 1", "Treatment 2"))
})
mt_plot <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + theme_figure() +
    ggplot2::labs(x = "Weight (1000 lb)", y = "Fuel economy (miles per gallon)")
}
status_of <- function(res, check) res$status[res$check == check]
detail_of <- function(res, check) res$detail[res$check == check]

expect_audit_shape <- function(res) {
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("check", "source", "status", "detail"))
  expect_identical(res$check, checks)
  expect_identical(nrow(res), 6L)
  for (col in names(res)) expect_type(res[[col]], "character")
  expect_true(all(res$status %in% c("pass", "fail", "note")))
  expect_identical(res$source, c("APA 7", "APA 7", "inferplotR", "APA 7", "APA 7", "APA 7"))
}

test_that("an inferplot figure with labelled axes and units passes", {
  p <- ggplot2::ggplot(pg_caps, ggplot2::aes(group, weight)) +
    inferplot(compare = welch, reference = "Control", adjust = "holm",
              omnibus = "anova_one_way") +
    ggplot2::geom_point(ggplot2::aes(shape = group), colour = "black") +
    ggplot2::labs(x = "Treatment", y = "Dried weight (g)", shape = "Group")
  res <- check_figure(p)
  expect_audit_shape(res)
  expect_identical(res$status, rep("pass", 6))
  expect_identical(res$detail, rep("", 6))
  titled <- check_figure(p + ggplot2::labs(title = "Plant growth"))
  expect_identical(status_of(titled, "embedded_title"), "note")
  expect_identical(titled$status[titled$check != "embedded_title"], rep("pass", 5))
})

test_that("theme_classic(9) fails text_size with the offenders named", {
  p <- ggplot2::ggplot(pg, ggplot2::aes(group, weight)) + ggplot2::geom_point() +
    ggplot2::theme_classic(9)
  res <- check_figure(p)
  expect_audit_shape(res)
  expect_identical(status_of(res, "text_size"), "fail")
  expect_match(detail_of(res, "text_size"), "axis.text.x", fixed = TRUE)
  expect_match(detail_of(res, "text_size"), "axis.text.y", fixed = TRUE)
  expect_match(detail_of(res, "text_size"), "7.2", fixed = TRUE)
})

test_that("small text layers fail text_size", {
  p <- mt_plot() + ggplot2::geom_point(shape = 1) +
    ggplot2::annotate("text", x = 3, y = 30, label = "Note", size = 2)
  res <- check_figure(p)
  expect_identical(status_of(res, "text_size"), "fail")
  expect_match(detail_of(res, "text_size"), "GeomText", fixed = TRUE)
})

test_that("serif families fail font_family", {
  p <- mt_plot() + ggplot2::geom_point(shape = 1) +
    ggplot2::theme(axis.text = ggplot2::element_text(family = "Times New Roman"))
  res <- check_figure(p)
  expect_identical(status_of(res, "font_family"), "fail")
  expect_match(detail_of(res, "font_family"), "Times New Roman", fixed = TRUE)
  sans <- mt_plot() + ggplot2::geom_point(shape = 1) +
    ggplot2::theme(axis.text = ggplot2::element_text(family = "DejaVu Sans"))
  expect_identical(status_of(check_figure(sans), "font_family"), "pass")
})

test_that("colours other than black and white fail two_values; hatch fills pass", {
  red <- check_figure(mt_plot() + ggplot2::geom_point(colour = "red"))
  expect_identical(status_of(red, "two_values"), "fail")
  expect_match(detail_of(red, "two_values"), "#FF0000", fixed = TRUE)

  bars <- data.frame(g = c("Alpha", "Beta", "Gamma"), v = c(3, 5, 4))
  hatched <- ggplot2::ggplot(bars, ggplot2::aes(g, v, fill = g)) +
    ggplot2::geom_col(colour = "black") + scale_fill_hatch() + theme_figure() +
    ggplot2::labs(x = "Condition", y = "Value (units)", fill = "Condition")
  res <- check_figure(hatched)
  expect_identical(status_of(res, "two_values"), "pass")

  grey <- check_figure(mt_plot() + ggplot2::geom_point(shape = 1) + ggplot2::theme_grey())
  expect_identical(status_of(grey, "two_values"), "fail")
})

test_that("missing axis labels fail and labels without units give a note", {
  no_y <- check_figure(mt_plot() + ggplot2::geom_point(shape = 1) + ggplot2::labs(y = NULL))
  expect_identical(status_of(no_y, "axis_labels"), "fail")
  empty_x <- check_figure(mt_plot() + ggplot2::geom_point(shape = 1) + ggplot2::labs(x = ""))
  expect_identical(status_of(empty_x, "axis_labels"), "fail")
  no_unit <- check_figure(mt_plot() + ggplot2::geom_point(shape = 1) +
                            ggplot2::labs(y = "Fuel economy"))
  expect_identical(status_of(no_unit, "axis_labels"), "note")
  expect_match(detail_of(no_unit, "axis_labels"), "Fuel economy", fixed = TRUE)
  percent <- check_figure(mt_plot() + ggplot2::geom_point(shape = 1) +
                            ggplot2::labs(y = "Share %"))
  expect_identical(status_of(percent, "axis_labels"), "pass")
  comma <- check_figure(mt_plot() + ggplot2::geom_point(shape = 1) +
                          ggplot2::labs(y = "Fuel economy, mpg"))
  expect_identical(status_of(comma, "axis_labels"), "pass")
})

test_that("a title or subtitle gives an embedded_title note", {
  base <- mt_plot() + ggplot2::geom_point(shape = 1)
  expect_identical(status_of(check_figure(base), "embedded_title"), "pass")
  expect_identical(status_of(check_figure(base + ggplot2::labs(title = "Cars")),
                             "embedded_title"), "note")
  expect_identical(status_of(check_figure(base + ggplot2::labs(subtitle = "Cars")),
                             "embedded_title"), "note")
})

test_that("lower-case legend labels give a legend_case note", {
  make <- function(levels) {
    d <- data.frame(x = 1:6, y = c(2, 4, 3, 5, 4, 6), g = rep(levels, 3))
    ggplot2::ggplot(d, ggplot2::aes(x, y, shape = g)) + ggplot2::geom_point() +
      theme_figure() + ggplot2::labs(x = "Dose (mg)", y = "Response (units)", shape = "Level")
  }
  p <- make(c("low", "high"))
  res <- check_figure(p)
  expect_identical(status_of(res, "legend_case"), "note")
  expect_match(detail_of(res, "legend_case"), "low", fixed = TRUE)
  expect_identical(status_of(check_figure(make(c("Low", "High"))), "legend_case"), "pass")
  hidden <- p + ggplot2::theme(legend.position = "none")
  expect_identical(status_of(check_figure(hidden), "legend_case"), "pass")
})

test_that("the return value is type-stable, even without layers", {
  for (p in list(ggplot2::ggplot(), ggplot2::ggplot(mtcars),
                 mt_plot() + ggplot2::geom_point(shape = 1) + ggplot2::facet_wrap(~am),
                 mt_plot() + ggplot2::geom_point(shape = 1) + ggplot2::facet_grid(am ~ vs))) {
    res <- check_figure(p)
    expect_audit_shape(res)
  }
  expect_identical(status_of(check_figure(ggplot2::ggplot()), "axis_labels"), "fail")
  expect_identical(check_figure(mt_plot(), target = "slide")$check, checks)
})

test_that("invalid input is rejected", {
  bad <- "inferplotR_invalid_input"
  expect_error(check_figure(mtcars), class = bad)
  expect_error(check_figure(mt_plot(), target = "poster"), class = bad)
})
