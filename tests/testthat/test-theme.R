theme_text_elements <- c("axis.text.x", "axis.text.y", "axis.title.x", "axis.title.y",
                         "legend.text", "legend.title", "strip.text.x", "plot.title",
                         "plot.subtitle", "plot.caption")

test_that("theme_figure returns a complete theme for every choice", {
  for (target in c("paper", "slide")) {
    for (legend in c("inside", "right", "bottom", "none")) {
      for (grid in c("none", "y", "x", "xy")) {
        th <- theme_figure(target, legend = legend, grid = grid)
        expect_true(isTRUE(attr(th, "complete")))
      }
    }
  }
  expect_true(isTRUE(attr(theme_figure(border = TRUE), "complete")))
})

test_that("all text resolves to 8 to 14 pt at base_size 9", {
  for (target in c("paper", "slide")) {
    th <- theme_figure(target, base_size = 9)
    for (el in theme_text_elements) {
      size <- ggplot2::calc_element(el, th)$size
      expect_true(is.numeric(size) && size >= 8 && size <= 14, info = paste(target, el))
    }
  }
})

test_that("paper base sizes outside the APA range warn", {
  expect_warning(theme_figure("paper", base_size = 7), class = "inferplotR_apa_text_size")
  expect_warning(theme_figure("paper", base_size = 15), class = "inferplotR_apa_text_size")
  expect_no_warning(theme_figure("slide", base_size = 7))
  expect_no_warning(theme_figure("slide", base_size = 15))
  expect_no_warning(theme_figure("paper", base_size = 8))
  expect_no_warning(theme_figure("paper", base_size = 14))
})

test_that("every colour and fill is ink, paper or NA", {
  allowed <- grDevices::col2rgb(c(.ip_ink, .ip_paper), alpha = TRUE)
  is_allowed <- function(x) {
    if (is.null(x)) return(TRUE)
    if (all(is.na(x))) return(TRUE)
    rgb <- grDevices::col2rgb(x, alpha = TRUE)
    all(vapply(seq_len(ncol(rgb)), function(i) {
      any(colSums(abs(allowed - rgb[, i])) == 0)
    }, logical(1)))
  }
  for (target in c("paper", "slide")) {
    for (grid in c("none", "xy")) {
      th <- theme_figure(target, grid = grid, border = TRUE)
      for (n in names(th)) {
        el <- th[[n]]
        # ggplot2 4.x elements carry "ggplot2::element"; older ones "element".
        if (!inherits(el, c("element", "ggplot2::element"))) next
        if (inherits(el, c("element_blank", "ggplot2::element_blank"))) next
        # ink, paper and accent exist only on the geom element of ggplot2 4.x.
        for (field in c("colour", "fill", "ink", "paper", "accent")) {
          # S7 elements store fields as attributes, list elements as names.
          present <- c(names(el), names(attributes(el)))
          value <- if (field %in% present) el[[field]] else NULL
          expect_true(is_allowed(value), info = paste(target, n, field))
        }
      }
    }
  }
})

test_that("grid choices draw dotted major lines or none", {
  th <- theme_figure(grid = "y")
  expect_true(inherits(th$panel.grid.major.y, "element_line"))
  expect_identical(th$panel.grid.major.y$linetype, "13")
  expect_true(inherits(th$panel.grid.major.x, "element_blank"))

  th <- theme_figure(grid = "x")
  expect_true(inherits(th$panel.grid.major.x, "element_line"))
  expect_true(inherits(th$panel.grid.major.y, "element_blank"))

  th <- theme_figure(grid = "none")
  expect_true(inherits(ggplot2::calc_element("panel.grid.major.x", th), "element_blank"))
  expect_true(inherits(ggplot2::calc_element("panel.grid.major.y", th), "element_blank"))
  expect_true(inherits(ggplot2::calc_element("panel.grid.minor.y", theme_figure(grid = "xy")),
                       "element_blank"))
})

test_that("border = TRUE draws an unfilled panel border", {
  th <- theme_figure(border = TRUE)
  expect_true(inherits(th$panel.border, "element_rect"))
  expect_true(is.na(th$panel.border$fill))
  expect_true(inherits(theme_figure()$panel.border, "element_blank"))
})

test_that("legend choices set position, background and keys", {
  th <- theme_figure(legend = "inside")
  expect_identical(th$legend.position, "inside")
  expect_equal(th$legend.position.inside, c(1, 1))
  expect_equal(th$legend.justification.inside, c(1, 1))
  expect_true(inherits(th$legend.key, "element_blank"))
  expect_identical(ggplot2::calc_element("legend.title", th)$face, "plain")
  expect_identical(theme_figure(legend = "bottom")$legend.position, "bottom")
  expect_identical(theme_figure(legend = "none")$legend.position, "none")
})

test_that("margins and strips follow the target", {
  paper <- ggplot2::calc_element("plot.margin", theme_figure("paper"))
  slide <- ggplot2::calc_element("plot.margin", theme_figure("slide"))
  expect_equal(grid::convertUnit(paper, "mm", valueOnly = TRUE), rep(2, 4))
  expect_equal(grid::convertUnit(slide, "mm", valueOnly = TRUE), rep(1, 4))
  strip <- ggplot2::calc_element("strip.text.x", theme_figure())
  expect_identical(strip$face, "bold")
  expect_equal(strip$hjust, 0)
  expect_true(inherits(theme_figure()$strip.background, "element_blank"))
})

test_that("plots with theme_figure build without warnings", {
  # The pdf() device warns about families outside its PostScript database, so
  # draw on a ragg device, as figures are meant to be; refresh the font cache
  # in case an earlier test left a mocked family in it.
  skip_if_not_installed("ragg")
  figure_font(reset = TRUE)
  ragg::agg_png(withr::local_tempfile(fileext = ".png"))
  device <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(device))
  df <- data.frame(x = c(1, 2, 3, 4), y = c(2, 1, 4, 3), g = c("a", "b", "a", "b"))
  for (legend in c("inside", "right", "bottom", "none")) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y, shape = g)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~g) +
      ggplot2::labs(title = "Title", subtitle = "Subtitle", caption = "Caption") +
      theme_figure(legend = legend, grid = "xy", border = TRUE)
    expect_no_warning(ggplot2::ggplotGrob(p))
  }
})

test_that("theme_figure validates its arguments", {
  expect_error(theme_figure(target = "poster"), class = "inferplotR_invalid_input")
  expect_error(theme_figure(base_size = "9"), class = "inferplotR_invalid_input")
  expect_error(theme_figure(base_size = -1), class = "inferplotR_invalid_input")
  expect_error(theme_figure(base_size = NA_real_), class = "inferplotR_invalid_input")
  expect_error(theme_figure(border = NA), class = "inferplotR_invalid_input")
  expect_error(theme_figure(base_family = 1), class = "inferplotR_invalid_input")
  expect_error(theme_figure(legend = "top"), class = "inferplotR_invalid_input")
  expect_error(theme_figure(grid = c("x", "y")), class = "inferplotR_invalid_input")
})

test_that("slide axis titles fall back to the base family", {
  local_mocked_bindings(.ip_has_font = function(family) FALSE)
  th <- theme_figure("slide", base_family = "serif")
  expect_identical(ggplot2::calc_element("axis.title.x", th)$family, "serif")
  expect_identical(ggplot2::calc_element("axis.text.x", th)$family, "serif")
  local_mocked_bindings(.ip_has_font = function(family) TRUE)
  th <- theme_figure("slide", base_family = "serif")
  expect_identical(ggplot2::calc_element("axis.title.x", th)$family, "IBM Plex Sans Condensed")
  th <- theme_figure("paper", base_family = "serif")
  expect_identical(ggplot2::calc_element("axis.title.x", th)$family, "serif")
})
