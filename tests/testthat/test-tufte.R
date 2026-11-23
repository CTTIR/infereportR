# The Tufte options of the figure layer (Bleisatz 1.3): palettes, range frame,
# dot-dash axes, quartile plot, paper grid, context lines, the margin slot and
# the ETbb font.

pts <- pts_tufte
palette_ok <- palette_ok_any

test_that("theme_figure() takes palette and axes", {
  th <- theme_figure(grid = "y", palette = "grey")
  g <- ggplot2::calc_element("panel.grid.major.y", th)
  expect_identical(g$colour, "#999999")
  expect_identical(g$linetype, "solid")
  one <- ggplot2::calc_element("panel.grid.major.y", theme_figure(grid = "y"))
  expect_identical(one$colour, "#000000")
  expect_identical(one$linetype, "13")
  rng <- theme_figure(axes = "range")
  expect_s3_class(ggplot2::calc_element("axis.line.x.bottom", rng), "element_blank")
  expect_false(inherits(ggplot2::calc_element("axis.ticks.x.bottom", rng), "element_blank"))
  none <- theme_figure(axes = "none")
  expect_s3_class(ggplot2::calc_element("axis.ticks.y.left", none), "element_blank")
  expect_error(theme_figure(palette = "colour"), class = "infereportR_invalid_input")
  expect_error(theme_figure(axes = "half"), class = "infereportR_invalid_input")
})

test_that("grid = \"paper\" draws the grid in the paper colour above the data", {
  skip_on_cran()
  for (mode in c("light", "dark")) {
    th <- theme_figure(grid = "paper", mode = mode)
    paper <- .ir_mode(mode)$paper
    expect_identical(ggplot2::calc_element("panel.grid.major.y", th)$colour, paper)
    expect_true(isTRUE(th$panel.ontop))
    expect_identical(ggplot2::calc_element("panel.background", th)$fill, NA)
    expect_s3_class(ggplot2::calc_element("axis.line.x.bottom", th), "element_blank")
    expect_s3_class(ggplot2::calc_element("axis.ticks.x.bottom", th), "element_blank")
    expect_s3_class(ggplot2::calc_element("panel.grid.major.x", th), "element_blank")
  }
})

test_that("figure_size() has the 52 x 40 mm margin slot", {
  expect_identical(figure_size("margin"), list(width = 52, height = 40, units = "mm"))
})

test_that("figure_font(\"etbb\") finds ETbb or ET Book, else serif", {
  withr::defer(figure_font(reset = TRUE))
  local_mocked_bindings(.ir_has_font = function(family) FALSE)
  expect_identical(figure_font("etbb", reset = TRUE), "serif")
  local_mocked_bindings(.ir_has_font = function(family) family == "ET Book")
  expect_identical(figure_font("etbb", reset = TRUE), "ET Book")
  local_mocked_bindings(.ir_has_font = function(family) TRUE)
  expect_identical(figure_font("etbb", reset = TRUE), "ETbb")
})

test_that("geom_rangeframe() runs from the data minimum to the maximum per panel", {
  p <- ggplot2::ggplot(pts, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    geom_rangeframe(sides = "bltr")
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  g <- GeomRangeframe$draw_panel(b$data[[2]], pp, b$layout$coord, sides = "bltr")
  segs <- g$children
  expect_length(segs, 4L)
  rx <- pp$x$continuous_range
  ry <- pp$y$continuous_range
  npc <- function(v, r) (v - r[1]) / diff(r)
  bottom <- segs[[1]]
  expect_equal(as.numeric(c(bottom$x0, bottom$x1)), npc(range(pts$x), rx))
  expect_equal(as.numeric(bottom$y0), 0)
  left <- segs[[2]]
  expect_equal(as.numeric(c(left$y0, left$y1)), npc(range(pts$y), ry))
  expect_equal(as.numeric(left$x0), 0)
  expect_equal(as.numeric(segs[[3]]$y0), 1)
  expect_equal(as.numeric(segs[[4]]$x0), 1)
  expect_identical(substr(segs[[1]]$gp$col, 1, 7), "#000000")
  dark <- ggplot2::ggplot_build(ggplot2::ggplot(pts, ggplot2::aes(x, y)) +
                                  geom_rangeframe(mode = "dark"))
  expect_identical(unique(dark$data[[1]]$colour), "#FFFFFF")
  # Per panel: each facet frames its own data.
  fp <- ggplot2::ggplot(transform(pts, f = c("a", "a", "b", "b", "b")), ggplot2::aes(x, y)) +
    geom_rangeframe(sides = "b") + ggplot2::facet_wrap(ggplot2::vars(f))
  fb <- ggplot2::ggplot_build(fp)
  d2 <- fb$data[[1]][fb$data[[1]]$PANEL == 2, ]
  g2 <- GeomRangeframe$draw_panel(d2, fb$layout$panel_params[[2]], fb$layout$coord,
                                  sides = "b")
  expect_equal(as.numeric(c(g2$children[[1]]$x0, g2$children[[1]]$x1)),
               npc(c(3, 9), fb$layout$panel_params[[2]]$x$continuous_range))
  expect_s3_class(GeomRangeframe$draw_panel(pts[0, ], pp, b$layout$coord), "null")
  expect_error(geom_rangeframe(sides = "bq"), class = "infereportR_invalid_input")
  expect_error(geom_rangeframe(mode = "grey"), class = "infereportR_invalid_input")
  expect_s3_class(draws_silently(p), "gtable")
})

test_that("geom_dotdash() draws one dash per observation on each axis", {
  p <- ggplot2::ggplot(pts, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    geom_dotdash(length = 1.5) + ggplot2::coord_cartesian(clip = "off")
  b <- ggplot2::ggplot_build(p)
  expect_identical(nrow(b$data[[2]]), nrow(pts))
  expect_true(inherits(p$layers[[2]]$geom, "GeomRug"))
  expect_true(p$layers[[2]]$geom_params$outside)
  expect_equal(as.numeric(p$layers[[2]]$geom_params$length), 1.5)
  expect_identical(unique(b$data[[2]]$colour), "#000000")
  grob <- p$layers[[2]]$geom$draw_panel(b$data[[2]], b$layout$panel_params[[1]],
                                        b$layout$coord, sides = "bl", outside = TRUE,
                                        length = grid::unit(1.5, "mm"))
  dashes <- vapply(grob$children, function(s) max(length(s$x0), length(s$y0)), integer(1))
  expect_identical(sum(dashes), 2L * nrow(pts))
  expect_error(geom_dotdash(length = 0), class = "infereportR_invalid_input")
  expect_error(geom_dotdash(sides = ""), class = "infereportR_invalid_input")
})

test_that("stat_quartiles() computes the five numbers with quantile type 7", {
  skip_on_cran()
  p <- ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) + stat_quartiles()
  d <- layer_data_of(p)
  for (i in seq_len(3)) {
    v <- PlantGrowth$weight[PlantGrowth$group == levels(PlantGrowth$group)[i]]
    q <- stats::quantile(v, c(0, 0.25, 0.5, 0.75, 1), type = 7, names = FALSE)
    expect_equal(unlist(d[i, c("ymin", "lower", "middle", "upper", "ymax")], use.names = FALSE), q)
    expect_identical(d$n[i], length(v))
  }
  expect_identical(unique(d$colour), "#000000")
  g <- GeomQuartiles$draw_panel(d, ggplot2::ggplot_build(p)$layout$panel_params[[1]],
                                ggplot2::ggplot_build(p)$layout$coord)
  segs <- g$children[[1]]
  # Two lines per group: min to Q1 and Q3 to max, the gap between is the IQR.
  expect_length(segs$x0, 6L)
  expect_s3_class(draws_silently(p), "gtable")
  expect_identical(nrow(StatQuartiles$compute_group(data.frame(x = 1, y = NA_real_))), 0L)
  expect_error(stat_quartiles(palette = "red"), class = "infereportR_invalid_input")
})

test_that("geom_papergrid() draws paper lines at the breaks of the scale", {
  d <- data.frame(g = c("a", "b"), v = c(12, 31))
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_col() +
    geom_papergrid(mode = "dark") +
    ggplot2::scale_y_continuous(breaks = c(10, 20, 30))
  b <- ggplot2::ggplot_build(p)
  expect_identical(unique(b$data[[2]]$colour), "#000000")
  g <- GeomPapergrid$draw_panel(b$data[[2]], b$layout$panel_params[[1]], b$layout$coord)
  expect_length(g$y0, 3L)
  expect_identical(substr(g$gp$col, 1, 7), "#000000")
  yr <- b$layout$panel_params[[1]]$y$continuous_range
  expect_equal(as.numeric(g$y0), (c(10, 20, 30) - yr[1]) / diff(yr))
  gx <- GeomPapergrid$draw_panel(b$data[[2]], b$layout$panel_params[[1]], b$layout$coord,
                                 axis = "x")
  expect_length(gx$x0, 2L)
  light <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(g, v)) + geom_papergrid())
  expect_identical(unique(light$data[[1]]$colour), "#FFFFFF")
  expect_error(geom_papergrid(axis = "z"), class = "infereportR_invalid_input")
})

ctx_data <- function() {
  d <- expand.grid(time = 1:4, id = 1:6)
  d$arm <- ifelse(d$id <= 3, "A", "B")
  d$score <- d$time * ifelse(d$arm == "A", 1, 2) + d$id / 10
  d
}

test_that("stat_context() draws grey context and the median per series in ink", {
  skip_on_cran()
  d <- ctx_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(time, score, group = id, series = arm)) +
    stat_context(highlight = "B", palette = "tufte")
  b <- layer_data_of(p)
  ctx <- b[b$role == "context", ]
  sm <- b[b$role == "summary", ]
  expect_identical(nrow(ctx), nrow(d))
  expect_identical(unique(ctx$colour), "#999999")
  expect_equal(unique(ctx$linewidth), .ir_lw(0.3))
  expect_identical(nrow(sm), 8L)
  a <- sm[sm$series == "A", ]
  expect_equal(a$y, vapply(1:4, function(t) stats::median(d$score[d$arm == "A" & d$time == t]),
                           numeric(1)))
  expect_identical(unique(a$colour), "#000000")
  expect_identical(unique(sm$colour[sm$series == "B"]), "#B3202A")
  expect_true(palette_ok(p + theme_figure(palette = "tufte") +
                           ggplot2::labs(x = "Week (n)", y = "Score (points)"), "tufte"))
  # 1-bit: the grey becomes an ink hairline; mean summaries, no context.
  one <- layer_data_of(ggplot2::ggplot(d, ggplot2::aes(time, score, group = id)) +
                         stat_context())
  expect_identical(unique(one$colour), "#000000")
  expect_equal(unique(one$linewidth[one$role == "context"]), .ir_lw(0.15))
  mean_only <- layer_data_of(ggplot2::ggplot(d, ggplot2::aes(time, score, group = id)) +
                               stat_context(fun = "mean", context = FALSE))
  expect_identical(unique(mean_only$role), "summary")
  expect_equal(mean_only$y, vapply(1:4, function(t) mean(d$score[d$time == t]), numeric(1)))
  none <- layer_data_of(ggplot2::ggplot(d, ggplot2::aes(time, score, group = id)) +
                          stat_context(fun = "none"))
  expect_identical(unique(none$role), "context")
  expect_s3_class(draws_silently(p + ggplot2::aes(linetype = arm)), "gtable")
  expect_s3_class(draws_silently(ggplot2::ggplot(d, ggplot2::aes(time, score, group = id)) +
                                   stat_context(transparent = TRUE)), "gtable")
  expect_error(stat_context(context = FALSE, fun = "none"), class = "infereportR_invalid_input")
  expect_error(stat_context(highlight = NA), class = "infereportR_invalid_input")
  expect_error(stat_context(fun = "max"), class = "infereportR_invalid_input")
  empty <- StatContext$compute_panel(data.frame(x = numeric(), y = numeric(), group = integer()),
                                     NULL)
  expect_identical(nrow(empty), 0L)
})

test_that("scale_colour_palette() maps roles by palette and mode", {
  skip_on_cran()
  d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_point()
  cols <- function(...) layer_data_of(p + scale_colour_palette(...))$colour
  expect_identical(cols(c("grey", "red", "green"), palette = "tufte"),
                   c("#999999", "#B3202A", "#2E7D32"))
  expect_identical(cols(c("grey", "red", "green"), palette = "tufte", mode = "dark"),
                   c("#999999", "#E5484D", "#5DB760"))
  expect_identical(cols(c("grey", "red", "paper")), c("#000000", "#000000", "#FFFFFF"))
  expect_identical(cols(c("ink", "grey", "red"), palette = "grey"),
                   c("#000000", "#999999", "#000000"))
  expect_error(ggplot2::ggplot_build(p + scale_color_palette(c("ink", "red"))),
               class = "infereportR_too_many_groups")
  expect_error(scale_colour_palette("blue"), class = "infereportR_invalid_input")
  expect_error(scale_colour_palette(), class = "infereportR_invalid_input")
  expect_error(scale_colour_palette("ink", aesthetics = NA), class = "infereportR_invalid_input")
})

test_that("check_figure() applies the palette rule", {
  skip_on_cran()
  d <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "c"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_point() +
    scale_colour_palette(c("grey", "red", "green"), palette = "tufte") +
    theme_figure(palette = "tufte") + ggplot2::labs(x = "x (mm)", y = "y (mm)")
  expect_false(palette_ok(p, "onebit"))
  expect_false(palette_ok(p, "grey"))
  expect_true(palette_ok(p, "tufte"))
  expect_identical(check_figure(p)$check[3], "two_values")
  expect_error(check_figure(p, palette = "c64"), class = "infereportR_invalid_input")
})

test_that("the layers and builders accept palette and stay in ink", {
  skip_on_cran()
  p <- ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) +
    report_figure("slide", compare = welch, comparisons = list(c("ctrl", "trt1")),
                  adjust = "none", palette = "grey")
  b <- ggplot2::ggplot_build(p)
  expect_identical(unique(b$data[[1]]$colour), "#999999")
  expect_true(palette_ok(p, "grey"))
  expect_false(palette_ok(p, "onebit"))
  expect_error(report_figure(palette = "x"), class = "infereportR_invalid_input")
  expect_error(stat_means(palette = "x"), class = "infereportR_invalid_input")
  expect_error(geom_endlabel(palette = "x"), class = "infereportR_invalid_input")
  expect_error(scale_colour_ink(palette = "x"), class = "infereportR_invalid_input")
})

# A color as "#RRGGBB" (an opaque alpha suffix dropped).
hex6 <- function(x) sub("^(#[0-9A-F]{6})FF$", "\\1", toupper(x))

# The colors a grob tree draws with (gp col and fill), without NA.
grob_colours <- function(g) {
  out <- c(g$gp$col, g$gp$fill)
  for (x in g$children) out <- c(out, grob_colours(x))
  unique(hex6(out[!is.na(out)]))
}

context_layers <- function(palette, mode = "light", ...) {
  pg2 <- data.frame(id = rep(1:6, 2), t = factor(rep(c("before", "after"), each = 6),
                                                levels = c("before", "after")),
                    v = c(5, 6, 7, 6, 8, 7, 6, 8, 8, 7, 9, 9))
  ecdf <- data.frame(v = c(1, 2, 2, 3, 5, 6, 4, 5, 7, 8, 8, 9),
                     g = rep(c("a", "b"), each = 6))
  list(
    bracket = ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) +
      ggplot2::geom_point() +
      stat_compare(method = welch, comparisons = list(c("ctrl", "trt1")), mode = mode,
                   palette = palette, ...),
    guides = ggplot2::ggplot(ecdf, ggplot2::aes(v, linetype = g)) +
      ggplot2::stat_ecdf() +
      stat_ecdf_guides(curve_labels = c(0.8, 0.3), mode = mode, palette = palette, ...),
    paired = ggplot2::ggplot(pg2, ggplot2::aes(t, v, id = id)) +
      stat_paired(method = "t_means_paired", mode = mode, palette = palette, ...)
  )
}

test_that("brackets, ECDF guides and paired units draw their context in the palette grey", {
  ink <- "#000000"
  grey <- "#999999"
  layers <- context_layers("grey")
  # bracket: bar and tips grey, label ink
  g <- ggplot2::layer_grob(layers$bracket, 2L)[[1]]
  expect_identical(hex6(g$children[[1]]$gp$col), grey)
  expect_identical(hex6(g$children[[2]]$gp$col), grey)
  expect_identical(hex6(g$children[[4]]$gp$col), ink)
  expect_identical(layer_data_of(layers$bracket, 2L)$line_colour[1], grey)
  # ECDF guides: guide line, its label and the drop lines grey, curve labels ink
  # (children in drawing order: guide line, its label, drop lines, curve labels)
  g <- ggplot2::layer_grob(layers$guides, 2L)[[1]]
  expect_identical(unname(vapply(g$children, function(x) hex6(x$gp$col), "")),
                   c(grey, grey, grey, ink))
  expect_s3_class(g$children[[4]], "text")
  # paired: unit lines and points grey, means and label ink
  g <- ggplot2::layer_grob(layers$paired, 1L)[[1]]
  expect_identical(hex6(g$children$lines$gp$col), grey)
  expect_identical(hex6(g$children$points$gp$col), grey)
  expect_identical(hex6(g$children$mean_line$gp$col), ink)
  expect_identical(hex6(g$children$label$gp$col), ink)
  # in the Tufte palette and dark mode the grey stays, the ink turns white
  dark <- context_layers("tufte", mode = "dark")
  expect_setequal(grob_colours(ggplot2::layer_grob(dark$paired, 1L)[[1]]),
                  c(grey, "#FFFFFF"))
  # a colour given to the layer applies to the context as well
  red <- context_layers("grey", colour = "#B3202A")
  expect_identical(grob_colours(ggplot2::layer_grob(red$bracket, 2L)[[1]]), "#B3202A")
  expect_identical(grob_colours(ggplot2::layer_grob(red$paired, 1L)[[1]]), "#B3202A")
  expect_identical(grob_colours(ggplot2::layer_grob(red$guides, 2L)[[1]]), "#B3202A")
  # check_figure() sees the second colour column
  p <- layers$bracket + ggplot2::labs(x = NULL, y = "Weight (g)") +
    theme_figure("slide", palette = "grey")
  expect_true(palette_ok_any(p, "grey"))
  expect_false(palette_ok_any(p, "onebit"))
})

test_that("in onebit the context layers stay exactly two values", {
  for (mode in c("light", "dark")) {
    layers <- context_layers("onebit", mode = mode)
    ink <- .ir_mode(mode)$ink
    for (nm in names(layers)) {
      i <- if (nm == "paired") 1L else 2L
      cols <- grob_colours(ggplot2::layer_grob(layers[[nm]], i)[[1]])
      expect_identical(cols, ink, label = paste(mode, nm))
      d <- layer_data_of(layers[[nm]], i)
      cc <- unlist(d[grepl("colour$", names(d))])
      expect_true(all(hex6(cc[!is.na(cc)]) == ink), label = paste(mode, nm))
      expect_true(palette_ok_any(layers[[nm]] + ggplot2::labs(x = "x (u)", y = "y (u)") +
                                   theme_figure("slide", mode = mode), "onebit"),
                  label = paste(mode, nm))
    }
  }
})
