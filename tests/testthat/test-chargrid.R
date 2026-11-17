# Fixtures --------------------------------------------------------------------

fx_lines <- function() {
  h <- c(0, 2, 4, 6, 8, 12, 24, 48)
  d <- rbind(
    data.frame(h = h, y = c(9, 14, 19, 21, 19, 15, 11, 9), g = "ISS 16-24"),
    data.frame(h = h, y = c(10, 15, 22, 28, 33, 31, 22, 16), g = "ISS >= 25")
  )
  ggplot2::ggplot(d, ggplot2::aes(h, y, shape = g, linetype = g)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Hours after admission", y = "C5a, ng/ml", title = "C5a after injury")
}

fx_bars <- function() {
  d <- data.frame(g = c("Peak <= 6 h", "Peak > 6 h"), v = c(12, 38))
  ggplot2::ggplot(d, ggplot2::aes(g, v)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::labs(x = NULL, y = "MODS, %")
}

# scale_fill_hatch() returns a list palette (ink, paper, grid patterns), so
# keys are matched element-wise with identical().
fx_list_fill <- function() scale_fill_hatch()

fx_hatch_bars <- function() {
  d <- data.frame(g = c("Control", "Low dose", "High dose"), v = c(14, 22, 31))
  ggplot2::ggplot(d, ggplot2::aes(g, v, fill = g)) +
    ggplot2::geom_col(width = 0.6) +
    fx_list_fill() +
    ggplot2::labs(x = "Arm", y = "Response, %", fill = NULL)
}

fx_means <- function() {
  s <- data.frame(g = c("A", "B", "C"), m = c(4.2, 5.1, 6.3), lo = c(3.1, 4.4, 5.0),
                  hi = c(5.3, 5.8, 7.6))
  ggplot2::ggplot(s, ggplot2::aes(g, m)) +
    ggplot2::geom_hline(yintercept = 5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Group", y = "Mean (95% CI)")
}

plot_cells <- function(x, lay) x$chars[lay$r0:lay$r1, lay$c0:lay$c1]

# Snapshots -------------------------------------------------------------------

test_that("fixture (a): two-series line and points with title and labels", {
  expect_snapshot(print(char_grid(fx_lines(), width = 64, height = 20)))
})

test_that("fixture (b): single-series bars share one fill glyph", {
  x <- char_grid(fx_bars(), width = 40, height = 16)
  expect_snapshot(print(x))
  body <- x$chars[2:14, 5:39]
  expect_true(any(body == "#"))
  expect_false(any(body %in% c("/", "=", ":")))
  expect_identical(nrow(x$legend), 0L)
})

test_that("fixture (c): three list-filled bars with a legend", {
  x <- char_grid(fx_hatch_bars(), width = 48, height = 18)
  expect_snapshot(print(x))
  expect_identical(x$legend$key, 1:3)
  expect_identical(x$legend$symbol, c("#", "/", "="))
})

test_that("fixture (d): error bars and means with a reference line", {
  expect_snapshot(print(char_grid(fx_means(), width = 48, height = 16)))
})

# Structure -------------------------------------------------------------------

test_that("axes hold their glyphs and a + at every break", {
  p <- fx_lines()
  x <- char_grid(p, width = 64, height = 20)
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  ylab <- as.character(pp$y$get_labels())
  ypos <- pp$y$break_positions()
  keep <- is.finite(ypos) & ypos >= 0 & ypos <= 1
  ypos <- ypos[keep]
  xpos <- pp$x$break_positions()
  xpos <- xpos[is.finite(xpos) & xpos >= 0 & xpos <= 1]
  # title row, blank row, y label row; bottom: axis, ticks, x label, one legend row
  r0 <- 4
  r1 <- 20 - 4
  ac <- max(nchar(ylab[keep])) + 2
  c0 <- ac + 1
  c1 <- 64 - 1
  expect_true(all(x$chars[r0:r1, ac] %in% c("|", "+")))
  expect_true(all(x$chars[r1 + 1, c0:c1] %in% c("-", "+")))
  expect_identical(x$chars[r1 + 1, ac], "+")
  for (p_ in xpos) {
    expect_identical(x$chars[r1 + 1, c0 + floor(p_ * (c1 - c0) + 0.5)], "+")
  }
  for (p_ in ypos) {
    expect_identical(x$chars[r1 - floor(p_ * (r1 - r0) + 0.5), ac], "+")
  }
  expect_identical(paste(x$chars[1, 1:16], collapse = ""), "C5a after injury")
})

test_that("layout follows the specified rows and columns", {
  lay <- inferplotR:::.ip_cg_layout(64, 20, title = TRUE, ylab = TRUE, xlab = TRUE,
                                    legend_rows = 1, ytick_width = 2)
  expect_identical(c(lay$r0, lay$r1, lay$ac, lay$c0, lay$c1), c(4L, 16L, 4L, 5L, 63L))
  expect_identical(c(lay$axis_row, lay$tick_row, lay$xlab_row, lay$legend_row),
                   c(17L, 18L, 19L, 20L))
  bare <- inferplotR:::.ip_cg_layout(30, 10)
  expect_identical(c(bare$r0, bare$r1), c(1L, 8L))
  cap <- inferplotR:::.ip_cg_layout(30, 12, caption = TRUE)
  expect_identical(cap$caption_row, 12L)
})

test_that("legend entries wrap greedily three blanks apart", {
  wrap <- inferplotR:::.ip_cg_legend_wrap
  expect_identical(wrap(c(5, 5, 5), 40), list(1:3))
  expect_identical(wrap(c(10, 10, 10), 25), list(1:2, 3L))
  expect_identical(wrap(integer(), 40), list())
})

# Mapping ---------------------------------------------------------------------

test_that("range minimum maps to (r1, c0) and maximum to (r0, c1)", {
  d <- data.frame(x = c(0, 10), y = c(0, 5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL)
  x <- char_grid(p, width = 30, height = 12)
  ylab <- as.character(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels())
  lay <- inferplotR:::.ip_cg_layout(30, 12, ytick_width = max(nchar(ylab[!is.na(ylab)])))
  expect_identical(x$chars[lay$r1, lay$c0], "*")
  expect_identical(x$chars[lay$r0, lay$c1], "*")
  map <- inferplotR:::.ip_cg_map(lay, c(0, 10), c(0, 5))
  expect_equal(map$col(c(0, 10)), c(lay$c0, lay$c1))
  expect_equal(map$row(c(0, 5)), c(lay$r1, lay$r0))
  flat <- inferplotR:::.ip_cg_map(lay, c(1, 1), c(1, 1))
  expect_equal(flat$col(1), lay$c0 + floor(0.5 * (lay$pw - 1) + 0.5))
})

# Slope glyphs and rasteriser -------------------------------------------------

test_that("slope glyphs use the cell-aspect rule", {
  slope <- inferplotR:::.ip_cg_glyph_slope
  expect_identical(slope(5, 0), "-")
  expect_identical(slope(-5, 0), "-")
  expect_identical(slope(0, 3), "|")
  expect_identical(slope(0, -3), "|")
  expect_identical(slope(4, -2), "/")
  expect_identical(slope(4, 2), "\\")
  # one row over four columns is 26.6 degrees with the factor 2 (14 without it)
  expect_identical(slope(4, -1), "/")
  expect_identical(slope(6, -1), "-")
  expect_identical(slope(1, -2), "|")
  expect_true(is.na(slope(0, 0)))
  expect_identical(slope(4, -2, "unicode"), "\u2571")
})

test_that("the rasteriser visits every step with half-up rounding", {
  cells <- inferplotR:::.ip_cg_line(1, 1, 5, 3)
  expect_identical(nrow(cells), 5L)
  expect_identical(cells$c, c(1, 2, 3, 4, 5))
  expect_identical(cells$r, c(1, 2, 2, 3, 3))
  expect_identical(nrow(inferplotR:::.ip_cg_line(2, 2, 2, 2)), 1L)
})

test_that("a zero-length segment draws the point glyph", {
  d <- data.frame(x = c(0, 10, 5), y = c(0, 10, 5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_segment(ggplot2::aes(xend = x, yend = y))
  x <- char_grid(p, width = 30, height = 12)
  expect_true(any(x$chars == "*"))
})

# Keys ------------------------------------------------------------------------

test_that("the key scale is chosen fill, colour, shape, linetype", {
  d <- data.frame(x = 1:4, y = 1:4, a = c("p", "p", "q", "q"), b = c("u", "v", "u", "v"))
  aes_of <- function(p) inferplotR:::.ip_cg_key_scale(ggplot2::ggplot_build(p))$aes
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y))
  expect_identical(aes_of(base + ggplot2::geom_col(ggplot2::aes(fill = a, colour = b))), "fill")
  expect_identical(aes_of(base + ggplot2::geom_point(ggplot2::aes(colour = a, shape = b))),
                   "colour")
  expect_identical(aes_of(base + ggplot2::geom_point(ggplot2::aes(shape = a))), "shape")
  expect_identical(aes_of(base + ggplot2::geom_line(ggplot2::aes(linetype = a))), "linetype")
  # a discrete fill with one break does not count; the colour scale is used
  one <- base + ggplot2::geom_col(ggplot2::aes(fill = "all", colour = a))
  expect_identical(aes_of(one), "colour")
  # a continuous colour scale is not a key scale
  expect_null(aes_of(base + ggplot2::geom_point(ggplot2::aes(colour = y))))
})

test_that("keys index the mapped breaks; without a key scale every key is 1", {
  d <- data.frame(x = 1:4, y = 1:4, a = c("q", "p", "q", "p"))
  b <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, shape = a)) +
                               ggplot2::geom_point())
  ks <- inferplotR:::.ip_cg_key_scale(b)
  expect_identical(inferplotR:::.ip_cg_keys(b$data[[1]], ks), c(2L, 1L, 2L, 1L))
  b0 <- ggplot2::ggplot_build(ggplot2::ggplot(d, ggplot2::aes(x, y, group = a)) +
                                ggplot2::geom_point())
  ks0 <- inferplotR:::.ip_cg_key_scale(b0)
  expect_null(ks0)
  expect_identical(inferplotR:::.ip_cg_keys(b0$data[[1]], ks0), rep(1L, 4))
  x <- char_grid(ggplot2::ggplot(d, ggplot2::aes(x, y, group = a)) + ggplot2::geom_point(),
                 width = 30, height = 12)
  expect_false(any(x$chars == "o"))
  expect_identical(nrow(x$legend), 0L)
})

test_that("list-column keys are matched with identical()", {
  b <- ggplot2::ggplot_build(fx_hatch_bars())
  ks <- inferplotR:::.ip_cg_key_scale(b)
  expect_true(is.list(b$data[[1]]$fill))
  expect_identical(ks$aes, "fill")
  keys <- inferplotR:::.ip_cg_keys(b$data[[1]], ks)
  expect_identical(sort(keys), 1:3)
  arms <- c("Control", "High dose", "Low dose")
  expect_identical(ks$labels[keys], arms[as.integer(b$data[[1]]$x)])
})

test_that("legend symbols are the first glyph drawn per key", {
  x <- char_grid(fx_lines(), width = 64, height = 20)
  expect_identical(x$legend$label, c("ISS 16-24", "ISS >= 25"))
  expect_true(x$legend$symbol[[1]] %in% c("-", "/", "|", "\\"))
  expect_identical(x$legend$symbol[[2]], ".")
  expect_identical(names(x$legend), c("key", "symbol", "label"))
})

# Charset and case ------------------------------------------------------------

test_that("unicode charset draws series 1 with box-drawing glyphs", {
  p <- fx_lines()
  x <- char_grid(p, width = 64, height = 20, charset = "unicode")
  lay <- inferplotR:::.ip_cg_layout(64, 20, title = TRUE, ylab = TRUE, xlab = TRUE,
                                    legend_rows = 1, ytick_width = 2)
  body <- plot_cells(x, lay)
  expect_true(any(body %in% c("\u2500", "\u2571", "\u2502", "\u2572")))
  expect_false(any(body %in% c("/", "\\")))
  expect_true(any(body == "\u25cf"))
  expect_identical(x$charset, "unicode")
})

test_that("case = 'upper' changes strings, not glyphs", {
  p <- fx_lines()
  a <- char_grid(p, width = 64, height = 20)
  u <- char_grid(p, width = 64, height = 20, case = "upper")
  expect_identical(paste(u$chars[1, 1:16], collapse = ""), "C5A AFTER INJURY")
  expect_identical(u$legend$label, c("ISS 16-24", "ISS >= 25"))
  lay <- inferplotR:::.ip_cg_layout(64, 20, title = TRUE, ylab = TRUE, xlab = TRUE,
                                    legend_rows = 1, ytick_width = 2)
  expect_identical(plot_cells(u, lay), plot_cells(a, lay))
  expect_identical(u$legend$symbol, a$legend$symbol)
  expect_true(any(grepl("HOURS AFTER ADMISSION", format(u), fixed = TRUE)))
})

# Errors ----------------------------------------------------------------------

test_that("unsupported plots and bad arguments raise classed errors", {
  p <- fx_bars()
  expect_error(char_grid(p + ggplot2::facet_wrap(~g)), class = "inferplotR_unsupported")
  expect_error(char_grid(p + ggplot2::coord_flip()), class = "inferplotR_unsupported")
  expect_error(char_grid(p + ggplot2::coord_polar()), class = "inferplotR_unsupported")
  expect_error(char_grid(p, width = 10), class = "inferplotR_invalid_input")
  expect_error(char_grid(p, height = 9), class = "inferplotR_invalid_input")
  expect_error(char_grid(p, width = 30.5), class = "inferplotR_invalid_input")
  expect_error(char_grid(p, charset = "ebcdic"), class = "inferplotR_invalid_input")
  expect_error(char_grid(p, case = "lower"), class = "inferplotR_invalid_input")
  expect_error(char_grid(data.frame(x = 1)), class = "inferplotR_invalid_input")
})

test_that("a grid too small for the labels raises too_small", {
  long <- fx_bars() + ggplot2::scale_y_continuous(labels = function(v) paste(v, "patients per day"))
  expect_error(char_grid(long, width = 24, height = 16), class = "inferplotR_too_small")
  crowded <- fx_hatch_bars() + ggplot2::labs(title = "T", caption = "C")
  expect_error(char_grid(crowded, width = 24, height = 10), class = "inferplotR_too_small")
})

# Skipped layers --------------------------------------------------------------

test_that("unsupported layers are skipped with one warning", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point()
  caught <- list()
  x <- withCallingHandlers(char_grid(p, width = 40, height = 14), warning = function(w) {
    caught[[length(caught) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  expect_length(caught, 1L)
  expect_s3_class(caught[[1]], "inferplotR_skipped_layer")
  expect_true(grepl("GeomBoxplot", conditionMessage(caught[[1]]), fixed = TRUE))
  expect_true(any(x$chars == "*"))
  both <- p + ggplot2::geom_violin()
  expect_warning(char_grid(both, width = 40, height = 14), class = "inferplotR_skipped_layer")
})

# Other handlers --------------------------------------------------------------

test_that("ribbons, smooths, steps, paths, ranges, reference lines and text draw", {
  d <- data.frame(x = 1:6, y = c(2, 4, 3, 5, 4, 6))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y))
  grid_of <- function(p) char_grid(p, width = 40, height = 14)$chars
  area <- grid_of(base + ggplot2::geom_area())
  expect_true(sum(area == "#") > 20)
  ribbon <- grid_of(base + ggplot2::geom_ribbon(ggplot2::aes(ymin = y - 1, ymax = y + 1)))
  expect_true(sum(ribbon == "#") > 20)
  smooth <- grid_of(base + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE))
  expect_true(any(smooth == "/"))
  expect_false(any(smooth == "#"))
  step <- grid_of(base + ggplot2::geom_step())
  expect_true(any(step == "|") && any(step[, 5:39] == "-"))
  path <- grid_of(ggplot2::ggplot(data.frame(x = c(1, 6, 1), y = c(1, 6, 6)),
                                  ggplot2::aes(x, y)) + ggplot2::geom_path())
  expect_true(any(path[, 5:39] == "-"))
  pr <- grid_of(base + ggplot2::geom_pointrange(ggplot2::aes(ymin = y - 1, ymax = y + 1)))
  expect_true(any(pr == "*") && any(pr[, 5:39] == "|"))
  lr <- grid_of(base + ggplot2::geom_linerange(ggplot2::aes(ymin = y - 1, ymax = y + 1)))
  expect_false(any(lr[1:11, 5:39] == "-"))
  hbar <- grid_of(base + ggplot2::geom_errorbar(ggplot2::aes(xmin = x - 0.4, xmax = x + 0.4),
                                                orientation = "y", width = 0.3))
  expect_true(any(hbar[1:11, 5:39] == "-"))
  vline <- grid_of(base + ggplot2::geom_point() + ggplot2::geom_vline(xintercept = 3.5))
  expect_true(sum(vline[, 5:39] == "|") >= 8)
  txt <- char_grid(base + ggplot2::geom_text(ggplot2::aes(label = "peak"),
                                             data = data.frame(x = 3.5, y = 4)),
                   width = 40, height = 14)
  expect_true(any(grepl("peak", format(txt), fixed = TRUE)))
  left <- char_grid(base + ggplot2::geom_label(ggplot2::aes(label = "ab"), hjust = "left",
                                               data = data.frame(x = 1, y = 2)),
                    width = 40, height = 14)
  expect_true(any(grepl("ab", format(left), fixed = TRUE)))
})

test_that("hjust strings map to numeric justification", {
  expect_identical(inferplotR:::.ip_cg_hjust(c("left", "right", "center", "0.25")),
                   c(0, 1, 0.5, 0.25))
  expect_identical(inferplotR:::.ip_cg_hjust(NULL), 0.5)
})

test_that("expression labels are deparsed and captions written", {
  p <- fx_bars() + ggplot2::labs(title = quote(alpha^2), caption = "Source: test")
  x <- format(char_grid(p, width = 40, height = 16))
  expect_identical(trimws(x[[1]]), "alpha^2")
  expect_identical(trimws(x[[16]]), "Source: test")
})

# Determinism and methods -----------------------------------------------------

test_that("the same input gives identical output", {
  expect_identical(char_grid(fx_lines()), char_grid(fx_lines()))
  expect_identical(char_grid(fx_hatch_bars(), charset = "unicode"),
                   char_grid(fx_hatch_bars(), charset = "unicode"))
})

test_that("format, as.character and print agree", {
  x <- char_grid(fx_means(), width = 48, height = 16)
  expect_s3_class(x, "inferplotR_chargrid")
  expect_identical(dim(x$chars), c(16L, 48L))
  out <- format(x)
  expect_type(out, "character")
  expect_length(out, 16L)
  expect_true(all(nchar(out, type = "chars") == 48L))
  expect_identical(as.character(x), out)
  u <- char_grid(fx_lines(), charset = "unicode")
  expect_true(all(nchar(format(u), type = "chars") == 64L))
  expect_output(res <- withVisible(print(x)))
  expect_false(res$visible)
  expect_identical(res$value, x)
})
