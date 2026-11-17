cf_plot <- function() {
  d <- data.frame(hour = c(0, 2, 4, 8, 12, 24), level = c(9, 14, 19, 17, 13, 10))
  ggplot2::ggplot(d, ggplot2::aes(hour, level)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Level over time", x = "Hours", y = "Level")
}

# Grid size of a bw or c64 figure (no padding, so the limits span the grid).
cf_dims <- function(fig) {
  lim <- fig$coordinates$limits
  c(ncol = diff(lim$x), nrow = diff(lim$y))
}

cf_margin_mm <- function(fig) {
  as.numeric(grid::convertUnit(fig$theme$plot.margin, "mm", valueOnly = TRUE))
}

cf_fill <- function(fig, element) ggplot2::calc_element(element, fig$theme)$fill

# Layer indices by geom class; GeomTile inherits GeomRect, so exclude it there.
cf_layer <- function(fig, geom, exclude = character()) {
  which(vapply(fig$layers, function(l) {
    inherits(l$geom, geom) && !inherits(l$geom, exclude)
  }, logical(1)))
}

cf_text_data <- function(fig) layer_data_of(fig, cf_layer(fig, "GeomText"))

# Non-blank rows of the figure's text, bottom last, as strings.
cf_rows <- function(fig) {
  d <- fig$data
  rows <- sort(unique(d$row))
  vapply(rows, function(r) {
    s <- d[d$row == r, , drop = FALSE]
    line <- rep(" ", max(s$col))
    line[s$col] <- s$glyph
    sub(" +$", "", paste(line, collapse = ""))
  }, "")
}

two_values <- function(fig) {
  res <- check_figure(fig)
  res$status[res$check == "two_values"]
}

# Tokens and defaults ---------------------------------------------------------

test_that("the C64 tokens are the Pepto values and the screen is black and white", {
  expect_identical(toupper(.ip_c64[["background"]]), "#352879")
  expect_identical(toupper(.ip_c64[["foreground"]]), "#6C5EB5")
  expect_identical(toupper(unname(.ip_screen)), c("#000000", "#FFFFFF"))
})

test_that("cell_ratio and char_width default to the IBM Plex Mono values", {
  expect_identical(formals(char_figure)$cell_ratio, 1.75)
  expect_identical(formals(char_figure)$char_width, 0.6)
  expect_identical(formals(char_size)$cell_ratio, 1.75)
  expect_identical(eval(formals(char_figure)$style), c("bw", "c64", "c64bw"))
})

# Structure -------------------------------------------------------------------

test_that("char_figure() returns a ggplot of one text layer with one row per non-blank cell", {
  p <- cf_plot()
  g <- char_grid(p)
  fig <- char_figure(p)
  expect_true(inherits(fig, "ggplot"))
  expect_length(fig$layers, 1L)
  expect_true(inherits(fig$layers[[1]]$geom, "GeomText"))
  expect_identical(names(fig$data), c("col", "row", "glyph"))
  expect_identical(nrow(fig$data), sum(g$chars != " "))
  expect_identical(fig$data$glyph, g$chars[cbind(fig$data$row, fig$data$col)])
  expect_false(any(fig$data$glyph == " "))
})

test_that("the bw figure is ink on paper without margin", {
  fig <- char_figure(cf_plot())
  built <- cf_text_data(fig)
  expect_identical(unique(built$colour), .ip_mode()$ink)
  expect_identical(cf_fill(fig, "panel.background"), .ip_paper)
  expect_identical(cf_fill(fig, "plot.background"), .ip_paper)
  expect_equal(cf_margin_mm(fig), rep(0, 4))
})

test_that("the c64 figure is light blue on blue inside a 6 % light-blue border", {
  fig <- char_figure(cf_plot(), style = "c64", width_mm = 100)
  built <- cf_text_data(fig)
  expect_identical(toupper(unique(built$colour)), "#6C5EB5")
  expect_identical(toupper(cf_fill(fig, "panel.background")), "#352879")
  expect_identical(toupper(cf_fill(fig, "plot.background")), "#6C5EB5")
  expect_identical(unique(built$colour), .ip_c64[["foreground"]])
  expect_equal(cf_margin_mm(fig), rep(6, 4))
  # 40 columns across the width inside the border
  expect_equal(unique(built$size), ((100 - 12) / 40) / 0.6)
})

test_that("text size fills one cell per glyph", {
  fig <- char_figure(cf_plot(), width_mm = 128, char_width = 0.5)
  expect_equal(unique(cf_text_data(fig)$size), (128 / 64) / 0.5)
})

test_that("coordinate limits span the grid at the cell ratio", {
  fig <- char_figure(cf_plot(), width = 30, height = 12, cell_ratio = 1.5)
  expect_true(inherits(fig$coordinates, "CoordCartesian"))
  expect_equal(fig$coordinates$ratio, 1.5)
  expect_equal(fig$coordinates$limits$x, c(0.5, 30.5))
  expect_equal(fig$coordinates$limits$y, c(-12.5, -0.5))
})

# c64bw -----------------------------------------------------------------------

test_that("c64bw draws white glyphs on a black screen inside a halftone border", {
  fig <- char_figure(cf_plot(), style = "c64bw", width = 40, height = 20, width_mm = 64)
  rects <- cf_layer(fig, "GeomRect", exclude = "GeomTile")
  expect_length(rects, 2L)
  expect_true(all(rects < cf_layer(fig, "GeomText")))
  border <- layer_data_of(fig, rects[[1]])
  screen <- layer_data_of(fig, rects[[2]])
  expect_true(inherits(border$fill[[1]], "GridPattern"))
  expect_identical(screen$fill, .ip_screen[["background"]])
  expect_identical(unique(cf_text_data(fig)$colour), .ip_screen[["foreground"]])
  expect_identical(cf_fill(fig, "plot.background"), .ip_mode()$paper)
  expect_equal(cf_margin_mm(fig), rep(0, 4))
  # the border fills the coordinate limits; the screen sits inside with padding
  lim <- fig$coordinates$limits
  expect_equal(c(border$xmin, border$xmax), lim$x)
  expect_equal(c(border$ymin, border$ymax), lim$y)
  # 22 rows: the grid plus READY. and the cursor row
  expect_equal(c(screen$xmin, screen$xmax), c(0.5 - 1, 40.5 + 1))
  expect_equal(c(screen$ymin, screen$ymax), c(-22.5 - 0.4, -0.5 + 0.4))
  # 3.2 mm of border on each side, in data units of the cell size
  cw <- (64 - 2 * 3.2) / (40 + 2)
  expect_equal((screen$xmin - lim$x[[1]]) * cw, 3.2)
  expect_equal((lim$y[[2]] - screen$ymax) * cw * 1.75, 3.2)
  expect_equal(unique(cf_text_data(fig)$size), cw / 0.6)
})

test_that("the c64bw halftone is ink dots on paper at the halftone spacing", {
  md <- .ip_mode()
  pat <- .ip_cf_halftone(md$ink, md$paper)
  expect_s3_class(pat, "GridTilingPattern")
  expect_equal(grid::convertUnit(pat$width, "mm", valueOnly = TRUE), .ip_halftone_spacing)
  expect_equal(grid::convertUnit(pat$height, "mm", valueOnly = TRUE), 0.75)
  kids <- .ip_cf_halftone_tile(md$ink, md$paper, .ip_halftone_spacing)$children
  expect_identical(kids[[1]]$gp$fill, md$paper)
  expect_identical(kids[[2]]$gp$fill, md$ink)
  expect_equal(grid::convertUnit(kids[[2]]$r, "mm", valueOnly = TRUE), 0.18 * 0.75)
})

test_that("c64bw defaults to upper case, READY. and the C64 grid", {
  fig <- char_figure(cf_plot(), style = "c64bw")
  rows <- cf_rows(fig)
  expect_identical(rows[[1]], "LEVEL OVER TIME")
  expect_identical(utils::tail(rows, 1L), "READY.")
  expect_identical(max(fig$data$col) <= 40L, TRUE)
  expect_identical(max(fig$data$row), 26L)
})

# Cursor ----------------------------------------------------------------------

test_that("the cursor is a filled 0.9 x 0.8 tile, not a glyph", {
  p <- cf_plot()
  for (style in c("bw", "c64", "c64bw")) {
    fig <- char_figure(p, style = style, ready = TRUE)
    tile <- cf_layer(fig, "GeomTile")
    expect_length(tile, 1L)
    d <- layer_data_of(fig, tile)
    nrow <- if (identical(style, "bw")) 22 else 27
    expect_equal(c(d$x, d$y), c(1, -nrow))
    expect_equal(c(d$xmax - d$xmin, d$ymax - d$ymin), c(0.9, 0.8))
    expect_identical(d$fill, unique(cf_text_data(fig)$colour))
    expect_true(is.na(d$colour))
    expect_false(any(fig$data$row == nrow))
    expect_false(any(fig$data$glyph %in% c("_", "█")))
  }
  expect_length(cf_layer(char_figure(p, style = "c64", ready = FALSE), "GeomTile"), 0L)
})

test_that("ready defaults to TRUE for the c64 styles and FALSE for bw", {
  p <- cf_plot()
  expect_identical(cf_dims(char_figure(p))[["nrow"]], 20)
  expect_identical(cf_dims(char_figure(p, style = "c64"))[["nrow"]], 27)
  expect_identical(cf_dims(char_figure(p, style = "c64", ready = FALSE))[["nrow"]], 25)
  expect_false(identical(utils::tail(cf_rows(char_figure(p)), 1L), "READY."))
})

test_that("ready = TRUE ends with READY. above the cursor row", {
  p <- cf_plot()
  fig <- char_figure(p, ready = TRUE)
  expect_identical(utils::tail(cf_rows(fig), 1L), "READY.")
  expect_identical(max(fig$data$row), 21L)
  expect_identical(cf_dims(fig)[["nrow"]], 22)
  uni <- char_figure(p, style = "c64", charset = "unicode")
  expect_identical(utils::tail(cf_rows(uni), 1L), "READY.")
})

test_that("c64 defaults are a 40 x 25 grid in upper case", {
  p <- cf_plot()
  fig <- char_figure(p, style = "c64", ready = FALSE)
  expect_identical(cf_dims(fig), c(ncol = 40, nrow = 25))
  expect_identical(cf_rows(fig)[[1]], "LEVEL OVER TIME")
  expect_identical(cf_dims(char_figure(p)), c(ncol = 64, nrow = 20))
  expect_identical(cf_rows(char_figure(p))[[1]], "Level over time")
})

# Slots -----------------------------------------------------------------------

test_that("a slot sets the rows by the D8 formula, READY rows included", {
  p <- cf_plot()
  rows_for <- function(style, ncol, w, h) {
    border <- switch(style, bw = 0, c64 = 0.06 * w, c64bw = 3.2)
    padx <- if (identical(style, "c64bw")) 1 else 0
    pady <- if (identical(style, "c64bw")) 0.4 else 0
    cw <- (w - 2 * border) / (ncol + 2 * padx)
    floor((h - 2 * border - 2 * pady * cw * 1.75) / (cw * 1.75))
  }
  bw <- char_figure(p, slot = "slide_side")
  expect_identical(cf_dims(bw), c(ncol = 64, nrow = rows_for("bw", 64, 86, 44)))
  c64 <- char_figure(p, style = "c64", slot = "slide_half")
  expect_identical(cf_dims(c64), c(ncol = 38, nrow = rows_for("c64", 38, 64, 44)))
  expect_identical(utils::tail(cf_rows(c64), 1L), "READY.")
  c64bw <- char_figure(p, style = "c64bw", slot = "slide_half")
  n <- rows_for("c64bw", 38, 64, 44)
  expect_identical(n, 14)
  expect_identical(max(c64bw$data$row), as.integer(n) - 1L)
  expect_identical(max(c64bw$data$col) <= 38L, TRUE)
  wide <- char_figure(p, width = 64, slot = "slide_full")
  expect_identical(cf_dims(wide), c(ncol = 64, nrow = rows_for("bw", 64, 134, 44)))
})

test_that("a slot figure fills the slot exactly", {
  p <- cf_plot()
  cases <- list(c("bw", "slide_side"), c("bw", "slide_half"), c("bw", "slide_full"),
                c("c64", "slide_half"), c("c64bw", "slide_half"), c("bw", "journal_column"))
  for (case in cases) {
    style <- case[[1]]
    slot <- case[[2]]
    fig <- char_figure(p, style = style, slot = slot)
    wh <- unlist(figure_size(slot)[c("width", "height")])
    expect_equal(unlist(char_size(fig)[c("width", "height")]), wh)
    lim <- fig$coordinates$limits
    margin <- cf_margin_mm(fig)
    cw <- unique(cf_text_data(fig)$size) * 0.6
    # panel plus margins is the slot, at the coordinate aspect
    expect_equal(diff(lim$x) * cw + margin[[2]] + margin[[4]], wh[[1]])
    expect_equal(diff(lim$y) * cw * 1.75 + margin[[1]] + margin[[3]], wh[[2]])
  }
})

test_that("slot conflicts and misfits raise classed errors", {
  p <- cf_plot()
  expect_error(char_figure(p, slot = "slide_huge"), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, slot = "slide_side", height = 12),
               class = "inferplotR_invalid_input")
  expect_error(char_figure(p, slot = "slide_side", width_mm = 86),
               class = "inferplotR_invalid_input")
  expect_error(char_size(p, slot = "slide_side", width_mm = 86),
               class = "inferplotR_invalid_input")
  # 38 c64 columns leave too few rows in the side slot
  expect_error(char_figure(p, style = "c64", slot = "slide_side"), class = "inferplotR_too_small")
  expect_error(char_size(p, style = "c64", slot = "slide_side"), class = "inferplotR_too_small")
  expect_no_error(char_figure(p, style = "c64", width = 60, slot = "slide_side"))
  tall <- char_grid(p, width = 30, height = 40)
  expect_error(char_figure(tall, slot = "slide_half"), class = "inferplotR_too_small")
  expect_error(char_size(tall, slot = "slide_half"), class = "inferplotR_too_small")
})

# Arguments -------------------------------------------------------------------

test_that("a chargrid is drawn as is and rejects grid arguments", {
  g <- char_grid(cf_plot(), width = 32, height = 12)
  fig <- char_figure(g)
  expect_identical(cf_dims(fig), c(ncol = 32, nrow = 12))
  expect_error(char_figure(g, width = 40), class = "inferplotR_invalid_input")
  expect_error(char_figure(g, height = 20), class = "inferplotR_invalid_input")
  expect_error(char_figure(g, charset = "unicode"), class = "inferplotR_invalid_input")
  expect_error(char_figure(g, case = "upper"), class = "inferplotR_invalid_input")
})

test_that("char_figure() validates its arguments", {
  p <- cf_plot()
  expect_error(char_figure(1:3), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, style = "amiga"), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, width_mm = 0), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, cell_ratio = -1), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, char_width = NA), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, ready = "yes"), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, family = ""), class = "inferplotR_invalid_input")
  expect_error(char_figure(p, style = "c64bw", width_mm = 6), class = "inferplotR_too_small")
  expect_error(char_size(p, width_mm = -5), class = "inferplotR_invalid_input")
  expect_error(char_size(p, style = "vic20"), class = "inferplotR_invalid_input")
  expect_error(char_size("x"), class = "inferplotR_invalid_input")
})

test_that("char_figure() passes skipped-layer warnings on", {
  p <- ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) + ggplot2::geom_boxplot()
  expect_warning(char_figure(p), class = "inferplotR_skipped_layer")
  expect_no_warning(char_size(p))
})

# char_size() -----------------------------------------------------------------

test_that("char_size() agrees across a ggplot, a chargrid and a figure (bw)", {
  p <- cf_plot()
  expected <- 20 * (134 / 64) * 1.75
  s_plot <- char_size(p)
  s_grid <- char_size(char_grid(p))
  s_fig <- char_size(char_figure(p))
  expect_equal(s_plot, list(width = 134, height = expected, units = "mm"))
  expect_equal(s_grid, s_plot)
  expect_equal(s_fig, s_plot)
})

test_that("char_size() agrees across a ggplot, a chargrid and a figure (c64)", {
  p <- cf_plot()
  w <- 100
  ratio <- 1.5
  border <- 0.06 * w
  expected <- list(width = w, height = 27 * ((w - 2 * border) / 40) * ratio + 2 * border,
                   units = "mm")
  s_plot <- char_size(p, width_mm = w, cell_ratio = ratio, style = "c64")
  s_grid <- char_size(char_grid(p, width = 40, height = 25), width_mm = w,
                      cell_ratio = ratio, style = "c64")
  s_fig <- char_size(char_figure(p, style = "c64", width_mm = w, cell_ratio = ratio))
  expect_equal(s_plot, expected)
  expect_equal(s_grid, expected)
  expect_equal(s_fig, expected)
})

test_that("char_size() agrees across a ggplot, a chargrid and a figure (c64bw)", {
  p <- cf_plot()
  cw <- (86 - 6.4) / 42
  expected <- list(width = 86, height = (27 + 0.8) * cw * 1.75 + 6.4, units = "mm")
  s_plot <- char_size(p, width_mm = 86, style = "c64bw")
  s_grid <- char_size(char_grid(p, width = 40, height = 25), width_mm = 86, style = "c64bw")
  s_fig <- char_size(char_figure(p, style = "c64bw", width_mm = 86))
  expect_equal(s_plot, expected)
  expect_equal(s_grid, expected)
  expect_equal(s_fig, expected)
})

test_that("char_size() agrees across input types in a slot", {
  p <- cf_plot()
  for (style in c("bw", "c64", "c64bw")) {
    fig <- char_figure(p, style = style, slot = "slide_half")
    g <- char_grid(p, width = if (identical(style, "bw")) 64 else 38, height = 10)
    expected <- list(width = 64, height = 44, units = "mm")
    expect_equal(char_size(fig), expected)
    expect_equal(char_size(p, style = style, slot = "slide_half"), expected)
    expect_equal(char_size(g, style = style, slot = "slide_half"), expected)
  }
})

test_that("a figure without READY rows is sized from its own geometry", {
  fig <- char_figure(cf_plot(), style = "c64", ready = FALSE)
  s <- char_size(fig, style = "bw")
  expect_equal(s$height, 25 * ((134 - 2 * 0.06 * 134) / 40) * 1.75 + 2 * 0.06 * 134)
  expect_equal(char_size(char_grid(cf_plot(), width = 40, height = 25), style = "c64",
                         ready = FALSE), s)
})

# Drawing and audit -----------------------------------------------------------

test_that("a figure saved at char_size() is written", {
  skip_if_not(cairo_pdf_works(), "cairo_pdf() writes no file here")
  p <- cf_plot()
  s <- char_size(p)
  tmp_pdf <- withr::local_tempfile(fileext = ".pdf")
  save_figure(char_figure(p), tmp_pdf, width = s$width, height = s$height)
  expect_true(file.exists(tmp_pdf))
  expect_gt(file.size(tmp_pdf), 0)
})

test_that("every style draws without warnings", {
  p <- cf_plot()
  expect_no_warning(draws_silently(char_figure(p)))
  expect_no_warning(draws_silently(char_figure(p, style = "c64")))
  expect_no_warning(draws_silently(char_figure(p, style = "c64bw", slot = "slide_half")))
})

test_that("bw and c64bw pass the two_values check; c64 is the documented exception", {
  p <- cf_plot()
  expect_identical(two_values(char_figure(p)), "pass")
  expect_identical(two_values(char_figure(p, ready = TRUE)), "pass")
  expect_identical(two_values(char_figure(p, style = "c64bw")), "pass")
  expect_identical(two_values(char_figure(p, style = "c64bw", slot = "slide_half")), "pass")
  expect_identical(two_values(char_figure(p, style = "c64")), "fail")
})
