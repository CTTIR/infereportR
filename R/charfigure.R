#' Draw a character grid as a figure
#'
#' `char_figure()` turns a character grid from [char_grid()] into a ggplot made
#' of text: one glyph per non-blank cell, laid out on a fixed grid of cells that
#' are `cell_ratio` times as tall as they are wide. The result can be saved with
#' [save_figure()] for print or slides, at the size [char_size()] computes.
#'
#' Three styles are available:
#'
#' * `"bw"`: black glyphs on white paper and no border, the 1-bit figure style
#'   of the package (default grid 64 x 20, labels as they are).
#' * `"c64"`: the screen of a Commodore 64 after power-on, light-blue glyphs on
#'   a blue screen inside a light-blue border of 6 % of `width_mm` on every
#'   side (default grid 40 x 25, labels in upper case). The two colours are
#'   VIC-II "blue" and "light blue" of the Pepto palette by Philip Timmermann.
#'   This style is for effect only: it is not black and white, so
#'   [check_figure()] reports it under `two_values`.
#' * `"c64bw"`: the C64 screen in two values. The screen is always black with
#'   white glyphs; the border of 3.2 mm stands in for the lighter border blue
#'   with a dot halftone (ink dots on paper, 0.75 mm apart, radius 0.18 times
#'   the spacing). One column and 0.4 rows of padding separate the glyphs from
#'   the screen edge. The border is drawn in data space, so the screen keeps its
#'   aspect and the figure fills its slot.
#'
#' The c64 styles end with `READY.` and a cursor by default, as on the real
#' machine. The cursor is a filled tile of 0.9 x 0.8 cells, not a glyph, since
#' IBM Plex Mono has no block element.
#'
#' **Size.** `width_mm` is the width of the whole figure, border included. The
#' cell width is `(width_mm - 2 * border) / (ncol + 2 * padding columns)` and the
#' text size `cell width / char_width` mm, where `char_width` is the advance
#' width of the font in em (0.6 for IBM Plex Mono). With `slot`, a preset of
#' [figure_size()] such as `"slide_side"`, the figure takes the slot's width
#' and height: the number of rows (`READY.` rows included for the c64 styles)
#' is the largest that fits, `floor((slot height - 2 * border - padding rows)
#' / (cell width * cell_ratio))`, and the space left over (less than one row)
#' is split above and below the grid (for `"c64bw"` the halftone border takes
#' it). In a slot, the c64 styles default to 38 columns, which suits
#' `"slide_half"`.
#'
#' For the look of the real machine, install the C64 Pro Mono font (Style64; its
#' licence does not allow bundling it, so it is not part of this package), make
#' it known to the graphics device (for example through the 'systemfonts'
#' package for 'ragg' devices) and call
#' `char_figure(p, style = "c64", family = "C64 Pro Mono", char_width = 1, cell_ratio = 1)`.
#' Its glyphs are square and one em wide. Pass the same `cell_ratio` to
#' [char_size()].
#'
#' @param x A ggplot, converted with [char_grid()], or an `inferplotR_chargrid`
#'   object. With a chargrid, `width`, `height`, `charset` and `case` must not be
#'   given (error of class `inferplotR_invalid_input`).
#' @param style `"bw"` (default), `"c64"` or `"c64bw"`.
#' @param width,height Grid size in characters for [char_grid()]. `NULL` uses
#'   64 columns for `"bw"` and 40 for the c64 styles (38 in a slot), and 20
#'   rows for `"bw"` and 25 for the c64 styles. With `slot`, `height` must be
#'   `NULL`: the slot sets the rows.
#' @param charset `"ascii"` (default) or `"unicode"`, passed to [char_grid()].
#' @param case `"asis"` or `"upper"`, passed to [char_grid()]; `NULL` uses
#'   `"asis"` for `"bw"` and `"upper"` for the c64 styles.
#' @param family Font family of the glyphs; `NULL` uses `figure_font("mono")`.
#' @param width_mm Width of the whole figure in millimetres, border included.
#'   `NULL` means the slot width with `slot`, else 134 (the Bleisatz slide text
#'   width). Must be `NULL` when `slot` is given.
#' @param cell_ratio Height of a cell divided by its width, default 1.75 (IBM
#'   Plex Mono).
#' @param char_width Advance width of one glyph of `family` in em, default 0.6.
#' @param ready Append a `READY.` row and a cursor row? `NULL` means `TRUE` for
#'   the c64 styles and `FALSE` for `"bw"`.
#' @param slot `NULL` (default) or a preset name of [figure_size()]: the figure
#'   then fills that slot exactly (see Size).
#' @return A ggplot. Its data (`fig$data`) has one row per non-blank cell of the
#'   grid (columns `col`, `row` and `glyph`), drawn by a `geom_text()` layer.
#'   The cursor is a `geom_tile()` layer, and `"c64bw"` adds two `geom_rect()`
#'   layers below the text (halftone border, then screen). The coordinates are
#'   `coord_fixed(ratio = cell_ratio)` and the theme is `theme_void()` with the
#'   style's backgrounds.
#' @seealso [char_grid()] for the grid, [char_size()] for the matching file size,
#'   [save_figure()] to save it, [check_figure()] for the black-and-white audit.
#' @examples
#' d <- data.frame(hour = c(0, 2, 4, 8, 12, 24), level = c(9, 14, 19, 17, 13, 10))
#' p <- ggplot2::ggplot(d, ggplot2::aes(hour, level)) +
#'   ggplot2::geom_line() +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Level over time", x = "Hours", y = "Level")
#'
#' # 1-bit black and white
#' bw <- char_figure(p)
#' s <- char_size(p)
#' f <- tempfile(fileext = ".png")
#' save_figure(bw, f, width = s$width, height = s$height, dpi = 150)
#'
#' # Commodore 64 screen with READY. and cursor
#' c64 <- char_figure(p, style = "c64")
#' char_size(c64)
#'
#' # The same screen in two values, filling a half-width slide slot
#' half <- char_figure(p, style = "c64bw", slot = "slide_half")
#' char_size(half)
#' @export
char_figure <- function(x, style = c("bw", "c64", "c64bw"), width = NULL, height = NULL,
                        charset = "ascii", case = NULL, family = NULL, width_mm = NULL,
                        cell_ratio = 1.75, char_width = 0.6, ready = NULL, slot = NULL) {
  style <- .ip_check_choice(style, .ip_cf_styles, "style")
  .ip_cf_check_positive(cell_ratio, "cell_ratio")
  .ip_cf_check_positive(char_width, "char_width")
  if (is.null(ready)) ready <- !identical(style, "bw")
  .ip_check_flag(ready, "ready")
  if (is.null(family)) family <- figure_font("mono") else .ip_check_string(family, "family")
  box <- .ip_cf_box(width_mm, slot)
  if (!is.null(slot) && !is.null(height)) {
    .ip_abort("height must not be given together with slot; the slot sets the rows")
  }

  if (inherits(x, "inferplotR_chargrid")) {
    given <- c(width = !is.null(width), height = !is.null(height),
               charset = !missing(charset), case = !is.null(case))
    if (any(given)) {
      .ip_abort(sprintf("%s must not be given when x is a chargrid",
                        paste(names(given)[given], collapse = ", ")))
    }
    grid <- x
  } else {
    .ip_check_plot(x, "x")
    grid <- .ip_cf_grid(x, style, width, height, charset, case, box, cell_ratio, ready)
  }

  m <- grid$chars
  if (ready) m <- .ip_cf_ready(m)
  geo <- .ip_cf_geometry(style, ncol(m), nrow(m), box$width, cell_ratio, box$height)

  keep <- m != " " & !is.na(m)
  cells <- data.frame(col = col(m)[keep], row = row(m)[keep], glyph = m[keep],
                      stringsAsFactors = FALSE)
  cells <- cells[order(cells$row, cells$col), , drop = FALSE]
  rownames(cells) <- NULL
  # char_size() reads the figure's size from here.
  attr(cells, "inferplotR_char_size") <- c(width = geo$width, height = geo$height)

  pal <- .ip_cf_palette(style)
  size <- geo$cw / char_width
  mapping <- do.call(ggplot2::aes, list(x = as.name("col"), y = call("-", as.name("row")),
                                        label = as.name("glyph")))
  fig <- ggplot2::ggplot(cells, mapping)
  if (identical(style, "c64bw")) fig <- fig + .ip_cf_c64bw_layers(geo, pal)
  if (ready) fig <- fig + .ip_cf_cursor(nrow(m), pal$ink)
  fig <- fig + ggplot2::geom_text(colour = pal$ink, family = family, size = size)
  fig +
    ggplot2::coord_fixed(ratio = cell_ratio, xlim = geo$xlim, ylim = geo$ylim, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = pal$border, colour = NA),
      panel.background = ggplot2::element_rect(fill = pal$panel, colour = NA),
      plot.margin = ggplot2::margin(geo$margin[[1L]], geo$margin[[2L]], geo$margin[[1L]],
                                    geo$margin[[2L]], unit = "mm")
    )
}

#' File size for a character figure
#'
#' `char_size()` gives the width and height at which a [char_figure()] is saved
#' without distortion. It follows the geometry of [char_figure()]: `width_mm`
#' is the whole width, border included; each cell is
#' `(width_mm - 2 * border) / (ncol + 2 * padding columns)` wide and
#' `cell_ratio` times as tall; the height is the rows (with any `READY.` rows)
#' plus the padding rows times the cell height, plus the border above and
#' below. With `slot`, the size is the slot's.
#'
#' The grid size is read from `x`:
#'
#' * a [char_figure()] result: its own size, as drawn (the other arguments are
#'   ignored);
#' * an `inferplotR_chargrid`: its `width` and `height`, plus the `READY.` rows
#'   when `ready` is `TRUE`;
#' * a ggplot: the grid [char_grid()] would draw with the defaults of
#'   [char_figure()] for `style` and `slot`.
#'
#' Pass the same `style`, `width_mm`, `cell_ratio`, `slot` and `ready` as to
#' [char_figure()].
#'
#' @param x A ggplot, an `inferplotR_chargrid` or a [char_figure()] result.
#' @inheritParams char_figure
#' @return A list with `width` and `height` (numbers, millimetres) and
#'   `units = "mm"`, ready for `save_figure(width = , height = )`.
#' @seealso [char_figure()], [save_figure()], [figure_size()] for the presets.
#' @examples
#' p <- ggplot2::ggplot(data.frame(x = 1:5, y = c(2, 4, 3, 5, 4)),
#'                      ggplot2::aes(x, y)) + ggplot2::geom_line()
#' char_size(p)
#' char_size(p, style = "c64")
#' char_size(p, style = "c64bw", slot = "slide_half")
#' char_size(char_grid(p, width = 32, height = 10), width_mm = 86)
#' @export
char_size <- function(x, width_mm = NULL, cell_ratio = 1.75, style = c("bw", "c64", "c64bw"),
                      slot = NULL, ready = NULL) {
  style <- .ip_check_choice(style, .ip_cf_styles, "style")
  .ip_cf_check_positive(cell_ratio, "cell_ratio")
  if (is.null(ready)) ready <- !identical(style, "bw")
  .ip_check_flag(ready, "ready")
  box <- .ip_cf_box(width_mm, slot)

  if (inherits(x, "inferplotR_chargrid")) {
    ncol <- x$width
    nrow <- x$height
  } else {
    .ip_check_plot(x, "x")
    own <- .ip_cf_own_size(x)
    if (!is.null(own)) {
      return(list(width = own[["width"]], height = own[["height"]], units = "mm"))
    }
    # Only the size matters here; char_figure() reports skipped layers itself.
    grid <- withCallingHandlers(
      .ip_cf_grid(x, style, NULL, NULL, "ascii", NULL, box, cell_ratio, ready),
      inferplotR_skipped_layer = function(w) invokeRestart("muffleWarning"),
      inferplotR_transliterated = function(w) invokeRestart("muffleWarning")
    )
    ncol <- grid$width
    nrow <- grid$height
  }
  if (ready) nrow <- nrow + .ip_cf_ready_rows
  geo <- .ip_cf_geometry(style, ncol, nrow, box$width, cell_ratio, box$height)
  list(width = geo$width, height = geo$height, units = "mm")
}

.ip_cf_styles <- c("bw", "c64", "c64bw")
.ip_cf_ready_rows <- 2L
# c64bw: border width in mm and inner padding in cells.
.ip_cf_c64bw_border <- 3.2
.ip_cf_c64bw_pad <- c(x = 1, y = 0.4)
# c64: the border is this share of the figure width on every side.
.ip_cf_c64_border <- 0.06

.ip_cf_check_positive <- function(x, arg) {
  .ip_check_number(x, arg, min = 0)
  if (x <= 0) .ip_abort(sprintf("%s must be a positive number", arg))
  invisible(x)
}

# Figure width and (with a slot) height in mm.
.ip_cf_box <- function(width_mm, slot) {
  if (is.null(slot)) {
    if (is.null(width_mm)) width_mm <- 134
    .ip_cf_check_positive(width_mm, "width_mm")
    return(list(width = width_mm, height = NULL))
  }
  slot <- .ip_check_choice(slot, .ip_presets$preset, "slot")
  if (!is.null(width_mm)) {
    .ip_abort("width_mm must not be given together with slot; the slot sets the width")
  }
  i <- match(slot, .ip_presets$preset)
  list(width = .ip_presets$width[[i]], height = .ip_presets$height[[i]])
}

# Border (mm) and padding (cells) of a style.
.ip_cf_frame <- function(style, width_mm) {
  switch(style,
    bw = list(border = 0, pad = c(x = 0, y = 0)),
    c64 = list(border = .ip_cf_c64_border * width_mm, pad = c(x = 0, y = 0)),
    c64bw = list(border = .ip_cf_c64bw_border, pad = .ip_cf_c64bw_pad)
  )
}

# Cell width in mm for ncol columns across width_mm.
.ip_cf_cell_width <- function(style, ncol, width_mm) {
  fr <- .ip_cf_frame(style, width_mm)
  (width_mm - 2 * fr$border) / (ncol + 2 * fr$pad[["x"]])
}

# Rows (READY rows included) that fit height_mm: SPEC amendment 01, D8.
.ip_cf_slot_rows <- function(style, ncol, width_mm, height_mm, cell_ratio) {
  fr <- .ip_cf_frame(style, width_mm)
  ch <- .ip_cf_cell_width(style, ncol, width_mm) * cell_ratio
  # The tolerance keeps an exact fit from losing a row to floating point.
  as.integer(floor((height_mm - 2 * fr$border - 2 * fr$pad[["y"]] * ch) / ch + 1e-9))
}

# The whole geometry of a figure of ncol x nrow cells (READY rows included).
# Without height_mm the figure is as tall as its content; with it, the content
# is centred and the space left over goes to the margin (bw, c64) or to the
# halftone border (c64bw).
.ip_cf_geometry <- function(style, ncol, nrow, width_mm, cell_ratio, height_mm = NULL) {
  fr <- .ip_cf_frame(style, width_mm)
  cw <- .ip_cf_cell_width(style, ncol, width_mm)
  if (!is.finite(cw) || cw <= 0) {
    .ip_abort(sprintf("width_mm = %s leaves no room for the grid", format(width_mm)),
              code = "too_small")
  }
  ch <- cw * cell_ratio
  natural <- (nrow + 2 * fr$pad[["y"]]) * ch + 2 * fr$border
  if (is.null(height_mm)) height_mm <- natural
  extra <- (height_mm - natural) / 2
  if (extra < -1e-9) {
    .ip_abort(sprintf("%d rows need %.1f mm, more than the %.1f mm of the slot", nrow, natural,
                      height_mm), code = "too_small")
  }
  extra <- max(extra, 0)
  sx <- c(0.5 - fr$pad[["x"]], ncol + 0.5 + fr$pad[["x"]])
  sy <- c(-nrow - 0.5 - fr$pad[["y"]], -0.5 + fr$pad[["y"]])
  geo <- list(cw = cw, ch = ch, border = fr$border, extra = extra, width = width_mm,
              height = height_mm, screen_x = sx, screen_y = sy)
  if (identical(style, "c64bw")) {
    bx <- fr$border / cw
    by <- (fr$border + extra) / ch
    geo$xlim <- sx + c(-bx, bx)
    geo$ylim <- sy + c(-by, by)
    geo$margin <- c(0, 0)
  } else {
    geo$xlim <- sx
    geo$ylim <- sy
    # top/bottom, left/right
    geo$margin <- c(fr$border + extra, fr$border)
  }
  geo
}

# Colours of a style. P13: thread mode and transparent into .ip_mode() here; bw
# and the c64bw border follow the mode, the c64 colours and the c64bw screen
# do not.
.ip_cf_palette <- function(style) {
  md <- .ip_mode()
  switch(style,
    bw = list(ink = md$ink, panel = md$knock, border = md$knock),
    c64 = list(ink = .ip_c64[["foreground"]], panel = .ip_c64[["background"]],
               border = .ip_c64[["foreground"]]),
    c64bw = list(ink = .ip_screen[["foreground"]], panel = NA_character_, border = md$knock,
                 screen = .ip_screen[["background"]], dots = md$ink, paper = md$paper)
  )
}

# The c64bw border: ink dots on paper, .ip_halftone_spacing apart, radius 0.18
# times the spacing. Built here rather than with hatch_pattern("dots") because
# its colours follow the mode.
.ip_cf_halftone <- function(dots, paper, spacing = .ip_halftone_spacing) {
  side <- grid::unit(spacing, "mm")
  grid::pattern(.ip_cf_halftone_tile(dots, paper, spacing), width = side, height = side,
                extend = "repeat")
}

# One tile of the halftone: a paper square with an ink dot in its centre.
.ip_cf_halftone_tile <- function(dots, paper, spacing) {
  side <- grid::unit(spacing, "mm")
  centre <- grid::unit(0.5, "npc")
  background <- grid::rectGrob(centre, centre, side, side,
                               gp = grid::gpar(col = NA, fill = paper))
  dot <- grid::circleGrob(centre, centre, r = grid::unit(0.18 * spacing, "mm"),
                          gp = grid::gpar(col = NA, fill = dots))
  grid::grobTree(background, dot)
}

.ip_cf_c64bw_layers <- function(geo, pal) {
  list(
    ggplot2::annotate("rect", xmin = geo$xlim[[1L]], xmax = geo$xlim[[2L]],
                      ymin = geo$ylim[[1L]], ymax = geo$ylim[[2L]],
                      fill = list(.ip_cf_halftone(pal$dots, pal$paper)), colour = NA),
    ggplot2::annotate("rect", xmin = geo$screen_x[[1L]], xmax = geo$screen_x[[2L]],
                      ymin = geo$screen_y[[1L]], ymax = geo$screen_y[[2L]],
                      fill = pal$screen, colour = NA)
  )
}

# The cursor: a filled tile of 0.9 x 0.8 cells in the first column of the last
# row, since IBM Plex Mono has no block glyph.
.ip_cf_cursor <- function(row, ink) {
  d <- data.frame(x = 1, y = -row)
  mapping <- do.call(ggplot2::aes, list(x = as.name("x"), y = as.name("y")))
  ggplot2::geom_tile(mapping, data = d, inherit.aes = FALSE, width = 0.9, height = 0.8,
                     fill = ink, colour = NA)
}

.ip_cf_grid <- function(plot, style, width, height, charset, case, box, cell_ratio, ready) {
  c64 <- !identical(style, "bw")
  if (is.null(width)) width <- if (!c64) 64L else if (is.null(box$height)) 40L else 38L
  if (is.null(height)) {
    height <- if (is.null(box$height)) {
      if (c64) 25L else 20L
    } else {
      rows <- .ip_cf_slot_rows(style, width, box$width, box$height, cell_ratio) -
        if (ready) .ip_cf_ready_rows else 0L
      if (rows < 10L) {
        .ip_abort(sprintf(
          "the slot leaves %d grid rows for %d columns, fewer than 10; use more columns",
          rows, as.integer(width)), code = "too_small")
      }
      rows
    }
  }
  if (is.null(case)) case <- if (c64) "upper" else "asis"
  char_grid(plot, width = width, height = height, charset = charset, case = case)
}

# Appends the two rows of the C64 prompt below the grid: READY. flush left and
# an empty row where the cursor tile is drawn.
.ip_cf_ready <- function(m) {
  prompt <- matrix(" ", nrow = .ip_cf_ready_rows, ncol = ncol(m))
  word <- strsplit("READY.", "", fixed = TRUE)[[1L]]
  n <- min(length(word), ncol(m))
  prompt[1L, seq_len(n)] <- word[seq_len(n)]
  rbind(m, prompt)
}

# The size char_figure() stored on a figure, or NULL for any other ggplot.
.ip_cf_own_size <- function(plot) {
  data <- plot$data
  if (!is.data.frame(data)) return(NULL)
  size <- attr(data, "inferplotR_char_size", exact = TRUE)
  if (is.null(size) || !all(c("col", "row", "glyph") %in% names(data))) return(NULL)
  size
}
