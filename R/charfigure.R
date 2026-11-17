#' Draw a character grid as a figure
#'
#' `char_figure()` turns a character grid from [char_grid()] into a ggplot made
#' only of text: one glyph per non-blank cell, laid out on a fixed grid of cells
#' that are `cell_ratio` times as tall as they are wide. The result can be saved
#' with [save_figure()] for print or slides, at the size [char_size()] computes.
#'
#' Two styles are available:
#'
#' * `"bw"`: black glyphs on white paper and no margin, the 1-bit figure style of
#'   the package (default grid 64 x 20, labels as they are).
#' * `"c64"`: the screen of a Commodore 64 after power-on, light-blue glyphs on a
#'   blue panel inside a light-blue border of 8 % of `width_mm` on every side
#'   (default grid 40 x 25, labels in upper case). By default the screen ends
#'   with `READY.` and a cursor, as on the real machine. The two colours are
#'   VIC-II "blue" and "light blue" of the Pepto palette by Philip Timmermann.
#'
#' The text size is chosen so that one glyph advance fills one cell:
#' `(width_mm / ncol) / char_width` mm, where `char_width` is the advance width
#' of the font in em. The default 0.6 fits IBM Plex Mono and DejaVu Sans Mono.
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
#' @param style `"bw"` (default) or `"c64"`.
#' @param width,height Grid size in characters for [char_grid()]; `NULL` uses 64 x
#'   20 for `"bw"` and 40 x 25 for `"c64"`.
#' @param charset `"ascii"` (default) or `"unicode"`, passed to [char_grid()].
#' @param case `"asis"` or `"upper"`, passed to [char_grid()]; `NULL` uses
#'   `"asis"` for `"bw"` and `"upper"` for `"c64"`.
#' @param family Font family of the glyphs; `NULL` uses `figure_font("mono")`.
#' @param width_mm Width of the grid in millimetres (without the c64 border),
#'   default 134, the Bleisatz slide text width.
#' @param cell_ratio Height of a cell divided by its width, default 2.
#' @param char_width Advance width of one glyph of `family` in em, default 0.6.
#' @param ready Append a `READY.` row and a cursor row (`_` for ascii, a full block
#'   for unicode)? `NULL` means `TRUE` for `"c64"` and `FALSE` for `"bw"`.
#' @return A ggplot with one `geom_text()` layer whose data has one row per
#'   non-blank cell (columns `col`, `row` and `glyph`), `coord_fixed()` limits
#'   that span the grid exactly and `theme_void()` with the style's backgrounds.
#' @seealso [char_grid()] for the grid, [char_size()] for the matching file size,
#'   [save_figure()] to save it, `check_figure()` for the black-and-white audit.
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
#' char_size(c64, style = "c64")
#' @export
char_figure <- function(x, style = c("bw", "c64"), width = NULL, height = NULL,
                        charset = "ascii", case = NULL, family = NULL, width_mm = 134,
                        cell_ratio = 2, char_width = 0.6, ready = NULL) {
  style <- .ip_check_choice(style, c("bw", "c64"), "style")
  .ip_cf_check_positive(width_mm, "width_mm")
  .ip_cf_check_positive(cell_ratio, "cell_ratio")
  .ip_cf_check_positive(char_width, "char_width")
  if (is.null(ready)) ready <- identical(style, "c64")
  .ip_check_flag(ready, "ready")
  if (is.null(family)) family <- figure_font("mono") else .ip_check_string(family, "family")

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
    grid <- .ip_cf_grid(x, style, width, height, charset, case)
  }

  m <- grid$chars
  if (ready) m <- .ip_cf_ready(m, grid$charset)
  ncol <- ncol(m)
  nrow <- nrow(m)

  keep <- m != " " & !is.na(m)
  cells <- data.frame(col = col(m)[keep], row = row(m)[keep], glyph = m[keep],
                      stringsAsFactors = FALSE)
  cells <- cells[order(cells$row, cells$col), , drop = FALSE]
  rownames(cells) <- NULL

  pal <- .ip_cf_palette(style)
  border <- if (identical(style, "c64")) .ip_cf_border(width_mm) else 0
  size <- (width_mm / ncol) / char_width

  # Symbols built here keep R CMD check free of global-variable notes.
  mapping <- do.call(ggplot2::aes, list(x = as.name("col"), y = call("-", as.name("row")),
                                        label = as.name("glyph")))
  ggplot2::ggplot(cells, mapping) +
    ggplot2::geom_text(colour = pal$ink, family = family, size = size) +
    ggplot2::coord_fixed(ratio = cell_ratio, xlim = c(0.5, ncol + 0.5),
                         ylim = c(-nrow - 0.5, -0.5), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = pal$border, colour = NA),
      panel.background = ggplot2::element_rect(fill = pal$panel, colour = NA),
      plot.margin = ggplot2::margin(border, border, border, border, unit = "mm")
    )
}

#' File size for a character figure
#'
#' `char_size()` gives the width and height at which a [char_figure()] is saved
#' without distortion: the grid fills `width_mm`, each cell is
#' `width_mm / ncol` wide and `cell_ratio` times as tall. For `"c64"` the border
#' of 8 % of `width_mm` is added on all four sides.
#'
#' The grid size is read from `x`:
#'
#' * an `inferplotR_chargrid`: its `width` and `height`;
#' * a ggplot: the grid [char_grid()] would draw with the defaults of
#'   [char_figure()] for `style` (64 x 20 for `"bw"`, 40 x 25 for `"c64"`);
#' * a [char_figure()] result: the coordinate limits of the figure, which already
#'   include any `READY.` rows.
#'
#' For a chargrid or a ggplot with `style = "c64"`, the two `READY.` rows that
#' [char_figure()] appends by default are counted. Pass the same `width_mm`,
#' `cell_ratio` and `style` as to [char_figure()].
#'
#' @param x A ggplot, an `inferplotR_chargrid` or a [char_figure()] result.
#' @param width_mm Width of the grid in millimetres (without the c64 border),
#'   default 134.
#' @param cell_ratio Height of a cell divided by its width, default 2.
#' @param style `"bw"` (default) or `"c64"`.
#' @return A list with `width` and `height` (numbers, millimetres) and
#'   `units = "mm"`, ready for `save_figure(width = , height = )`.
#' @seealso [char_figure()], [save_figure()], [figure_size()] for the fixed
#'   presets.
#' @examples
#' p <- ggplot2::ggplot(data.frame(x = 1:5, y = c(2, 4, 3, 5, 4)),
#'                      ggplot2::aes(x, y)) + ggplot2::geom_line()
#' char_size(p)
#' char_size(p, style = "c64")
#' char_size(char_grid(p, width = 32, height = 10), width_mm = 86)
#' @export
char_size <- function(x, width_mm = 134, cell_ratio = 2, style = c("bw", "c64")) {
  style <- .ip_check_choice(style, c("bw", "c64"), "style")
  .ip_cf_check_positive(width_mm, "width_mm")
  .ip_cf_check_positive(cell_ratio, "cell_ratio")
  ready_rows <- if (identical(style, "c64")) 2L else 0L

  if (inherits(x, "inferplotR_chargrid")) {
    ncol <- x$width
    nrow <- x$height + ready_rows
  } else {
    .ip_check_plot(x, "x")
    lim <- .ip_cf_limits(x)
    if (!is.null(lim)) {
      ncol <- diff(lim$x)
      nrow <- diff(lim$y)
    } else {
      # Only the size matters here; char_figure() reports skipped layers itself.
      grid <- withCallingHandlers(
        .ip_cf_grid(x, style, NULL, NULL, "ascii", NULL),
        inferplotR_skipped_layer = function(w) invokeRestart("muffleWarning")
      )
      ncol <- grid$width
      nrow <- grid$height + ready_rows
    }
  }

  border <- if (identical(style, "c64")) .ip_cf_border(width_mm) else 0
  list(width = width_mm + 2 * border,
       height = nrow * (width_mm / ncol) * cell_ratio + 2 * border,
       units = "mm")
}

.ip_cf_check_positive <- function(x, arg) {
  .ip_check_number(x, arg, min = 0)
  if (x <= 0) .ip_abort(sprintf("%s must be a positive number", arg))
  invisible(x)
}

# The C64 border: 8 % of the grid width on every side.
.ip_cf_border <- function(width_mm) 0.08 * width_mm

.ip_cf_palette <- function(style) {
  if (identical(style, "c64")) {
    fg <- .ip_c64[["foreground"]]
    list(ink = fg, panel = .ip_c64[["background"]], border = fg)
  } else {
    list(ink = .ip_ink, panel = .ip_paper, border = .ip_paper)
  }
}

.ip_cf_grid <- function(plot, style, width, height, charset, case) {
  c64 <- identical(style, "c64")
  if (is.null(width)) width <- if (c64) 40L else 64L
  if (is.null(height)) height <- if (c64) 25L else 20L
  if (is.null(case)) case <- if (c64) "upper" else "asis"
  char_grid(plot, width = width, height = height, charset = charset, case = case)
}

# Appends the two rows of the C64 prompt, flush left, below the grid.
.ip_cf_ready <- function(m, charset) {
  prompt <- matrix(" ", nrow = 2L, ncol = ncol(m))
  word <- strsplit("READY.", "", fixed = TRUE)[[1L]]
  n <- min(length(word), ncol(m))
  prompt[1L, seq_len(n)] <- word[seq_len(n)]
  prompt[2L, 1L] <- .ip_glyphs[[charset]]$cursor
  rbind(m, prompt)
}

# Coordinate limits of a char_figure() result, or NULL for any other ggplot.
.ip_cf_limits <- function(plot) {
  # ggplot2 4.x builds coord_fixed() as a CoordCartesian with a ratio, older
  # versions as a CoordFixed; both carry the ratio field.
  coord <- plot$coordinates
  if (!inherits(coord, "CoordCartesian") || is.null(coord$ratio)) return(NULL)
  lim <- coord$limits
  data <- plot$data
  if (is.null(lim$x) || is.null(lim$y) || !is.data.frame(data) ||
      !all(c("col", "row", "glyph") %in% names(data))) {
    return(NULL)
  }
  list(x = lim$x, y = lim$y)
}
