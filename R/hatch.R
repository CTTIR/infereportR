# Hatch fills built from plain grid primitives (SPEC section 4.3). No pattern
# package: a tile is a small grob tree repeated with grid::pattern().

# Tile coordinates are absolute offsets from the centre of the filled shape,
# because grid resolves pattern geometry against the shape's bounding box. With
# npc units the tile would stretch with every bar.
.ip_tile_unit <- function(f, tile) {
  grid::unit(0.5, "npc") + grid::unit((f - 0.5) * tile, "mm")
}

# Segment end points in tile fractions for each line type. The diagonals are
# y = x + k for k in -1, 0, 1, so neighbouring tiles join at the corners.
.ip_hatch_segments <- function(type) {
  rising <- list(x0 = c(0, -0.5, 0.5), y0 = c(0, 0.5, -0.5),
                 x1 = c(1, 0.5, 1.5), y1 = c(1, 1.5, 0.5))
  falling <- list(x0 = 1 - rising$x0, y0 = rising$y0,
                  x1 = 1 - rising$x1, y1 = rising$y1)
  switch(type,
    hatch = rising,
    backhatch = falling,
    crosshatch = Map(c, rising, falling),
    horizontal = list(x0 = -0.1, y0 = 0.5, x1 = 1.1, y1 = 0.5),
    vertical = list(x0 = 0.5, y0 = -0.1, x1 = 0.5, y1 = 1.1)
  )
}

.ip_hatch_tile <- function(type, spacing, linewidth) {
  diagonal <- type %in% c("hatch", "backhatch", "crosshatch")
  # A diagonal tile of side spacing * sqrt(2) puts the lines spacing mm apart.
  tile <- if (diagonal) spacing * sqrt(2) else spacing
  u <- function(f) .ip_tile_unit(f, tile)
  side <- grid::unit(tile, "mm")
  background <- grid::rectGrob(u(0.5), u(0.5), side, side,
                               gp = grid::gpar(col = NA, fill = .ip_paper))
  if (identical(type, "dots")) {
    marks <- grid::circleGrob(u(0.5), u(0.5), r = grid::unit(0.18 * tile, "mm"),
                              gp = grid::gpar(col = NA, fill = .ip_ink))
  } else {
    s <- .ip_hatch_segments(type)
    marks <- grid::segmentsGrob(u(s$x0), u(s$y0), u(s$x1), u(s$y1),
                                gp = grid::gpar(col = .ip_ink, fill = NA,
                                                lwd = linewidth * 96 / 72.27,
                                                lineend = "square"))
  }
  grid::pattern(grid::grobTree(background, marks), width = side, height = side,
                extend = "repeat")
}

#' Hatch pattern fill
#'
#' Returns a fill value for black-and-white figures. `"solid"` gives black ink
#' and `"open"` gives white paper; every other type gives a repeating
#' [grid::pattern()] tile drawn in black on white:
#'
#' * `"hatch"`: diagonal lines rising to the right;
#' * `"backhatch"`: diagonal lines falling to the right;
#' * `"crosshatch"`: both diagonals;
#' * `"dots"`: a regular grid of filled dots;
#' * `"horizontal"` and `"vertical"`: straight lines.
#'
#' `spacing` is the perpendicular distance between neighbouring lines (or the
#' distance between dot centres), so a pattern keeps its density whatever the
#' size of the filled shape.
#'
#' Pattern fills need R 4.1.0 or later and a graphics device that supports
#' patterns, such as [grDevices::cairo_pdf()], [grDevices::svg()], [grDevices::pdf()]
#' or the 'ragg' devices (`ragg::agg_png()`). Other devices draw no fill or
#' warn.
#'
#' @param type One of `"solid"`, `"open"`, `"hatch"`, `"dots"`,
#'   `"crosshatch"`, `"backhatch"`, `"horizontal"` or `"vertical"`.
#' @param spacing Distance between lines in millimetres, between 0.5 and 10.
#' @param linewidth Width of the hatch lines in points, between 0.1 and 2.
#' @return For `"solid"` and `"open"`, a single colour string. Otherwise a
#'   grid pattern object (class `GridTilingPattern`, inheriting `GridPattern`)
#'   usable as a `fill` value.
#' @seealso [scale_fill_hatch()] uses these patterns as a discrete fill scale.
#' @examples
#' hatch_pattern("solid")
#' p <- hatch_pattern("crosshatch", spacing = 2)
#' class(p)
#'
#' # Fill a shape directly with grid on a pattern-capable device.
#' if (capabilities("cairo")) {
#'   file <- tempfile(fileext = ".pdf")
#'   grDevices::cairo_pdf(file, width = 2, height = 2)
#'   grid::grid.rect(width = 0.8, height = 0.8, gp = grid::gpar(fill = p))
#'   grDevices::dev.off()
#'   unlink(file)
#' }
#' @export
hatch_pattern <- function(type, spacing = 1.6, linewidth = 0.4) {
  if (missing(type)) .ip_abort("type must be given")
  type <- .ip_check_choice(type, .ip_hatch_types, "type")
  .ip_check_number(spacing, "spacing", min = 0.5, max = 10)
  .ip_check_number(linewidth, "linewidth", min = 0.1, max = 2)
  switch(type,
    solid = .ip_ink,
    open = .ip_paper,
    .ip_hatch_tile(type, spacing, linewidth)
  )
}

#' Hatch fill scale
#'
#' A discrete fill scale that distinguishes groups by black-and-white hatch
#' patterns instead of colour. The default sequence is `"solid"`, `"open"`,
#' `"hatch"`, `"dots"`, `"crosshatch"` and `"backhatch"`; `"horizontal"` and
#' `"vertical"` are also available. Each pattern is built with
#' [hatch_pattern()], and the palette returns a list, so the built `fill`
#' column is a list of colours and patterns. Legend keys show the patterns.
#'
#' The scale never recycles: if the data have more groups than `patterns`,
#' building the plot fails with an error of class `inferplotR_too_many_groups`.
#'
#' Pattern fills need R 4.1.0 or later and a pattern-capable graphics device,
#' such as [grDevices::cairo_pdf()], [grDevices::svg()] or the 'ragg' devices
#' (`ragg::agg_png()`). Give the bars an ink outline (`colour = "black"` in the
#' geom) so that `"open"` bars stay visible.
#'
#' @inheritParams scale_linetype_figure
#' @param patterns Character vector of pattern types, used in order.
#' @inheritParams hatch_pattern
#' @return A ggplot2 discrete scale object for the `fill` aesthetic.
#' @seealso [hatch_pattern()], [scale_linetype_figure()], [scale_shape_figure()].
#' @examples
#' d <- data.frame(group = c("A", "B", "C"), value = c(3, 5, 2))
#' p <- ggplot2::ggplot(d, ggplot2::aes(group, value, fill = group)) +
#'   ggplot2::geom_col(colour = "black", width = 0.6) +
#'   scale_fill_hatch(name = "Group")
#'
#' # Draw the 3-bar chart on a pattern-capable device.
#' if (capabilities("cairo")) {
#'   file <- tempfile(fileext = ".pdf")
#'   grDevices::cairo_pdf(file, width = 3, height = 2.5)
#'   print(p)
#'   grDevices::dev.off()
#'   unlink(file)
#' }
#' @export
scale_fill_hatch <- function(..., patterns = .ip_hatches, spacing = 1.6, linewidth = 0.4) {
  if (!is.character(patterns) || !length(patterns) || anyNA(patterns) ||
      !all(patterns %in% .ip_hatch_types)) {
    .ip_abort(sprintf("patterns must be a non-empty character vector of %s",
                      paste0("\"", .ip_hatch_types, "\"", collapse = ", ")))
  }
  .ip_check_number(spacing, "spacing", min = 0.5, max = 10)
  .ip_check_number(linewidth, "linewidth", min = 0.1, max = 2)
  fills <- lapply(patterns, hatch_pattern, spacing = spacing, linewidth = linewidth)
  ggplot2::discrete_scale(aesthetics = "fill",
                          palette = .ip_palette(fills, "fill patterns"), ...)
}
