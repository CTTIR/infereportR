# Discrete scales that replace colour with line type, point shape and a single
# ink value (SPEC section 4.3). Hatch fills live in R/hatch.R.

# One palette factory for every scale: a fixed sequence that refuses to recycle,
# because recycled line types or shapes would silently merge groups.
.ip_palette <- function(values, what) {
  force(values)
  force(what)
  max_n <- length(values)
  function(n) {
    if (n > max_n) {
      .ip_abort(sprintf(
        "%d %s are needed but at most %d are supported; use direct labels or facets instead",
        n, what, max_n), code = "too_many_groups")
    }
    values[seq_len(n)]
  }
}

# Validates a user-supplied `values` vector for the linetype and shape scales.
.ip_check_values <- function(values, arg = "values") {
  ok <- (is.character(values) || is.numeric(values)) && length(values) >= 1L &&
    !anyNA(values)
  if (ok && is.character(values)) ok <- all(nzchar(values))
  if (!ok) {
    .ip_abort(sprintf("%s must be a non-empty character or numeric vector without NA", arg))
  }
  invisible(values)
}

#' Line type scale for black-and-white figures
#'
#' A discrete line type scale that distinguishes groups by dash pattern
#' instead of color. The default sequence has six line types: `"solid"`,
#' `"22"` (dashed), `"13"` (dotted), `"1343"` (dot-dash), `"73"` (long dash)
#' and `"2262"` (long dash-dash).
#'
#' The scale never recycles: if the data have more groups than there are line
#' types, building the plot fails with an error of class
#' `inferplotR_too_many_groups`. Use direct labels or facets in that case.
#'
#' @param ... Arguments passed on to [ggplot2::discrete_scale()], for example
#'   `name`, `labels`, `breaks` or `guide`.
#' @param values `NULL` (default) for the built-in sequence, or a character or
#'   numeric vector of line types to use in order.
#' @return A ggplot2 discrete scale object for the `linetype` aesthetic.
#' @seealso [scale_shape_figure()], [scale_colour_ink()], [scale_fill_hatch()];
#'   `theme_figure()` for the matching theme.
#' @examples
#' d <- data.frame(x = rep(1:5, 2), y = c(1:5, 5:1), g = rep(c("A", "B"), each = 5))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, linetype = g)) +
#'   ggplot2::geom_line() +
#'   scale_linetype_figure(name = "Group")
#' @export
scale_linetype_figure <- function(..., values = NULL) {
  if (is.null(values)) {
    values <- .ip_linetypes
  } else {
    .ip_check_values(values)
  }
  ggplot2::discrete_scale(aesthetics = "linetype",
                          palette = .ip_palette(values, "line types"), ...)
}

#' Point shape scale for black-and-white figures
#'
#' A discrete shape scale that distinguishes groups by point shape instead of
#' color. The default sequence has eight shapes that alternate between filled
#' and open: `16, 1, 15, 0, 17, 2, 18, 5` (circle, square, triangle, diamond).
#'
#' The scale never recycles: if the data have more groups than there are
#' shapes, building the plot fails with an error of class
#' `inferplotR_too_many_groups`. Use direct labels or facets in that case.
#'
#' @inheritParams scale_linetype_figure
#' @param values `NULL` (default) for the built-in sequence, or a numeric or
#'   character vector of shapes to use in order.
#' @return A ggplot2 discrete scale object for the `shape` aesthetic.
#' @seealso [scale_linetype_figure()], [scale_colour_ink()], [scale_fill_hatch()].
#' @examples
#' d <- data.frame(x = 1:6, y = c(2, 3, 5, 4, 6, 7), g = rep(c("A", "B"), 3))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, shape = g)) +
#'   ggplot2::geom_point(size = 2) +
#'   scale_shape_figure(name = "Group")
#' @export
scale_shape_figure <- function(..., values = NULL) {
  if (is.null(values)) {
    values <- .ip_shapes
  } else {
    .ip_check_values(values)
  }
  ggplot2::discrete_scale(aesthetics = "shape",
                          palette = .ip_palette(values, "shapes"), ...)
}

#' Ink-only color scale
#'
#' A discrete color scale that maps every level to the ink color: black in
#' light mode, white in dark mode (see [theme_figure()]). The groups and
#' the legend survive, so the scale is meant to be combined with
#' [scale_linetype_figure()] or [scale_shape_figure()], which then carry the
#' distinction between groups. `scale_color_ink()` is an alias.
#'
#' @inheritParams scale_linetype_figure
#' @param aesthetics Character vector of aesthetics the scale applies to;
#'   `"colour"` by default. Use `c("colour", "fill")` to set both.
#' @param mode `"light"` (default, black ink) or `"dark"` (white ink).
#' @return A ggplot2 discrete scale object.
#' @seealso [scale_linetype_figure()], [scale_shape_figure()].
#' @examples
#' d <- data.frame(x = rep(1:5, 2), y = c(1:5, 5:1), g = rep(c("A", "B"), each = 5))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g, linetype = g, shape = g)) +
#'   ggplot2::geom_line() +
#'   ggplot2::geom_point() +
#'   scale_colour_ink(name = "Group") +
#'   scale_linetype_figure(name = "Group") +
#'   scale_shape_figure(name = "Group")
#' @export
scale_colour_ink <- function(..., aesthetics = "colour", mode = "light") {
  ink <- .ip_mode(mode)$ink
  if (!is.character(aesthetics) || !length(aesthetics) || anyNA(aesthetics) ||
      !all(nzchar(aesthetics))) {
    .ip_abort("aesthetics must be a non-empty character vector without NA")
  }
  ggplot2::discrete_scale(aesthetics = aesthetics,
                          palette = function(n) rep(ink, n), ...)
}

#' @rdname scale_colour_ink
#' @export
scale_color_ink <- scale_colour_ink
