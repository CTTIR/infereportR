#' @title ggproto objects of inferplotR
#' @description
#' The ggproto classes behind the layers of inferplotR. They are exported so
#' that other extensions can build on them; most users call the layer
#' functions instead.
#'
#' * `StatEndlabel`, the stat of [geom_endlabel()]: per group, it keeps the
#'   rows with finite `x` and `y` and returns the one with the largest `x`
#'   (the first one on ties). A group without finite rows yields no row.
#' * `GeomBracket` draws a comparison bracket (a bar with two downward tips)
#'   and its label; used by [stat_compare()].
#' * `StatMeans`, `StatCompare`, `StatOmnibus`, `StatCorrelation` and
#'   `StatCorrelationFit` run explicitly named 'infercalcR' procedures per
#'   panel and label them with 'infereportR' display strings.
#'
#' @format NULL
#' @usage NULL
#' @seealso [geom_endlabel()], [stat_means()], [stat_compare()], [stat_omnibus()],
#'   [stat_correlation()], [ggplot2::Stat]
#' @rdname inferplotR-ggproto
#' @export
StatEndlabel <- ggplot2::ggproto(
  "StatEndlabel", ggplot2::Stat,
  required_aes = c("x", "y", "label"),
  compute_group = function(data, scales) {
    keep <- is.finite(as.numeric(data$x)) & is.finite(as.numeric(data$y))
    data <- data[keep, , drop = FALSE]
    if (!nrow(data)) return(data[0, , drop = FALSE])
    data[which.max(as.numeric(data$x)), , drop = FALSE]
  }
)

#' End labels instead of a legend
#'
#' Labels each group at its last point, the point with the largest finite `x`,
#' so that lines can be identified without a legend or colour. The layer uses
#' the stat `StatEndlabel` and the geom [ggplot2::GeomText]; the text is always
#' drawn in the ink colour.
#'
#' End labels sit to the right of the data and are clipped by the panel unless
#' the plot leaves room for them. Either switch clipping off and widen the right
#' plot margin (`ggplot2::coord_cartesian(clip = "off")` plus
#' `ggplot2::theme(plot.margin = ...)`), or add extra expansion to the x scale
#' (`ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.3)))`).
#' Use `show.legend = FALSE` (the default) and drop the legend of the line
#' layer, since the labels replace it.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. The
#'   layer requires `x`, `y` and `label`.
#' @param data The data to be displayed in this layer, as in
#'   [ggplot2::layer()].
#' @param ... Other arguments passed on to [ggplot2::layer()] as fixed
#'   aesthetics or parameters, for example `vjust` or `fontface`. `colour` is
#'   fixed to the ink colour and cannot be changed.
#' @param hjust Horizontal justification of the label; the default `-0.15`
#'   places the text just right of the last point.
#' @param size Text size in mm (the ggplot2 unit of `size`). `NULL` means
#'   9 pt, that is `9 / ggplot2::.pt`.
#' @param family Font family. `NULL` means `figure_font("sans")`.
#' @param na.rm If `FALSE` (the default), rows with missing or non-finite
#'   values are removed with a warning, as in every ggplot2 layer; if `TRUE`,
#'   silently. Either way a group is labelled at its last finite point.
#' @param show.legend Should this layer be included in the legends? Defaults to
#'   `FALSE`, since the labels replace the legend.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#' @return A ggplot2 layer (a `LayerInstance` object) that can be added to a
#'   plot with `+`.
#' @seealso [figure_font()] for the default family, [save_figure()] for export;
#'   `theme_figure()` for the matching theme.
#' @examples
#' d <- data.frame(
#'   time = rep(1:5, 2),
#'   score = c(1, 2, 3, 3.5, 4, 2, 2.4, 2.6, 3.1, 3.2),
#'   arm = rep(c("Treatment", "Control"), each = 5)
#' )
#' p <- ggplot2::ggplot(d, ggplot2::aes(time, score, group = arm, label = arm)) +
#'   ggplot2::geom_line(ggplot2::aes(linetype = arm), show.legend = FALSE) +
#'   geom_endlabel() +
#'   ggplot2::coord_cartesian(clip = "off") +
#'   ggplot2::theme(plot.margin = ggplot2::margin(5.5, 60, 5.5, 5.5))
#' p
#' @export
geom_endlabel <- function(mapping = NULL, data = NULL, ..., hjust = -0.15, size = NULL,
                          family = NULL, na.rm = FALSE, show.legend = FALSE,
                          inherit.aes = TRUE) {
  .ip_check_number(hjust, "hjust")
  if (is.null(size)) {
    size <- .ip_base_size / ggplot2::.pt
  } else {
    .ip_check_number(size, "size", min = 0)
    if (size <= 0) .ip_abort("size must be a positive number")
  }
  if (is.null(family)) family <- figure_font("sans") else .ip_check_string(family, "family")
  .ip_check_flag(na.rm, "na.rm")
  if (!(is.logical(show.legend) && length(show.legend) == 1L)) {
    .ip_abort("show.legend must be TRUE, FALSE or NA")
  }
  .ip_check_flag(inherit.aes, "inherit.aes")
  dots <- list(...)
  # The ink colour is the point of the layer, so a user colour is dropped.
  nms <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))
  dots <- dots[!nms %in% c("colour", "color", "col")]
  params <- c(list(hjust = hjust, size = size, family = family, colour = .ip_ink,
                   na.rm = na.rm), dots)
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatEndlabel, geom = ggplot2::GeomText,
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = params
  )
}
