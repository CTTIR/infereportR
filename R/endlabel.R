#' @title ggproto objects of inferplotR
#' @description
#' The ggproto classes behind the layers of inferplotR. They are exported so
#' that other extensions can build on them; most users call the layer
#' functions instead.
#'
#' * `StatEndlabel`, the stat of [geom_endlabel()]: per group, it keeps the
#'   rows with finite `x` and `y` and returns the one with the largest `x`
#'   (the first one on ties). A group without finite rows yields no row.
#' * `GeomEndlabel`, the geom of [geom_endlabel()]: a [ggplot2::GeomText]
#'   that avoids collisions at draw time. In the panel viewport it sorts the
#'   labels by y, pushes them apart to at least 1.15 text heights while keeping
#'   their mean position, and draws them left-justified 2.5 mm right of their
#'   point.
#' * `GeomBracket` draws a comparison bracket (a bar with two downward tips)
#'   and its label; used by [stat_compare()].
#' * `StatMeans`, `StatCompare`, `StatOmnibus`, `StatCorrelation` and
#'   `StatCorrelationFit` run explicitly named 'infercalcR' procedures per
#'   panel and label them with 'infereportR' display strings.
#' * `StatPaired` and `GeomPaired`, the stat and geom of [stat_paired()]: the
#'   paired 'infercalcR' result per panel, one line per unit, the mean line
#'   and its label.
#' * `StatStacklabel`, the stat of [stat_stacklabel()]: keeps the rows of the
#'   last (or first) x position and labels them with their fill level.
#' * `StatEcdfGuides` and `GeomEcdfGuides`, the stat and geom of
#'   [stat_ecdf_guides()]: dotted quantile guides and curve labels.
#'
#' @format NULL
#' @usage NULL
#' @seealso [geom_endlabel()], [stat_means()], [stat_compare()], [stat_omnibus()],
#'   [stat_correlation()], [stat_paired()], [stat_stacklabel()],
#'   [stat_ecdf_guides()], [ggplot2::Stat]
#' @name inferplotR-ggproto
#' @aliases StatEndlabel
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

# Pushes sorted positions apart to at least `gap` and shifts the result so
# that the mean position is unchanged. Returns positions in the input order.
.ip_spread <- function(y, gap) {
  n <- length(y)
  if (n < 2L) return(y)
  o <- order(y)
  s <- y[o]
  for (i in seq_len(n)[-1L]) s[i] <- max(s[i], s[i - 1L] + gap)
  s <- s + (mean(y) - mean(s))
  out <- numeric(n)
  out[o] <- s
  out
}

# Height of "Ag" in npc of the current viewport, set in the label's font.
.ip_text_height_npc <- function(fontsize, fontfamily) {
  grid::pushViewport(grid::viewport(gp = grid::gpar(fontsize = fontsize,
                                                    fontfamily = fontfamily)))
  on.exit(grid::popViewport(), add = TRUE)
  grid::convertHeight(grid::stringHeight("Ag"), "npc", valueOnly = TRUE)
}

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomEndlabel <- ggplot2::ggproto(
  "GeomEndlabel", ggplot2::GeomText,
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)
    if (!nrow(data)) return(grid::nullGrob())
    label <- data$label
    if (is.factor(label)) label <- as.character(label)
    family <- data$family
    family[is.na(family)] <- ""
    grid::gTree(
      x = as.numeric(data$x), y = as.numeric(data$y), label = label,
      angle = data$angle, vjust = data$vjust, hjust = data$hjust,
      col = ggplot2::alpha(data$colour, data$alpha),
      fontsize = data$size * ggplot2::.pt, fontfamily = family,
      fontface = data$fontface, lineheight = data$lineheight,
      cl = "inferplotR_endlabel"
    )
  }
)

# The labels are placed when the grob is drawn, because only then is the panel
# viewport, and with it the text height in npc, known.
#' @exportS3Method grid::makeContent
makeContent.inferplotR_endlabel <- function(x) {
  heights <- vapply(seq_along(x$y), function(i) {
    .ip_text_height_npc(x$fontsize[[i]], x$fontfamily[[i]])
  }, numeric(1))
  y <- .ip_spread(x$y, .ip_endlabel_gap * max(heights))
  labels <- grid::textGrob(
    x$label,
    x = grid::unit(x$x, "npc") + grid::unit(.ip_endlabel_nudge, "mm"),
    y = grid::unit(y, "npc"), hjust = x$hjust, vjust = x$vjust, rot = x$angle,
    gp = grid::gpar(col = x$col, fontsize = x$fontsize, fontfamily = x$fontfamily,
                    fontface = x$fontface, lineheight = x$lineheight),
    name = "labels"
  )
  grid::setChildren(x, grid::gList(labels))
}

#' End labels instead of a legend
#'
#' Labels each group at its last point, the point with the largest finite `x`,
#' so that lines can be identified without a legend or color. The layer uses
#' the stat [StatEndlabel] and the geom [GeomEndlabel]; the text is always
#' drawn in the ink color of `mode` (black in light, white in dark mode), at
#' 8 pt by default.
#'
#' Labels never overlap. When the plot is drawn, `GeomEndlabel` sorts the labels
#' of a panel by their y position and pushes them apart until neighbors are at
#' least 1.15 text heights apart (the height of `"Ag"` in the label's font),
#' then shifts the group back so that its mean position is unchanged. Each label
#' starts 2.5 mm to the right of its point.
#'
#' End labels sit to the right of the data, so they extend beyond the panel and
#' are clipped unless the plot leaves room for them. Switch clipping off with
#' `ggplot2::coord_cartesian(clip = "off")` and widen the right plot margin with
#' `ggplot2::theme(plot.margin = ...)`: about 17 mm for a label such as
#' `"ISS >= 25"` at 8 pt, that is the label width plus the 2.5 mm nudge. Keep
#' `show.legend = FALSE` (the default) and drop the legend of the line layer,
#' since the labels replace it.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. The
#'   layer requires `x`, `y` and `label`.
#' @param data The data to be displayed in this layer, as in
#'   [ggplot2::layer()].
#' @param ... Other arguments passed on to [ggplot2::layer()] as fixed
#'   aesthetics or parameters, for example `vjust` or `fontface`. `colour` is
#'   fixed to the ink color and cannot be changed.
#' @param hjust Horizontal justification of the label relative to its anchor,
#'   which lies 2.5 mm right of the last point. The default `0` left-justifies
#'   the text there.
#' @param size Text size in mm (the ggplot2 unit of `size`). `NULL` means
#'   8 pt, that is `8 / ggplot2::.pt`.
#' @param family Font family. `NULL` means `figure_font("sans")`.
#' @param mode `"light"` (default, black ink) or `"dark"` (white ink); match
#'   the mode of [theme_figure()].
#' @param transparent Accepted for a uniform interface (text has no
#'   background); must be `TRUE` or `FALSE`.
#' @param na.rm If `FALSE` (the default), rows with missing or non-finite
#'   values are removed with a warning, as in every ggplot2 layer; if `TRUE`,
#'   silently. Either way a group is labeled at its last finite point.
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
#'   ggplot2::theme(plot.margin = ggplot2::margin(2, 17, 2, 2, unit = "mm"))
#' p
#' @export
geom_endlabel <- function(mapping = NULL, data = NULL, ..., hjust = 0, size = NULL,
                          family = NULL, mode = "light", transparent = FALSE, na.rm = FALSE,
                          show.legend = FALSE, inherit.aes = TRUE) {
  ink <- .ip_mode(mode, transparent)$ink
  .ip_check_number(hjust, "hjust")
  if (is.null(size)) {
    size <- .ip_label_size_pt / ggplot2::.pt
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
  params <- c(list(hjust = hjust, size = size, family = family, colour = ink,
                   na.rm = na.rm), dots)
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatEndlabel, geom = GeomEndlabel,
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = params
  )
}
