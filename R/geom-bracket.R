#' ggproto objects of inferplotR
#'
#' The ggproto classes behind the layers of inferplotR. They are exported so
#' that other extensions can reuse them; most users call the layer functions
#' instead.
#'
#' * `GeomBracket` draws a comparison bracket (a bar with two downward tips)
#'   and its label; used by `stat_compare()`.
#' * `StatMeans`, `StatCompare`, `StatOmnibus`, `StatCorrelation` and
#'   `StatCorrelationFit` run explicitly named 'infercalcR' procedures per
#'   panel and label them with 'infereportR' display strings.
#'
#' @seealso [stat_means()], [stat_compare()], [stat_omnibus()],
#'   [stat_correlation()]
#' @format NULL
#' @usage NULL
#' @name inferplotR-ggproto
#' @keywords internal
NULL

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBracket <- ggplot2::ggproto("GeomBracket", ggplot2::Geom,
  required_aes = c("x", "xend", "y", "label"),
  optional_aes = "tip",
  # Quosures, evaluated at draw time, so the tokens need not exist at collation.
  default_aes = ggplot2::aes(colour = .ip_ink, linewidth = .ip_lw(.ip_rule_hair),
                             size = .ip_base_size / ggplot2::.pt, family = "", tip = 0,
                             alpha = NA),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord) {
    if (!nrow(data)) return(grid::nullGrob())
    tip <- if (is.null(data$tip)) rep(0, nrow(data)) else data$tip
    tip[!is.finite(tip)] <- 0
    corner <- function(x, y) {
      coord$transform(data.frame(x = x, y = y, PANEL = data$PANEL), panel_params)
    }
    left_top <- corner(data$x, data$y)
    right_top <- corner(data$xend, data$y)
    left_tip <- corner(data$x, data$y - tip)
    right_tip <- corner(data$xend, data$y - tip)
    centre <- corner((data$x + data$xend) / 2, data$y)
    family <- data$family
    family[is.na(family) | !nzchar(family)] <- figure_font("sans")
    line_gp <- grid::gpar(col = data$colour, lwd = data$linewidth * ggplot2::.pt,
                          lineend = "butt")
    segments <- function(a, b) {
      grid::segmentsGrob(a$x, a$y, b$x, b$y, default.units = "native", gp = line_gp)
    }
    grid::grobTree(
      segments(left_top, right_top),
      segments(left_top, left_tip),
      segments(right_top, right_tip),
      grid::textGrob(as.character(data$label), centre$x, centre$y, default.units = "native",
                     vjust = -0.3,
                     gp = grid::gpar(col = data$colour, fontsize = data$size * ggplot2::.pt,
                                     fontfamily = family)),
      name = "inferplotR.bracket"
    )
  }
)
