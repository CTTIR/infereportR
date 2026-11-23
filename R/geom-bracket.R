#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBracket <- ggplot2::ggproto("GeomBracket", ggplot2::Geom,
  required_aes = c("x", "xend", "y", "label"),
  optional_aes = "tip",
  # Quosures, evaluated at draw time, so the tokens need not exist at collation.
  # line_colour: the colour of the bar and tips; NA draws them in `colour`,
  # the colour of the label.
  default_aes = ggplot2::aes(colour = .ir_ink, linewidth = .ir_lw(.ir_rule_hair),
                             size = .ir_label_size_pt / ggplot2::.pt, family = "", tip = 0,
                             alpha = NA, line_colour = NA),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord) {
    # Headroom rows (stat_compare()) only train the y scale.
    if (!is.null(data$headroom)) data <- data[!(data$headroom %in% TRUE), , drop = FALSE]
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
    family[is.na(family) | !nzchar(family)] <- .ir_sans_family()
    line_colour <- data$line_colour
    if (is.null(line_colour)) line_colour <- NA
    line_colour <- ifelse(is.na(line_colour), data$colour, line_colour)
    line_gp <- grid::gpar(col = line_colour, lwd = data$linewidth * ggplot2::.pt,
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
      name = "infereportR.bracket"
    )
  }
)
