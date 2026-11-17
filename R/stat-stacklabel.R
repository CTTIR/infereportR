# Category names beside a stacked bar instead of a legend (amendment D12).

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatStacklabel <- ggplot2::ggproto("StatStacklabel", ggplot2::Stat,
  required_aes = c("x", "y", "fill"),
  compute_panel = function(data, scales, side = "right", offset = 0.38) {
    data <- data[is.finite(as.numeric(data$x)) & is.finite(data$y), , drop = FALSE]
    if (!nrow(data)) {
      return(data.frame(x = double(), y = double(), label = character(), group = integer(),
                        stringsAsFactors = FALSE))
    }
    xs <- as.numeric(data$x)
    at <- if (side == "right") max(xs) else min(xs)
    data <- data[xs == at, , drop = FALSE]
    # One label per category: rows of the same group are summed, so the label
    # sits at the centre of the category's whole segment.
    groups <- unique(data$group)
    rows <- lapply(groups, function(g) {
      cell <- data[data$group == g, , drop = FALSE]
      out <- cell[1L, , drop = FALSE]
      out$y <- sum(cell$y)
      out
    })
    out <- do.call(rbind, rows)
    # The label is the fill level name, never the mapped fill value.
    out$label <- as.character(out$fill)
    out$x <- at + if (side == "right") offset else -offset
    out$hjust <- if (side == "right") 0 else 1
    rownames(out) <- NULL
    out
  }
)

#' Category labels beside stacked bars
#'
#' Names the categories of a stacked bar chart beside its last (or first)
#' bar, instead of a legend. The stat keeps the rows at the last
#' (`side = "right"`) or first (`side = "left"`) x position, sums the rows of
#' each category, sets `label` to the fill level name and shifts x by
#' `offset`. The layer is positioned with
#' `ggplot2::position_fill(vjust = 0.5)` or
#' `ggplot2::position_stack(vjust = 0.5)`, so each label sits at the centre
#' of its segment exactly as the bars stack.
#'
#' The bars and the labels must use the same kind of stacking: bars drawn
#' with `position = "fill"` (percentages) need `position = "fill"` here,
#' bars stacked with `position_stack()` (the `geom_col()` default) need
#' `"stack"`. Map `fill` (and `x`, `y`) the same way as for the bars, so the
#' groups and their order agree. The labels usually extend beyond the panel:
#' add `coord_cartesian(clip = "off")` and a right plot margin.
#'
#' Nothing is computed beyond positions; this layer runs no inference.
#'
#' @param mapping,data,inherit.aes As in [ggplot2::layer()]. Needs `x`, `y`
#'   and `fill`.
#' @param ... Further parameters passed to [ggplot2::geom_text()].
#' @param side `"right"` (labels right of the last bar, `hjust = 0`) or
#'   `"left"` (left of the first bar, `hjust = 1`).
#' @param offset Distance from the bar centre in x units; 0.38 by default.
#' @param position `"fill"` (default) or `"stack"`, matching the bars.
#' @param size Text size in mm (ggplot2 units); 8 pt by default.
#' @param family Font family; IBM Plex Sans Condensed when installed, else
#'   the figure font.
#' @param mode `"light"` (default, black ink) or `"dark"` (white ink); match
#'   the mode of [theme_figure()].
#' @param transparent If `TRUE`, the layer draws no background fill (open
#'   marks stay empty); accepted by every layer of the package.
#' @return A ggplot2 layer.
#' @seealso [scale_fill_hatch()]
#' @examples
#' library(ggplot2)
#' d <- data.frame(group = factor(rep(c("A", "B", "C"), each = 3)),
#'                 outcome = factor(rep(c("None", "Single", "Multiple"), 3),
#'                                  levels = c("None", "Single", "Multiple")),
#'                 n = c(52, 9, 3, 47, 15, 9, 24, 17, 16))
#' ggplot(d, aes(group, n, fill = outcome)) +
#'   geom_col(position = "fill", colour = "black", width = 0.62) +
#'   stat_stacklabel() +
#'   scale_fill_hatch(patterns = c("open", "hatch", "solid")) +
#'   coord_cartesian(clip = "off") +
#'   labs(x = "Group", y = "Patients (proportion)") +
#'   theme_figure("slide", legend = "none") +
#'   theme(plot.margin = margin(1, 23, 1, 1, "mm"))
#' @export
stat_stacklabel <- function(mapping = NULL, data = NULL, ..., side = c("right", "left"),
                            offset = 0.38, position = c("fill", "stack"), size = NULL,
                            family = NULL, mode = "light", transparent = FALSE,
                            inherit.aes = TRUE) {
  side <- .ip_check_choice(side, c("right", "left"), "side")
  .ip_check_number(offset, "offset")
  position <- .ip_check_choice(position, c("fill", "stack"), "position")
  size <- .ip_check_optional_number(size, "size", min = 0)
  family <- .ip_check_optional_string(family, "family")
  ink <- .ip_mode(mode, transparent)$ink
  pos <- switch(position,
    fill = ggplot2::position_fill(vjust = 0.5),
    stack = ggplot2::position_stack(vjust = 0.5)
  )
  dots <- list(...)
  params <- c(list(side = side, offset = offset,
                   size = if (is.null(size)) .ip_label_size_pt / ggplot2::.pt else size,
                   family = if (is.null(family)) .ip_condensed_family() else family),
              dots)
  if (!"colour" %in% names(dots) && !"color" %in% names(dots)) params$colour <- ink
  ggplot2::layer(stat = StatStacklabel, data = data, mapping = mapping, geom = "text",
                 position = pos, show.legend = FALSE, inherit.aes = inherit.aes,
                 params = params)
}
