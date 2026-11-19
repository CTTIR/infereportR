#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatOmnibus <- ggplot2::ggproto("StatOmnibus", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_layer = function(self, data, params, layout) {
    .ir_compute_layer(self, data, params, layout)
  },
  compute_panel = function(data, scales, method, method_args = list(), label_x = -Inf,
                           label_y = Inf) {
    panel <- data$PANEL[1L]
    data <- .ir_finite_xy(data)
    if (!nrow(data)) return(.ir_text_empty())
    levels <- if (.ir_is_discrete(scales$x)) {
      as.character(scales$x$get_limits())[data$x]
    } else {
      format(data$x)
    }
    id <- .ir_result_id("omnibus", method, panel)
    spec <- .ir_spec(method)
    args <- c(list(spec, x = data$y, group = factor(levels, levels = unique(levels))),
              method_args)
    result <- .ir_wrap(do.call(infercalcR::run_inference, c(args, result_id = id)),
                       "infercalcR::run_inference()")
    shown <- .ir_display(result)
    shown <- shown[1L, , drop = FALSE]
    # report_table() has no display string for degrees of freedom, so none is
    # shown; the figure layer does not format numbers itself.
    text <- paste0(shown$estimand, ": ", shown$statistic_display, ", ", shown$p_display)
    out <- data.frame(x = label_x, y = label_y, label = text, group = 1L, PANEL = panel,
                      stringsAsFactors = FALSE)
    .ir_attach_result(out, result, id)
  }
)

# Gap in mm between an omnibus label at a panel edge (label_x = -Inf or Inf)
# and the axis line there.
.ir_omnibus_inset <- 1

# geom_text() with the edge gap: a label at x = -Inf moves right by the gap,
# one at x = Inf left. Other labels are drawn as by GeomText.
GeomOmnibusText <- ggplot2::ggproto("GeomOmnibusText", ggplot2::GeomText,
  draw_panel = function(self, data, panel_params, coord, ...) {
    grob <- ggplot2::ggproto_parent(ggplot2::GeomText, self)$draw_panel(data, panel_params,
                                                                       coord, ...)
    shift <- ifelse(data$x == -Inf, 1, ifelse(data$x == Inf, -1, 0))
    if (inherits(grob, "text") && length(shift) && any(shift != 0)) {
      grob$x <- grob$x + grid::unit(shift * .ir_omnibus_inset, "mm")
    }
    grob
  }
)

.ir_text_empty <- function() {
  out <- data.frame(x = double(), y = double(), label = character(), group = integer(),
                    stringsAsFactors = FALSE)
  out$.ir_result <- list()
  out$.ir_result_id <- character()
  out
}

#' Omnibus test across groups
#'
#' Runs one explicitly named omnibus procedure of 'infercalcR' per panel,
#' with the y values as outcome and the x positions as groups, and writes its
#' display strings of [report_table()] as a text label: the estimand, the test
#' statistic and the p-value. Each panel is its own analysis.
#'
#' Degrees of freedom are not shown, because [report_table()] provides no
#' display string for them and the figure layer does not format numbers itself. The
#' full result, including `df` and `df.denominator`, is available from
#' [inference_results()]. Rows with non-finite x or y are dropped first.
#'
#' @param mapping,data,inherit.aes As in [ggplot2::layer()].
#' @param ... Further parameters passed to [ggplot2::geom_text()].
#' @param method The 'infercalcR' method id, one of `"anova_one_way"`,
#'   `"anova_one_way_welch"` and `"kruskal_wallis"`. Required.
#' @param method_args A named list of further arguments for
#'   [infercalcR::run_inference()]. The data arguments are set by the layer.
#' @param label_x,label_y Label position in data units; `-Inf`/`Inf` mean the
#'   panel edges (top left by default). A label at the left or right edge is
#'   set 1 mm in from the axis.
#' @param hjust,vjust Label justification.
#' @param text_size Label size in mm (ggplot2 text units); 8 pt (the in-plot
#'   label size) by default.
#' @param family Label font family; `figure_font("sans")` by default.
#' @param mode `"light"` (default, black ink) or `"dark"` (white ink); match
#'   the mode of [theme_figure()].
#' @param transparent If `TRUE`, the layer draws no background fill (open
#'   marks stay empty); passed by [report_figure()] with `mode`.
#' @param palette `"onebit"` (default), `"grey"` or `"tufte"`, as in
#'   [theme_figure()]. The layer draws in ink in every palette; the argument
#'   keeps the interface of the figure layer uniform.
#' @return A ggplot2 layer drawing one label per panel. The computed data has
#'   columns `x`, `y`, `label`, `.ir_result` and `.ir_result_id`.
#' @seealso [stat_compare()], [inference_results()]
#' @examples
#' library(ggplot2)
#' ggplot(PlantGrowth, aes(group, weight)) +
#'   geom_point(shape = 1, colour = "black") +
#'   stat_omnibus(method = "anova_one_way")
#' @export
stat_omnibus <- function(mapping = NULL, data = NULL, ..., method, method_args = list(),
                         label_x = -Inf, label_y = Inf, hjust = 0, vjust = 1,
                         text_size = NULL, family = NULL, mode = "light",
                         transparent = FALSE, palette = "onebit", inherit.aes = TRUE) {
  m <- .ir_mode(mode, transparent, palette)
  method <- .ir_check_method(if (missing(method)) NULL else method, "omnibus")
  .ir_check_method_args(method_args)
  .ir_check_position(label_x, "label_x")
  .ir_check_position(label_y, "label_y")
  .ir_check_number(hjust, "hjust")
  .ir_check_number(vjust, "vjust")
  dots <- list(...)
  params <- c(list(method = method, method_args = method_args, label_x = label_x,
                   label_y = label_y, hjust = hjust, vjust = vjust),
              .ir_text_params(dots, text_size, family, m$ink), dots)
  ggplot2::layer(stat = StatOmnibus, data = data, mapping = mapping, geom = GeomOmnibusText,
                 position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
                 params = params)
}
