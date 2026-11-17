#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatOmnibus <- ggplot2::ggproto("StatOmnibus", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_layer = function(self, data, params, layout) {
    .ip_compute_layer(self, data, params, layout)
  },
  compute_panel = function(data, scales, method, method_args = list(), label_x = -Inf,
                           label_y = Inf) {
    panel <- data$PANEL[1L]
    data <- .ip_finite_xy(data)
    if (!nrow(data)) return(.ip_text_empty())
    levels <- if (.ip_is_discrete(scales$x)) {
      as.character(scales$x$get_limits())[data$x]
    } else {
      format(data$x)
    }
    id <- .ip_result_id("omnibus", method, panel)
    spec <- .ip_spec(method)
    args <- c(list(spec, x = data$y, group = factor(levels, levels = unique(levels))),
              method_args)
    result <- .ip_wrap(do.call(infercalcR::run_inference, c(args, result_id = id)),
                       "infercalcR::run_inference()")
    shown <- .ip_display(result)
    shown <- shown[1L, , drop = FALSE]
    # infereportR 0.1.0 has no display string for degrees of freedom, so none is
    # shown; inferplotR does not format numbers itself.
    text <- paste0(shown$estimand, ": ", shown$statistic_display, ", ", shown$p_display)
    out <- data.frame(x = label_x, y = label_y, label = text, group = 1L, PANEL = panel,
                      stringsAsFactors = FALSE)
    .ip_attach_result(out, result, id)
  }
)

.ip_text_empty <- function() {
  out <- data.frame(x = double(), y = double(), label = character(), group = integer(),
                    stringsAsFactors = FALSE)
  out$.ip_result <- list()
  out$.ip_result_id <- character()
  out
}

#' Omnibus test across groups
#'
#' Runs one explicitly named omnibus procedure of 'infercalcR' per panel,
#' with the y values as outcome and the x positions as groups, and writes its
#' 'infereportR' display strings as a text label: the estimand, the test
#' statistic and the p-value. Each panel is its own analysis.
#'
#' Degrees of freedom are not shown, because 'infereportR' 0.1.0 provides no
#' display string for them and inferplotR does not format numbers itself. The
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
#'   panel edges (top left by default).
#' @param hjust,vjust Label justification.
#' @param text_size Label size in mm (ggplot2 text units); 9 pt by default.
#' @param family Label font family; `figure_font("sans")` by default.
#' @return A ggplot2 layer drawing one label per panel. The computed data has
#'   columns `x`, `y`, `label`, `.ip_result` and `.ip_result_id`.
#' @seealso [stat_compare()], [inference_results()]
#' @examples
#' library(ggplot2)
#' ggplot(PlantGrowth, aes(group, weight)) +
#'   geom_point(shape = 1, colour = "black") +
#'   stat_omnibus(method = "anova_one_way")
#' @export
stat_omnibus <- function(mapping = NULL, data = NULL, ..., method, method_args = list(),
                         label_x = -Inf, label_y = Inf, hjust = 0, vjust = 1,
                         text_size = NULL, family = NULL, inherit.aes = TRUE) {
  method <- .ip_check_method(if (missing(method)) NULL else method, "omnibus")
  .ip_check_method_args(method_args)
  .ip_check_position(label_x, "label_x")
  .ip_check_position(label_y, "label_y")
  .ip_check_number(hjust, "hjust")
  .ip_check_number(vjust, "vjust")
  dots <- list(...)
  params <- c(list(method = method, method_args = method_args, label_x = label_x,
                   label_y = label_y, hjust = hjust, vjust = vjust),
              .ip_text_params(dots, text_size, family), dots)
  ggplot2::layer(stat = StatOmnibus, data = data, mapping = mapping, geom = "text",
                 position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
                 params = params)
}
