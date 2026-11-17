.ip_correlation_symbol <- c(correlation_pearson = "r = ", correlation_spearman = "rho = ",
                            correlation_kendall = "tau = ")

# The figure label: symbol, estimate, interval and p. Without an interval
# (Spearman and Kendall in infercalcR 0.1.0 report none) the interval part is
# left out; figure_report() keeps infereportR's full strings.
.ip_correlation_label <- function(method, shown, res) {
  has_interval <- !is.null(res$conf.low) && !is.null(res$conf.high) &&
    is.finite(res$conf.low) && is.finite(res$conf.high)
  interval <- if (has_interval) paste0(" ", shown$interval_display) else ""
  paste0(.ip_correlation_symbol[[method]], shown$estimate_display, interval, ", ",
         shown$p_display)
}

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCorrelation <- ggplot2::ggproto("StatCorrelation", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_layer = function(self, data, params, layout) {
    .ip_compute_layer(self, data, params, layout)
  },
  compute_panel = function(data, scales, method, alternative = NULL, conf.level = NULL,
                           method_args = list(), label_x = -Inf, label_y = Inf) {
    if (.ip_is_discrete(scales$x) || .ip_is_discrete(scales$y)) {
      .ip_abort("stat_correlation() needs continuous x and y", code = "unsupported")
    }
    panel <- data$PANEL[1L]
    data <- .ip_finite_xy(data)
    if (!nrow(data)) return(.ip_text_empty())
    id <- .ip_result_id("correlation", method, panel)
    spec <- .ip_spec(method, alternative = alternative, conf.level = conf.level)
    frame <- data.frame(id = seq_len(nrow(data)), x = data$x, y = data$y)
    result <- .ip_wrap(do.call(infercalcR::run_inference,
                               c(list(spec, data = frame), method_args, result_id = id)),
                       "infercalcR::run_inference()")
    shown <- .ip_display(result)
    shown <- shown[1L, , drop = FALSE]
    text <- .ip_correlation_label(method, shown, infercalcR::result_data(result)[1L, ])
    out <- data.frame(x = label_x, y = label_y, label = text, group = 1L, PANEL = panel,
                      stringsAsFactors = FALSE)
    .ip_attach_result(out, result, id)
  }
)

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCorrelationFit <- ggplot2::ggproto("StatCorrelationFit", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_layer = function(self, data, params, layout) {
    .ip_compute_layer(self, data, params, layout)
  },
  compute_panel = function(data, scales, conf.level = NULL, n_points = 80L) {
    panel <- data$PANEL[1L]
    data <- .ip_finite_xy(data)
    empty <- data.frame(x = double(), y = double(), ymin = double(), ymax = double(),
                        group = integer(), stringsAsFactors = FALSE)
    empty$.ip_result <- list()
    empty$.ip_result_id <- character()
    if (!nrow(data)) return(empty)
    id <- .ip_result_id("correlation_fit", "prediction_model_linear", panel)
    spec <- .ip_spec("prediction_model_linear", conf.level = conf.level)
    frame <- data.frame(id = seq_len(nrow(data)), x = data$x, y = data$y)
    # independent_units = TRUE declares what stat_correlation(fit = TRUE)
    # documents: one observation per independent unit.
    model <- .ip_run(spec, data = frame, predictors = "x", independent_units = TRUE,
                     result_id = id)
    grid_x <- seq(min(data$x), max(data$x), length.out = n_points)
    fitted <- .ip_wrap(infercalcR::predict_inference(model, data.frame(x = grid_x),
                                                     interval = "mean",
                                                     conf.level = conf.level),
                       "infercalcR::predict_inference()")
    fd <- infercalcR::result_data(fitted)
    out <- data.frame(x = grid_x, y = fd$estimate, ymin = fd$conf.low, ymax = fd$conf.high,
                      group = 1L, PANEL = panel, stringsAsFactors = FALSE)
    .ip_attach_result(out, model, id)
  }
)

# Fitted line (solid) with the confidence band as two dotted ink lines and no
# fill, so the figure stays 1-bit.
.ip_geom_fit <- ggplot2::ggproto("GeomInferplotFit", ggplot2::Geom,
  required_aes = c("x", "y", "ymin", "ymax"),
  default_aes = ggplot2::aes(colour = .ip_ink, linewidth = .ip_lw(.ip_axis_line),
                             alpha = NA),
  draw_key = ggplot2::draw_key_path,
  draw_panel = function(data, panel_params, coord) {
    data <- data[order(data$x), , drop = FALSE]
    path <- function(y, lty) {
      keep <- is.finite(data$x) & is.finite(y)
      if (sum(keep) < 2L) return(grid::nullGrob())
      p <- coord$transform(data.frame(x = data$x[keep], y = y[keep], PANEL = data$PANEL[keep]),
                           panel_params)
      grid::polylineGrob(p$x, p$y, default.units = "native",
                         gp = grid::gpar(col = data$colour[1L],
                                         lwd = data$linewidth[1L] * ggplot2::.pt,
                                         lty = lty, lineend = "butt"))
    }
    grid::grobTree(path(data$ymin, "13"), path(data$ymax, "13"), path(data$y, "solid"),
                   name = "inferplotR.fit")
  }
)

#' Correlation with an optional fitted line
#'
#' Runs one explicitly named correlation procedure of 'infercalcR' per panel
#' and writes its 'infereportR' display strings as a label: the coefficient
#' (prefixed `r = `, `rho = ` or `tau = `), its interval and the p-value. When
#' 'infercalcR' reports no interval (Spearman and Kendall in 'infercalcR'
#' 0.1.0), the label leaves the interval out; [figure_report()] still has the
#' full 'infereportR' strings. Each panel is its own analysis. Rows with
#' non-finite x or y are dropped first.
#'
#' With `fit = TRUE` a second layer draws the fitted mean line of a linear
#' prediction model from 'infercalcR' (`prediction_model_linear`) with its
#' pointwise confidence band from [infercalcR::predict_inference()]
#' (`interval = "mean"`), drawn as two dotted lines without fill. Requesting
#' the fit declares that each row is an independent unit. The line is never
#' fitted with [stats::lm()] directly.
#'
#' @param mapping,data,inherit.aes As in [ggplot2::layer()].
#' @param ... Further parameters passed to [ggplot2::geom_text()].
#' @param method The 'infercalcR' method id, one of `"correlation_pearson"`,
#'   `"correlation_spearman"` and `"correlation_kendall"`. Required.
#' @param alternative,conf.level Passed to [infercalcR::method_spec()] when
#'   set; otherwise the 'infercalcR' defaults apply. `conf.level` also sets
#'   the level of the fitted band.
#' @param method_args A named list of further arguments for
#'   [infercalcR::run_inference()]. The data arguments are set by the layer.
#' @param label_x,label_y Label position in data units; `-Inf`/`Inf` mean the
#'   panel edges (top left by default).
#' @param hjust,vjust Label justification.
#' @param fit If `TRUE`, also draw the fitted line and its confidence band.
#' @param text_size Label size in mm (ggplot2 text units); 8 pt (the in-plot
#'   label size) by default.
#' @param family Label font family; `figure_font("sans")` by default.
#' @return A ggplot2 layer; with `fit = TRUE`, a list of two layers (fit,
#'   then label) that `+` adds together. The computed data carries
#'   `.ip_result` and `.ip_result_id`.
#' @seealso [stat_compare()], [inference_results()]
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(shape = 1) +
#'   stat_correlation(method = "correlation_pearson", fit = TRUE)
#' @export
stat_correlation <- function(mapping = NULL, data = NULL, ..., method, alternative = NULL,
                             conf.level = NULL, method_args = list(), label_x = -Inf,
                             label_y = Inf, hjust = 0, vjust = 1, fit = FALSE,
                             text_size = NULL, family = NULL, inherit.aes = TRUE) {
  method <- .ip_check_method(if (missing(method)) NULL else method, "correlation")
  alternative <- .ip_check_alternative(alternative)
  conf.level <- .ip_check_conf_level(conf.level)
  .ip_check_method_args(method_args)
  .ip_check_position(label_x, "label_x")
  .ip_check_position(label_y, "label_y")
  .ip_check_number(hjust, "hjust")
  .ip_check_number(vjust, "vjust")
  .ip_check_flag(fit, "fit")
  dots <- list(...)
  params <- c(list(method = method, alternative = alternative, conf.level = conf.level,
                   method_args = method_args, label_x = label_x, label_y = label_y,
                   hjust = hjust, vjust = vjust),
              .ip_text_params(dots, text_size, family), dots)
  text <- ggplot2::layer(stat = StatCorrelation, data = data, mapping = mapping,
                         geom = "text", position = "identity", show.legend = FALSE,
                         inherit.aes = inherit.aes, params = params)
  if (!fit) return(text)
  line <- ggplot2::layer(stat = StatCorrelationFit, data = data, mapping = mapping,
                         geom = .ip_geom_fit, position = "identity", show.legend = FALSE,
                         inherit.aes = inherit.aes, params = list(conf.level = conf.level))
  list(line, text)
}
