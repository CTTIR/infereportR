#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMeans <- ggplot2::ggproto("StatMeans", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_layer = function(self, data, params, layout) {
    .ip_compute_layer(self, data, params, layout)
  },
  compute_panel = function(data, scales, method = "t_mean_one_sample", conf.level = 0.95,
                           label_type = "none") {
    panel <- data$PANEL[1L]
    data <- .ip_finite_xy(data)
    if (!nrow(data)) return(.ip_means_empty())
    discrete <- .ip_is_discrete(scales$x)
    limits <- if (discrete) scales$x$get_limits() else NULL
    keys <- unique(data[, c("x", "group")])
    keys <- keys[order(keys$x, keys$group), , drop = FALSE]
    # A group id is appended only when several groups share one x position.
    shared_x <- anyDuplicated(keys$x) > 0L
    spec <- .ip_spec(method, conf.level = conf.level)
    rows <- lapply(seq_len(nrow(keys)), function(i) {
      cell <- data[data$x == keys$x[i] & data$group == keys$group[i], , drop = FALSE]
      level <- if (discrete) as.character(limits[keys$x[i]]) else format(keys$x[i])
      if (shared_x) level <- paste0(level, ":group", keys$group[i])
      id <- .ip_result_id("means", method, panel, level)
      result <- .ip_means_run(spec, cell$y, id)
      shown <- .ip_display(result)
      res <- infercalcR::result_data(result)
      res <- res[1L, , drop = FALSE]
      shown <- shown[match(res$row_id, shown$row_id), , drop = FALSE]
      text <- switch(label_type,
        none = "",
        estimate = shown$estimate_display,
        estimate_ci = paste(shown$estimate_display, shown$interval_display)
      )
      out <- data.frame(x = keys$x[i], y = res$estimate, ymin = res$conf.low,
                        ymax = res$conf.high, n = res$n, label = text,
                        group = keys$group[i], PANEL = panel, stringsAsFactors = FALSE)
      .ip_attach_result(out, result, id)
    })
    do.call(rbind, rows)
  }
)

.ip_means_empty <- function() {
  out <- data.frame(x = double(), y = double(), ymin = double(), ymax = double(),
                    n = double(), label = character(), group = integer(),
                    stringsAsFactors = FALSE)
  out$.ip_result <- list()
  out$.ip_result_id <- character()
  out
}

# infercalcR needs at least two observations for a mean interval and stops
# otherwise. Such a group is still recorded, as an infercalcR result whose
# status is "undefined" and whose reason is infercalcR's own message, so the
# figure keeps the row and the label shows the status instead of a number.
.ip_means_run <- function(spec, y, id) {
  if (length(y) < 2L) {
    attempt <- tryCatch(infercalcR::run_inference(spec, x = y, mu0 = 0, result_id = id),
                        error = function(e) e)
    if (!inherits(attempt, "error")) return(attempt)
    rows <- data.frame(row_id = "mean", component = "contrast", estimand = "mean",
                       n = length(y), status = "undefined",
                       reason = paste("infercalcR:", conditionMessage(attempt)),
                       stringsAsFactors = FALSE)
    return(.ip_wrap(infercalcR::new_inference_result(
      rows, list(method_id = spec$method_id, result_id = id)),
      "infercalcR::new_inference_result()"))
  }
  .ip_run(spec, x = y, mu0 = 0, result_id = id)
}

#' Means with confidence intervals
#'
#' Adds the mean and its confidence interval for every x position (and
#' group) of a panel. The numbers come from the one-sample t procedure of
#' 'infercalcR' (`t_mean_one_sample`), used here as an interval estimate, not
#' as a test; labels, when requested, are 'infereportR' display strings. Each
#' panel is its own analysis, so facets work.
#'
#' Rows with non-finite x or y are dropped before the call, as ggplot2 does
#' for drawing. A group with fewer than two observations has no interval:
#' its row is kept with the status reported by 'infercalcR' and nothing is
#' drawn for the missing interval.
#'
#' @param mapping,data,position,show.legend,inherit.aes As in
#'   [ggplot2::layer()].
#' @param geom The geom used to draw the means; `"pointrange"` by default.
#' @param ... Further parameters passed to the geom, such as `linewidth`.
#' @param method The 'infercalcR' method id. Only `"t_mean_one_sample"` is
#'   supported.
#' @param conf.level Confidence level of the intervals.
#' @param label `"none"` (default), `"estimate"` or `"estimate_ci"`: the text
#'   placed in the `label` column, for geoms that draw it.
#' @param na.rm If `TRUE`, missing values are removed silently by the geom.
#' @return A ggplot2 layer. The computed data has columns `x`, `y` (the mean),
#'   `ymin`, `ymax`, `n`, `label`, `.ip_result` (the 'infercalcR' result) and
#'   `.ip_result_id`.
#' @seealso [inference_results()], [figure_report()], [stat_compare()]
#' @examples
#' library(ggplot2)
#' ggplot(PlantGrowth, aes(group, weight)) +
#'   geom_point(shape = 1, colour = "black") +
#'   stat_means()
#' @export
stat_means <- function(mapping = NULL, data = NULL, geom = "pointrange",
                       position = "identity", ..., method = "t_mean_one_sample",
                       conf.level = 0.95, label = c("none", "estimate", "estimate_ci"),
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  method <- .ip_check_method(method, "means")
  conf.level <- .ip_check_conf_level(conf.level)
  if (is.null(conf.level)) .ip_abort("conf.level must be a single number between 0 and 1")
  label <- .ip_check_choice(label, c("none", "estimate", "estimate_ci"), "label")
  .ip_check_flag(na.rm, "na.rm")
  dots <- list(...)
  params <- c(list(method = method, conf.level = conf.level, label_type = label, na.rm = na.rm),
              dots)
  if (!"colour" %in% names(dots) && !"color" %in% names(dots)) params$colour <- .ip_ink
  ggplot2::layer(stat = StatMeans, data = data, mapping = mapping, geom = geom,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = params)
}
