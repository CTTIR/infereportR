# Paired designs (amendment D9): one line per unit between two levels of x,
# the level means joined by a heavy line, and the paired result as a label.

# Drawing sizes: per-unit lines 0.4 pt, open end points 1.1 mm, the mean line
# 2 pt with filled points of 2.4 mm.
.ip_paired_point <- 1.1
.ip_paired_mean_point <- 2.4
.ip_paired_mean_line <- 2
.ip_paired_lineheight <- 0.95

.ip_paired_heading <- c(t_means_paired = "Mean change",
                        signed_rank_wilcoxon = "Pseudomedian change")

.ip_paired_empty <- function() {
  out <- data.frame(x = double(), y = double(), id = character(), part = character(),
                    label = character(), group = integer(), stringsAsFactors = FALSE)
  out$.ip_result <- list()
  out$.ip_result_id <- character()
  out
}

# The label: heading, estimate (with unit), interval and p, each on a line.
# Every number is an infereportR display string.
.ip_paired_label <- function(method, shown, unit) {
  estimate <- shown$estimate_display
  if (!is.null(unit)) estimate <- paste(estimate, unit)
  paste(.ip_paired_heading[[method]], estimate, shown$interval_display, shown$p_display,
        sep = "\n")
}

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPaired <- ggplot2::ggproto("StatPaired", ggplot2::Stat,
  required_aes = c("x", "y", "id"),
  compute_layer = function(self, data, params, layout) {
    .ip_compute_layer(self, data, params, layout)
  },
  compute_panel = function(data, scales, method, alternative = NULL, conf.level = NULL,
                           method_args = list(), label_type = "estimate_ci_p",
                           unit = NULL) {
    if (!.ip_is_discrete(scales$x)) {
      .ip_abort("stat_paired() needs a discrete x with exactly two levels")
    }
    levels <- scales$x$get_limits()
    if (length(levels) != 2L) {
      .ip_abort(sprintf("stat_paired() needs exactly two x levels (before, after), not %d",
                        length(levels)))
    }
    panel <- data$PANEL[1L]
    x <- as.integer(data$x)
    if (anyNA(data$id)) .ip_abort("the id aesthetic must not contain missing values")
    key <- paste(x, as.character(data$id), sep = "\r")
    if (anyDuplicated(key)) {
      .ip_abort("each id may appear only once per x level")
    }
    ok <- is.finite(data$y)
    ids <- unique(as.character(data$id))
    value <- function(level) {
      v <- rep(NA_real_, length(ids))
      at <- x == level & ok
      v[match(as.character(data$id[at]), ids)] <- data$y[at]
      v
    }
    before <- value(1L)
    after <- value(2L)
    # Ids with a missing or non-finite value at either level are dropped.
    keep <- is.finite(before) & is.finite(after)
    if (!any(keep)) return(.ip_paired_empty())
    ids <- ids[keep]
    before <- before[keep]
    after <- after[keep]
    id <- .ip_result_id("paired", method, panel)
    spec <- .ip_spec(method, alternative = alternative, conf.level = conf.level)
    frame <- data.frame(id = ids, x = after, y = before, stringsAsFactors = FALSE)
    result <- .ip_wrap(do.call(infercalcR::run_inference,
                               c(list(spec, data = frame), method_args, result_id = id)),
                       "infercalcR::run_inference()")
    shown <- .ip_display(result)[1L, , drop = FALSE]
    text <- if (label_type == "none") "" else .ip_paired_label(method, shown, unit)
    n <- length(ids)
    # The means are geometry (where the heavy line sits); the printed numbers
    # come only from infereportR.
    means <- c(mean(before), mean(after))
    out <- data.frame(
      x = c(rep(1, n), rep(2, n), 1, 2, 2),
      y = c(before, after, means, means[2L]),
      id = c(ids, ids, NA, NA, NA),
      part = c(rep("unit", 2L * n), "mean", "mean", "label"),
      label = c(rep("", 2L * n + 2L), text),
      group = c(rep(seq_len(n), 2L), 0L, 0L, -1L),
      PANEL = panel,
      stringsAsFactors = FALSE
    )
    .ip_attach_result(out, result, id)
  }
)

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPaired <- ggplot2::ggproto("GeomPaired", ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = character(),
  default_aes = ggplot2::aes(colour = .ip_ink, alpha = NA, stroke = 0.5,
                             size = .ip_label_size_pt / ggplot2::.pt, family = ""),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord) {
    if (!nrow(data)) return(grid::nullGrob())
    data <- coord$transform(data, panel_params)
    ink <- ggplot2::alpha(data$colour[1L], data$alpha[1L])
    stroke <- data$stroke[1L]
    point_lwd <- stroke * ggplot2::.stroke / 2
    units <- data[data$part == "unit", , drop = FALSE]
    means <- data[data$part == "mean", , drop = FALSE]
    label <- data[data$part == "label", , drop = FALSE]
    grobs <- list()
    if (nrow(units)) {
      units <- units[order(units$id, units$x), , drop = FALSE]
      grobs$lines <- grid::polylineGrob(
        name = "lines",
        units$x, units$y, id = match(units$id, unique(units$id)), default.units = "native",
        gp = grid::gpar(col = ink, lwd = .ip_rule_hair * 96 / 72.27, lineend = "butt"))
      grobs$points <- grid::pointsGrob(
        name = "points",
        units$x, units$y, pch = 1, default.units = "native",
        gp = grid::gpar(col = ink, fill = NA, lwd = point_lwd,
                        fontsize = .ip_paired_point * ggplot2::.pt + point_lwd))
    }
    if (nrow(means)) {
      means <- means[order(means$x), , drop = FALSE]
      grobs$mean_line <- grid::polylineGrob(
        name = "mean_line",
        means$x, means$y, default.units = "native",
        gp = grid::gpar(col = ink, lwd = .ip_paired_mean_line * 96 / 72.27,
                        lineend = "round"))
      grobs$mean_points <- grid::pointsGrob(
        name = "mean_points",
        means$x, means$y, pch = 19, default.units = "native",
        gp = grid::gpar(col = ink, fill = ink, lwd = point_lwd,
                        fontsize = .ip_paired_mean_point * ggplot2::.pt + point_lwd))
    }
    if (nrow(label) && nzchar(label$label[1L])) {
      family <- label$family[1L]
      if (is.na(family)) family <- ""
      grobs$label <- grid::textGrob(
        name = "label",
        label$label[1L],
        x = grid::unit(label$x[1L], "native") + grid::unit(.ip_endlabel_nudge, "mm"),
        y = grid::unit(label$y[1L], "native"), hjust = 0, vjust = 0.5,
        gp = grid::gpar(col = ink, fontsize = label$size[1L] * ggplot2::.pt,
                        fontfamily = family, lineheight = .ip_paired_lineheight))
    }
    do.call(grid::grobTree, c(grobs, list(name = "inferplotR.paired")))
  }
)

#' Paired designs as a slopegraph
#'
#' Runs one explicitly named paired procedure of 'infercalcR' per panel and
#' draws the pairing itself: one thin line per unit (`id`) between the two
#' levels of x, open points at both ends, the mean of each level joined by a
#' heavy line with filled points, and a label to the right of the second
#' level with the change, its interval and the p-value as 'infereportR'
#' display strings.
#'
#' x must be discrete with exactly two levels: the first is "before", the
#' second "after". The estimand is **after minus before**: the layer calls
#' `infercalcR::run_inference(spec, data = data.frame(id, x = <after>,
#' y = <before>))`. Ids with a missing or non-finite value at either level are
#' dropped before the call, so the result's `n` is the number of complete
#' pairs. An id may appear only once per level.
#'
#' Accepted method ids: `"t_means_paired"` (mean of the differences, paired
#' t) and `"signed_rank_wilcoxon"` (pseudomedian of the differences; with
#' tied differences 'infercalcR' needs `method_args = list(exact = FALSE)`).
#' The label heading is "Mean change" or "Pseudomedian change". The level
#' means drawn as the heavy line are geometry; every printed number comes
#' from 'infereportR'.
#'
#' The label sits 2.5 mm right of the second level and usually extends beyond
#' the panel: add `coord_cartesian(clip = "off")` and a right plot margin
#' (about 21 mm at 8 pt for a unit such as "mmol/l").
#'
#' @param mapping,data,inherit.aes As in [ggplot2::layer()]. `id` is a
#'   required aesthetic.
#' @param ... Further parameters passed to the geom, such as `stroke`.
#' @param method The 'infercalcR' method id, `"t_means_paired"` or
#'   `"signed_rank_wilcoxon"`. Required.
#' @param conf.level,alternative Passed to [infercalcR::method_spec()] when
#'   set; otherwise the 'infercalcR' defaults apply.
#' @param method_args A named list of further arguments for
#'   [infercalcR::run_inference()]. The data arguments are set by the layer.
#' @param label `"estimate_ci_p"` (default) writes the label, `"none"` draws
#'   no label.
#' @param unit `NULL` or a single string appended to the estimate, such as
#'   `"mmol/l"`.
#' @param mode `"light"` (default, black ink) or `"dark"` (white ink); match
#'   the mode of [theme_figure()].
#' @param transparent If `TRUE`, the layer draws no background fill (open
#'   marks stay empty); accepted by every layer of the package.
#' @param na.rm Kept for the layer interface; incomplete pairs are always
#'   dropped.
#' @return A ggplot2 layer. Its computed data has the columns `x`, `y`, `id`,
#'   `part` (`"unit"`, `"mean"` or `"label"`), `label`, `.ip_result` and
#'   `.ip_result_id`, so [inference_results()] and [figure_report()] return
#'   the paired result.
#' @seealso [stat_compare()] for independent groups, [inference_results()]
#' @examples
#' library(ggplot2)
#' set.seed(9)
#' before <- round(rnorm(14, 4.4, 1.3), 1)
#' after <- round(pmax(before - rnorm(14, 1.6, 0.8), 0.6), 1)
#' d <- data.frame(id = rep(1:14, 2), value = c(before, after),
#'                 time = factor(rep(c("Admission", "24 h"), each = 14),
#'                               levels = c("Admission", "24 h")))
#' ggplot(d, aes(time, value, id = id)) +
#'   stat_paired(method = "t_means_paired", unit = "mmol/l") +
#'   coord_cartesian(clip = "off") +
#'   labs(x = NULL, y = "Lactate (mmol/l)") +
#'   theme_figure("slide") +
#'   theme(plot.margin = margin(1, 21, 1, 1, "mm"))
#' @export
stat_paired <- function(mapping = NULL, data = NULL, ..., method, conf.level = NULL,
                        alternative = NULL, method_args = list(),
                        label = c("estimate_ci_p", "none"), unit = NULL, mode = "light",
                        transparent = FALSE, na.rm = FALSE, inherit.aes = TRUE) {
  method <- .ip_check_method(if (missing(method)) NULL else method, "paired")
  conf.level <- .ip_check_conf_level(conf.level)
  alternative <- .ip_check_alternative(alternative)
  .ip_check_method_args(method_args)
  label <- .ip_check_choice(label, c("estimate_ci_p", "none"), "label")
  unit <- .ip_check_optional_string(unit, "unit")
  ink <- .ip_mode(mode, transparent)$ink
  .ip_check_flag(na.rm, "na.rm")
  dots <- list(...)
  params <- c(list(method = method, alternative = alternative, conf.level = conf.level,
                   method_args = method_args, label_type = label, unit = unit,
                   family = figure_font("sans"), na.rm = na.rm),
              dots)
  if (!"colour" %in% names(dots) && !"color" %in% names(dots)) params$colour <- ink
  ggplot2::layer(stat = StatPaired, data = data, mapping = mapping, geom = GeomPaired,
                 position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
                 params = params)
}
