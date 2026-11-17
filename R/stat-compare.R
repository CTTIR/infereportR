#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCompare <- ggplot2::ggproto("StatCompare", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_layer = function(self, data, params, layout) {
    .ip_compute_layer(self, data, params, layout)
  },
  compute_panel = function(data, scales, method, comparisons = NULL, reference = NULL,
                           adjust = NULL, family_id = NULL, alternative = NULL,
                           conf.level = NULL, method_args = list(), label_type = "p",
                           first = 0.10, step = 0.20, tip_length = 0.02, headroom_steps = 1,
                           y_label = "y") {
    if (!.ip_is_discrete(scales$x)) {
      .ip_abort("stat_compare() needs a discrete x scale", code = "unsupported")
    }
    panel <- data$PANEL[1L]
    levels <- as.character(scales$x$get_limits())
    pairs <- .ip_compare_pairs(comparisons, reference, levels)
    .ip_check_adjust(adjust, length(pairs))
    data <- .ip_finite_xy(data)
    if (!nrow(data) || !length(pairs)) return(.ip_compare_empty())
    id <- .ip_result_id("compare", method, panel)
    spec <- .ip_spec(method, alternative = alternative, conf.level = conf.level)
    values <- function(level) data$y[data$x == match(level, levels)]
    rows <- lapply(pairs, function(pair) {
      args <- c(list(spec, x = values(pair[[2L]]), y = values(pair[[1L]])), method_args)
      run <- .ip_wrap(do.call(infercalcR::run_inference, c(args, result_id = id)),
                      "infercalcR::run_inference()")
      # Each two-sample method reports one row (a contrast or a rank test).
      infercalcR::result_data(run)[1L, , drop = FALSE]
    })
    rows <- do.call(rbind, rows)
    rows$row_id <- vapply(pairs, function(p) paste0(p[[2L]], "-", p[[1L]]), character(1))
    rows$variable_id <- rep(y_label, nrow(rows))
    rows$contrast <- vapply(pairs, function(p) paste(p[[2L]], "minus", p[[1L]]), character(1))
    rownames(rows) <- NULL
    result <- .ip_wrap(infercalcR::new_inference_result(rows, list(
      method_id = method, result_id = id,
      population = sprintf("figure data, panel %s", as.character(panel)))),
      "infercalcR::new_inference_result()")
    adjusted <- !is.null(adjust) && !identical(adjust, "none")
    if (adjusted) {
      result <- .ip_wrap(infercalcR::adjust_inference(
        result, method = adjust, family_id = if (is.null(family_id)) id else family_id),
        "infercalcR::adjust_inference()")
    }
    shown <- .ip_display(result, p = if (adjusted) "adjusted" else "raw")
    shown <- shown[match(rows$row_id, shown$row_id), , drop = FALSE]
    finite_y <- data$y
    top <- max(finite_y)
    rng <- diff(range(finite_y))
    if (!is.finite(rng) || rng <= 0) rng <- max(abs(top), 1)
    k <- length(pairs)
    bar <- top + (first + (seq_len(k) - 1L) * step) * rng
    lefts <- vapply(pairs, function(p) match(p[[1L]], levels), integer(1)) + 0
    rights <- vapply(pairs, function(p) match(p[[2L]], levels), integer(1)) + 0
    out <- data.frame(
      x = lefts, xend = rights, y = bar, yend = bar,
      label = .ip_compare_labels(shown, label_type),
      tip = rep(tip_length * rng, k), headroom = rep(FALSE, k),
      group = seq_len(k), PANEL = panel, stringsAsFactors = FALSE)
    # One invisible row `headroom_steps` steps above the top bracket trains the
    # y scale, so the label of the top bracket is never clipped. GeomBracket
    # skips it.
    room <- top + (first + (k - 1 + headroom_steps) * step) * rng
    out <- rbind(out, data.frame(
      x = lefts[[1L]], xend = lefts[[1L]], y = room, yend = room, label = "", tip = 0,
      headroom = TRUE, group = k + 1L, PANEL = panel, stringsAsFactors = FALSE))
    .ip_attach_result(out, result, id)
  }
)

.ip_compare_empty <- function() {
  out <- data.frame(x = double(), xend = double(), y = double(), yend = double(),
                    label = character(), tip = double(), headroom = logical(),
                    group = integer(), stringsAsFactors = FALSE)
  out$.ip_result <- list()
  out$.ip_result_id <- character()
  out
}

.ip_compare_labels <- function(shown, label) {
  switch(label,
    p = shown$p_display,
    estimate_ci = paste(shown$estimate_display, shown$interval_display),
    estimate_ci_p = paste0(shown$estimate_display, " ", shown$interval_display, ", ",
                           shown$p_display),
    stars = {
      p <- shown$p_selected
      stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", NA)))
      # Without a stored p the display string carries the status, never "ns".
      ifelse(is.na(p) | is.na(stars), shown$p_display, stars)
    }
  )
}

.ip_check_comparisons <- function(comparisons) {
  if (is.null(comparisons)) return(NULL)
  ok <- is.list(comparisons) && length(comparisons) > 0L &&
    all(vapply(comparisons, function(p) {
      is.character(p) && length(p) == 2L && !anyNA(p) && all(nzchar(p)) && p[[1L]] != p[[2L]]
    }, logical(1)))
  if (!ok) {
    .ip_abort("comparisons must be a list of pairs of two different x levels")
  }
  comparisons
}

.ip_compare_pairs <- function(comparisons, reference, levels) {
  if (!is.null(comparisons)) {
    unknown <- setdiff(unlist(comparisons), levels)
    if (length(unknown)) {
      .ip_abort(sprintf("comparisons name unknown x levels: %s",
                        paste0("\"", unknown, "\"", collapse = ", ")))
    }
    return(comparisons)
  }
  if (!is.null(reference)) {
    if (!reference %in% levels) {
      .ip_abort(sprintf("reference \"%s\" is not an x level", reference))
    }
    return(lapply(setdiff(levels, reference), function(l) c(reference, l)))
  }
  if (length(levels) < 2L) return(list())
  idx <- utils::combn(length(levels), 2L)
  lapply(seq_len(ncol(idx)), function(j) levels[idx[, j]])
}

.ip_check_adjust <- function(adjust, n_pairs) {
  if (is.null(adjust)) {
    if (n_pairs > 1L) {
      .ip_abort(
        "adjust is missing: declare the multiplicity adjustment; 'none' is allowed")
    }
    return(NULL)
  }
  .ip_check_choice(adjust, c("none", stats::p.adjust.methods), "adjust")
}

#' Pairwise comparisons with brackets
#'
#' Compares x groups pairwise with an explicitly named two-sample procedure
#' of 'infercalcR', adjusts the comparison family as declared, and draws one
#' bracket per comparison, labeled with 'infereportR' display strings. Each
#' panel is its own family, so facets work.
#'
#' For a pair `c(A, B)` the estimand is B minus A: the values of B are passed
#' as `x` and those of A as `y`, and the contrast reads "B minus A". The
#' pairs of one panel form one 'infercalcR' result (a comparison family) with
#' row ids `"B-A"`, built like the families in the 'infereportR' vignette
#' "Comparison families, adjusted p-values and plot annotations".
#'
#' Nothing is chosen from the data: `method` has no default, and `adjust`
#' must be declared whenever more than one comparison is drawn (`"none"` is
#' allowed). With explicit `comparisons` this is checked when the layer is
#' created; with `reference` or all pairs, when the plot is built, where
#' ggplot2 reports it with the inferplotR condition as parent. Rows with
#' non-finite x or y are dropped before the call.
#'
#' Bracket i (in pair order) sits at `top + (first + (i - 1) * step) * range`
#' of the panel's finite y values, with tips of `tip * range`. One invisible
#' headroom row per panel, at `top + (first + (k - 1 + headroom) * step) *
#' range` for k brackets, trains the y scale so the label of the top bracket
#' is never clipped; it is marked `headroom = TRUE` in the layer data and not
#' drawn.
#'
#' @param mapping,data,inherit.aes As in [ggplot2::layer()].
#' @param ... Further parameters passed to the geom, such as `linewidth`.
#' @param method The 'infercalcR' method id, one of
#'   `"t_means_two_independent_welch"`, `"t_means_two_independent_student"`,
#'   `"rank_sum_mann_whitney"` and `"brunner_munzel"`. Required.
#' @param comparisons A list of length-2 character vectors of x levels.
#' @param reference One x level compared with every other level.
#'   `comparisons` and `reference` are exclusive; with neither, all pairs are
#'   compared in level order.
#' @param adjust The multiplicity adjustment, one of [stats::p.adjust.methods]
#'   (`"none"` allowed); required with more than one comparison.
#' @param family_id The family id recorded by [infercalcR::adjust_inference()];
#'   defaults to the layer's result id.
#' @param alternative,conf.level Passed to [infercalcR::method_spec()] when
#'   set; otherwise the 'infercalcR' defaults apply.
#' @param method_args A named list of further arguments for
#'   [infercalcR::run_inference()], such as `exact` for
#'   `"rank_sum_mann_whitney"`. The data arguments are set by the layer.
#' @param label `"p"` (default), `"estimate_ci"`, `"estimate_ci_p"` or
#'   `"stars"` (`***` < 0.001, `**` < 0.01, `*` < 0.05, otherwise the p
#'   display string).
#' @param first Height of the first bracket above the highest value, as a
#'   fraction of the panel's y range (0.10 by default).
#' @param step Distance between stacked brackets, as a fraction of the
#'   panel's y range (0.20 by default; leaves room for one label line at
#'   slide size).
#' @param tip Tip length as a fraction of the panel's y range.
#' @param headroom Room above the top bracket, in steps (1 by default). With
#'   2, a label at the panel top (such as the [stat_omnibus()] label that
#'   [inferplot()] adds) sits one step above the top bracket label instead of
#'   on it.
#' @param text_size Label size in mm (ggplot2 text units); 8 pt by default.
#' @param family Label font family; `figure_font("sans")` by default.
#' @param mode `"light"` (default, black ink) or `"dark"` (white ink); match
#'   the mode of [theme_figure()].
#' @param transparent If `TRUE`, the layer draws no background fill (open
#'   marks stay empty); passed by [inferplot()] with `mode`.
#' @param na.rm If `TRUE`, missing values are removed silently by the geom.
#' @return A ggplot2 layer. The computed data has columns `x`, `xend`, `y`,
#'   `yend`, `label`, `tip`, `headroom` (`TRUE` for the invisible headroom
#'   row), `.ip_result` (the family result) and `.ip_result_id`.
#' @seealso [stat_omnibus()], [inference_results()], [figure_report()],
#'   [infercalcR::adjust_inference()]
#' @examples
#' library(ggplot2)
#' ggplot(PlantGrowth, aes(group, weight)) +
#'   geom_point(shape = 1, colour = "black") +
#'   stat_compare(method = "t_means_two_independent_welch", reference = "ctrl",
#'                adjust = "holm")
#' @export
stat_compare <- function(mapping = NULL, data = NULL, ..., method, comparisons = NULL,
                         reference = NULL, adjust, family_id = NULL, alternative = NULL,
                         conf.level = NULL, method_args = list(),
                         label = c("p", "estimate_ci", "estimate_ci_p", "stars"),
                         first = 0.10, step = 0.20, tip = 0.02, headroom = 1, text_size = NULL,
                         family = NULL, mode = "light", transparent = FALSE,
                         na.rm = FALSE, inherit.aes = TRUE) {
  m <- .ip_mode(mode, transparent)
  method <- .ip_check_method(if (missing(method)) NULL else method, "compare")
  comparisons <- .ip_check_comparisons(comparisons)
  reference <- .ip_check_optional_string(reference, "reference")
  if (!is.null(comparisons) && !is.null(reference)) {
    .ip_abort("give comparisons or reference, not both")
  }
  adjust <- if (missing(adjust)) NULL else adjust
  if (!is.null(adjust) || !is.null(comparisons)) {
    adjust <- .ip_check_adjust(adjust, if (is.null(comparisons)) 1L else length(comparisons))
  }
  family_id <- .ip_check_optional_string(family_id, "family_id")
  alternative <- .ip_check_alternative(alternative)
  conf.level <- .ip_check_conf_level(conf.level)
  .ip_check_method_args(method_args)
  label <- .ip_check_choice(label, c("p", "estimate_ci", "estimate_ci_p", "stars"), "label")
  .ip_check_number(first, "first", min = 0)
  .ip_check_number(step, "step", min = 0)
  .ip_check_number(tip, "tip", min = 0)
  .ip_check_number(headroom, "headroom", min = 1)
  .ip_check_flag(na.rm, "na.rm")
  dots <- list(...)
  params <- c(list(method = method, comparisons = comparisons, reference = reference,
                   adjust = adjust, family_id = family_id, alternative = alternative,
                   conf.level = conf.level, method_args = method_args, label_type = label,
                   first = first, step = step, tip_length = tip, headroom_steps = headroom,
                   na.rm = na.rm),
              .ip_text_params(dots, text_size, family, m$ink), dots)
  layer <- ggplot2::layer(stat = StatCompare, data = data, mapping = mapping,
                          geom = GeomBracket, position = "identity", show.legend = FALSE,
                          inherit.aes = inherit.aes, params = params)
  .ip_labelled_layer(layer)
}
