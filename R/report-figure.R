#' One-line black-and-white figure with inference layers
#'
#' `report_figure()` is added to a ggplot with `+`, like a theme. It returns a
#' plain list of 'ggplot2' components: the [theme_figure()] theme, the
#' black-and-white scales, the raw observations and the inference layers you
#' ask for. 'ggplot2' adds a list element by element, so
#' `ggplot(...) + report_figure(...)` is exactly the same as adding the parts by
#' hand (see the examples).
#'
#' The list holds, in this order:
#' 1. `theme_figure(target, base_size = base_size, legend = legend, mode =
#'    mode, transparent = transparent)`;
#' 2. [scale_colour_ink()], [scale_linetype_figure()], [scale_shape_figure()]
#'    and [scale_fill_hatch()];
#' 3. unless `points = "none"`, the raw observations as open circles in the
#'    ink colour (`geom_point(shape = 1, colour = "black", size = 1.2)` in
#'    light mode), jittered
#'    horizontally with `position_jitter(width = 0.08, height = 0, seed = 1)`
#'    for `points = "jitter"` (meant for a discrete x; the fixed seed keeps the
#'    figure reproducible) or unmoved for `points = "plain"` (a continuous x);
#' 4. if `means = TRUE`, `stat_means(conf.level = conf.level, offset =
#'    offset)`, where `offset` is 0.28 x units for `points = "jitter"` (the
#'    mean and its interval sit right of the jittered observations, so they
#'    never overlap) and 0 otherwise;
#' 5. if `omnibus` is set, `stat_omnibus(method = omnibus)`;
#' 6. if `compare` is set, `stat_compare(method = compare, comparisons =
#'    comparisons, reference = reference, adjust = adjust, label = label)`,
#'    with `headroom = 2` when `omnibus` is also set, so the omnibus label at
#'    the panel top sits above the top bracket label;
#' 7. if `correlation` is set, `stat_correlation(method = correlation)`.
#'
#' `mode` and `transparent` are passed to every component that draws (the
#' theme, [scale_colour_ink()], [scale_fill_hatch()], the points and every
#' layer), so one argument switches the whole figure between light, dark and
#' transparent; see [save_figure_set()] to write all four versions.
#' `palette` is passed the same way; with `"grey"` or `"tufte"` the raw points
#' are drawn in grey as context, and every other component stays in ink.
#'
#' No test is ever added unless its procedure is named: `compare`, `omnibus`
#' and `correlation` take 'infercalcR' method ids and default to `NULL`. With
#' more than one comparison, `adjust` must be declared (`"none"` is allowed),
#' as in [stat_compare()]: with explicit `comparisons` this is checked here,
#' with `reference` or all pairs when the plot is built.
#'
#' Every argument is checked before any layer is created, and the layer
#' constructors check theirs too. Arguments that only make sense with
#' `compare` (`comparisons`, `reference`, `adjust`) are rejected without it.
#'
#' @param target `"paper"` (default) or `"slide"`, passed to [theme_figure()].
#' @param compare The 'infercalcR' method id for pairwise comparisons (see
#'   [stat_compare()]), or `NULL` (default) for none.
#' @param comparisons,reference Which pairs to compare, as in
#'   [stat_compare()]; only with `compare`.
#' @param adjust The multiplicity adjustment for the comparisons, one of
#'   [stats::p.adjust.methods]; required with more than one comparison.
#' @param omnibus The 'infercalcR' method id of an omnibus test (see
#'   [stat_omnibus()]), or `NULL` (default).
#' @param correlation The 'infercalcR' method id of a correlation (see
#'   [stat_correlation()]), or `NULL` (default).
#' @param means If `TRUE` (default), draw group means with confidence
#'   intervals ([stat_means()]).
#' @param points How to draw the raw observations: `"jitter"` (default),
#'   `"plain"` or `"none"`.
#' @param label Label type of the comparison brackets, as in [stat_compare()].
#' @param conf.level Confidence level of the mean intervals.
#' @param legend Legend placement, as in [theme_figure()].
#' @param base_size Base font size in points, as in [theme_figure()].
#' @param mode `"light"` (default, black on white) or `"dark"` (white on
#'   black), as in [theme_figure()].
#' @param transparent If `TRUE`, no background is drawn, as in
#'   [theme_figure()].
#' @param palette `"onebit"` (default), `"grey"` or `"tufte"`, as in
#'   [theme_figure()].
#' @param ... Must be empty; any other argument is an error of class
#'   `infereportR_invalid_input`.
#' @return A plain list of 'ggplot2' components (theme, scales and layers) to
#'   add to a plot with `+`.
#' @seealso [theme_figure()], [stat_means()], [stat_compare()],
#'   [stat_omnibus()], [stat_correlation()], [inference_results()],
#'   [check_figure()]
#' @examples
#' library(ggplot2)
#' # One line (print p to draw it; the tests run when it is drawn):
#' p <- ggplot(PlantGrowth, aes(group, weight)) +
#'   report_figure(compare = "t_means_two_independent_welch", reference = "ctrl",
#'                 adjust = "holm", omnibus = "anova_one_way") +
#'   labs(x = "Group", y = "Dried weight (g)")
#'
#' \donttest{
#' p
#'
#' # The same figure, part by part:
#' q <- ggplot(PlantGrowth, aes(group, weight)) +
#'   theme_figure("paper", base_size = 9, legend = "inside") +
#'   scale_colour_ink() + scale_linetype_figure() + scale_shape_figure() +
#'   scale_fill_hatch() +
#'   geom_point(shape = 1, colour = "black", size = 1.2,
#'              position = position_jitter(width = 0.08, height = 0, seed = 1)) +
#'   stat_means(conf.level = 0.95) +
#'   stat_omnibus(method = "anova_one_way") +
#'   stat_compare(method = "t_means_two_independent_welch", reference = "ctrl",
#'                adjust = "holm", label = "p") +
#'   labs(x = "Group", y = "Dried weight (g)")
#'
#' names(inference_results(p))
#' }
#'
#' # White on black for a dark slide (print it to draw):
#' dark <- ggplot(PlantGrowth, aes(group, weight)) +
#'   report_figure("slide", mode = "dark") +
#'   labs(x = "Group", y = "Dried weight (g)")
#' @export
report_figure <- function(target = c("paper", "slide"), compare = NULL, comparisons = NULL,
                          reference = NULL, adjust = NULL, omnibus = NULL, correlation = NULL,
                          means = TRUE, points = c("jitter", "plain", "none"), label = "p",
                          conf.level = 0.95, legend = "inside", base_size = 9,
                          mode = "light", transparent = FALSE, palette = "onebit", ...) {
  dots <- list(...)
  if (length(dots)) {
    nm <- names(dots)
    nm <- if (is.null(nm)) rep("", length(dots)) else nm
    nm[!nzchar(nm)] <- "<unnamed>"
    .ir_abort(sprintf("report_figure() has no argument %s",
                      paste0("\"", nm, "\"", collapse = ", ")))
  }
  target <- .ir_check_choice(target, c("paper", "slide"), "target")
  points <- .ir_check_choice(points, c("jitter", "plain", "none"), "points")
  .ir_check_flag(means, "means")
  conf.level <- .ir_check_conf_level(conf.level)
  if (is.null(conf.level)) .ir_abort("conf.level must be a single number between 0 and 1")
  legend <- .ir_check_choice(legend, c("inside", "right", "bottom", "none"), "legend")
  label <- .ir_check_choice(label, c("p", "estimate_ci", "estimate_ci_p", "stars"), "label")
  .ir_check_number(base_size, "base_size")
  if (base_size <= 0) .ir_abort("base_size must be a positive number")
  m <- .ir_mode(mode, transparent, palette)
  if (!is.null(compare)) .ir_check_method(compare, "compare", "compare")
  if (!is.null(omnibus)) .ir_check_method(omnibus, "omnibus", "omnibus")
  if (!is.null(correlation)) .ir_check_method(correlation, "correlation", "correlation")
  if (is.null(compare)) {
    stray <- c("comparisons", "reference", "adjust")[
      !c(is.null(comparisons), is.null(reference), is.null(adjust))]
    if (length(stray)) {
      .ir_abort(sprintf("%s only apply with compare; name the comparison procedure",
                        paste(stray, collapse = ", ")))
    }
  }

  # Layer constructors run (and validate) before anything is returned.
  theme <- theme_figure(target, base_size = base_size, legend = legend, mode = mode,
                        transparent = transparent, palette = palette)
  out <- list(theme, scale_colour_ink(mode = mode, palette = palette), scale_linetype_figure(),
              scale_shape_figure(), scale_fill_hatch(mode = mode, transparent = transparent))
  if (points != "none") {
    position <- if (points == "jitter") {
      ggplot2::position_jitter(width = 0.08, height = 0, seed = 1)
    } else {
      ggplot2::position_identity()
    }
    # The raw points are context: grey when the palette has a grey.
    out <- c(out, list(ggplot2::geom_point(shape = 1, colour = m$grey, size = 1.2,
                                           position = position)))
  }
  if (means) {
    # Beside jittered points the summary moves right, so they never overlap.
    offset <- if (points == "jitter") .ir_means_offset else 0
    out <- c(out, list(stat_means(conf.level = conf.level, offset = offset, mode = mode,
                                  transparent = transparent, palette = palette)))
  }
  if (!is.null(omnibus)) {
    out <- c(out, list(stat_omnibus(method = omnibus, mode = mode, transparent = transparent,
                                    palette = palette)))
  }
  if (!is.null(compare)) {
    # With an omnibus label at the panel top, one more step of headroom keeps
    # it clear of the top bracket label.
    headroom <- if (is.null(omnibus)) 1 else 2
    out <- c(out, list(stat_compare(method = compare, comparisons = comparisons,
                                    reference = reference, adjust = adjust, label = label,
                                    headroom = headroom, mode = mode,
                                    transparent = transparent, palette = palette)))
  }
  if (!is.null(correlation)) {
    out <- c(out, list(stat_correlation(method = correlation, mode = mode,
                                        transparent = transparent, palette = palette)))
  }
  structure(out, class = "list")
}
