# The twelve Bleisatz example figures, rebuilt with the package API
# (amendment D15, A6). The data come from .ip_gallery_data(); every figure is
# a function of (data, mode, transparent) that returns a ggplot.

# File name, slot and caption of each figure, as in the reference table
# captions.csv of the Bleisatz gallery. Shipped as an R object (no data files in
# the package); `name` is the file name without extension.
.ip_gallery_captions <- data.frame(
  name = c("01-timecourse-direct-labels", "02-raw-data-mean-ci", "03-stacked-hatch",
           "04-scatter-correlation", "05-ecdf-distribution", "06-forest-subgroups",
           "07-small-multiples", "08-correlation-matrix", "09-paired-slopegraph",
           "10-character-mode", "11-character-mode-c64", "12-character-mode-c64-bw"),
  slot = c("side", "side", "side", "half", "side", "full", "full", "side", "half", "side",
           "half", "half"),
  width_mm = c(86, 86, 86, 64, 86, 134, 134, 86, 64, 86, 64, 64),
  height_mm = rep(44, 12),
  caption = c(
    paste("Lines are labelled at their ends; there is no legend to decode. Line type and",
          "marker shape separate the groups, never colour."),
    paste("Every observation is shown (open circles), with the mean and 95% CI beside it.",
          "Comparisons with ISS < 16: Welch t-tests, Holm-adjusted p."),
    paste("Open, hatched and solid fills replace colour. Categories are named beside the",
          "last bar instead of in a legend."),
    paste("Pearson correlation of log IL-6 with ISS, with its 95% CI from infercalcR. The",
          "log axis is labelled in original units."),
    paste("The empirical distribution function shows every patient, the medians (dotted)",
          "and the spread in one figure."),
    paste("Subgroup estimates as differences with 95% CIs (Welch, infercalcR). Marker area",
          "is proportional to n; the dotted line is no difference."),
    paste("Three markers, one design: identical x axes, free y axes in their own units,",
          "lines labelled once in the last panel."),
    paste("Spearman correlations: above the diagonal, circle area shows the strength, filled",
          "is positive and open is negative. The values are mirrored below the diagonal."),
    paste("Each line is one patient; the thick line is the mean. Paired t-test from",
          "infercalcR: mean change with its 95% CI."),
    paste("Character mode: the time course from figure 1 redrawn with text characters, as",
          "on an 80s home computer."),
    paste("The same plot in C64 colours (Pepto palette) with the READY prompt. For effect",
          "only; not for data slides."),
    paste("The C64 screen in two values: black screen, white glyphs, a dot-halftone border",
          "for the lighter border blue.")),
  stringsAsFactors = FALSE
)

.ip_gallery_presets <- c(side = "slide_side", half = "slide_half", full = "slide_full")

# Counts shown under the x labels, written by infereportR.
.ip_gallery_n <- function(n) infereportR::format_estimate(as.numeric(n))

# 01 Time course with direct labels.
.ip_gallery_01 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ip_mode(mode, transparent)
  ggplot2::ggplot(d$med, ggplot2::aes(.data$hours, .data$C5a, linetype = .data$severity,
                                      shape = .data$severity, label = .data$severity)) +
    ggplot2::geom_line(linewidth = .ip_lw(.ip_data_line), colour = m$ink) +
    ggplot2::geom_point(size = 1.6, colour = m$ink, fill = m$knock, stroke = .ip_lw(0.6)) +
    geom_endlabel(mode = mode, transparent = transparent) +
    scale_linetype_figure() +
    scale_shape_figure(values = c(16, 21)) +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 24, 48),
                                expand = ggplot2::expansion(mult = c(0.02, 0))) +
    ggplot2::scale_y_continuous(limits = c(5, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Hours after admission", y = "C5a (ng/ml), median") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent) +
    ggplot2::theme(plot.margin = ggplot2::margin(1, 18, 1, 1, "mm"))
}

# 02 Raw data, mean and CI, Holm-adjusted Welch comparisons with ISS < 16.
.ip_gallery_02 <- function(d, mode = "light", transparent = FALSE) {
  il6 <- d$il6
  groups <- levels(il6$group)
  n <- .ip_gallery_n(table(il6$group))
  ggplot2::ggplot(il6, ggplot2::aes(.data$group, .data$value)) +
    inferplot("slide", compare = "t_means_two_independent_welch",
              comparisons = list(groups[c(1, 2)], groups[c(1, 3)]), adjust = "holm",
              legend = "none", mode = mode, transparent = transparent) +
    ggplot2::scale_x_discrete(labels = paste0(groups, "\nn = ", n),
                              expand = ggplot2::expansion(add = c(0.4, 0.5))) +
    # The comparison layer reserves headroom for the top bracket label.
    ggplot2::scale_y_continuous(limits = c(0, NA), breaks = seq(0, 200, 50),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "IL-6 at admission (pg/ml)") +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 1, 1, 1, "mm"))
}

# 03 Stacked percentages with hatch fills, categories named beside the last bar.
.ip_gallery_03 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ip_mode(mode, transparent)
  counts <- d$counts
  groups <- levels(counts$group)
  ntot <- .ip_gallery_n(tapply(counts$n, counts$group, sum))
  ggplot2::ggplot(counts, ggplot2::aes(.data$group, .data$n, fill = .data$outcome)) +
    ggplot2::geom_col(position = "fill", width = 0.62, colour = m$ink,
                      linewidth = .ip_lw(.ip_axis_line)) +
    stat_stacklabel(mode = mode, transparent = transparent) +
    scale_fill_hatch(patterns = c("open", "hatch", "solid"), mode = mode,
                     transparent = transparent) +
    ggplot2::scale_x_discrete(labels = paste0(sub("ISS ", "", groups), "\nn = ", ntot),
                              expand = ggplot2::expansion(add = 0.45)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%"),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Injury Severity Score", y = "Patients") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent) +
    ggplot2::theme(plot.margin = ggplot2::margin(1, 23, 1, 1, "mm"))
}

# 04 Scatter with a Pearson correlation of log10 IL-6 (the stat sees the
# transformed scale) and ISS, labeled on two lines with n.
.ip_gallery_04 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ip_mode(mode, transparent)
  ggplot2::ggplot(d$sc, ggplot2::aes(.data$iss, .data$il6)) +
    ggplot2::geom_point(shape = 1, size = 1.2, colour = m$ink, stroke = .ip_lw(0.5)) +
    stat_correlation(method = "correlation_pearson", label = "estimate_ci_p_n", label_x = 9,
                     mode = mode, transparent = transparent) +
    ggplot2::scale_y_log10(breaks = c(10, 30, 100, 300, 1000), limits = c(10, 1000)) +
    ggplot2::scale_x_continuous(breaks = c(9, 16, 25, 50)) +
    ggplot2::labs(x = "Injury Severity Score", y = "IL-6 (pg/ml), log scale") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent)
}

# 05 Empirical distribution functions with median guides and curve labels.
.ip_gallery_05 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ip_mode(mode, transparent)
  ttp <- data.frame(severity = d$cohort$severity, peak = d$cohort$peak)
  ggplot2::ggplot(ttp, ggplot2::aes(.data$peak, linetype = .data$severity)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = .ip_lw(.ip_data_line), colour = m$ink,
                       pad = FALSE) +
    stat_ecdf_guides(curve_labels = c(0.85, 0.35), mode = mode, transparent = transparent) +
    scale_linetype_figure() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    ggplot2::coord_cartesian(xlim = c(2, 24), ylim = c(0, 1), clip = "off") +
    ggplot2::labs(x = "Hours to C5a peak", y = "Cumulative proportion") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent)
}

# 06 Forest plot of seven Welch results (late minus early peak).
.ip_gallery_06 <- function(d, mode = "light", transparent = FALSE) {
  fp <- d$fp
  subsets <- .ip_gallery_subsets(fp)
  spec <- infercalcR::method_spec("t_means_two_independent_welch")
  results <- lapply(names(subsets), function(nm) {
    s <- subsets[[nm]]
    infercalcR::run_inference(spec, x = fp$il6[s & fp$late == 1],
                              y = fp$il6[s & fp$late == 0], result_id = nm)
  })
  names(results) <- names(subsets)
  figure_forest(results, xlab = "IL-6 difference, late minus early peak (pg/ml)",
                xlim = c(-30, 60), mode = mode, transparent = transparent,
                slot = "slide_full") +
    ggplot2::scale_x_continuous(breaks = c(-20, 0, 20, 40))
}

# 07 Small multiples: three markers, free y, end labels in the last panel only.
.ip_gallery_07 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ip_mode(mode, transparent)
  med <- d$med
  markers <- c("C3a (ng/ml)", "C5a (ng/ml)", "IL-6 (pg/ml)")
  mlong <- rbind(
    data.frame(marker = markers[1], severity = med$severity, hours = med$hours, value = med$C3a),
    data.frame(marker = markers[2], severity = med$severity, hours = med$hours, value = med$C5a),
    data.frame(marker = markers[3], severity = med$severity, hours = med$hours, value = med$IL6))
  mlong$marker <- factor(mlong$marker, levels = markers)
  last <- mlong[mlong$marker == markers[3], , drop = FALSE]
  ggplot2::ggplot(mlong, ggplot2::aes(.data$hours, .data$value, linetype = .data$severity,
                                      label = .data$severity)) +
    ggplot2::geom_line(linewidth = .ip_lw(.ip_data_line), colour = m$ink) +
    geom_endlabel(data = last, mode = mode, transparent = transparent) +
    ggplot2::facet_wrap(ggplot2::vars(.data$marker), scales = "free_y", nrow = 1) +
    scale_linetype_figure() +
    ggplot2::scale_x_continuous(breaks = c(0, 12, 24, 48)) +
    ggplot2::scale_y_continuous(limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Hours after admission", y = "Median") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent) +
    ggplot2::theme(panel.spacing = grid::unit(6, "mm"),
                   plot.margin = ggplot2::margin(1, 17, 1, 1, "mm"))
}

# 08 Spearman correlation matrix without colour.
.ip_gallery_08 <- function(d, mode = "light", transparent = FALSE) {
  figure_correlation_matrix(as.data.frame(d$mk), method = "correlation_spearman",
                            mode = mode, transparent = transparent)
}

# 09 Paired slopegraph with the paired t-test.
.ip_gallery_09 <- function(d, mode = "light", transparent = FALSE) {
  pl <- d$pl
  long <- data.frame(id = rep(pl$id, 2), value = c(pl$admission, pl$h24),
                     time = factor(rep(c("Admission", "24 h"), each = nrow(pl)),
                                   levels = c("Admission", "24 h")))
  ggplot2::ggplot(long, ggplot2::aes(.data$time, .data$value, id = .data$id)) +
    stat_paired(method = "t_means_paired", unit = "mmol/l", mode = mode,
                transparent = transparent) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.25, 0.1))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "Lactate (mmol/l)") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent) +
    ggplot2::theme(plot.margin = ggplot2::margin(1, 21, 1, 1, "mm"))
}

# The source plot of the character figures: the medians of figure 01.
.ip_gallery_char_source <- function(d) {
  ggplot2::ggplot(d$med, ggplot2::aes(.data$hours, .data$C5a, shape = .data$severity,
                                      label = .data$severity)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$severity)) +
    ggplot2::geom_point() +
    geom_endlabel() +
    ggplot2::labs(x = "Hours after admission", y = "C5a (ng/ml)")
}

# 10 Character mode, 1 bit.
.ip_gallery_10 <- function(d, mode = "light", transparent = FALSE) {
  char_figure(.ip_gallery_char_source(d), style = "bw", slot = "slide_side", mode = mode,
              transparent = transparent)
}

# 11 Character mode in C64 colours.
.ip_gallery_11 <- function(d, mode = "light", transparent = FALSE) {
  char_figure(.ip_gallery_char_source(d), style = "c64", slot = "slide_half", mode = mode,
              transparent = transparent)
}

# 12 The C64 screen in two values.
.ip_gallery_12 <- function(d, mode = "light", transparent = FALSE) {
  char_figure(.ip_gallery_char_source(d), style = "c64bw", slot = "slide_half", mode = mode,
              transparent = transparent)
}

.ip_gallery_builders <- list(
  .ip_gallery_01, .ip_gallery_02, .ip_gallery_03, .ip_gallery_04, .ip_gallery_05,
  .ip_gallery_06, .ip_gallery_07, .ip_gallery_08, .ip_gallery_09, .ip_gallery_10,
  .ip_gallery_11, .ip_gallery_12
)

#' The Bleisatz example figures
#'
#' `figure_gallery()` writes the twelve example figures of the Bleisatz Beamer
#' theme, each in up to four modes, with [save_figure_set()]. Every figure
#' teaches one rule of the black-and-white system and is sized for one
#' Bleisatz slot (all 44 mm high): `"slide_full"` (134 mm), `"slide_side"`
#' (86 mm) or `"slide_half"` (64 mm).
#'
#' | File | Slot | Built with |
#' |---|---|---|
#' | `01-timecourse-direct-labels` | side | [theme_figure()], [scale_linetype_figure()], [scale_shape_figure()], [geom_endlabel()] |
#' | `02-raw-data-mean-ci` | side | [inferplot()] with Welch comparisons against ISS < 16, Holm-adjusted |
#' | `03-stacked-hatch` | side | `geom_col()`, [scale_fill_hatch()], [stat_stacklabel()] |
#' | `04-scatter-correlation` | half | [stat_correlation()] (Pearson, two-line label with n) on a log10 y axis |
#' | `05-ecdf-distribution` | side | `stat_ecdf()`, [stat_ecdf_guides()] |
#' | `06-forest-subgroups` | full | [figure_forest()] on seven Welch results, x axis -30 to 60 |
#' | `07-small-multiples` | full | facets with free y, [geom_endlabel()] in the last panel |
#' | `08-correlation-matrix` | side | [figure_correlation_matrix()] (Spearman) |
#' | `09-paired-slopegraph` | half | [stat_paired()] (paired t-test) |
#' | `10-character-mode` | side | [char_figure()], style `"bw"` |
#' | `11-character-mode-c64` | half | [char_figure()], style `"c64"` |
#' | `12-character-mode-c64-bw` | half | [char_figure()], style `"c64bw"` |
#'
#' All data are simulated with fixed seeds (the simulation of the Bleisatz
#' gallery, by the same authors), so the figures and every number on them are
#' the same on every run. The random state of the session is left as it was:
#' `.Random.seed` is restored afterwards, or removed again if it did not
#' exist. Statistics come from 'infercalcR' and displayed numbers from
#' 'infereportR'.
#'
#' The files are written as `dir/<mode>/<name>.pdf` (and `.png` with
#' `png = TRUE`), the folder convention of [save_figure_set()]. A Bleisatz
#' deck regenerates its `figures/` folder with
#' `Rscript -e 'inferplotR::figure_gallery("figures")'`.
#'
#' The captions of the gallery deck are kept in the package as the internal
#' data frame `inferplotR:::.ip_gallery_captions` (columns `name`, `slot`,
#' `width_mm`, `height_mm` and `caption`); no data file is shipped.
#'
#' @param dir Directory that holds the mode folders; a temporary directory by
#'   default.
#' @param modes Mode folders to write, any of `"light"`, `"dark"`,
#'   `"light-transparent"` and `"dark-transparent"` (all four by default).
#'   `character()` writes nothing.
#' @param png If `TRUE`, also write a 300 dpi PNG beside each PDF; `FALSE` by
#'   default.
#' @return The rows of [save_figure_set()] for all figures, bound in figure
#'   order: a data.frame with one row per written file and the columns `mode`,
#'   `file`, `format`, `width_mm` and `height_mm` (12 x modes x formats rows;
#'   0 rows with the same columns for `modes = character()`).
#' @seealso [save_figure_set()], [figure_size()]
#' @examples
#' \donttest{
#' if (capabilities("cairo")) {
#'   dir <- file.path(tempdir(), "gallery")
#'   files <- figure_gallery(dir, modes = "light")
#'   files[, c("file", "width_mm", "height_mm")]
#'   unlink(dir, recursive = TRUE)
#' }
#' }
#' @export
figure_gallery <- function(dir = tempdir(),
                           modes = c("light", "dark", "light-transparent", "dark-transparent"),
                           png = FALSE) {
  .ip_check_string(dir, "dir")
  if (!is.character(modes) || anyNA(modes) || anyDuplicated(modes) ||
      !all(modes %in% .ip_set_modes)) {
    .ip_abort(sprintf("modes must be distinct values of %s",
                      paste0("\"", .ip_set_modes, "\"", collapse = ", ")))
  }
  .ip_check_flag(png, "png")
  if (!length(modes)) return(.ip_set_empty())
  data <- .ip_gallery_data()
  rows <- lapply(seq_len(nrow(.ip_gallery_captions)), function(i) {
    builder <- .ip_gallery_builders[[i]]
    build <- function(mode, transparent) builder(data, mode = mode, transparent = transparent)
    save_figure_set(build, .ip_gallery_captions$name[[i]],
                    preset = .ip_gallery_presets[[.ip_gallery_captions$slot[[i]]]], dir = dir,
                    modes = modes, png = png)
  })
  out <- do.call(rbind, c(list(.ip_set_empty()), rows))
  rownames(out) <- NULL
  out
}
