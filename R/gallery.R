# The twelve Bleisatz example figures, rebuilt with the package API
# (amendment D15, A6). The data come from .ir_gallery_data(); every figure is
# a function of (data, mode, transparent) that returns a ggplot.

# File name, slot and caption of each figure, as in the reference table
# captions.csv of the Bleisatz gallery. Shipped as an R object (no data files in
# the package); `name` is the file name without extension.
.ir_gallery_captions <- data.frame(
  name = c("01-timecourse-direct-labels", "02-raw-data-mean-ci", "03-stacked-hatch",
           "04-scatter-correlation", "05-ecdf-distribution", "06-forest-subgroups",
           "07-small-multiples", "08-correlation-matrix", "09-paired-slopegraph",
           "10-character-mode", "11-character-mode-c64", "12-character-mode-c64-bw"),
  slot = c("side", "side", "side", "half", "side", "full", "full", "side", "half", "side",
           "half", "half"),
  width_mm = c(86, 86, 86, 64, 86, 134, 134, 86, 64, 86, 64, 64),
  height_mm = rep(44, 12),
  caption = c(
    paste("Lines are labeled at their ends; there is no legend to decode. Line type and",
          "marker shape separate the groups, never color."),
    paste("Every observation is shown (open circles), with the mean and 95% CI beside it.",
          "Comparisons with Injury Severity Score (ISS) < 16: Welch t-tests, Holm-adjusted",
          "p."),
    paste("Open, hatched and solid fills replace color. Categories are named beside the",
          "last bar instead of in a legend."),
    paste("Pearson correlation of log IL-6 with ISS, with its 95% CI from infercalcR. The",
          "log axis is labeled in original units."),
    paste("The empirical distribution function shows every patient, the medians (dotted)",
          "and the spread in one figure."),
    paste("Subgroup estimates as differences with 95% CIs (Welch, infercalcR). Marker area",
          "is proportional to n; the dotted line is no difference."),
    paste("Three markers, one design: identical x axes, free y axes in their own units,",
          "lines labeled once in the last panel."),
    paste("Spearman correlations: above the diagonal, circle area shows the strength, filled",
          "is positive and open is negative. The values are mirrored below the diagonal."),
    paste("Each line is one patient; the thick line is the mean. Paired t-test from",
          "infercalcR: mean change with its 95% CI."),
    paste("Character mode: the time course from figure 1 redrawn with text characters, as",
          "on a 1980s home computer."),
    paste("The same plot in C64 colors (Pepto palette) with the READY prompt. For effect",
          "only; not for data slides."),
    paste("The C64 screen in two values: black screen, white glyphs, a dot-halftone border",
          "for the lighter border blue.")),
  stringsAsFactors = FALSE
)

.ir_gallery_presets <- c(side = "slide_side", half = "slide_half", full = "slide_full")

# Counts shown under the x labels, written by format_estimate().
.ir_gallery_n <- function(n) format_estimate(as.numeric(n))

# 01 Time course with direct labels.
.ir_gallery_01 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ir_mode(mode, transparent)
  ggplot2::ggplot(d$med, ggplot2::aes(.data$hours, .data$C5a, linetype = .data$severity,
                                      shape = .data$severity, label = .data$severity)) +
    ggplot2::geom_line(linewidth = .ir_lw(.ir_data_line), colour = m$ink) +
    ggplot2::geom_point(size = 1.6, colour = m$ink, fill = m$knock, stroke = .ir_lw(0.6)) +
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
.ir_gallery_02 <- function(d, mode = "light", transparent = FALSE) {
  il6 <- d$il6
  groups <- levels(il6$group)
  n <- .ir_gallery_n(table(il6$group))
  ggplot2::ggplot(il6, ggplot2::aes(.data$group, .data$value)) +
    report_figure("slide", compare = "t_means_two_independent_welch",
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
.ir_gallery_03 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ir_mode(mode, transparent)
  counts <- d$counts
  groups <- levels(counts$group)
  ntot <- .ir_gallery_n(tapply(counts$n, counts$group, sum))
  ggplot2::ggplot(counts, ggplot2::aes(.data$group, .data$n, fill = .data$outcome)) +
    ggplot2::geom_col(position = "fill", width = 0.62, colour = m$ink,
                      linewidth = .ir_lw(.ir_axis_line)) +
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
.ir_gallery_04 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ir_mode(mode, transparent)
  ggplot2::ggplot(d$sc, ggplot2::aes(.data$iss, .data$il6)) +
    ggplot2::geom_point(shape = 1, size = 1.2, colour = m$ink, stroke = .ir_lw(0.5)) +
    stat_correlation(method = "correlation_pearson", label = "estimate_ci_p_n", label_x = 9,
                     mode = mode, transparent = transparent) +
    ggplot2::scale_y_log10(breaks = c(10, 30, 100, 300, 1000), limits = c(10, 1000)) +
    ggplot2::scale_x_continuous(breaks = c(9, 16, 25, 50)) +
    ggplot2::labs(x = "Injury Severity Score", y = "IL-6 (pg/ml), log scale") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent)
}

# 05 Empirical distribution functions with median guides and curve labels.
.ir_gallery_05 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ir_mode(mode, transparent)
  ttp <- data.frame(severity = d$cohort$severity, peak = d$cohort$peak)
  ggplot2::ggplot(ttp, ggplot2::aes(.data$peak, linetype = .data$severity)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = .ir_lw(.ir_data_line), colour = m$ink,
                       pad = FALSE) +
    stat_ecdf_guides(curve_labels = c(0.85, 0.75), mode = mode, transparent = transparent) +
    scale_linetype_figure() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    ggplot2::coord_cartesian(xlim = c(2, 24), ylim = c(0, 1), clip = "off") +
    ggplot2::labs(x = "Hours to C5a peak", y = "Cumulative proportion") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent)
}

# 06 Forest plot of seven Welch results (late minus early peak).
.ir_gallery_06 <- function(d, mode = "light", transparent = FALSE) {
  fp <- d$fp
  subsets <- .ir_gallery_subsets(fp)
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
.ir_gallery_07 <- function(d, mode = "light", transparent = FALSE) {
  m <- .ir_mode(mode, transparent)
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
    ggplot2::geom_line(linewidth = .ir_lw(.ir_data_line), colour = m$ink) +
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

# 08 Spearman correlation matrix without color.
.ir_gallery_08 <- function(d, mode = "light", transparent = FALSE) {
  figure_correlation_matrix(as.data.frame(d$mk), method = "correlation_spearman",
                            mode = mode, transparent = transparent)
}

# 09 Paired slopegraph with the paired t-test.
.ir_gallery_09 <- function(d, mode = "light", transparent = FALSE) {
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
.ir_gallery_char_source <- function(d) {
  ggplot2::ggplot(d$med, ggplot2::aes(.data$hours, .data$C5a, shape = .data$severity,
                                      label = .data$severity)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$severity)) +
    ggplot2::geom_point() +
    geom_endlabel() +
    # Breaks of figure 01 without 6: on the narrow character grids of
    # figures 11 and 12 the default breaks 10 and 20 nearly touch.
    ggplot2::scale_x_continuous(breaks = c(0, 12, 24, 48)) +
    ggplot2::labs(x = "Hours after admission", y = "C5a (ng/ml)")
}

# 10 Character mode, 1 bit.
.ir_gallery_10 <- function(d, mode = "light", transparent = FALSE) {
  char_figure(.ir_gallery_char_source(d), style = "bw", slot = "slide_side", mode = mode,
              transparent = transparent)
}

# 11 Character mode in C64 colors.
.ir_gallery_11 <- function(d, mode = "light", transparent = FALSE) {
  char_figure(.ir_gallery_char_source(d), style = "c64", slot = "slide_half", mode = mode,
              transparent = transparent)
}

# 12 The C64 screen in two values.
.ir_gallery_12 <- function(d, mode = "light", transparent = FALSE) {
  char_figure(.ir_gallery_char_source(d), style = "c64bw", slot = "slide_half", mode = mode,
              transparent = transparent)
}

.ir_gallery_builders <- list(
  .ir_gallery_01, .ir_gallery_02, .ir_gallery_03, .ir_gallery_04, .ir_gallery_05,
  .ir_gallery_06, .ir_gallery_07, .ir_gallery_08, .ir_gallery_09, .ir_gallery_10,
  .ir_gallery_11, .ir_gallery_12
)

#' The Bleisatz example figures
#'
#' `figure_gallery()` writes the twelve example figures of the Bleisatz Beamer
#' theme, each in up to four modes, with [save_figure_set()]; with
#' `set = "tufte"` it writes the Tufte set instead (see below). Every figure
#' teaches one rule of the black-and-white system and is sized for one
#' Bleisatz slot (all 44 mm high): `"slide_full"` (134 mm), `"slide_side"`
#' (86 mm) or `"slide_half"` (64 mm).
#'
#' | File | Slot | Built with |
#' |---|---|---|
#' | `01-timecourse-direct-labels` | side | [theme_figure()], [scale_linetype_figure()], [scale_shape_figure()], [geom_endlabel()] |
#' | `02-raw-data-mean-ci` | side | [report_figure()] with Welch comparisons against ISS < 16, Holm-adjusted |
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
#' exist. Statistics come from 'infercalcR' and displayed numbers from the
#' reporting functions ([report_table()], [format_estimate()]).
#'
#' The files are written as `dir/<mode>/<name>.pdf` (and `.png` with
#' `png = TRUE`), the folder convention of [save_figure_set()]. A Bleisatz
#' deck regenerates its `figures/` folder with
#' `Rscript -e 'infereportR::figure_gallery("figures")'`.
#'
#' The file names, slots, sizes and captions of the gallery deck are returned
#' by [gallery_captions()]; no data file is shipped.
#'
#' **The Tufte set** (`set = "tufte"`, Bleisatz 1.3) has seven figure types
#' from Tufte's books, nine files with the two margin variants, built with
#' the Tufte options of the package:
#'
#' | File | Slot | Built with |
#' |---|---|---|
#' | `13-sparklines` | side | [figure_sparklines()] |
#' | `14-range-frame-dot-dash` | half | [geom_dotdash()], `theme_figure(axes = "range")` |
#' | `15-quartile-plot` | half | [stat_quartiles()], [geom_rangeframe()] |
#' | `16-bars-white-grid` | side | [geom_papergrid()], `theme_figure(axes = "none")` |
#' | `17-slopegraph` | side | [figure_slopegraph()] |
#' | `18-small-multiples` | full | facets, [stat_context()] for the stratum median |
#' | `14-range-frame-dot-dash-margin` | margin | figure 14 at 52 x 40 mm |
#' | `15-quartile-plot-margin` | margin | figure 15 at 52 x 40 mm |
#' | `19-context-grey` | side | [stat_context()], [geom_rangeframe()] |
#'
#' It is written in six folders, the convention of Bleisatz
#' `figures/tufte/<folder>/`: `light` and `dark` in Tufte's palette (ink,
#' paper, grey, red and green; `palette = "tufte"`), `light-grey` and
#' `dark-grey` (ink, paper and the grey) and `light-1bit` and `dark-1bit`
#' (two values; the grey becomes thin or dotted ink). So for a deck,
#' `figure_gallery(file.path("figures", "tufte"), set = "tufte")`.
#'
#' @param dir Directory that holds the mode folders; a temporary directory by
#'   default.
#' @param modes Mode folders to write. For `set = "bleisatz"` any of
#'   `"light"`, `"dark"`, `"light-transparent"` and `"dark-transparent"`
#'   (all four by default); for `set = "tufte"` any folder of
#'   [save_figure_set()], by default the six `"light"`, `"dark"`,
#'   `"light-grey"`, `"dark-grey"`, `"light-1bit"` and `"dark-1bit"`.
#'   `character()` writes nothing.
#' @param png If `TRUE`, also write a PNG beside each PDF; `FALSE` by
#'   default.
#' @param dpi Resolution of the PNG files in dots per inch, 300 by default.
#' @param set `"bleisatz"` (default) for the twelve figures 01-12 or
#'   `"tufte"` for the nine files of figures 13-19.
#' @return The rows of [save_figure_set()] for all figures, bound in figure
#'   order: a data.frame with one row per written file and the columns `mode`,
#'   `file`, `format`, `width_mm` and `height_mm` (12 or 9 x modes x formats
#'   rows; 0 rows with the same columns for `modes = character()`).
#' @seealso [gallery_captions()], [save_figure_set()], [figure_size()]
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
                           png = FALSE, dpi = 300, set = c("bleisatz", "tufte")) {
  .ir_check_string(dir, "dir")
  set <- .ir_check_choice(set, c("bleisatz", "tufte"), "set")
  if (set == "tufte") {
    if (missing(modes)) modes <- .ir_tufte_modes
    .ir_check_set_modes(modes)
  } else {
    .ir_check_set_modes(modes, .ir_set_modes)
  }
  .ir_check_flag(png, "png")
  .ir_check_number(dpi, "dpi", min = 0)
  if (dpi <= 0) .ir_abort("dpi must be a positive number")
  if (!length(modes)) return(.ir_set_empty())
  data <- .ir_gallery_data()
  if (set == "tufte") return(.ir_tufte_gallery(data, dir, modes, png, dpi))
  rows <- lapply(seq_len(nrow(.ir_gallery_captions)), function(i) {
    builder <- .ir_gallery_builders[[i]]
    build <- function(mode, transparent) builder(data, mode = mode, transparent = transparent)
    save_figure_set(build, .ir_gallery_captions$name[[i]],
                    preset = .ir_gallery_presets[[.ir_gallery_captions$slot[[i]]]], dir = dir,
                    modes = modes, png = png, dpi = dpi)
  })
  out <- do.call(rbind, c(list(.ir_set_empty()), rows))
  rownames(out) <- NULL
  out
}

#' File names, slots and captions of the gallery figures
#'
#' `gallery_captions()` returns the table behind [figure_gallery()]: one row
#' per example figure of the Bleisatz gallery deck, in figure order, with the
#' file name, the slide slot, the slot size and the caption used in the deck
#' and in the vignette "The figure gallery". `set = "tufte"` returns the nine
#' files of the Tufte set (figures 13-19 with the two margin variants), with
#' the captions of the Bleisatz Tufte set. The tables are kept in the package
#' as R objects; no data file is shipped.
#'
#' @param set `"bleisatz"` (default) or `"tufte"`.
#' @return A data.frame with 12 rows (9 for `"tufte"`) and the columns `name`
#'   (file name without extension, as written by [figure_gallery()]), `slot`
#'   (`"full"`, `"side"`, `"half"` or `"margin"`), `width_mm` and
#'   `height_mm` (the size of the slot in mm) and `caption` (character).
#' @seealso [figure_gallery()], [figure_size()]
#' @examples
#' captions <- gallery_captions()
#' captions[, c("name", "slot", "width_mm")]
#' strwrap(captions$caption[[6]], width = 60)
#' gallery_captions("tufte")[, c("name", "slot", "width_mm", "height_mm")]
#' @export
gallery_captions <- function(set = c("bleisatz", "tufte")) {
  set <- .ir_check_choice(set, c("bleisatz", "tufte"), "set")
  if (set == "tufte") .ir_tufte_captions else .ir_gallery_captions
}

.ir_tufte_gallery <- function(data, dir, modes, png, dpi) {
  rows <- lapply(seq_len(nrow(.ir_tufte_captions)), function(i) {
    builder <- .ir_tufte_builders[[i]]
    slot <- .ir_tufte_captions$slot[[i]]
    narrow <- slot == "margin"
    build <- function(mode, transparent, palette) {
      builder(data, mode = mode, transparent = transparent, palette = palette, narrow = narrow)
    }
    save_figure_set(build, .ir_tufte_captions$name[[i]], preset = .ir_tufte_presets[[slot]],
                    dir = dir, modes = modes, png = png, dpi = dpi, palette = "tufte")
  })
  out <- do.call(rbind, c(list(.ir_set_empty()), rows))
  rownames(out) <- NULL
  out
}
