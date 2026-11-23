# The Tufte set of the Bleisatz figures (13-19), rebuilt with the package API.
# The data preparation of each figure (aggregation, sampling, label positions)
# is ported verbatim from CTTIR's make-tufte-figures.R (Bleisatz 1.3,
# 2026-11-19); the plotting uses the package's layers and builders. Every
# builder is a function of (data, mode, transparent, palette, narrow) where
# `narrow` is TRUE for the 52 mm margin slot. Called in the C64 type
# (.ir_with_type("c64", ...)), the builders draw the C64 set: the type sets
# family, sizes and colors, the builders use the short labels of the margin
# slot and type out what the font lacks, and .ir_c64_finish() does the rest.

.ir_tufte_captions <- data.frame(
  name = c("13-sparklines", "14-range-frame-dot-dash", "15-quartile-plot",
           "16-bars-white-grid", "17-slopegraph", "18-small-multiples",
           "14-range-frame-dot-dash-margin", "15-quartile-plot-margin", "19-context-grey"),
  slot = c("side", "half", "half", "side", "side", "full", "margin", "margin", "side"),
  width_mm = c(86, 64, 64, 86, 86, 134, 52, 52, 86),
  height_mm = c(44, 44, 44, 44, 44, 44, 40, 40, 44),
  caption = c(
    paste("Sparklines: the median of each marker over 48 hours at the size of a word.",
          "The two strata share one scale per marker. Green marks the peak, red the last",
          "value, and the numbers beside them carry the same colour; the grey band is the",
          "interquartile range."),
    paste("Range frame and dot-dash plot: each axis runs from the smallest to the largest",
          "value and is drawn as one dash per patient, so the axes show both margins."),
    paste("Quartile plot: lines from the minimum to the lower quartile and from the upper",
          "quartile to the maximum; the gap is the interquartile range, the dot the median."),
    paste("Bars without frame, axis line or ticks: the grid is drawn in the colour of the",
          "paper and runs through the bars, where it is needed."),
    paste("Slopegraph: mean IL-6 at the early and the late peak by subgroup. Names and",
          "values stand on both sides, spaced by value; the slope is the change."),
    paste("Small multiples: twenty patients on identical scales, ordered by the hour of",
          "their C5a peak. The thin line in each panel is the median of the patient's",
          "stratum."),
    paste("The range frame and dot-dash plot of figure 14 at the width of the margin of a",
          "Tufte handout."),
    "The quartile plot of figure 15 at the width of the margin of a Tufte handout.",
    paste("Every patient as a thin line for context, the two medians in ink. The axes are",
          "range frames: they end where the data end.")),
  stringsAsFactors = FALSE
)

.ir_tufte_presets <- c(side = "slide_side", half = "slide_half", full = "slide_full",
                       margin = "margin")

.ir_tufte_ge25 <- "ISS \u2265 25"
# The data use a hyphen in "16-24"; figures show an en dash. Every string a
# Tufte figure shows passes here, so the C64 type gets plain characters.
.ir_tufte_show <- function(x) {
  x <- gsub("16-24", "16\u201324", x, fixed = TRUE)
  if (.ir_type_c64()) x <- .ir_c64_text(x)
  x
}

# Tufte's theme on top of the figure theme: range-frame axes (lines drawn as
# data), hairline ticks of 0.9 mm only where a value is labeled, no grid.
.ir_tufte_theme <- function(mode, transparent, palette) {
  m <- .ir_mode(mode, transparent, palette)
  theme_figure("slide", legend = "none", mode = mode, transparent = transparent,
               palette = palette, axes = "range") +
    ggplot2::theme(axis.ticks = ggplot2::element_line(colour = m$ink,
                                                      linewidth = .ir_lw(.ir_rule_hair)),
                   axis.ticks.length = grid::unit(0.9, "mm"),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(1.5, 1.5, 1, 1, "mm"))
}

# 13 Sparklines: marker x severity, median over 48 h with its IQR band (not in
# 1-bit, as in the reference), the peak and the last value marked.
.ir_tufte_13 <- function(d, mode = "light", transparent = FALSE, palette = "tufte",
                         narrow = FALSE) {
  m <- .ir_mode(mode, transparent, palette)
  markers <- c(C5a = "C5a", C3a = "C3a", IL6 = "IL-6")
  units <- c(C5a = "ng/ml", C3a = "ng/ml", IL6 = "pg/ml")
  sev <- c("ISS 16-24", .ir_tufte_ge25)
  rows <- lapply(names(markers), function(mk) {
    sub <- d$long[d$long$severity %in% sev, ]
    q <- stats::aggregate(sub[[mk]], list(sev = sub$severity, hours = sub$hours),
                          function(v) stats::quantile(v, c(0.25, 0.5, 0.75)))
    q <- data.frame(sev = q$sev, hours = q$hours, lo = q$x[, 1], md = q$x[, 2], hi = q$x[, 3])
    q$marker <- markers[[mk]]
    q
  })
  q <- do.call(rbind, rows)
  q$marker <- factor(q$marker, levels = unname(markers))
  q$row <- factor(.ir_tufte_show(as.character(q$sev)), levels = .ir_tufte_show(sev))
  band <- m$palette != "onebit"
  figure_sparklines(q, "hours", "md", "row", group = "marker",
                    lower = if (band) "lo", upper = if (band) "hi",
                    units = stats::setNames(unname(units), unname(markers)),
                    digits = c(C5a = 1, C3a = 0, "IL-6" = 0), marks = "both", extremes = "max",
                    numbers = c("max", "last"), x_unit = " h",
                    titles = c(series = .ir_tufte_show("Median, 0\u201348 h"), max = "Peak",
                               last = "48 h"),
                    slot = "slide_side", mode = mode, transparent = transparent,
                    palette = palette)
}

# 14 Range frame and dot-dash plot: the axes are one dash per value.
.ir_tufte_14 <- function(d, mode = "light", transparent = FALSE, palette = "tufte",
                         narrow = FALSE) {
  m <- .ir_mode(mode, transparent, palette)
  narrow <- narrow || .ir_type_c64()
  s <- d$sc
  xr <- range(s$iss)
  yr <- range(s$il6)
  ggplot2::ggplot(s, ggplot2::aes(.data$iss, .data$il6)) +
    geom_dotdash(mode = mode, transparent = transparent, palette = palette) +
    ggplot2::geom_point(colour = m$ink, size = 0.8, stroke = 0) +
    ggplot2::scale_x_continuous(breaks = if (narrow) c(xr[1], 25, xr[2]) else
                                  c(xr[1], 16, 25, xr[2]),
                                expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::scale_y_continuous(breaks = .ir_round(c(yr[1], 100, yr[2])),
                                expand = ggplot2::expansion(mult = 0.03)) +
    ggplot2::labs(x = if (narrow) "ISS" else "Injury Severity Score", y = "IL-6 (pg/ml)") +
    ggplot2::coord_cartesian(clip = "off") +
    .ir_tufte_theme(mode, transparent, palette) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2.2,
                                                                                unit = "mm")),
                   axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 2.2,
                                                                                unit = "mm")))
}

# 15 Quartile plot with a range frame; the raw values beside in grey (not in
# 1-bit).
.ir_tufte_15 <- function(d, mode = "light", transparent = FALSE, palette = "tufte",
                         narrow = FALSE) {
  m <- .ir_mode(mode, transparent, palette)
  narrow <- narrow || .ir_type_c64()
  v <- d$il6
  lev <- c("ISS < 16", "ISS 16-24", .ir_tufte_ge25)
  v$x <- match(v$group, lev)
  yr <- range(v$value)
  n <- format_estimate(as.numeric(tabulate(v$x, 3)))
  labels <- if (narrow) {
    paste0(sub("ISS ", "ISS\n", .ir_tufte_show(lev), fixed = TRUE), "\nn=", n)
  } else {
    paste0(.ir_tufte_show(lev), "\nn = ", n)
  }
  p <- ggplot2::ggplot(v, ggplot2::aes(.data$x, .data$value)) +
    geom_rangeframe(sides = "l", mode = mode, transparent = transparent, palette = palette)
  if (m$palette != "onebit") {
    p <- p + ggplot2::geom_point(ggplot2::aes(x = .data$x + 0.22), colour = m$grey,
                                 size = 0.55, stroke = 0)
  }
  p +
    stat_quartiles(ggplot2::aes(group = .data$x), mode = mode, transparent = transparent,
                   palette = palette) +
    ggplot2::scale_x_continuous(breaks = 1:3, labels = labels, limits = c(0.6, 3.5),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = .ir_round(c(yr[1], 50, 100, yr[2])),
                                expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(x = NULL, y = "IL-6 (pg/ml)") +
    ggplot2::coord_cartesian(clip = "off") +
    .ir_tufte_theme(mode, transparent, palette) +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
}

# 16 Bars without axis: paper-coloured grid lines through the bars carry the
# scale, drawn as a layer so the labels above the last bars stay on top. In
# 1-bit "single" is an open bar, "multiple" ink.
.ir_tufte_16 <- function(d, mode = "light", transparent = FALSE, palette = "tufte",
                         narrow = FALSE) {
  m <- .ir_mode(mode, transparent, palette)
  cts <- d$counts[d$counts$outcome != "No organ failure", ]
  lev <- c("ISS < 16", "ISS 16-24", .ir_tufte_ge25)
  cts$g <- match(cts$group, lev)
  cts$x <- cts$g + ifelse(cts$outcome == "Single organ", -0.17, 0.17)
  single <- cts$outcome == "Single organ"
  onebit <- m$palette == "onebit"
  fill_single <- if (onebit) m$paper else m$grey
  grid_y <- seq(5, 30, by = 5)
  last <- cts[cts$g == 3, ]
  cond <- .ir_condensed_family()
  small <- (.ir_label_pt() - 1) / ggplot2::.pt
  ls <- last$outcome == "Single organ"
  labels <- data.frame(x = c(last$x[ls] + 0.15, last$x[!ls] - 0.15),
                       y = c(last$pct[ls], last$pct[!ls]) + 1.2,
                       label = c("single", "multiple"), hjust = c(1, 0))
  ggplot2::ggplot(cts) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$x - 0.15, xmax = .data$x + 0.15, ymin = 0,
                                    ymax = .data$pct),
                       fill = ifelse(single, fill_single, m$red),
                       colour = ifelse(single & onebit, m$ink, NA),
                       linewidth = .ir_lw(.ir_rule_hair)) +
    geom_papergrid(mode = mode, transparent = transparent, palette = palette) +
    ggplot2::geom_text(data = labels, ggplot2::aes(.data$x, .data$y, label = .data$label,
                                                   hjust = .data$hjust),
                       vjust = 0, family = cond, size = small, colour = m$ink) +
    ggplot2::scale_x_continuous(breaks = 1:3,
                                labels = sub("ISS ", "ISS\n", .ir_tufte_show(lev), fixed = TRUE),
                                expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::scale_y_continuous(breaks = grid_y, labels = paste0(grid_y, "%"),
                                limits = c(0, 34), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, title = "Organ failure, % of patients") +
    ggplot2::coord_cartesian(clip = "off") +
    theme_figure("slide", legend = "none", axes = "none", mode = mode,
                 transparent = transparent, palette = palette) +
    ggplot2::theme(axis.ticks.length = grid::unit(0.9, "mm"),
                   plot.margin = ggplot2::margin(1.5, 1.5, 1, 1, "mm"),
                   plot.title = ggplot2::element_text(family = cond, size = .ir_label_pt(),
                                                      colour = m$ink, face = "plain", hjust = 0,
                                                      margin = ggplot2::margin(b = 1.5,
                                                                               unit = "mm")),
                   plot.title.position = "plot")
}

# 17 Slopegraph (table-graphic): mean IL-6 at the early and late peak by
# subgroup, names and values on both sides.
.ir_tufte_17 <- function(d, mode = "light", transparent = FALSE, palette = "tufte",
                         narrow = FALSE) {
  f <- d$fp
  groups <- .ir_gallery_subsets(f)
  names(groups) <- .ir_tufte_show(names(groups))
  s <- do.call(rbind, lapply(names(groups), function(g) {
    w <- groups[[g]]
    data.frame(name = g, early = mean(f$il6[w & f$late == 0]), late = mean(f$il6[w & f$late == 1]))
  }))
  figure_slopegraph(s, "name", "early", "late", highlight = "All patients",
                    titles = c("Early peak", "Late peak"), note = "mean IL-6, pg/ml",
                    digits = 0L, gap = 0.115, slot = "slide_side", mode = mode,
                    transparent = transparent, palette = palette)
}

# The twenty patients of figure 18 and their peaks (make-tufte-figures.R).
.ir_tufte_18_data <- function(d) {
  L <- d$long
  pk <- do.call(rbind, lapply(split(L, L$id), function(p) {
    data.frame(id = p$id[1], severity = p$severity[1], peak_h = p$hours[which.max(p$C5a)],
               peak = max(p$C5a))
  }))
  pick <- .ir_with_seed(20261117, {
    c(sample(pk$id[pk$severity == "ISS 16-24"], 10),
      sample(pk$id[pk$severity == .ir_tufte_ge25], 10))
  })
  pk <- pk[pk$id %in% pick, ]
  pk <- pk[order(pk$severity != "ISS 16-24", pk$peak_h, -pk$peak), ]
  pk$pos <- stats::ave(seq_len(nrow(pk)), pk$severity, FUN = seq_along)
  pk$sev <- factor(.ir_tufte_show(pk$severity),
                   levels = .ir_tufte_show(c("ISS 16-24", .ir_tufte_ge25)))
  L <- merge(L[L$id %in% pick, ], pk[, c("id", "pos", "sev")], by = "id")
  med <- d$med[d$med$severity %in% c("ISS 16-24", .ir_tufte_ge25), ]
  med$sev <- factor(.ir_tufte_show(med$severity), levels = levels(pk$sev))
  ctxl <- merge(pk[, c("pos", "sev")], med, by = "sev")
  ctxl$grp <- paste(ctxl$sev, ctxl$pos)
  peaks <- do.call(rbind, lapply(split(L, L$id), function(p) p[which.max(p$C5a), ]))
  list(L = L, pk = pk, ctxl = ctxl, peaks = peaks)
}

# 18 Small multiples: twenty patients on the same scales, their stratum's
# median as context (dashed hairline in 1-bit); each panel names the hour of
# the peak.
.ir_tufte_18 <- function(d, mode = "light", transparent = FALSE, palette = "tufte",
                         narrow = FALSE) {
  m <- .ir_mode(mode, transparent, palette)
  x <- .ir_tufte_18_data(d)
  onebit <- m$palette == "onebit"
  pt <- .ir_label_pt()
  cond <- .ir_condensed_family()
  x$pk$label <- paste0(format_estimate(as.numeric(x$pk$peak_h)), " h")
  ggplot2::ggplot(x$L, ggplot2::aes(.data$hours, .data$C5a)) +
    stat_context(data = x$ctxl, ggplot2::aes(group = .data$grp), fun = "none",
                 context_linetype = if (onebit) "22" else "solid",
                 linewidth = .ir_lw(if (onebit) .ir_fine_line else .ir_thin_line),
                 mode = mode, transparent = transparent, palette = palette) +
    ggplot2::geom_line(ggplot2::aes(group = .data$id), colour = m$ink,
                       linewidth = .ir_lw(.ir_thin_line)) +
    ggplot2::geom_point(data = x$peaks, colour = m$red, size = 0.7) +
    ggplot2::geom_text(data = x$pk, ggplot2::aes(x = 48, y = 45, label = .data$label),
                       hjust = 1, vjust = 1, family = cond, size = (pt - 1) / ggplot2::.pt,
                       colour = m$ink) +
    ggplot2::facet_grid(ggplot2::vars(.data$sev), ggplot2::vars(.data$pos), switch = "y") +
    ggplot2::scale_x_continuous(breaks = c(0, 48), expand = ggplot2::expansion(mult = 0.04)) +
    ggplot2::scale_y_continuous(breaks = c(10, 40), limits = c(5, 46), expand = c(0, 0),
                                position = "right") +
    ggplot2::labs(x = .ir_tufte_show(paste("Hours, 0\u201348 in every panel; the number is",
                                           "the hour of the C5a peak")),
                  y = NULL) +
    .ir_tufte_theme(mode, transparent, palette) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(family = .ir_sans_family(), size = pt,
                                                colour = m$ink, face = "plain", angle = 0,
                                                hjust = 0,
                                                margin = ggplot2::margin(r = 2, unit = "mm")),
      strip.placement = "outside", strip.background = ggplot2::element_blank(),
      panel.spacing.x = grid::unit(1.6, "mm"), panel.spacing.y = grid::unit(2.4, "mm"),
      axis.text = ggplot2::element_text(size = pt - 1.5),
      axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(family = cond, size = pt - 1, hjust = 0,
                                           margin = ggplot2::margin(t = 1.2, unit = "mm")))
}

# 19 Context in grey: every patient a thin line (in 1-bit a sample of 32 as
# hairlines), the medians of all patients in ink, labeled at their ends;
# range frames.
.ir_tufte_19 <- function(d, mode = "light", transparent = FALSE, palette = "tufte",
                         narrow = FALSE) {
  m <- .ir_mode(mode, transparent, palette)
  sev <- c("ISS 16-24", .ir_tufte_ge25)
  all <- d$long[d$long$severity %in% sev, ]
  L <- all
  if (m$palette == "onebit") {
    L <- .ir_with_seed(20261117, L[L$id %in% sample(unique(L$id), 32), ])
  }
  med <- d$med[d$med$severity %in% sev, ]
  endv <- med[med$hours == 48, ]
  endv <- endv[order(endv$C5a), ]
  ly <- endv$C5a
  if (diff(ly) < 2.6) ly <- mean(ly) + c(-1.3, 1.3)
  yr <- range(all$C5a)
  pt <- .ir_label_pt()
  ends <- data.frame(y = c(ly, max(ly) + 2.8),
                     label = c(.ir_tufte_show(as.character(endv$severity)), "median"),
                     colour = c(ifelse(endv$severity == .ir_tufte_ge25, m$red, m$ink), m$grey),
                     family = c(.ir_sans_family(), .ir_sans_family(),
                                .ir_condensed_family()),
                     size = c(pt, pt, pt - 1) / ggplot2::.pt, stringsAsFactors = FALSE)
  ggplot2::ggplot(L, ggplot2::aes(.data$hours, .data$C5a)) +
    geom_rangeframe(data = all, mode = mode, transparent = transparent, palette = palette) +
    stat_context(ggplot2::aes(group = .data$id), fun = "none", mode = mode,
                 transparent = transparent, palette = palette) +
    stat_context(data = all, ggplot2::aes(group = .data$id, series = .data$severity,
                                          linetype = .data$severity),
                 context = FALSE, highlight = .ir_tufte_ge25, mode = mode,
                 transparent = transparent, palette = palette) +
    ggplot2::geom_text(data = ends, ggplot2::aes(x = 49.5, y = .data$y, label = .data$label),
                       hjust = 0, colour = ends$colour, family = ends$family, size = ends$size) +
    ggplot2::scale_linetype_manual(values = c("solid", "42"), guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 24, 48),
                                expand = ggplot2::expansion(mult = c(0.02, 0))) +
    ggplot2::scale_y_continuous(breaks = .ir_round(c(yr[1], 20, 30, yr[2])),
                                expand = ggplot2::expansion(mult = 0.03)) +
    ggplot2::labs(x = "Hours after admission", y = "C5a (ng/ml)") +
    ggplot2::coord_cartesian(clip = "off") +
    .ir_tufte_theme(mode, transparent, palette) +
    # The end labels are wider in the C64 type.
    ggplot2::theme(plot.margin = ggplot2::margin(1.5, if (.ir_type_c64()) 23 else 16, 1, 1,
                                                 "mm"))
}

.ir_tufte_builders <- list(
  .ir_tufte_13, .ir_tufte_14, .ir_tufte_15, .ir_tufte_16, .ir_tufte_17, .ir_tufte_18,
  .ir_tufte_14, .ir_tufte_15, .ir_tufte_19
)

# Figure i of the Tufte set (in the order of .ir_tufte_captions), in the
# current type.
.ir_tufte_build <- function(i, data, mode = "light", transparent = FALSE, palette = "tufte") {
  narrow <- .ir_tufte_captions$slot[[i]] == "margin"
  p <- .ir_tufte_builders[[i]](data, mode = mode, transparent = transparent, palette = palette,
                               narrow = narrow)
  if (.ir_type_c64()) p <- .ir_c64_finish(p)
  p
}

# The C64 set after make-tufte-figures.R: every theme text in the C64
# family and one weight; in figures with axes, axis text and titles at the
# label size and the y title above the axis, horizontal, as in character
# mode.
.ir_c64_finish <- function(p) {
  family <- .ir_sans_family()
  pt <- .ir_label_pt()
  th <- p$theme
  text <- names(Filter(function(e) inherits(e, "element_text"), th))
  # inherit.blank keeps the children of blanked elements (the axes of the
  # canvas figures) blank.
  p <- p + do.call(ggplot2::theme, stats::setNames(lapply(text, function(nm) {
    ggplot2::element_text(family = family, face = "plain", inherit.blank = TRUE)
  }), text))
  axis <- ggplot2::calc_element("axis.text.x.bottom", .ir_full_theme(p))
  if (is.null(axis) || inherits(axis, "element_blank")) return(p)
  p <- p + ggplot2::theme(axis.text = ggplot2::element_text(size = pt),
                          axis.title = ggplot2::element_text(size = pt))
  yt <- p$labels$y
  if (is.character(yt) && length(yt) == 1L && nzchar(yt)) {
    p <- p + ggplot2::labs(y = NULL, title = yt) +
      ggplot2::theme(plot.title.position = "plot",
                     plot.title = ggplot2::element_text(family = family, face = "plain",
                                                        size = pt, hjust = 0,
                                                        margin = ggplot2::margin(0, 0, 1.2, 0,
                                                                                 "mm")))
  }
  p
}
