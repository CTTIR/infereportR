#' Black-and-white ggplot2 theme for paper and slide figures
#'
#' A complete 'ggplot2' theme for figures in APA-style papers and on Bleisatz
#' slides. It is built on [ggplot2::theme_classic()] and then modified so that
#' the figure is strictly black and white: all text and lines are black, all
#' backgrounds are white without an outline, and there is no gray anywhere.
#'
#' Every text element resolves to between 8 and 14 pt at the default
#' `base_size = 9`: axis text, axis titles, legend text and title, strip text
#' and the caption use `rel(1)`, the subtitle `rel(1)` and the title
#' `rel(1.15)`. The APA Style page on figures asks for text between 8 and 14
#' point inside the image (American Psychological Association, *Figures*,
#' <https://apastyle.apa.org/style-grammar-guidelines/tables-figures/figures>).
#' With `target = "paper"`, a `base_size` outside that range therefore gives a
#' warning of class `infereportR_apa_text_size`; the theme is still returned.
#'
#' Axis lines are 0.5 pt, ticks are 0.4 pt hairlines of 1.1 mm drawn outside
#' the panel. Major grid lines, when requested, are black 0.4 pt dotted lines
#' (linetype `"13"`), so no gray is needed; minor grid lines are never drawn.
#' Strip text is bold and flush left, without a strip background. Title,
#' subtitle and caption are flush left and aligned to the whole plot.
#'
#' `target = "paper"` uses one font family for all text and 2 mm plot margins.
#' `target = "slide"` uses 1 mm margins, so figures sit flush in Bleisatz
#' slots, and sets the axis titles in IBM Plex Sans Condensed when
#' 'systemfonts' reports that family (otherwise in the base family).
#'
#' The theme only styles non-data elements. In 'ggplot2' versions with a
#' `geom` theme element, its ink, paper and accent are also set to the mode's
#' ink and background so that default geom colors stay black and white. One
#' default is left to 'ggplot2': the default fill of bars and areas
#' (`geom_col()`, `geom_bar()`, `geom_histogram()`, `geom_area()`) is a dark
#' grey (in 'ggplot2' 4.x a mix of ink and paper), which [check_figure()]
#' reports under `two_values`. Set `fill`, or map it and add
#' [scale_fill_hatch()].
#'
#' **Modes.** `mode = "light"` (default) draws black ink on white paper,
#' `mode = "dark"` white ink on black, for dark Bleisatz slides
#' (`\usetheme[dark]{Bleisatz}`) and inverse frames. With
#' `transparent = TRUE` the plot, panel and legend backgrounds are not drawn
#' (`fill = NA`), so only the information is left; open fills of the `geom`
#' element are empty too. Pair it with `save_figure(transparent = TRUE)`.
#'
#' **Palettes.** The figure stays strictly two-valued by default
#' (`palette = "onebit"`). `palette = "grey"` adds one grey (`#999999`) for
#' context, and `palette = "tufte"` adds Tufte's red and green on top (see
#' `vignette("tufte")`). In the theme the palette only changes requested grid
#' lines: with a grey in the palette they are solid grey hairlines instead of
#' dotted ink. The layers and builders of the package take the same argument.
#'
#' **Tufte options.** `axes = "range"` drops the axis lines, so that
#' [geom_rangeframe()] can draw them from the smallest to the largest value of
#' the data; the ticks stay. `axes = "none"` drops axis lines and ticks.
#' `grid = "paper"` draws horizontal grid lines in the color of the paper on
#' top of the data (`panel.ontop`, with an empty panel background) and
#' removes the axis lines and ticks: the lines cut through the bars, where a
#' scale is needed. Use it for bar charts; since the theme grid lies above
#' every layer, use [geom_papergrid()] with `axes = "none"` when labels must
#' stay above the grid.
#'
#' @param target `"paper"` (default) for journal figures or `"slide"` for
#'   Bleisatz slides.
#' @param base_size Base font size in points, a single positive number.
#' @param base_family Base font family, a single string. `NULL` (default)
#'   uses `figure_font("sans")`.
#' @param legend Legend placement: `"inside"` (default, top right corner of
#'   the panel), `"right"`, `"bottom"` or `"none"`.
#' @param grid Major grid lines: `"none"` (default), `"y"` (horizontal lines),
#'   `"x"` (vertical lines), `"xy"` (both) or `"paper"` (horizontal lines in
#'   the paper color, drawn above the data; no axis lines, no ticks).
#' @param border If `TRUE`, draw a black rectangle without fill around the
#'   panel.
#' @param mode `"light"` (default, black on white) or `"dark"` (white on
#'   black).
#' @param transparent If `TRUE`, no background is drawn.
#' @param palette `"onebit"` (default, two values), `"grey"` or `"tufte"`.
#' @param axes `"full"` (default) draws the axis lines across the panel;
#'   `"range"` leaves them to [geom_rangeframe()]; `"none"` draws neither
#'   axis lines nor ticks.
#' @return A complete 'ggplot2' theme object (`attr(, "complete")` is `TRUE`)
#'   that can be added to a plot with `+`.
#' @seealso [figure_font()] for the default family; `check_figure()` audits a
#'   finished figure and `save_figure()` writes it at a preset size.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, shape = factor(cyl))) +
#'   geom_point() +
#'   labs(x = "Weight (1000 lb)", y = "Miles per gallon", shape = "Cylinders")
#'
#' # Journal figure: one family, 2 mm margins, legend inside the panel.
#' p + theme_figure("paper")
#'
#' # Slide figure: 1 mm margins, dotted horizontal grid, legend at the bottom.
#' # Print it to draw; the dark version has no background.
#' slide <- p + theme_figure("slide", grid = "y", legend = "bottom")
#' dark <- p + theme_figure("slide", mode = "dark", transparent = TRUE)
#'
#' # Tufte's range frame: the axes end where the data end.
#' framed <- p + geom_rangeframe() + theme_figure("slide", axes = "range")
#' @export
theme_figure <- function(target = c("paper", "slide"), base_size = 9, base_family = NULL,
                         legend = c("inside", "right", "bottom", "none"),
                         grid = c("none", "y", "x", "xy", "paper"), border = FALSE,
                         mode = "light", transparent = FALSE, palette = "onebit",
                         axes = c("full", "range", "none")) {
  target <- .ir_check_choice(target, c("paper", "slide"), "target")
  # The C64 type of the Tufte set scales the default size (see figure_gallery()).
  if (missing(base_size)) base_size <- base_size * .ir_type()$scale
  .ir_check_number(base_size, "base_size")
  if (base_size <= 0) .ir_abort("base_size must be a positive number")
  if (!is.null(base_family)) .ir_check_string(base_family, "base_family")
  legend <- .ir_check_choice(legend, c("inside", "right", "bottom", "none"), "legend")
  grid <- .ir_check_choice(grid, c("none", "y", "x", "xy", "paper"), "grid")
  .ir_check_flag(border, "border")
  axes <- .ir_check_choice(axes, c("full", "range", "none"), "axes")
  m <- .ir_mode(mode, transparent, palette)
  ink <- m$ink

  if (target == "paper" && (base_size < .ir_text_min_pt || base_size > .ir_text_max_pt)) {
    .ir_warn(sprintf("base_size %s lies outside the APA range of %s to %s pt",
                     format(base_size), format(.ir_text_min_pt), format(.ir_text_max_pt)),
             "apa_text_size")
  }
  if (is.null(base_family)) base_family <- .ir_sans_family()

  axis_title_family <- base_family
  if (target == "slide" && !.ir_type_c64() && isTRUE(.ir_has_font("IBM Plex Sans Condensed"))) {
    axis_title_family <- "IBM Plex Sans Condensed"
  }
  margin_mm <- if (target == "slide") 1 else 2

  grid_line <- if (m$palette == "onebit") {
    ggplot2::element_line(colour = ink, linewidth = .ir_lw(.ir_rule_hair), linetype = "13")
  } else {
    ggplot2::element_line(colour = m$grey, linewidth = .ir_lw(.ir_rule_hair),
                          linetype = "solid")
  }
  grid_x <- if (grid %in% c("x", "xy")) grid_line else ggplot2::element_blank()
  grid_y <- if (grid %in% c("y", "xy")) grid_line else ggplot2::element_blank()
  panel_border <- if (border) {
    ggplot2::element_rect(fill = NA, colour = ink, linewidth = .ir_lw(.ir_axis_line))
  } else {
    ggplot2::element_blank()
  }

  legend_args <- list(legend.position = legend)
  if (legend == "inside") {
    legend_args <- list(legend.position = "inside", legend.position.inside = c(1, 1),
                        legend.justification.inside = c(1, 1))
  }

  base <- ggplot2::theme_classic(base_size = base_size, base_family = base_family,
                                 base_line_size = .ir_lw(.ir_axis_line),
                                 base_rect_size = .ir_lw(.ir_axis_line))

  changes <- ggplot2::theme(
    line = ggplot2::element_line(colour = ink),
    rect = ggplot2::element_rect(fill = m$knock, colour = ink),
    text = ggplot2::element_text(family = base_family, colour = ink, size = base_size),
    axis.line = ggplot2::element_line(colour = ink, linewidth = .ir_lw(.ir_axis_line)),
    axis.ticks = ggplot2::element_line(colour = ink, linewidth = .ir_lw(.ir_rule_hair)),
    axis.ticks.length = grid::unit(.ir_tick_length, "mm"),
    axis.text = ggplot2::element_text(colour = ink, size = ggplot2::rel(1)),
    axis.title = ggplot2::element_text(colour = ink, size = ggplot2::rel(1),
                                       family = axis_title_family),
    legend.background = ggplot2::element_rect(fill = m$knock, colour = NA),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour = ink, size = ggplot2::rel(1)),
    legend.title = ggplot2::element_text(colour = ink, size = ggplot2::rel(1),
                                         face = "plain"),
    panel.background = ggplot2::element_rect(fill = m$knock, colour = NA),
    panel.border = panel_border,
    # ggplot2 3.5 keeps theme_classic's grey on the parent element; set it to
    # ink so every theme colour is one of the two values on every version.
    panel.grid = ggplot2::element_line(colour = m$ink),
    panel.grid.major.x = grid_x,
    panel.grid.major.y = grid_y,
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = m$knock, colour = NA),
    plot.title = ggplot2::element_text(colour = ink, size = ggplot2::rel(1.15),
                                       hjust = 0),
    plot.subtitle = ggplot2::element_text(colour = ink, size = ggplot2::rel(1),
                                          hjust = 0),
    plot.caption = ggplot2::element_text(colour = ink, size = ggplot2::rel(1),
                                         hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(margin_mm, margin_mm, margin_mm, margin_mm, unit = "mm"),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(colour = ink, size = ggplot2::rel(1),
                                       face = "bold", hjust = 0)
  )
  out <- base + changes + do.call(ggplot2::theme, legend_args)
  if (axes != "full") out <- out + ggplot2::theme(axis.line = ggplot2::element_blank())
  if (axes == "none") out <- out + ggplot2::theme(axis.ticks = ggplot2::element_blank())
  if (grid == "paper") {
    # The grid must lie above the bars, so the panel is drawn on top of the
    # data with an empty background.
    out <- out + ggplot2::theme(
      axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      panel.ontop = TRUE,
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.grid.major.y = ggplot2::element_line(colour = m$paper,
                                                 linewidth = .ir_lw(.ir_paper_grid),
                                                 linetype = "solid"))
  }
  out <- .ir_theme_geom_bw(out, m)
  attr(out, "complete") <- TRUE
  out
}

# ggplot2 >= 4.0 has a `geom` theme element whose accent colour (blue by
# default) colours geoms such as geom_smooth(); set it to the mode's ink, and
# its paper (the fill of open shapes) to the mode's background.
# Looked up dynamically so older ggplot2 versions keep working.
.ir_theme_geom_bw <- function(th, m = .ir_mode()) {
  ns <- asNamespace("ggplot2")
  if (!exists("element_geom", envir = ns, inherits = FALSE)) return(th)
  element_geom <- get("element_geom", envir = ns, inherits = FALSE)
  th + ggplot2::theme(geom = element_geom(ink = m$ink, paper = m$knock,
                                          accent = m$ink))
}
