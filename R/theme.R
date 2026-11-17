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
#' warning of class `inferplotR_apa_text_size`; the theme is still returned.
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
#' ink and background so that default geom colors stay black and white.
#'
#' **Modes.** `mode = "light"` (default) draws black ink on white paper,
#' `mode = "dark"` white ink on black, for dark Bleisatz slides
#' (`\usetheme[dark]{Bleisatz}`) and inverse frames. With
#' `transparent = TRUE` the plot, panel and legend backgrounds are not drawn
#' (`fill = NA`), so only the information is left; open fills of the `geom`
#' element are empty too. Pair it with `save_figure(transparent = TRUE)`.
#'
#' @param target `"paper"` (default) for journal figures or `"slide"` for
#'   Bleisatz slides.
#' @param base_size Base font size in points, a single positive number.
#' @param base_family Base font family, a single string. `NULL` (default)
#'   uses `figure_font("sans")`.
#' @param legend Legend placement: `"inside"` (default, top right corner of
#'   the panel), `"right"`, `"bottom"` or `"none"`.
#' @param grid Major grid lines: `"none"` (default), `"y"` (horizontal lines),
#'   `"x"` (vertical lines) or `"xy"` (both).
#' @param border If `TRUE`, draw a black rectangle without fill around the
#'   panel.
#' @param mode `"light"` (default, black on white) or `"dark"` (white on
#'   black).
#' @param transparent If `TRUE`, no background is drawn.
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
#' p + theme_figure("slide", grid = "y", legend = "bottom")
#'
#' # Dark slide figure without a background.
#' p + theme_figure("slide", mode = "dark", transparent = TRUE)
#' @export
theme_figure <- function(target = c("paper", "slide"), base_size = 9, base_family = NULL,
                         legend = c("inside", "right", "bottom", "none"),
                         grid = c("none", "y", "x", "xy"), border = FALSE,
                         mode = "light", transparent = FALSE) {
  target <- .ip_check_choice(target, c("paper", "slide"), "target")
  .ip_check_number(base_size, "base_size")
  if (base_size <= 0) .ip_abort("base_size must be a positive number")
  if (!is.null(base_family)) .ip_check_string(base_family, "base_family")
  legend <- .ip_check_choice(legend, c("inside", "right", "bottom", "none"), "legend")
  grid <- .ip_check_choice(grid, c("none", "y", "x", "xy"), "grid")
  .ip_check_flag(border, "border")
  m <- .ip_mode(mode, transparent)
  ink <- m$ink

  if (target == "paper" && (base_size < .ip_text_min_pt || base_size > .ip_text_max_pt)) {
    .ip_warn(sprintf("base_size %s lies outside the APA range of %s to %s pt",
                     format(base_size), format(.ip_text_min_pt), format(.ip_text_max_pt)),
             "apa_text_size")
  }
  if (is.null(base_family)) base_family <- figure_font("sans")

  axis_title_family <- base_family
  if (target == "slide" && isTRUE(.ip_has_font("IBM Plex Sans Condensed"))) {
    axis_title_family <- "IBM Plex Sans Condensed"
  }
  margin_mm <- if (target == "slide") 1 else 2

  grid_line <- ggplot2::element_line(colour = ink, linewidth = .ip_lw(.ip_rule_hair),
                                     linetype = "13")
  grid_x <- if (grid %in% c("x", "xy")) grid_line else ggplot2::element_blank()
  grid_y <- if (grid %in% c("y", "xy")) grid_line else ggplot2::element_blank()
  panel_border <- if (border) {
    ggplot2::element_rect(fill = NA, colour = ink, linewidth = .ip_lw(.ip_axis_line))
  } else {
    ggplot2::element_blank()
  }

  legend_args <- list(legend.position = legend)
  if (legend == "inside") {
    legend_args <- list(legend.position = "inside", legend.position.inside = c(1, 1),
                        legend.justification.inside = c(1, 1))
  }

  base <- ggplot2::theme_classic(base_size = base_size, base_family = base_family,
                                 base_line_size = .ip_lw(.ip_axis_line),
                                 base_rect_size = .ip_lw(.ip_axis_line))

  changes <- ggplot2::theme(
    line = ggplot2::element_line(colour = ink),
    rect = ggplot2::element_rect(fill = m$knock, colour = ink),
    text = ggplot2::element_text(family = base_family, colour = ink, size = base_size),
    axis.line = ggplot2::element_line(colour = ink, linewidth = .ip_lw(.ip_axis_line)),
    axis.ticks = ggplot2::element_line(colour = ink, linewidth = .ip_lw(.ip_rule_hair)),
    axis.ticks.length = grid::unit(.ip_tick_length, "mm"),
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
  out <- .ip_theme_geom_bw(out, m)
  attr(out, "complete") <- TRUE
  out
}

# ggplot2 >= 4.0 has a `geom` theme element whose accent colour (blue by
# default) colours geoms such as geom_smooth(); set it to the mode's ink, and
# its paper (the fill of open shapes) to the mode's background.
# Looked up dynamically so older ggplot2 versions keep working.
.ip_theme_geom_bw <- function(th, m = .ip_mode()) {
  ns <- asNamespace("ggplot2")
  if (!exists("element_geom", envir = ns, inherits = FALSE)) return(th)
  element_geom <- get("element_geom", envir = ns, inherits = FALSE)
  th + ggplot2::theme(geom = element_geom(ink = m$ink, paper = m$knock,
                                          accent = m$ink))
}
