# Tufte's figure types as layers (Bleisatz 1.3, Tufte options): range frame,
# dot-dash axes, quartile plot and context lines. Written from the
# descriptions in Tufte's books as summarized in the Bleisatz README and from
# its preview figures; every colour comes from the mode resolver.

# A layer parameter list with the ink colour, unless the user set a colour.
.ir_ink_params <- function(dots, ink) {
  nms <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))
  if (!any(nms %in% c("colour", "color", "col"))) dots$colour <- ink
  dots
}

.ir_check_sides <- function(sides) {
  ok <- is.character(sides) && length(sides) == 1L && !is.na(sides) && nzchar(sides) &&
    grepl("^[bltr]+$", sides)
  if (!ok) .ir_abort("sides must be a single string of the letters b, l, t and r")
  invisible(sides)
}

#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRangeframe <- ggplot2::ggproto(
  "GeomRangeframe", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(colour = .ir_ink, linewidth = .ir_lw(.ir_rule_hair),
                             linetype = "solid", alpha = NA),
  draw_key = ggplot2::draw_key_path,
  draw_panel = function(data, panel_params, coord, sides = "bl", lineend = "butt") {
    x <- as.numeric(data$x)
    y <- as.numeric(data$y)
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    first <- data[1L, , drop = FALSE]
    frames <- list()
    edge <- function(side) {
      along_x <- side %in% c("b", "t")
      span <- if (along_x) x else y
      if (!length(span)) return(NULL)
      at <- if (side %in% c("b", "l")) -Inf else Inf
      d <- if (along_x) {
        data.frame(x = range(span), y = c(at, at))
      } else {
        data.frame(x = c(at, at), y = range(span))
      }
      coord$transform(d, panel_params)
    }
    for (side in strsplit(sides, "")[[1L]]) {
      d <- edge(side)
      if (is.null(d)) next
      frames[[length(frames) + 1L]] <- grid::segmentsGrob(
        d$x[1L], d$y[1L], d$x[2L], d$y[2L], default.units = "native",
        gp = grid::gpar(col = ggplot2::alpha(first$colour, first$alpha),
                        lwd = first$linewidth * ggplot2::.pt, lty = first$linetype,
                        lineend = lineend))
    }
    if (!length(frames)) return(grid::nullGrob())
    do.call(grid::grobTree, frames)
  }
)

#' Range frame: axis lines from the smallest to the largest value
#'
#' Tufte's range frame. Each axis line runs only from the minimum to the
#' maximum of the data, so the frame itself reports the range. The layer
#' takes the range of its own `x` and `y` per panel and draws the lines at
#' the panel edges (`sides`), where the axis lines would be, in ink.
#'
#' Pair it with `theme_figure(axes = "range")`, which removes the full axis
#' lines and keeps the ticks. A useful companion is a scale whose outer
#' breaks are the data extremes, for example
#' `scale_y_continuous(breaks = c(min(y), 50, 100, max(y)))`. The frame is
#' drawn by a geom from the layer data (transformed by the coordinate system,
#' so it follows scale transformations and facets), because a theme element
#' cannot know the data.
#'
#' @param mapping Aesthetic mappings, as in [ggplot2::layer()]. The layer
#'   needs `x` and `y`.
#' @param data The data of the layer, as in [ggplot2::layer()]; pass `data`
#'   to frame a range other than that of the plot data (for example all
#'   patients when the plot shows a sample).
#' @param stat The stat, as in [ggplot2::layer()].
#' @param position The position adjustment, as in [ggplot2::layer()].
#' @param show.legend,inherit.aes As in [ggplot2::layer()].
#' @param ... Other arguments passed on to [ggplot2::layer()], such as
#'   `linewidth`.
#' @param sides Which edges carry a frame: a string of `"b"` (bottom), `"l"`
#'   (left), `"t"` (top) and `"r"` (right); `"bl"` by default.
#' @param mode,transparent,palette As in [theme_figure()]. The frame is ink.
#' @param na.rm If `TRUE`, missing values are removed silently.
#' @return A ggplot2 layer.
#' @seealso [theme_figure()], [geom_dotdash()], `vignette("tufte")`.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(size = 0.8) +
#'   geom_rangeframe() +
#'   scale_x_continuous(breaks = range(mtcars$wt)) +
#'   labs(x = "Weight (1000 lb)", y = "Miles per gallon (mpg)") +
#'   theme_figure("slide", axes = "range")
#' @export
geom_rangeframe <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., sides = "bl", mode = "light",
                            transparent = FALSE, palette = "onebit", na.rm = FALSE,
                            show.legend = FALSE, inherit.aes = TRUE) {
  m <- .ir_mode(mode, transparent, palette)
  .ir_check_sides(sides)
  .ir_check_flag(na.rm, "na.rm")
  params <- c(list(sides = sides, na.rm = na.rm), .ir_ink_params(list(...), m$ink))
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomRangeframe,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = params)
}

#' Dot-dash axes: one dash per observation
#'
#' Tufte's dot-dash plot. Each axis is drawn as one short dash per
#' observation, a rug placed outside the panel where the axis line would be,
#' so the axes show both marginal distributions. Built on
#' [ggplot2::GeomRug] with `outside = TRUE`: one dash per row with a finite
#' value, in ink, 0.4 pt wide and `length` mm long.
#'
#' The dashes lie outside the panel, so switch clipping off
#' (`coord_cartesian(clip = "off")`), drop the ticks and axis lines (they
#' would collide with the dashes) and move the axis text away from the panel
#' by a little more than `length`, as in the example. Breaks at the data
#' extremes turn the dashed axes into range frames.
#'
#' @inheritParams geom_rangeframe
#' @param ... Other arguments passed on to [ggplot2::layer()].
#' @param sides Which axes carry dashes: `"bl"` (default), any string of
#'   `"b"`, `"l"`, `"t"` and `"r"`.
#' @param length Length of a dash in mm, 1.3 by default.
#' @return A ggplot2 layer.
#' @seealso [geom_rangeframe()], `vignette("tufte")`.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(size = 0.8) +
#'   geom_dotdash() +
#'   coord_cartesian(clip = "off") +
#'   labs(x = "Weight (1000 lb)", y = "Miles per gallon (mpg)") +
#'   theme_figure("slide", axes = "range") +
#'   theme(axis.ticks = element_blank(),
#'         axis.text.x = element_text(margin = margin(t = 2.2, unit = "mm")),
#'         axis.text.y = element_text(margin = margin(r = 2.2, unit = "mm")))
#' @export
geom_dotdash <- function(mapping = NULL, data = NULL, position = "identity", ...,
                         sides = "bl", length = 1.3, mode = "light", transparent = FALSE,
                         palette = "onebit", na.rm = FALSE, show.legend = FALSE,
                         inherit.aes = TRUE) {
  m <- .ir_mode(mode, transparent, palette)
  .ir_check_sides(sides)
  .ir_check_number(length, "length", min = 0)
  if (length <= 0) .ir_abort("length must be a positive number")
  .ir_check_flag(na.rm, "na.rm")
  dots <- .ir_ink_params(list(...), m$ink)
  if (is.null(dots$linewidth)) dots$linewidth <- .ir_lw(.ir_rule_hair)
  params <- c(list(sides = sides, outside = TRUE, length = grid::unit(length, "mm"),
                   na.rm = na.rm), dots)
  ggplot2::layer(data = data, mapping = mapping, stat = "identity", geom = ggplot2::GeomRug,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = params)
}

#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuartiles <- ggplot2::ggproto(
  "StatQuartiles", ggplot2::Stat,
  required_aes = c("x", "y"),
  # y becomes the five numbers.
  dropped_aes = "y",
  compute_group = function(data, scales) {
    y <- data$y[is.finite(data$y)]
    if (!length(y)) {
      return(data.frame(x = numeric(), ymin = numeric(), lower = numeric(),
                        middle = numeric(), upper = numeric(), ymax = numeric(),
                        n = integer()))
    }
    q <- stats::quantile(y, c(0, 0.25, 0.5, 0.75, 1), type = 7, names = FALSE)
    data.frame(x = data$x[1L], ymin = q[1L], lower = q[2L], middle = q[3L], upper = q[4L],
               ymax = q[5L], n = length(y))
  }
)

#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomQuartiles <- ggplot2::ggproto(
  "GeomQuartiles", ggplot2::Geom,
  required_aes = c("x", "ymin", "lower", "middle", "upper", "ymax"),
  default_aes = ggplot2::aes(colour = .ir_ink, linewidth = .ir_lw(.ir_data_line),
                             linetype = "solid", size = 1.1, shape = 19, stroke = 0.5,
                             fill = NA, alpha = NA),
  draw_key = ggplot2::draw_key_pointrange,
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    if (!nrow(data)) return(grid::nullGrob())
    d <- coord$transform(data, panel_params)
    col <- ggplot2::alpha(d$colour, d$alpha)
    lines <- grid::segmentsGrob(
      x0 = c(d$x, d$x), y0 = c(d$ymin, d$upper), x1 = c(d$x, d$x), y1 = c(d$lower, d$ymax),
      default.units = "native",
      gp = grid::gpar(col = c(col, col), lwd = c(d$linewidth, d$linewidth) * ggplot2::.pt,
                      lty = c(d$linetype, d$linetype), lineend = lineend))
    dots <- grid::pointsGrob(
      d$x, d$middle, pch = d$shape, default.units = "native",
      gp = grid::gpar(col = col, fill = ggplot2::alpha(d$fill, d$alpha),
                      fontsize = d$size * ggplot2::.pt + d$stroke * ggplot2::.stroke / 2,
                      lwd = d$stroke * ggplot2::.stroke / 2))
    grid::grobTree(lines, dots)
  }
)

#' Quartile plot: the box plot without the box
#'
#' Tufte's quartile plot. Per group, a line runs from the minimum to the
#' lower quartile and another from the upper quartile to the maximum; the
#' gap between them is the interquartile range, and a dot marks the median.
#'
#' The five numbers are geometry, computed with
#' `stats::quantile(type = 7)` (R's default, as in the Bleisatz reference
#' script). The built data hold them as `ymin`, `lower`, `middle`, `upper`,
#' `ymax` and `n`. The layer prints no number; if quartiles or medians are
#' reported as text, compute them as finalised results (for example a
#' descriptive summary from 'infercalcR') and format them with
#' [report_table()] or [report_descriptive()], never from the plot.
#'
#' Lines are 0.8 pt and the median dot 1.1 mm, in ink. Groups follow the
#' usual rules: a discrete `x` groups by level; with a numeric `x`, map
#' `group`.
#'
#' @inheritParams geom_rangeframe
#' @param geom The geom; `GeomQuartiles` by default.
#' @param ... Other arguments passed on to [ggplot2::layer()], such as
#'   `linewidth` or `size`.
#' @return A ggplot2 layer.
#' @seealso [geom_rangeframe()], `vignette("tufte")`.
#' @examples
#' library(ggplot2)
#' p <- ggplot(PlantGrowth, aes(group, weight)) +
#'   stat_quartiles() +
#'   labs(x = NULL, y = "Dried weight (g)") +
#'   theme_figure("slide")
#' @export
stat_quartiles <- function(mapping = NULL, data = NULL, geom = GeomQuartiles,
                           position = "identity", ..., mode = "light", transparent = FALSE,
                           palette = "onebit", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE) {
  m <- .ir_mode(mode, transparent, palette)
  .ir_check_flag(na.rm, "na.rm")
  params <- c(list(na.rm = na.rm), .ir_ink_params(list(...), m$ink))
  ggplot2::layer(data = data, mapping = mapping, stat = StatQuartiles, geom = geom,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = params)
}

# Rows of `d` with the columns of `cols`, missing ones filled with NA, so that
# context and summary rows bind.
.ir_align_cols <- function(d, cols) {
  for (nm in setdiff(cols, names(d))) d[[nm]] <- rep(NA, nrow(d))
  d[, cols, drop = FALSE]
}

#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatContext <- ggplot2::ggproto(
  "StatContext", ggplot2::Stat,
  required_aes = c("x", "y"),
  optional_aes = "series",
  compute_panel = function(data, scales, fun = "median", context = TRUE, highlight = NULL,
                           colours = list(context = .ir_ink, summary = .ir_ink,
                                          highlight = .ir_ink),
                           widths = list(context = .ir_lw(.ir_fine_line),
                                         summary = .ir_lw(.ir_data_line))) {
    data <- data[is.finite(data$x) & is.finite(data$y), , drop = FALSE]
    keep_colour <- "colour" %in% names(data)
    keep_width <- "linewidth" %in% names(data)
    out <- list()
    if (context && nrow(data)) {
      ctx <- data
      ctx$role <- rep("context", nrow(ctx))
      if (!keep_colour) ctx$colour <- rep(colours$context, nrow(ctx))
      if (!keep_width) ctx$linewidth <- rep(widths$context, nrow(ctx))
      out[[1L]] <- ctx
    }
    if (fun != "none" && nrow(data)) {
      f <- switch(fun, median = stats::median, mean = mean)
      series <- if ("series" %in% names(data)) as.character(data$series) else rep("", nrow(data))
      next_group <- max(c(0, data$group)) + 1L
      for (s in unique(series)) {
        d <- data[series == s, , drop = FALSE]
        xs <- sort(unique(d$x))
        ys <- vapply(xs, function(v) f(d$y[d$x == v]), numeric(1))
        rest <- d[rep(1L, length(xs)), setdiff(names(d), c("x", "y", "group")), drop = FALSE]
        sm <- cbind(data.frame(x = xs, y = ys, group = next_group), rest)
        next_group <- next_group + 1L
        sm$role <- rep("summary", nrow(sm))
        hit <- !is.null(highlight) && s %in% highlight
        if (!keep_colour) {
          sm$colour <- rep(if (hit) colours$highlight else colours$summary, nrow(sm))
        }
        if (!keep_width) sm$linewidth <- rep(widths$summary, nrow(sm))
        out[[length(out) + 1L]] <- sm
      }
    }
    if (!length(out)) {
      d <- data[0, , drop = FALSE]
      d$role <- character()
      return(d)
    }
    cols <- unique(unlist(lapply(out, names)))
    out <- do.call(rbind, lapply(out, .ir_align_cols, cols = cols))
    rownames(out) <- NULL
    out
  }
)

#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomContext <- ggplot2::ggproto(
  "GeomContext", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(colour = .ir_ink, linewidth = .ir_lw(.ir_data_line),
                             linetype = "solid", alpha = NA),
  draw_key = ggplot2::draw_key_path,
  draw_panel = function(data, panel_params, coord, casing = NA, context_linetype = "solid",
                        lineend = "round", linejoin = "round") {
    if (!nrow(data)) return(grid::nullGrob())
    if (is.null(data$role)) data$role <- rep("summary", nrow(data))
    d <- coord$transform(data, panel_params)
    d <- d[order(d$role, d$group, d$x), , drop = FALSE]
    lines <- function(rows, col = NULL, lwd = NULL, lty = NULL) {
      if (!nrow(rows)) return(NULL)
      ids <- unique(rows$group)
      first <- rows[match(ids, rows$group), , drop = FALSE]
      grid::polylineGrob(
        rows$x, rows$y, id = match(rows$group, ids), default.units = "native",
        gp = grid::gpar(col = if (is.null(col)) ggplot2::alpha(first$colour, first$alpha) else col,
                        lwd = (if (is.null(lwd)) first$linewidth else lwd) * ggplot2::.pt,
                        lty = if (is.null(lty)) first$linetype else lty,
                        lineend = lineend, linejoin = linejoin))
    }
    ctx <- d[d$role == "context", , drop = FALSE]
    sm <- d[d$role == "summary", , drop = FALSE]
    grobs <- list(lines(ctx, lty = context_linetype))
    # A paper-coloured casing separates the summary from its context.
    if (nrow(sm) && !is.na(casing)) {
      grobs <- c(grobs, list(lines(sm, col = casing, lwd = .ir_lw(.ir_casing_line),
                                   lty = "solid")))
    }
    grobs <- c(grobs, list(lines(sm)))
    grobs <- Filter(Negate(is.null), grobs)
    if (!length(grobs)) return(grid::nullGrob())
    do.call(grid::grobTree, grobs)
  }
)

#' Context lines in grey, the summary in ink
#'
#' Tufte's context grey. Every unit (patient, subject, region) is drawn as a
#' thin line for context, and a summary of them, the median by default, is
#' drawn in ink on top, separated from the context by a casing in the colour
#' of the paper. Map `group` to the unit; map `series` to draw one summary
#' per series (for example per stratum), and map `linetype` to the same
#' variable to tell the summaries apart without colour.
#'
#' The context lines are `#999999`, 0.3 pt, with `palette = "grey"` or
#' `"tufte"`. In `palette = "onebit"` the grey becomes ink drawn as a 0.15 pt
#' hairline (pass `context_linetype = "22"` for dashed hairlines), so the
#' figure stays two-valued. The summary is ink, 0.8 pt; the series named in
#' `highlight` is drawn in Tufte's red with `palette = "tufte"` (in ink
#' otherwise).
#'
#' The summary is geometry, computed per series at each distinct `x` from
#' the rows of the layer: the median (`stats::median()`) or the mean. With
#' `context = FALSE` the layer draws only the summary, and with
#' `fun = "none"` only the context lines, so a summary of all units can be
#' drawn over context lines of a sample, or a precomputed line can be drawn
#' as context. The built data keep a `role` column (`"context"` or
#' `"summary"`) and the drawn `colour` and `linewidth` of each row.
#'
#' @inheritParams geom_rangeframe
#' @param ... Other arguments passed on to [ggplot2::layer()].
#' @param fun Summary of the units at each `x`: `"median"` (default),
#'   `"mean"` or `"none"`.
#' @param context If `TRUE` (default), draw the units as context lines.
#' @param highlight `NULL` (default) or values of `series` whose summary is
#'   drawn in red (`palette = "tufte"`).
#' @param casing If `TRUE` (default), draw a 2.6 pt line in the paper colour
#'   under each summary (not in transparent modes).
#' @param context_linetype Line type of the context lines, `"solid"` by
#'   default.
#' @return A ggplot2 layer.
#' @seealso [scale_colour_palette()], [geom_rangeframe()], `vignette("tufte")`.
#' @examples
#' library(ggplot2)
#' d <- data.frame(id = rep(1:12, each = 5), time = rep(1:5, 12),
#'                 arm = rep(c("A", "B"), each = 30))
#' d$score <- d$time * ifelse(d$arm == "A", 1, 1.5) + sin(d$id * d$time)
#' p <- ggplot(d, aes(time, score, group = id, series = arm, linetype = arm)) +
#'   stat_context(highlight = "B", palette = "tufte") +
#'   labs(x = "Week", y = "Score (points)") +
#'   theme_figure("slide", legend = "none", palette = "tufte")
#' @export
stat_context <- function(mapping = NULL, data = NULL, ..., fun = c("median", "mean", "none"),
                         context = TRUE, highlight = NULL, casing = TRUE,
                         context_linetype = "solid", mode = "light", transparent = FALSE,
                         palette = "onebit", na.rm = FALSE, show.legend = FALSE,
                         inherit.aes = TRUE) {
  m <- .ir_mode(mode, transparent, palette)
  fun <- .ir_check_choice(fun, c("median", "mean", "none"), "fun")
  .ir_check_flag(context, "context")
  .ir_check_flag(casing, "casing")
  .ir_check_flag(na.rm, "na.rm")
  if (!context && fun == "none") {
    .ir_abort("context = FALSE with fun = \"none\" draws nothing; keep one of them")
  }
  if (!is.null(highlight) && (!is.atomic(highlight) || !length(highlight) || anyNA(highlight))) {
    .ir_abort("highlight must be NULL or a vector of series values without NA")
  }
  if (!is.null(highlight)) highlight <- as.character(highlight)
  .ir_check_string(context_linetype, "context_linetype")
  onebit <- m$palette == "onebit"
  colours <- list(context = m$grey, summary = m$ink, highlight = m$red)
  widths <- list(context = .ir_lw(if (onebit) .ir_fine_line else .ir_context_line),
                 summary = .ir_lw(.ir_data_line))
  params <- c(list(fun = fun, context = context, highlight = highlight, colours = colours,
                   widths = widths, casing = if (casing) m$knock else NA_character_,
                   context_linetype = context_linetype, na.rm = na.rm), list(...))
  ggplot2::layer(data = data, mapping = mapping, stat = StatContext, geom = GeomContext,
                 position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
                 params = params)
}

#' @rdname infereportR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPapergrid <- ggplot2::ggproto(
  "GeomPapergrid", ggplot2::Geom,
  required_aes = character(),
  default_aes = ggplot2::aes(colour = .ir_paper, linewidth = .ir_lw(.ir_paper_grid),
                             linetype = "solid", alpha = NA),
  draw_key = ggplot2::draw_key_path,
  draw_panel = function(data, panel_params, coord, axis = "y") {
    first <- data[1L, , drop = FALSE]
    gp <- grid::gpar(col = ggplot2::alpha(first$colour, first$alpha),
                     lwd = first$linewidth * ggplot2::.pt, lty = first$linetype,
                     lineend = "butt")
    # The break positions of a view scale are rescaled to the panel (0-1),
    # for continuous and discrete scales alike.
    view <- panel_params[[axis]]
    at <- if (is.null(view)) numeric() else as.numeric(view$break_positions())
    at <- at[is.finite(at)]
    if (!length(at)) return(grid::nullGrob())
    if (axis == "y") {
      grid::segmentsGrob(0, at, 1, at, default.units = "npc", gp = gp)
    } else {
      grid::segmentsGrob(at, 0, at, 1, default.units = "npc", gp = gp)
    }
  }
)

#' Paper grid as a layer
#'
#' Grid lines in the colour of the paper, drawn as a layer at the breaks of
#' the `y` (or `x`) scale, so that they cut through the bars below them:
#' Tufte's grid that appears only where it is needed. `theme_figure(grid =
#' "paper")` draws the same grid from the theme, on top of every layer; use
#' this layer instead when text, such as labels above the bars, must stay on
#' top of the grid: add it after the bars and before the text, and set
#' `theme_figure(axes = "none")` to drop the axis lines and ticks.
#'
#' The lines are 0.9 pt wide and have the paper colour of `mode` (white in
#' light mode, black in dark mode), also with `transparent = TRUE`: they cut
#' the bars there too.
#'
#' @param ... Other arguments passed on to [ggplot2::layer()], such as
#'   `linewidth`.
#' @param axis `"y"` (default, horizontal lines at the y breaks, for
#'   vertical bars) or `"x"`.
#' @param mode,transparent,palette As in [theme_figure()].
#' @return A ggplot2 layer.
#' @seealso [theme_figure()], `vignette("tufte")`.
#' @examples
#' library(ggplot2)
#' d <- data.frame(group = c("A", "B", "C"), pct = c(12, 21, 30))
#' # The fill is set: the default bar fill of ggplot2 is a dark grey.
#' p <- ggplot(d, aes(group, pct)) +
#'   geom_col(width = 0.5, fill = "black") +
#'   geom_papergrid() +
#'   geom_text(aes(label = group), vjust = -0.5, size = 3) +
#'   scale_y_continuous(breaks = c(10, 20, 30), labels = c("10%", "20%", "30%")) +
#'   labs(x = NULL, y = NULL) +
#'   theme_figure("slide", axes = "none")
#' @export
geom_papergrid <- function(..., axis = c("y", "x"), mode = "light", transparent = FALSE,
                           palette = "onebit") {
  m <- .ir_mode(mode, transparent, palette)
  axis <- .ir_check_choice(axis, c("y", "x"), "axis")
  params <- c(list(axis = axis), .ir_ink_params(list(...), m$paper))
  ggplot2::layer(data = data.frame(.ir_grid = 1), mapping = NULL, stat = "identity",
                 geom = GeomPapergrid, position = "identity", show.legend = FALSE,
                 inherit.aes = FALSE, params = params)
}
