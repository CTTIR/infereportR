# Tufte's table-graphics as figure builders (Bleisatz 1.3): the slopegraph and
# sparklines, and the LaTeX string of the theme's \sparkline. Both builders
# draw on a canvas measured in millimetres of their slot, with text columns
# measured in the figure font, and write every number with the formatting
# functions of the package.

# Space between a text column and its neighbour, in mm.
.ir_tufte_pad <- 3

# The slot size in mm without the 1 mm plot margins.
.ir_canvas <- function(slot) {
  slot <- .ir_check_choice(slot, .ir_presets$preset, "slot")
  size <- figure_size(slot)
  c(width = size$width - 2, height = size$height - 2)
}

# The theme of a canvas figure: the figure theme without axes.
.ir_canvas_theme <- function(mode, transparent, palette) {
  theme_figure("slide", legend = "none", mode = mode, transparent = transparent,
               palette = palette) +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(1, 1, 1, 1, "mm"))
}

.ir_check_column <- function(data, col, arg, numeric = FALSE, optional = FALSE) {
  if (optional && is.null(col)) return(invisible(NULL))
  .ir_check_string(col, arg)
  if (!col %in% names(data)) {
    .ir_abort(sprintf("%s must name a column of data; \"%s\" is not one", arg, col))
  }
  if (numeric && !is.numeric(data[[col]])) {
    .ir_abort(sprintf("the column %s (\"%s\") must be numeric", arg, col))
  }
  invisible(col)
}

.ir_check_decimals <- function(digits, arg = "digits") {
  ok <- is.numeric(digits) && length(digits) >= 1L && !anyNA(digits) &&
    all(digits == floor(digits)) && all(digits >= 0) && all(digits <= 15)
  if (!ok) .ir_abort(sprintf("%s must hold integers from 0 to 15", arg))
  invisible(digits)
}

# The value of a per-group setting (named by group, or one for all).
.ir_per_group <- function(x, g) {
  if (length(x) == 1L && is.null(names(x))) return(x[[1L]])
  if (!is.null(names(x)) && g %in% names(x)) return(x[[g]])
  NULL
}

#' Slopegraph as a table-graphic
#'
#' Tufte's slopegraph: each row of `data` is a line from its value on the
#' left to its value on the right, and names and values stand on both sides
#' as in a table, each at the height of its value. The slope is the change.
#' Labels on one side are spread so that neighbours are at least `gap` apart
#' (the spreading of [geom_endlabel()]: sorted, pushed apart, the mean
#' position kept), so close values never overlap.
#'
#' The lines are grey, 0.6 pt, except the rows named in `highlight`, which
#' are 0.8 pt and red with `palette = "tufte"`. In `palette = "onebit"` the
#' grey lines are ink hairlines and the highlighted line ink, so the figure
#' stays two-valued. Values are written with fixed `digits` decimals by the
#' package's formatting functions; the plotted positions are the stored
#' values. The figure is drawn on a canvas of the slot (`figure_size(slot)`),
#' with the text columns measured in the figure font, so save it with the
#' same preset.
#'
#' @param data A data.frame with one row per line.
#' @param label,left,right Names of the columns that hold the row labels and
#'   the left and right values (numeric).
#' @param highlight `NULL` (default) or labels of rows to emphasise.
#' @param titles Column titles above the left and right labels, a character
#'   vector of length 2; `c(left, right)` by default, `NULL` for none.
#' @param note `NULL` (default) or a short note centred above the titles,
#'   in grey, for example the measure and its unit.
#' @param digits Decimals of the values, 0 by default.
#' @param gap Minimum distance between neighbouring labels as a fraction of
#'   the value range, 0.115 by default.
#' @param slot Size preset the figure is drawn for, `"slide_side"` by
#'   default.
#' @param mode,transparent,palette As in [theme_figure()].
#' @return A ggplot. Layer 1 holds the lines (`colour` and `linewidth` per
#'   row); the text layers hold the label positions `y` and labels.
#' @seealso [figure_sparklines()], [geom_endlabel()], `vignette("tufte")`.
#' @examples
#' d <- data.frame(group = c("All", "Younger", "Older", "Female"),
#'                 before = c(62, 67, 57, 71), after = c(85, 78, 90, 85))
#' p <- figure_slopegraph(d, "group", "before", "after", highlight = "All",
#'                   titles = c("Before", "After"), palette = "tufte")
#' @export
figure_slopegraph <- function(data, label, left, right, highlight = NULL,
                              titles = c(left, right), note = NULL, digits = 0L, gap = 0.115,
                              slot = "slide_side", mode = "light", transparent = FALSE,
                              palette = "onebit") {
  if (!is.data.frame(data) || !nrow(data)) .ir_abort("data must be a data.frame with rows")
  .ir_check_column(data, label, "label")
  .ir_check_column(data, left, "left", numeric = TRUE)
  .ir_check_column(data, right, "right", numeric = TRUE)
  if (!is.null(titles) && (!is.character(titles) || length(titles) != 2L || anyNA(titles))) {
    .ir_abort("titles must be NULL or a character vector of length 2")
  }
  if (!is.null(note)) .ir_check_string(note, "note")
  .ir_check_decimals(digits)
  if (length(digits) != 1L) .ir_abort("digits must be a single integer from 0 to 15")
  .ir_check_number(gap, "gap", min = 0)
  if (!is.null(highlight) && (!is.atomic(highlight) || anyNA(highlight))) {
    .ir_abort("highlight must be NULL or a vector of labels")
  }
  m <- .ir_mode(mode, transparent, palette)
  canvas <- .ir_canvas(slot)
  s <- data.frame(name = as.character(data[[label]]), left = as.numeric(data[[left]]),
                  right = as.numeric(data[[right]]), stringsAsFactors = FALSE)
  if (!all(is.finite(c(s$left, s$right)))) .ir_abort("left and right values must be finite")

  rng <- range(c(s$left, s$right))
  g <- if (diff(rng) > 0) diff(rng) * gap else max(abs(rng), 1) * gap
  s$ly <- .ir_spread(s$left, g)
  s$ry <- .ir_spread(s$right, g)
  top <- max(c(s$ly, s$ry))
  bot <- min(c(s$ly, s$ry))

  fs <- .ir_label_pt() - 0.5
  sans <- .ir_sans_family()
  cond <- .ir_condensed_family()
  s$ltext <- paste0(s$name, "  ", .ir_format_fixed(s$left, digits))
  s$rtext <- paste0(.ir_format_fixed(s$right, digits), "  ", s$name)
  wl <- max(.ir_text_width_mm(c(s$ltext, titles[1L]), sans, fs))
  wr <- max(.ir_text_width_mm(c(s$rtext, titles[2L]), sans, fs))
  pad <- 1.2
  w <- canvas[["width"]]
  sw <- min(0.345 * w, w - wl - wr - 2 * pad)
  if (sw < 0.1 * w) {
    .ir_abort(sprintf("the labels (%.0f mm) leave no room for the slopes in slot \"%s\"",
                      wl + wr + 2 * pad, slot))
  }
  x0 <- (w - (wl + wr + 2 * pad + sw)) / 2 + wl + pad
  x1 <- x0 + sw

  s$main <- s$name %in% as.character(highlight)
  onebit <- m$palette == "onebit"
  s$colour <- ifelse(s$main, m$red, m$grey)
  s$linewidth <- ifelse(s$main, .ir_lw(.ir_data_line),
                        .ir_lw(if (onebit) .ir_rule_hair else .ir_thin_line))
  # Highlighted lines are drawn last, on top of the others.
  s <- s[order(s$main), , drop = FALSE]
  size <- fs / ggplot2::.pt
  p <- ggplot2::ggplot(s) +
    ggplot2::geom_segment(ggplot2::aes(x = x0, xend = x1, y = .data$left, yend = .data$right),
                          colour = s$colour, linewidth = s$linewidth, lineend = "round") +
    ggplot2::geom_text(ggplot2::aes(x = x0 - pad, y = .data$ly, label = .data$ltext), hjust = 1,
                       family = sans, size = size, colour = m$ink) +
    ggplot2::geom_text(ggplot2::aes(x = x1 + pad, y = .data$ry, label = .data$rtext), hjust = 0,
                       family = sans, size = size, colour = m$ink)
  ytop <- top + g * 0.6
  if (!is.null(titles)) {
    hd <- data.frame(x = c(x0 - pad, x1 + pad), y = top + g * 1.25, label = titles,
                     hjust = c(1, 0))
    p <- p + ggplot2::geom_text(data = hd, ggplot2::aes(x = .data$x, y = .data$y,
                                                        label = .data$label, hjust = .data$hjust),
                                family = cond, size = size, colour = m$ink)
    ytop <- top + g * 1.9
  }
  if (!is.null(note)) {
    nd <- data.frame(x = (x0 + x1) / 2, y = top + g * 2.5, label = note)
    p <- p + ggplot2::geom_text(data = nd, ggplot2::aes(x = .data$x, y = .data$y,
                                                        label = .data$label),
                                hjust = 0.5, family = cond, size = (fs - 1) / ggplot2::.pt,
                                colour = m$grey)
    ytop <- top + g * 3
  }
  p +
    ggplot2::coord_cartesian(xlim = c(0, w), ylim = c(bot - g * 0.6, ytop), expand = FALSE,
                             clip = "off") +
    .ir_canvas_theme(mode, transparent, palette)
}

#' Sparklines: word-sized series in a table
#'
#' Tufte's sparklines: "data-intense, design-simple, word-sized graphics",
#' set as a table with the numbers beside them. Each line of the table is
#' one series (`row`, optionally within a `group` such as a marker), drawn
#' small, without axes, and followed by number columns.
#'
#' * **Scale.** The series of one `group` share one vertical scale (with
#'   the band, if given), so they can be compared; without `group`, each row
#'   has its own. All rows share the horizontal range of `x`.
#' * **Band.** With `lower` and `upper`, for example the interquartile range,
#'   a band lies behind the line: grey with `palette = "grey"` or `"tufte"`,
#'   two dotted ink hairlines in `"onebit"`.
#' * **Marks.** `marks = "last"` marks the last value, `"minmax"` the
#'   extremes chosen by `extremes`, `"both"` all of them, `"none"` nothing.
#'   With `palette = "tufte"` the last value is red and the extremes green,
#'   as in Tufte's own sparklines (he used blue for the extremes); otherwise
#'   the extremes are filled ink dots and the last value an open dot.
#' * **Numbers.** `numbers` selects the columns written beside the lines,
#'   any of `"min"`, `"max"` and `"last"`, in that order; by default those
#'   that are marked. Each number has the colour of its mark. They are written
#'   with fixed `digits` decimals by the package's formatting functions; with
#'   `x_unit`, the `x` of an extreme follows it (for example the hour of the
#'   peak, `"6 h"`).
#'
#' The figure is drawn on a canvas of the slot (`figure_size(slot)`): the
#' text columns are measured in the figure font, the sparkline column takes
#' the rest of the width, and the rows share the height. Save it with the
#' same preset.
#'
#' @param data A data.frame in long form: one row per point of a series.
#' @param x,y Names of the numeric columns of position and value.
#' @param row Name of the column that identifies a series (its label).
#' @param group `NULL` (default) or the name of a column that groups rows;
#'   its label is written once, left of its first row, and its rows share
#'   one vertical scale.
#' @param lower,upper `NULL` (default) or names of numeric columns of a band
#'   behind the line.
#' @param units `NULL` (default) or a character vector of units named by the
#'   levels of `group` (or one for all), written in grey below the group
#'   label.
#' @param digits Decimals of the numbers: one integer, or integers named by
#'   the levels of `group`. 0 by default.
#' @param marks `"last"` (default), `"minmax"`, `"both"` or `"none"`.
#' @param extremes Which extremes `"minmax"` and `"both"` mark: `"both"`
#'   (default), `"max"` or `"min"`.
#' @param numbers `NULL` (default: the marked values) or any of `"min"`,
#'   `"max"` and `"last"`; `character()` for none.
#' @param x_unit `NULL` (default) or a suffix such as `" h"`: the `x` of each
#'   extreme is written after its value.
#' @param titles `NULL` (default) or a character vector of column titles
#'   named by `"series"`, `"min"`, `"max"` and `"last"`; a hairline separates
#'   them from the table.
#' @param slot Size preset the figure is drawn for, `"slide_side"` by
#'   default.
#' @param mode,transparent,palette As in [theme_figure()].
#' @return A ggplot.
#' @seealso [sparkline_tex()] for sparklines set by LaTeX,
#'   [figure_slopegraph()], `vignette("tufte")`.
#' @examples
#' d <- data.frame(week = rep(1:8, 2), arm = rep(c("Drug", "Placebo"), each = 8),
#'                 score = c(3, 5, 6, 8, 7, 6, 5, 5, 3, 4, 4, 5, 5, 4, 4, 4))
#' p <- figure_sparklines(d, "week", "score", "arm", marks = "both", extremes = "max",
#'                   x_unit = " wk", titles = c(series = "Score", max = "Peak", last = "Week 8"),
#'                   palette = "tufte")
#' @export
figure_sparklines <- function(data, x, y, row, group = NULL, lower = NULL, upper = NULL,
                              units = NULL, digits = 0L,
                              marks = c("last", "minmax", "both", "none"),
                              extremes = c("both", "max", "min"), numbers = NULL,
                              x_unit = NULL, titles = NULL, slot = "slide_side",
                              mode = "light", transparent = FALSE, palette = "onebit") {
  if (!is.data.frame(data) || !nrow(data)) .ir_abort("data must be a data.frame with rows")
  .ir_check_column(data, x, "x", numeric = TRUE)
  .ir_check_column(data, y, "y", numeric = TRUE)
  .ir_check_column(data, row, "row")
  .ir_check_column(data, group, "group", optional = TRUE)
  .ir_check_column(data, lower, "lower", numeric = TRUE, optional = TRUE)
  .ir_check_column(data, upper, "upper", numeric = TRUE, optional = TRUE)
  if (xor(is.null(lower), is.null(upper))) .ir_abort("give both lower and upper, or neither")
  marks <- .ir_check_choice(marks, c("last", "minmax", "both", "none"), "marks")
  extremes <- .ir_check_choice(extremes, c("both", "max", "min"), "extremes")
  .ir_check_decimals(digits)
  if (!is.null(units) && (!is.character(units) || anyNA(units))) {
    .ir_abort("units must be NULL or a character vector")
  }
  if (!is.null(x_unit) && (!is.character(x_unit) || length(x_unit) != 1L || is.na(x_unit))) {
    .ir_abort("x_unit must be NULL or a single string")
  }
  cols_all <- c("series", "min", "max", "last")
  if (!is.null(titles) && (!is.character(titles) || is.null(names(titles)) || anyNA(titles) ||
                           !all(names(titles) %in% cols_all))) {
    .ir_abort(paste("titles must be NULL or a character vector named by \"series\",",
                    "\"min\", \"max\" and \"last\""))
  }
  m <- .ir_mode(mode, transparent, palette)
  canvas <- .ir_canvas(slot)
  w <- canvas[["width"]]
  h_all <- canvas[["height"]]

  ext <- switch(extremes, both = c("min", "max"), max = "max", min = "min")
  marked <- switch(marks, last = "last", minmax = ext, both = c(ext, "last"), none = character())
  if (is.null(numbers)) numbers <- marked
  if (!is.character(numbers) || anyNA(numbers) || !all(numbers %in% c("min", "max", "last"))) {
    .ir_abort("numbers must be NULL or values of \"min\", \"max\" and \"last\"")
  }
  numbers <- intersect(c("min", "max", "last"), numbers)

  d <- data.frame(x = data[[x]], y = data[[y]], row = as.character(data[[row]]),
                  stringsAsFactors = FALSE)
  d$group <- if (is.null(group)) "" else as.character(data[[group]])
  d$lo <- if (is.null(lower)) NA_real_ else data[[lower]]
  d$hi <- if (is.null(upper)) NA_real_ else data[[upper]]
  d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
  if (!nrow(d)) .ir_abort("data have no finite x and y")
  lev <- function(v, col) {
    if (!is.null(col) && is.factor(data[[col]])) {
      intersect(levels(data[[col]]), unique(v))
    } else {
      unique(v)
    }
  }
  glev <- lev(d$group, group)
  rlev <- lev(d$row, row)
  lines <- unique(d[, c("group", "row")])
  lines <- lines[order(match(lines$group, glev), match(lines$row, rlev)), , drop = FALSE]
  n <- nrow(lines)
  for (g in glev) {
    dg <- .ir_per_group(digits, g)
    if (is.null(dg)) .ir_abort(sprintf("digits has no value for group \"%s\"", g))
  }

  # Text columns, measured in the figure font.
  pt <- .ir_label_pt()
  sans <- .ir_sans_family()
  cond <- .ir_condensed_family()
  pad <- .ir_tufte_pad
  width_of <- function(s, family = sans, size = pt) {
    s <- s[!is.na(s) & nzchar(s)]
    if (!length(s)) 0 else max(.ir_text_width_mm(s, family, size))
  }
  stats_of <- function(dd) {
    dd <- dd[order(dd$x), , drop = FALSE]
    list(d = dd, min = which.min(dd$y), max = which.max(dd$y), last = nrow(dd))
  }
  per_line <- lapply(seq_len(n), function(i) {
    stats_of(d[d$group == lines$group[i] & d$row == lines$row[i], , drop = FALSE])
  })
  value_text <- function(i, what) {
    st <- per_line[[i]]
    .ir_format_fixed(st$d$y[st[[what]]], .ir_per_group(digits, lines$group[i]))
  }
  x_text <- function(i, what) {
    st <- per_line[[i]]
    paste0(format_estimate(st$d$x[st[[what]]]), x_unit)
  }
  unit_of <- function(g) if (is.null(units)) NULL else .ir_per_group(units, g)
  unit_text <- vapply(glev, function(g) {
    u <- unit_of(g)
    if (is.null(u)) "" else u
  }, character(1))

  wg <- if (is.null(group)) 0 else max(width_of(glev), width_of(unit_text, cond, pt - 1.5))
  x_row <- if (is.null(group)) 0 else wg + pad
  wr <- width_of(lines$row, cond)
  x0 <- x_row + wr + pad
  # Number columns from the right edge: value right-aligned, then the x of an
  # extreme left-aligned beside it.
  colpos <- list()
  cur <- w
  for (what in rev(numbers)) {
    tv <- vapply(seq_len(n), value_text, character(1), what = what)
    wv <- width_of(c(tv, if (!is.null(titles) && what %in% names(titles)) titles[[what]]))
    if (!is.null(x_unit) && what != "last") {
      tx <- vapply(seq_len(n), x_text, character(1), what = what)
      wx <- width_of(tx, cond, pt - 1)
      colpos[[what]] <- c(value = cur - wx - 1.5, x = cur - wx)
      cur <- cur - wx - 1.5 - wv - 2 * pad
    } else {
      colpos[[what]] <- c(value = cur, x = NA)
      cur <- cur - wv - 2 * pad
    }
  }
  x1 <- if (length(numbers)) cur + pad else w
  if (x1 - x0 < 0.1 * w) {
    .ir_abort(sprintf("the text columns leave no room for the sparklines in slot \"%s\"", slot))
  }

  # Rows: the header on top, the rows sharing the remaining height.
  head_h <- if (is.null(titles)) 1.2 else 7.4
  top <- h_all - head_h
  step <- if (n > 1L) (top - 1.2) / (n - 1L) else 0
  h <- if (n > 1L) min(3.4, 0.51 * step) else min(3.4, top)
  xr <- range(d$x)
  sx <- function(v) if (diff(xr) > 0) x0 + (v - xr[1L]) / diff(xr) * (x1 - x0) else (x0 + x1) / 2

  paths <- list()
  bands <- list()
  pts <- list()
  texts <- list()
  add_text <- function(x, y, label, hjust = 0, family = sans, size = pt, colour = m$ink) {
    texts[[length(texts) + 1L]] <<- data.frame(x = x, y = y, label = label, hjust = hjust,
                                               family = family, size = size / ggplot2::.pt,
                                               colour = colour, stringsAsFactors = FALSE)
  }
  colour_of <- c(min = m$green, max = m$green, last = m$red)
  for (i in seq_len(n)) {
    g <- lines$group[i]
    st <- per_line[[i]]
    dd <- st$d
    scale_rows <- if (is.null(group)) d$row == lines$row[i] else d$group == g
    rng <- range(c(d$y[scale_rows], d$lo[scale_rows], d$hi[scale_rows]), finite = TRUE)
    centre <- top - (i - 1L) * step
    y0 <- centre - h / 2
    sy <- function(v) {
      if (diff(rng) > 0) y0 + (v - rng[1L]) / diff(rng) * h else rep(centre, length(v))
    }
    xs <- sx(dd$x)
    if (!is.null(lower)) {
      ok <- is.finite(dd$lo) & is.finite(dd$hi)
      if (any(ok)) {
        bands[[length(bands) + 1L]] <- data.frame(
          x = c(xs[ok], rev(xs[ok])), y = c(sy(dd$lo[ok]), rev(sy(dd$hi[ok]))),
          id = i, edge = c(rep("lo", sum(ok)),
                                                                       rep("hi", sum(ok))),
          ex = c(xs[ok], rev(xs[ok])))
      }
    }
    paths[[i]] <- data.frame(x = xs, y = sy(dd$y), id = i)
    for (what in marked) {
      k <- st[[what]]
      open <- what == "last" && m$palette != "tufte"
      pts[[length(pts) + 1L]] <- data.frame(x = xs[k], y = sy(dd$y[k]), what = what,
                                            colour = if (what == "last") m$red else m$green,
                                            fill = if (open) m$knock else
                                              (if (what == "last") m$red else m$green),
                                            shape = if (open) 21 else 19,
                                            stringsAsFactors = FALSE)
    }
    first <- i == 1L || lines$group[i - 1L] != g
    if (!is.null(group) && first) {
      add_text(0, centre, g)
      u <- unit_of(g)
      if (!is.null(u) && nzchar(u)) {
        add_text(0, centre - 0.46 * max(step, 6.68), u, family = cond, size = pt - 1.5,
                 colour = m$grey)
      }
    }
    add_text(x_row, centre, lines$row[i], family = cond)
    for (what in numbers) {
      pos <- colpos[[what]]
      add_text(pos[["value"]], centre, value_text(i, what), hjust = 1,
               colour = if (what %in% marked) colour_of[[what]] else m$ink)
      if (!is.na(pos[["x"]])) add_text(pos[["x"]], centre, x_text(i, what), family = cond,
                                       size = pt - 1)
    }
  }
  if (!is.null(titles)) {
    head_y <- h_all - 1.6
    if ("series" %in% names(titles)) add_text(x0, head_y, titles[["series"]], family = cond,
                                              size = pt - 1)
    for (what in intersect(numbers, names(titles))) {
      add_text(colpos[[what]][["value"]], head_y, titles[[what]], hjust = 1, family = cond,
               size = pt - 1)
    }
  }
  paths <- do.call(rbind, paths)
  texts <- do.call(rbind, texts)
  p <- ggplot2::ggplot()
  if (length(bands)) {
    bands <- do.call(rbind, bands)
    if (m$palette == "onebit") {
      edges <- bands
      edges$edge_id <- paste(edges$id, edges$edge)
      p <- p + ggplot2::geom_path(data = edges, ggplot2::aes(.data$ex, .data$y,
                                                             group = .data$edge_id),
                                  colour = m$ink, linewidth = .ir_lw(0.3), linetype = "13")
    } else {
      p <- p + ggplot2::geom_polygon(data = bands, ggplot2::aes(.data$x, .data$y,
                                                                group = .data$id),
                                     fill = m$grey, colour = NA)
    }
  }
  p <- p + ggplot2::geom_path(data = paths, ggplot2::aes(.data$x, .data$y, group = .data$id),
                              colour = m$ink, linewidth = .ir_lw(.ir_thin_line),
                              lineend = "round", linejoin = "round")
  if (length(pts)) {
    pts <- do.call(rbind, pts)
    p <- p + ggplot2::geom_point(data = pts, ggplot2::aes(.data$x, .data$y),
                                 colour = pts$colour, fill = pts$fill, shape = pts$shape,
                                 size = 0.9, stroke = 0.5)
  }
  p <- p + ggplot2::geom_text(data = texts, ggplot2::aes(.data$x, .data$y, label = .data$label),
                              hjust = texts$hjust, family = texts$family, size = texts$size,
                              colour = texts$colour)
  if (!is.null(titles)) {
    rule <- data.frame(x = 0, xend = w, y = h_all - 4.1)
    p <- p + ggplot2::geom_segment(data = rule, ggplot2::aes(x = .data$x, xend = .data$xend,
                                                             y = .data$y, yend = .data$y),
                                   colour = m$ink, linewidth = .ir_lw(.ir_rule_hair))
  }
  p +
    ggplot2::coord_cartesian(xlim = c(0, w), ylim = c(0, h_all), expand = FALSE, clip = "off") +
    .ir_canvas_theme(mode, transparent, palette)
}

#' A sparkline for Bleisatz decks and documents
#'
#' Returns the `\sparkline[...]{...}` command of the Bleisatz Beamer theme
#' and document class (version 1.3), which draws a word-sized line in the
#' text: in the deck's palette, with the last value red and the extremes
#' green under `palette=tufte`, filled and open ink dots otherwise, and a
#' grey band (dotted rules in 1-bit). Use it in tables and running text of
#' decks and documents written from R.
#'
#' Only the options given are written, so the theme's defaults apply
#' otherwise (`width=4em`, `height=1.5ex`, `marks=last`). The values are
#' written with fixed `digits` decimals by the package's formatting
#' functions; `at`, `band` and `range` are written in full precision.
#'
#' @param x Numeric vector of at least two finite values.
#' @param at `NULL` (default, evenly spaced) or the x positions of the
#'   values, increasing, as long as `x`.
#' @param width,height `NULL` (default) or TeX dimensions, such as `"5em"`
#'   and `"1.5ex"`.
#' @param marks `NULL` (default) or `"last"`, `"minmax"`, `"both"` or
#'   `"none"`.
#' @param band `NULL` (default) or a numeric vector `c(lo, hi)`: a normal
#'   range behind the line.
#' @param range `NULL` (default) or a numeric vector `c(min, max)` that fixes
#'   the vertical scale, to compare several sparklines.
#' @param digits Decimals of the values, 1 by default.
#' @return A single string, the LaTeX command.
#' @seealso [figure_sparklines()] for a figure of sparklines.
#' @examples
#' sparkline_tex(c(9.0, 16.9, 20.4, 20.7, 20.1, 17.7, 11.7, 9.4),
#'               at = c(0, 2, 4, 6, 8, 12, 24, 48), width = "5em",
#'               range = c(8, 32), marks = "both")
#' @export
sparkline_tex <- function(x, at = NULL, width = NULL, height = NULL, marks = NULL,
                          band = NULL, range = NULL, digits = 1L) {
  if (!is.numeric(x) || length(x) < 2L || !all(is.finite(x))) {
    .ir_abort("x must be a numeric vector of at least two finite values")
  }
  .ir_check_decimals(digits)
  if (length(digits) != 1L) .ir_abort("digits must be a single integer from 0 to 15")
  num <- function(v) paste(format_estimate(v, digits = 15L), collapse = ",")
  pair <- function(v, arg) {
    if (!is.numeric(v) || length(v) != 2L || !all(is.finite(v)) || v[[1L]] > v[[2L]]) {
      .ir_abort(sprintf("%s must be NULL or two increasing finite numbers", arg))
    }
    sprintf("%s={%s}", arg, num(v))
  }
  dim <- function(v, arg) {
    .ir_check_string(v, arg)
    if (!grepl("^[0-9]*[.]?[0-9]+[a-z]{2}$", v)) {
      .ir_abort(sprintf("%s must be a TeX dimension such as \"4em\"", arg))
    }
    sprintf("%s=%s", arg, v)
  }
  opts <- character()
  if (!is.null(at)) {
    if (!is.numeric(at) || length(at) != length(x) || !all(is.finite(at)) ||
        any(diff(at) <= 0)) {
      .ir_abort("at must be NULL or increasing finite numbers, one per value of x")
    }
    opts <- c(opts, sprintf("at={%s}", num(at)))
  }
  if (!is.null(width)) opts <- c(opts, dim(width, "width"))
  if (!is.null(height)) opts <- c(opts, dim(height, "height"))
  if (!is.null(band)) opts <- c(opts, pair(band, "band"))
  if (!is.null(range)) opts <- c(opts, pair(range, "range"))
  if (!is.null(marks)) {
    marks <- .ir_check_choice(marks, c("last", "minmax", "both", "none"), "marks")
    opts <- c(opts, sprintf("marks=%s", marks))
  }
  values <- paste(.ir_format_fixed(x, digits), collapse = ", ")
  if (length(opts)) {
    sprintf("\\sparkline[%s]{%s}", paste(opts, collapse = ", "), values)
  } else {
    sprintf("\\sparkline{%s}", values)
  }
}
