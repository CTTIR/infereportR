#' Audit a figure against the APA figure rules
#'
#' Builds the plot once and reports where it departs from the rules of the
#' APA Style page on figures (American Psychological Association, *Figures*,
#' 7th ed., <https://apastyle.apa.org/style-grammar-guidelines/tables-figures/figures>)
#' and from inferplotR's own black-and-white rule. The rules it implements,
#' as worded there: "use a sans serif font" of "between 8 and 14 points" for
#' text in the image; place the legend within the figure's borders, in title
#' case; label axes clearly and give units of measurement; put the figure
#' number (bold) and title (italic title case) above the figure; put notes
#' below it.
#'
#' The six checks, always in this order:
#' \describe{
#'   \item{`text_size`}{(APA 7) every theme text element that will be drawn
#'     (axis text and titles, legend text and title when a legend is drawn,
#'     strip text when facetted, title, subtitle and caption when set) and
#'     every text label layer ([ggplot2::geom_text()],
#'     [ggplot2::geom_label()], the brackets of [stat_compare()]) resolves to
#'     8-14 pt. `fail` lists the offenders.}
#'   \item{`font_family`}{(APA 7) `fail` if a resolved family looks like a
#'     serif face (matches "serif", "times", "georgia", "garamond",
#'     "palatino" or "cambria" but not "sans").}
#'   \item{`two_values`}{(inferplotR) every `colour` and `fill` value in the
#'     built layer data, and the colours of drawn theme lines, rectangles and
#'     text, is black, white, `NA`, fully transparent or a grid pattern.
#'     This holds in every mode: light (black on white), dark (white on
#'     black) and the transparent modes, whose backgrounds are empty (`NA`).
#'     `fail` lists the offending colours as hex codes. The `"c64"` style of
#'     [char_figure()] is the one documented exception: its C64 blues fail
#'     here by design.}
#'   \item{`axis_labels`}{(APA 7) `fail` if the x or y label is missing or
#'     empty; `note` if a label of a continuous axis has no unit marker (a
#'     parenthesis, `%` or a comma followed by a unit). Discrete axes have no
#'     unit of measurement and need none.}
#'   \item{`embedded_title`}{(APA 7) `note` if a title or subtitle is set:
#'     APA puts the figure number and title above the image, in the
#'     manuscript text, not inside it.}
#'   \item{`legend_case`}{(APA 7) `note` if a legend title or key label
#'     starts with a lower-case letter (APA asks for title case); `pass` if no
#'     legend is drawn.}
#' }
#'
#' What it cannot check: the figure number, the figure title and the note
#' are placed in the manuscript, outside the image, so their presence,
#' wording and placement are not visible to the audit; neither is whether
#' the legend text is really in title case beyond its first letter, or
#' whether a unit marker names the right unit. The audit reads the plot
#' specification and does not draw it, so text added outside layers and the
#' theme (for example with grid annotations) is not seen.
#'
#' @param plot A ggplot.
#' @param target `"paper"` (default) or `"slide"`. Both targets apply the
#'   same rules in this version: the slide theme shares the paper text range.
#' @return A `data.frame` with exactly 6 rows (`text_size`, `font_family`,
#'   `two_values`, `axis_labels`, `embedded_title`, `legend_case`) and the
#'   character columns `check`, `source` (`"APA 7"` or `"inferplotR"`),
#'   `status` (`"pass"`, `"fail"` or `"note"`) and `detail` (`""` when there
#'   is nothing to report).
#' @seealso [theme_figure()], [inferplot()], [save_figure()]
#' @examples
#' library(ggplot2)
#' # A default ggplot is not black and white (grey panel, white grid lines,
#' # red points) and its axis labels give no units.
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red")
#' check_figure(p)
#'
#' # The same data as an APA figure.
#' q <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(shape = 1) +
#'   theme_figure() +
#'   labs(x = "Weight (1000 lb)", y = "Fuel economy (miles per gallon)")
#' check_figure(q)
#' @export
check_figure <- function(plot, target = c("paper", "slide")) {
  .ip_check_plot(plot)
  target <- .ip_check_choice(target, c("paper", "slide"), "target")
  b <- ggplot2::ggplot_build(plot)
  theme <- .ip_full_theme(plot)
  labs <- .ip_plot_labels(plot)
  legend <- .ip_legends(b, labs, theme)
  elements <- .ip_drawn_text(b, theme, labs, legend)
  layers <- .ip_text_layers(plot, b)

  rows <- list(
    .ip_check_text_size(theme, elements, layers),
    .ip_check_font_family(theme, elements, layers),
    .ip_check_two_values(b, theme, elements, legend),
    .ip_check_axis_labels(b, labs),
    .ip_check_title(labs),
    .ip_check_legend_case(legend)
  )
  data.frame(
    check = c("text_size", "font_family", "two_values", "axis_labels", "embedded_title",
              "legend_case"),
    source = c("APA 7", "APA 7", "inferplotR", "APA 7", "APA 7", "APA 7"),
    status = vapply(rows, function(r) r[["status"]], character(1)),
    detail = vapply(rows, function(r) r[["detail"]], character(1)),
    stringsAsFactors = FALSE
  )
}

.ip_result_row <- function(status, offenders = character()) {
  list(status = status, detail = paste(offenders, collapse = "; "))
}

# The plot theme if complete, else completed by the session default theme.
.ip_full_theme <- function(plot) {
  th <- plot$theme
  if (isTRUE(attr(th, "complete"))) return(th)
  if (is.null(th) || !length(th)) return(ggplot2::theme_get())
  ggplot2::theme_get() + th
}

.ip_plot_labels <- function(plot) {
  ns <- asNamespace("ggplot2")
  if (exists("get_labs", envir = ns, inherits = FALSE)) {
    get("get_labs", envir = ns, inherits = FALSE)(plot)
  } else {
    plot$labels
  }
}

.ip_has_text <- function(x) {
  if (is.null(x) || inherits(x, "waiver")) return(FALSE)
  if (is.character(x)) return(length(x) > 0L && !all(is.na(x)) && any(nzchar(x[!is.na(x)])))
  # Expressions and calls are drawn as plotmath.
  is.language(x) || is.expression(x)
}

.ip_label_text <- function(x) {
  if (is.character(x)) return(paste(x[!is.na(x)], collapse = " "))
  paste(deparse(x), collapse = " ")
}

# The legends that will be drawn: one entry per non-position aesthetic with a
# guide, with its title and key labels. ggplot2 reports no guide data for an
# aesthetic whose guide is "none"; a theme legend position "none" hides all.
.ip_legends <- function(b, labs, theme) {
  if (identical(theme[["legend.position"]], "none")) return(list())
  scales <- b$plot$scales$scales
  aesthetics <- unique(unlist(lapply(scales, function(s) {
    if (inherits(s, "ScaleContinuousPosition") || inherits(s, "ScaleDiscretePosition")) {
      return(NULL)
    }
    s$aesthetics[[1L]]
  })))
  out <- list()
  for (a in aesthetics) {
    key <- tryCatch(ggplot2::get_guide_data(b, a), error = function(e) NULL)
    if (!is.data.frame(key) || !nrow(key)) next
    scale <- b$plot$scales$get_scales(a)
    title <- if (!is.null(scale) && !inherits(scale$name, "waiver")) scale$name else labs[[a]]
    labels <- if (".label" %in% names(key)) key$.label else character()
    out[[a]] <- list(title = title, labels = labels)
  }
  out
}

# Theme text elements that will be drawn, by name.
.ip_drawn_text <- function(b, theme, labs, legend) {
  names <- c("axis.text.x.bottom", "axis.text.y.left")
  if (.ip_has_text(labs$x)) names <- c(names, "axis.title.x.bottom")
  if (.ip_has_text(labs$y)) names <- c(names, "axis.title.y.left")
  if (length(legend)) {
    names <- c(names, "legend.text")
    if (any(vapply(legend, function(l) .ip_has_text(l$title), logical(1)))) {
      names <- c(names, "legend.title")
    }
  }
  facet <- b$layout$facet
  if (inherits(facet, "FacetWrap")) names <- c(names, "strip.text.x.top")
  if (inherits(facet, "FacetGrid")) {
    if (length(facet$params$cols)) names <- c(names, "strip.text.x.top")
    if (length(facet$params$rows)) names <- c(names, "strip.text.y.right")
  }
  for (what in c("title", "subtitle", "caption")) {
    if (.ip_has_text(labs[[what]])) names <- c(names, paste0("plot.", what))
  }
  drawn <- vapply(names, function(n) {
    !inherits(ggplot2::calc_element(n, theme), "element_blank")
  }, logical(1))
  names[drawn]
}

# Text-drawing layers with their built data.
.ip_text_layers <- function(plot, b) {
  out <- list()
  for (i in seq_along(plot$layers)) {
    geom <- plot$layers[[i]]$geom
    if (!(inherits(geom, "GeomText") || inherits(geom, "GeomLabel") ||
          inherits(geom, "GeomBracket"))) next
    d <- b$data[[i]]
    if (!nrow(d)) next
    out[[length(out) + 1L]] <- list(name = sprintf("layer %d (%s)", i, class(geom)[1L]),
                                    data = d)
  }
  out
}

.ip_fmt_pt <- function(x) paste0(format(signif(x, 3)), " pt")

.ip_check_text_size <- function(theme, elements, layers) {
  ok <- function(pt) {
    pt >= .ip_text_min_pt - 1e-8 & pt <= .ip_text_max_pt + 1e-8
  }
  offenders <- character()
  for (n in elements) {
    size <- ggplot2::calc_element(n, theme)$size
    if (is.numeric(size) && length(size) == 1L && is.finite(size) && !ok(size)) {
      offenders <- c(offenders, paste(n, .ip_fmt_pt(size)))
    }
  }
  for (l in layers) {
    if (is.null(l$data$size)) next
    pt <- unique(l$data$size[is.finite(l$data$size)]) * ggplot2::.pt
    bad <- pt[!ok(pt)]
    if (length(bad)) {
      offenders <- c(offenders, paste(l$name, paste(.ip_fmt_pt(bad), collapse = ", ")))
    }
  }
  .ip_result_row(if (length(offenders)) "fail" else "pass", offenders)
}

.ip_is_serif <- function(family) {
  grepl("serif|times|georgia|garamond|palatino|cambria", family, ignore.case = TRUE) &
    !grepl("sans", family, ignore.case = TRUE)
}

.ip_check_font_family <- function(theme, elements, layers) {
  offenders <- character()
  for (n in elements) {
    family <- ggplot2::calc_element(n, theme)$family
    if (is.character(family) && length(family) == 1L && !is.na(family) &&
        .ip_is_serif(family)) {
      offenders <- c(offenders, sprintf("%s \"%s\"", n, family))
    }
  }
  for (l in layers) {
    family <- unique(as.character(l$data$family))
    family <- family[!is.na(family) & .ip_is_serif(family)]
    if (length(family)) {
      offenders <- c(offenders, sprintf("%s %s", l$name,
                                        paste0("\"", family, "\"", collapse = ", ")))
    }
  }
  .ip_result_row(if (length(offenders)) "fail" else "pass", offenders)
}

# Colours that are not black, white, missing or fully transparent, as hex codes.
.ip_off_colours <- function(values) {
  if (is.list(values)) {
    values <- values[!vapply(values, function(v) inherits(v, "GridPattern"), logical(1))]
    atomic <- vapply(values, function(v) is.atomic(v) && length(v) == 1L, logical(1))
    values <- unlist(values[atomic], use.names = FALSE)
  }
  if (is.null(values) || !length(values)) return(character())
  values <- unique(values)
  values <- values[!is.na(values)]
  if (is.numeric(values)) values <- values[values != 0]
  if (!length(values)) return(character())
  rgba <- tryCatch(grDevices::col2rgb(values, alpha = TRUE), error = function(e) NULL)
  if (is.null(rgba)) return(as.character(values))
  ink <- grDevices::col2rgb(.ip_ink, alpha = TRUE)[, 1L]
  paper <- grDevices::col2rgb(.ip_paper, alpha = TRUE)[, 1L]
  keep <- vapply(seq_len(ncol(rgba)), function(j) {
    v <- rgba[, j]
    !(v[[4L]] == 0 || all(v == ink) || all(v == paper))
  }, logical(1))
  rgba <- rgba[, keep, drop = FALSE]
  if (!ncol(rgba)) return(character())
  hex <- ifelse(rgba[4L, ] == 255,
                grDevices::rgb(rgba[1L, ], rgba[2L, ], rgba[3L, ], maxColorValue = 255),
                grDevices::rgb(rgba[1L, ], rgba[2L, ], rgba[3L, ], rgba[4L, ],
                               maxColorValue = 255))
  unique(hex)
}

.ip_check_two_values <- function(b, theme, elements, legend) {
  offenders <- character()
  for (i in seq_along(b$data)) {
    d <- b$data[[i]]
    for (column in intersect(c("colour", "fill"), names(d))) {
      off <- .ip_off_colours(d[[column]])
      if (length(off)) {
        offenders <- c(offenders, sprintf("layer %d %s %s", i, column,
                                          paste(off, collapse = ", ")))
      }
    }
  }
  lines <- c("axis.line.x.bottom", "axis.line.y.left", "axis.ticks.x.bottom",
             "axis.ticks.y.left", "panel.grid.major.x", "panel.grid.major.y",
             "panel.grid.minor.x", "panel.grid.minor.y")
  rects <- c("panel.background", "panel.border", "plot.background")
  if (length(legend)) rects <- c(rects, "legend.background", "legend.key")
  if (any(startsWith(elements, "strip.text.x"))) rects <- c(rects, "strip.background.x")
  if (any(startsWith(elements, "strip.text.y"))) rects <- c(rects, "strip.background.y")
  for (n in c(lines, rects, elements)) {
    el <- ggplot2::calc_element(n, theme)
    if (is.null(el) || inherits(el, "element_blank")) next
    fields <- if (n %in% rects) c("colour", "fill") else "colour"
    for (f in fields) {
      off <- .ip_off_colours(el[[f]])
      if (length(off)) offenders <- c(offenders, sprintf("%s %s %s", n, f,
                                                         paste(off, collapse = ", ")))
    }
  }
  .ip_result_row(if (length(offenders)) "fail" else "pass", offenders)
}

.ip_has_unit <- function(text) {
  grepl("\\(.*\\)|%|,\\s*\\S", text, perl = TRUE)
}

.ip_check_axis_labels <- function(b, labs) {
  missing <- character()
  no_unit <- character()
  scales <- list(x = b$layout$panel_scales_x, y = b$layout$panel_scales_y)
  for (a in c("x", "y")) {
    if (!.ip_has_text(labs[[a]])) {
      missing <- c(missing, sprintf("%s label missing", a))
      next
    }
    sc <- scales[[a]]
    discrete <- length(sc) > 0L && isTRUE(sc[[1L]]$is_discrete())
    text <- .ip_label_text(labs[[a]])
    if (!discrete && !.ip_has_unit(text)) {
      no_unit <- c(no_unit, sprintf("%s label without unit: \"%s\"", a, text))
    }
  }
  if (length(missing)) return(.ip_result_row("fail", c(missing, no_unit)))
  if (length(no_unit)) return(.ip_result_row("note", no_unit))
  .ip_result_row("pass")
}

.ip_check_title <- function(labs) {
  set <- c("title", "subtitle")[c(.ip_has_text(labs$title), .ip_has_text(labs$subtitle))]
  if (!length(set)) return(.ip_result_row("pass"))
  .ip_result_row("note", sprintf(
    "%s set inside the image; APA places the figure number and title above the figure",
    paste(set, collapse = " and ")))
}

.ip_check_legend_case <- function(legend) {
  offenders <- character()
  for (a in names(legend)) {
    texts <- character()
    title <- legend[[a]]$title
    if (.ip_has_text(title) && is.character(title)) texts <- c(texts, title)
    labels <- legend[[a]]$labels
    if (is.character(labels) || is.factor(labels)) texts <- c(texts, as.character(labels))
    texts <- texts[!is.na(texts)]
    low <- unique(texts[grepl("^[[:lower:]]", texts)])
    if (length(low)) {
      offenders <- c(offenders, sprintf("%s legend: %s", a,
                                        paste0("\"", low, "\"", collapse = ", ")))
    }
  }
  .ip_result_row(if (length(offenders)) "note" else "pass", offenders)
}
