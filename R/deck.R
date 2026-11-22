# Bleisatz Beamer decks from figures: slide constructors, the deck, and the
# .tex writer. Slides keep their text as given; save_deck() escapes it, saves
# every figure at its slot size with save_figure_set() and writes a .tex file
# that uses only the commands of the Bleisatz theme.

.ir_slide_layouts <- c("side", "full", "half", "text")
.ir_deck_presets <- c(side = "slide_side", full = "slide_full", half = "slide_half")

.ir_new_slide <- function(type, ...) {
  structure(c(list(type = type), list(...)), class = "infereportR_slide")
}

.ir_check_slide_text <- function(text) {
  .ir_check_text(text, "text")
  if (!is.null(names(text)) && anyNA(names(text))) .ir_abort("names of text must not be NA")
  invisible(text)
}

#' Slides of a Bleisatz deck
#'
#' `figure_slide()`, `section_slide()`, `keyfigure_slide()` and
#' `slide_sparkline()` make the slides that [figure_deck()] collects into a
#' Bleisatz Beamer deck. Each slide keeps its text as given; [save_deck()]
#' escapes it for LaTeX, saves the figures and writes the `.tex` file.
#'
#' **Slots.** A figure slide places its figure in one of the three slots of
#' the theme, and [save_deck()] saves the figure at exactly that size
#' (all 44 mm high), so it is included at 100 %:
#'
#' | `layout` | Slot | LaTeX |
#' |---|---|---|
#' | `"side"` | `slide_side`, 86 mm | `\sidefigure[0.64]{...}{caption}`: the numbered caption beside the figure |
#' | `"half"` | `slide_half`, 64 mm | a `0.48\textwidth` column with the figure, a `0.46\textwidth` column with the caption and `text` |
#' | `"full"` | `slide_full`, 134 mm | `\includegraphics[width=\textwidth]`, the numbered caption on the source line |
#' | `"text"` | no figure | `text` as a paragraph (one string) or a list (several) |
#'
#' **Figures.** `plot` is a ggplot, or a function `function(mode, transparent)`
#' (optionally with `palette`) that returns one, as for [save_figure_set()].
#' A ggplot is saved unchanged in every mode folder of the deck, so a dark
#' deck or an inverse slide shows the light figure: pass a mode-aware
#' function for correct dark figures. With `plot = NULL` and a `name`, a
#' figure slide shows a figure that another slide of the deck defines under
#' that name (as the gallery deck shows figure 1 again on an inverse frame).
#'
#' **Text.** Every text argument is a character vector. In `text`, named
#' elements get their name as a small label above them (`c(Rule = "Label
#' lines directly.")`, as in the gallery deck). All text is escaped for
#' LaTeX (`& % $ # _ { } ~ ^ \`); non-ASCII characters pass through, except
#' that greater-or-equal, less-or-equal, the multiplication sign and the
#' minus sign become their math commands under pdfLaTeX. LaTeX made by
#' [tex_red()], [tex_green()], [tex_muted()] and [sparkline_tex()],
#' citation commands such as `\parencite{key}` and `\ref{label}` are kept as
#' they are; wrap a string in [I()] to pass it through verbatim.
#'
#' `slide_sparkline()` sets a table of sparklines, one row per series, with
#' the peak in green and the last value in red (Tufte's sparkline table;
#' in two values the theme sets them in ink and italic). All rows share one
#' vertical scale unless `range` is given.
#'
#' @param plot A ggplot, a function `function(mode, transparent)` returning
#'   one, or `NULL` (no figure, or one defined by another slide under
#'   `name`).
#' @param title The slide title: a single string, best written as the claim
#'   the slide shows.
#' @param layout `"side"` (default), `"full"`, `"half"` or `"text"`.
#' @param caption Caption of the figure, a single string, or `NULL`.
#' @param text A character vector: for `"text"` slides the paragraph or the
#'   list items; for `"side"` and `"half"` slides more paragraphs beside the
#'   figure. Names become small labels. Not used with `"full"`.
#' @param source The source line at the bottom of the slide, or `NULL`.
#' @param name File name of the figure, without directory and extension;
#'   by default `"<nn>-<title>"`, from the slide number and the title in
#'   ASCII.
#' @param inverse If `TRUE`, the slide is set as an inverse frame and its
#'   figure taken from the opposite mode folder.
#' @param value,what,uncertainty The key figure: the number as shown, what it
#'   measures, and its uncertainty and n, each a single string.
#' @param values A list of numeric vectors, one per row of the table (a
#'   single numeric vector is one row).
#' @param labels Row labels, one per element of `values`; by default the
#'   names of `values`.
#' @param at,band,range,marks,width Passed to [sparkline_tex()] for every
#'   row. `range` defaults to the range of all values, so the rows share
#'   one scale.
#' @param digits Decimals of the values in the sparklines and of the peak
#'   and last value.
#' @param header Column heads of the table: four strings for the label, the
#'   sparkline, the peak and the last value.
#' @return An object of class `infereportR_slide`.
#' @seealso [figure_deck()], [save_deck()], [render_deck()]
#' @examples
#' library(ggplot2)
#' p <- ggplot(PlantGrowth, aes(group, weight)) + geom_point(shape = 1) +
#'   theme_figure("slide")
#' figure_slide(p, "Treatment 2 plants weigh most", caption = "Dried weight by group.",
#'              text = c(Rule = "Show every observation."), source = "R datasets.")
#' section_slide("Results")
#' keyfigure_slide("Most patients improve", "43 %", "of patients improve within a week",
#'                 "95 % CI 38 to 48, n = 312")
#' slide_sparkline("C5a peaks within hours",
#'                 list(`ISS 16-24` = c(9.0, 16.9, 20.4, 20.7, 20.1, 17.7, 11.7, 9.4)),
#'                 at = c(0, 2, 4, 6, 8, 12, 24, 48))
#' @export
figure_slide <- function(plot = NULL, title, layout = c("side", "full", "half", "text"),
                         caption = NULL, text = NULL, source = NULL, name = NULL,
                         inverse = FALSE) {
  if (missing(title)) .ir_abort("title must be given")
  .ir_check_string(title, "title")
  layout <- .ir_check_choice(layout, .ir_slide_layouts, "layout")
  .ir_check_text(caption, "caption", single = TRUE)
  .ir_check_slide_text(text)
  .ir_check_text(source, "source", single = TRUE)
  if (!is.null(name)) .ir_check_base_name(name)
  .ir_check_flag(inverse, "inverse")
  if (layout == "text") {
    if (!is.null(plot)) .ir_abort("a \"text\" slide has no plot")
    if (is.null(text)) .ir_abort("a \"text\" slide needs text")
    if (!is.null(caption)) .ir_abort("a \"text\" slide has no caption")
  } else {
    if (is.null(plot) && is.null(name)) {
      .ir_abort("a figure slide needs a plot, or the name of a figure of another slide")
    }
    if (!is.null(plot)) .ir_check_figure_plot(plot)
    if (layout == "full" && !is.null(text)) .ir_abort("a \"full\" slide has no room for text")
  }
  .ir_new_slide("figure", title = title, layout = layout, plot = plot, caption = caption,
                text = text, source = source, name = name, inverse = inverse)
}

#' @rdname figure_slide
#' @export
section_slide <- function(title) {
  if (missing(title)) .ir_abort("title must be given")
  .ir_check_string(title, "title")
  .ir_new_slide("section", title = title)
}

#' @rdname figure_slide
#' @export
keyfigure_slide <- function(title, value, what, uncertainty, source = NULL) {
  if (missing(title) || missing(value) || missing(what) || missing(uncertainty)) {
    .ir_abort("title, value, what and uncertainty must be given")
  }
  .ir_check_string(title, "title")
  .ir_check_string(value, "value")
  .ir_check_string(what, "what")
  .ir_check_string(uncertainty, "uncertainty")
  .ir_check_text(source, "source", single = TRUE)
  .ir_new_slide("keyfigure", title = title, value = value, what = what,
                uncertainty = uncertainty, source = source, inverse = FALSE)
}

#' @rdname figure_slide
#' @export
slide_sparkline <- function(title, values, labels = names(values), at = NULL, band = NULL,
                            range = NULL, marks = "both", width = "5em", digits = 1L,
                            header = c("", "", "Peak", "Last"), text = NULL,
                            source = NULL) {
  if (missing(title)) .ir_abort("title must be given")
  .ir_check_string(title, "title")
  spark <- .ir_sparkline_rows(values, labels, at, band, range, marks, width, digits, header)
  .ir_check_slide_text(text)
  .ir_check_text(source, "source", single = TRUE)
  .ir_new_slide("sparkline", title = title, spark = spark, text = text, source = source,
                inverse = FALSE)
}

# Shared by slide_sparkline() and document_sparkline(): checked rows, each
# with its \sparkline call (made by sparkline_tex()), peak and last value.
.ir_sparkline_rows <- function(values, labels, at, band, range, marks, width, digits,
                               header) {
  if (is.numeric(values)) values <- list(values)
  if (!is.list(values) || !length(values) ||
      !all(vapply(values, function(v) is.numeric(v) && length(v) >= 2L && all(is.finite(v)),
                  logical(1L)))) {
    .ir_abort("values must be a list of numeric vectors of at least two finite values")
  }
  if (is.null(labels)) .ir_abort("labels must be given (or values named)")
  if (!is.character(labels) || length(labels) != length(values) || anyNA(labels)) {
    .ir_abort("labels must be a character vector with one label per element of values")
  }
  if (!is.character(header) || length(header) != 4L || anyNA(header)) {
    .ir_abort("header must be four strings")
  }
  .ir_check_decimals(digits)
  if (length(digits) != 1L) .ir_abort("digits must be a single integer from 0 to 15")
  if (is.null(range)) range <- base::range(unlist(values))
  lines <- vapply(values, function(v) {
    sparkline_tex(v, at = at, width = width, marks = marks, band = band, range = range,
                  digits = digits)
  }, character(1L))
  peak <- vapply(values, function(v) .ir_format_fixed(max(v), digits), character(1L))
  last <- vapply(values, function(v) .ir_format_fixed(v[[length(v)]], digits), character(1L))
  list(labels = unname(labels), lines = unname(lines), peak = unname(peak),
       last = unname(last), header = header)
}

.ir_sparkline_table <- function(spark, font) {
  head <- paste(.ir_tex(spark$header, font), collapse = " & ")
  rows <- sprintf("    %s & %s & %s & %s \\\\", .ir_tex(spark$labels, font), spark$lines,
                  vapply(spark$peak, tex_green, character(1L)),
                  vapply(spark$last, tex_red, character(1L)))
  c("  \\begin{tabular}{@{}lcrr@{}}", "    \\toprule", paste0("    ", head, " \\\\"),
    "    \\midrule", rows, "    \\bottomrule", "  \\end{tabular}")
}

#' A Bleisatz Beamer deck
#'
#' `figure_deck()` collects slides into a deck with the title page and the
#' options of the Bleisatz theme (version 1.3, bundled with the package; see
#' [deck_theme_file()]). [save_deck()] writes it as an ordinary Bleisatz
#' deck that the author keeps editing, and [render_deck()] compiles it.
#'
#' **Theme options.** Every option of the theme is an argument; `NULL`
#' means the theme's default, and only the options given are written into
#' `\usetheme[...]{Bleisatz}`:
#'
#' | Argument | Values | Theme default |
#' |---|---|---|
#' | `palette` | `"onebit"`, `"grey"`, `"tufte"` | `onebit` |
#' | `font` | `"plex"`, `"mono"`, `"c64"`, `"etbb"` | `plex` |
#' | `rules` | `"heavy"`, `"hair"` | `heavy` |
#' | `typography` | `"bleisatz"`, `"tufte"` | `bleisatz` |
#' | `emphasis` | `"inverse"`, `"italic"` | `inverse` |
#' | `figures` | `"bleisatz"`, `"tufte"` | `bleisatz` |
#' | `progress` | `"bar"`, `"dots"`, `"none"` | `bar` |
#' | `head` | `TRUE`, `FALSE` | running head |
#' | `sectionpages` | `TRUE`, `FALSE` | contents divider at each section |
#'
#' `tufte = TRUE` sets every aspect to Tufte's value (palette `tufte`, ET
#' Book, hairlines, Tufte typography, italic emphasis, the Tufte figure set,
#' no head, no progress, no section pages); options given beside it win.
#' `mode = "dark"` sets the option `dark`, `total = TRUE` the option `total`.
#'
#' **Figures follow the deck.** [save_deck()] saves every figure in the
#' deck's palette into the folders the theme's `\figurefolder{figures}`
#' searches: `figures/<mode>/` for each of `figure_modes`, or
#' `figures/tufte/<mode><suffix>/` when the deck uses the Tufte figure set
#' (`figures = "tufte"`, or `tufte = TRUE`), with the suffix `""`, `"-grey"`
#' or `"-1bit"` for the palettes `tufte`, `grey` and `onebit` (only the light
#' and dark modes exist there); C64 decks use `figures/c64/` instead of
#' `figures/`. The deck's own mode must be among `figure_modes`, and so must
#' the opposite mode when a slide is inverse.
#'
#' **Fonts.** `font = "c64"` needs LuaLaTeX and the font C64 Pro Mono,
#' which is not part of the theme or of this package (free to use, not to
#' redistribute: <https://style64.org/c64-truetype>); without it the theme
#' falls back to IBM Plex Mono. [render_deck()] then uses LuaLaTeX. IBM Plex
#' and ET Book come with TeX Live.
#'
#' @param slides A list of slides made by [figure_slide()],
#'   [section_slide()], [keyfigure_slide()] and [slide_sparkline()].
#' @param title Title of the deck, a single string.
#' @param subtitle,author,institute,date,venue Title page fields: `NULL` or
#'   character vectors; several strings are set on separate lines. `date =
#'   NULL` gives the date of compilation.
#' @param mode `"light"` (default) or `"dark"`.
#' @param palette,font,rules,typography,emphasis,figures,progress Theme
#'   options, see above; `NULL` (default) for the theme's default.
#' @param head,sectionpages `NULL` (default), `TRUE` or `FALSE`: the running
#'   head and the contents divider at each section.
#' @param tufte If `TRUE`, all Tufte aspects at once.
#' @param total If `TRUE`, the folio shows the total ("7 / 24").
#' @param figure_modes Mode folders the figures are saved in: any of
#'   `"light"`, `"dark"`, `"light-transparent"` and `"dark-transparent"`
#'   (all four by default).
#' @return An object of class `infereportR_deck`: a list with the elements
#'   `slides`, `meta`, `options`, `mode` and `figure_modes`.
#' @seealso [figure_slide()], [save_deck()], [render_deck()],
#'   [figure_gallery_deck()], `vignette("slides-and-documents")`
#' @examples
#' deck <- figure_deck(list(section_slide("Background"),
#'                          figure_slide(title = "One claim per slide", layout = "text",
#'                                       text = c("Evidence, not decoration",
#'                                                "One idea per slide"))),
#'                     title = "Write the main claim as a sentence", author = "A. Author",
#'                     progress = "dots")
#' deck
#' @export
figure_deck <- function(slides, title, subtitle = NULL, author = NULL, institute = NULL,
                        date = NULL, venue = NULL, mode = c("light", "dark"), palette = NULL,
                        font = NULL, rules = NULL, typography = NULL, emphasis = NULL,
                        figures = NULL, progress = NULL, head = NULL, sectionpages = NULL,
                        tufte = FALSE, total = FALSE,
                        figure_modes = c("light", "dark", "light-transparent",
                                         "dark-transparent")) {
  if (missing(slides)) .ir_abort("slides must be given")
  if (inherits(slides, "infereportR_slide")) slides <- list(slides)
  if (!is.list(slides) || !length(slides) ||
      !all(vapply(slides, inherits, logical(1L), "infereportR_slide"))) {
    .ir_abort("slides must be a non-empty list of slides made by figure_slide() and friends")
  }
  if (missing(title)) .ir_abort("title must be given")
  .ir_check_string(title, "title")
  meta <- list(title = title, subtitle = subtitle, author = author, institute = institute,
               date = date, venue = venue)
  for (key in setdiff(names(meta), "title")) .ir_check_text(meta[[key]], key)
  mode <- .ir_check_choice(mode, .ir_modes, "mode")
  opts <- .ir_bz_check(list(palette = palette, font = font, rules = rules,
                            typography = typography, emphasis = emphasis, figures = figures,
                            progress = progress, head = head, sectionpages = sectionpages,
                            tufte = tufte, total = total))
  .ir_check_set_modes(figure_modes, .ir_set_modes)
  slides <- .ir_name_figures(slides)
  .ir_check_figure_modes(slides, mode, figure_modes, opts)
  structure(list(slides = unname(slides), meta = meta, options = opts, mode = mode,
                 figure_modes = figure_modes),
            class = "infereportR_deck")
}

# Default file names, then the check that every name is defined once.
.ir_name_figures <- function(items, prefix = "") {
  for (i in seq_along(items)) {
    s <- items[[i]]
    if (!is.null(s$layout) && s$layout != "text" && is.null(s$name)) {
      label <- if (!is.null(s$title)) s$title else if (!is.null(s$caption)) s$caption else ""
      items[[i]]$name <- sprintf("%02d-%s", i, .ir_slug(label))
    }
  }
  fig <- Filter(function(s) !is.null(s$name), items)
  defined <- vapply(Filter(function(s) !is.null(s$plot), fig), `[[`, "", "name")
  if (anyDuplicated(defined)) {
    .ir_abort(sprintf("figure names must be unique; defined twice: %s",
                      paste(unique(defined[duplicated(defined)]), collapse = ", ")))
  }
  used <- vapply(fig, `[[`, "", "name")
  missing_names <- setdiff(used, defined)
  if (length(missing_names)) {
    .ir_abort(sprintf("no slide or block defines the figure %s",
                      paste(missing_names, collapse = ", ")))
  }
  # A figure keeps the slot of the slide that defines it.
  slot <- list()
  for (s in Filter(function(s) !is.null(s$plot), fig)) slot[[s$name]] <- s$layout
  for (s in fig) {
    if (!identical(slot[[s$name]], s$layout)) {
      .ir_abort(sprintf("figure %s is shown in two different slots", s$name))
    }
  }
  items
}

.ir_check_figure_modes <- function(items, mode, figure_modes, opts) {
  has_fig <- any(vapply(items, function(s) !is.null(s$name), logical(1L)))
  if (!has_fig) return(invisible(TRUE))
  if (!mode %in% figure_modes) {
    .ir_abort(sprintf("figure_modes must include the mode \"%s\" of the deck", mode))
  }
  inverse <- any(vapply(items, function(s) isTRUE(s$inverse) && !is.null(s$name),
                        logical(1L)))
  if (inverse && !.ir_opposite_mode(mode) %in% figure_modes) {
    .ir_abort(sprintf("figure_modes must include \"%s\" for the inverse slides",
                      .ir_opposite_mode(mode)))
  }
  invisible(TRUE)
}

# --- The .tex file ---------------------------------------------------------

.ir_text_paragraphs <- function(text, font, size = NULL) {
  if (is.null(text)) return(character())
  nm <- names(text)
  out <- character()
  for (k in seq_along(text)) {
    if (!is.null(nm) && nzchar(nm[[k]])) {
      out <- c(out, sprintf("\\smalllabel{%s}\\par\\smallskip", .ir_tex(nm[[k]], font)))
    }
    body <- .ir_tex(if (inherits(text, "AsIs")) I(text[[k]]) else text[[k]], font)
    out <- c(out, if (is.null(size)) paste0(body, "\\par") else sprintf("{%s %s\\par}", size,
                                                                      body))
    if (k < length(text)) out <- c(out, "\\medskip")
  }
  out
}

.ir_slide_tex <- function(s, n, font, folders, mode) {
  esc <- function(x) .ir_tex(x, font)
  ind <- function(x, k = 2L) if (length(x)) paste0(strrep(" ", k), x) else character()
  source_line <- if (!is.null(s$source)) sprintf("\\source{%s}", esc(s$source))
  small <- if (font == "c64") "\\footnotesize" else "\\small"
  frame <- switch(s$type,
    section = return(c(sprintf("\\section{%s}", esc(s$title)), "")),
    keyfigure = c(sprintf("\\keyfigure{%s}{%s}%%", esc(s$value), esc(s$what)),
                  sprintf("  {%s}", esc(s$uncertainty)), source_line),
    statement = c("\\begin{statement}", ind(esc(s$text)), "\\end{statement}"),
    sparkline = c(if (!is.null(s$text)) c(.ir_text_paragraphs(s$text, font), "\\medskip"),
                  sub("^  ", "", .ir_sparkline_table(s$spark, font)), source_line),
    figure = .ir_figure_frame(s, font, small, source_line, folders, mode)
  )
  opts <- if (identical(s$type, "statement")) "[plain]" else ""
  title <- if (identical(s$type, "statement")) "" else sprintf("{%s}", esc(s$title))
  out <- c(sprintf("\\begin{frame}%s%s", opts, title), ind(frame), "\\end{frame}")
  if (isTRUE(s$inverse)) out <- c("\\begin{inverse}", out, "\\end{inverse}")
  c(out, "")
}

.ir_figure_frame <- function(s, font, small, source_line, folders, mode) {
  esc <- function(x) .ir_tex(x, font)
  path <- if (isTRUE(s$inverse) && s$layout != "text") {
    sprintf("\\graphicspath{{%s}}", folders$folder(.ir_opposite_mode(mode)))
  }
  switch(s$layout,
    text = {
      if (length(s$text) == 1L && is.null(names(s$text))) {
        c("\\begin{measure}[0.8]", paste0("  ", esc(s$text)), "\\end{measure}", source_line)
      } else if (is.null(names(s$text))) {
        c("\\begin{itemize}", sprintf("  \\item %s", esc(s$text)), "\\end{itemize}",
          source_line)
      } else {
        c(.ir_text_paragraphs(s$text, font), source_line)
      }
    },
    side = {
      cap <- c(if (!is.null(s$caption)) esc(s$caption),
               if (!is.null(s$text)) c(if (!is.null(s$caption)) "\\par\\medskip",
                                       .ir_text_paragraphs(s$text, font)))
      c(path, sprintf("\\sidefigure[0.64]{\\includegraphics[width=\\linewidth]{%s}}", s$name),
        "  {%", paste0("   ", cap), "  }", source_line)
    },
    full = {
      cap <- if (!is.null(s$caption)) {
        sprintf("\\fullcaption{%s}", paste(c(esc(s$caption), esc(s$source)), collapse = " "))
      } else {
        source_line
      }
      c(path, sprintf("\\includegraphics[width=\\textwidth]{%s}", s$name), cap)
    },
    half = {
      col2 <- c(if (!is.null(s$caption)) c("\\figlabel\\par\\smallskip",
                                           sprintf("{%s %s\\par}", small, esc(s$caption))),
                if (!is.null(s$text)) c(if (!is.null(s$caption)) "\\medskip",
                                        .ir_text_paragraphs(s$text, font, small)))
      c(path, "\\begin{columns}[T,onlytextwidth]", "  \\begin{column}{0.48\\textwidth}",
        sprintf("    \\includegraphics[width=\\linewidth]{%s}", s$name), "  \\end{column}",
        "  \\begin{column}{0.46\\textwidth}", paste0("    ", col2), "  \\end{column}",
        "\\end{columns}", source_line)
    }
  )
}

.ir_title_fields <- function(meta, font, fields) {
  out <- character()
  for (key in fields) {
    v <- meta[[key]]
    if (is.null(v)) next
    out <- c(out, sprintf("\\%s{%s}", key, .ir_tex_lines(v, font)))
  }
  out
}

# The lines of the .tex file of a deck.
.ir_deck_tex <- function(deck) {
  eff <- .ir_bz_effective(deck$options)
  font <- eff$font
  folders <- .ir_bz_folders(eff, deck$figure_modes)
  opts <- .ir_bz_option_string(deck$options, deck$mode)
  theme <- if (length(opts)) {
    sprintf("\\usetheme[%s]{Bleisatz}", paste(opts, collapse = ","))
  } else {
    "\\usetheme{Bleisatz}"
  }
  engine <- if (font == "c64") "lualatex" else "pdflatex"
  body <- unlist(lapply(seq_along(deck$slides), function(i) {
    .ir_slide_tex(deck$slides[[i]], i, font, folders, deck$mode)
  }))
  c(sprintf("%% !TEX program = %s", engine),
    "% A Bleisatz deck written by infereportR::save_deck(); edit it freely.",
    if (font == "c64") c("% font=c64 needs LuaLaTeX and C64 Pro Mono (not bundled:",
                         "% https://style64.org/c64-truetype); without it IBM Plex Mono."),
    "\\documentclass[aspectratio=169,11pt]{beamer}",
    theme,
    "\\figurefolder{figures}",
    if (font == "c64") c("% Without C64 Pro Mono the theme falls back to font=mono and its",
                         "% \\figurefolder to figures/<mode>/; the C64 set stays in use:",
                         sprintf("\\graphicspath{{%s}}", .ir_bz_folders(eff, deck$figure_modes)$folder(deck$mode))),
    "% figures placed without \\sidefigure still take the next number",
    "\\providecommand\\figlabel{\\stepcounter{bzfigure}\\smalllabel{Figure~\\thebzfigure}}",
    "\\providecommand\\fullcaption[1]{\\stepcounter{bzfigure}\\source{Figure~\\thebzfigure. #1}}",
    "",
    .ir_title_fields(deck$meta, font, c("title", "subtitle", "author", "institute", "date",
                                        "venue")),
    "",
    "\\begin{document}",
    "",
    "\\begin{frame}[plain]", "  \\titlepage", "\\end{frame}", "",
    body,
    "\\end{document}")
}

# The figures of a deck or document: one row per figure to save.
.ir_figure_jobs <- function(items, presets) {
  defs <- Filter(function(s) !is.null(s$plot) && !is.null(s$name), items)
  lapply(defs, function(s) list(name = s$name, plot = s$plot, preset = presets[[s$layout]]))
}

# The file paths save_figure_set() will write for the jobs.
.ir_figure_targets <- function(jobs, dir, folders, png) {
  formats <- if (png) c("pdf", "png") else "pdf"
  unlist(lapply(jobs, function(j) {
    unlist(lapply(folders$modes, function(m) {
      file.path(dir, folders$dir, m, paste0(j$name, ".", formats))
    }))
  }))
}

.ir_save_figures <- function(jobs, dir, folders, png) {
  rows <- lapply(jobs, function(j) {
    save_figure_set(.ir_as_build(j$plot), j$name, preset = j$preset,
                    dir = file.path(dir, folders$dir), modes = folders$modes, png = png,
                    palette = folders$palette)
  })
  out <- do.call(rbind, c(list(.ir_set_empty()), rows))
  if (!nrow(out)) {
    return(data.frame(file = character(), type = character(), stringsAsFactors = FALSE))
  }
  data.frame(file = out$file, type = paste0("figure-", out$format), stringsAsFactors = FALSE)
}

#' Write a Bleisatz deck
#'
#' `save_deck()` writes a deck made by [figure_deck()] into `dir`: every
#' figure, saved with [save_figure_set()] at the size of its slot into the
#' mode folders the theme's `\figurefolder{figures}` finds (see
#' [figure_deck()]), the bundled theme file `beamerthemeBleisatz.sty`, and
#' the `.tex` file. The `.tex` file is an ordinary Bleisatz deck that uses
#' only the commands of the theme, so it can be edited and compiled like any
#' other: with [render_deck()], `latexmk -pdf deck.tex`, or on Overleaf.
#'
#' Nothing is written outside `dir`, which has no default and is created if
#' needed. Existing files are not overwritten unless `overwrite = TRUE`: if
#' any file the deck would write exists, nothing is written and a
#' condition of class `infereportR_file_exists` is raised.
#'
#' @param deck A deck made by [figure_deck()].
#' @param dir Directory to write into. Required.
#' @param file File name of the `.tex` file, without a directory part.
#' @param overwrite If `TRUE`, replace existing files.
#' @param png If `TRUE`, also write a PNG beside each figure PDF.
#' @return Invisibly, a data.frame with one row per written file and the
#'   columns `file` (the path) and `type` (`"tex"`, `"theme"`,
#'   `"figure-pdf"` or `"figure-png"`).
#' @seealso [figure_deck()], [render_deck()], [save_document()]
#' @examples
#' library(ggplot2)
#' p <- ggplot(PlantGrowth, aes(group, weight)) + geom_point(shape = 1) +
#'   theme_figure("slide") + labs(x = NULL, y = "Dried weight (g)")
#' deck <- figure_deck(list(figure_slide(p, "Treatment 2 plants weigh most",
#'                                       caption = "Every plant, by group.")),
#'                     title = "Plant growth", figure_modes = "light")
#' dir <- file.path(tempdir(), "deck")
#' if (capabilities("cairo")) {
#'   files <- save_deck(deck, dir)
#'   files
#'   head(readLines(file.path(dir, "deck.tex")), 12)
#' }
#' unlink(dir, recursive = TRUE)
#' @export
save_deck <- function(deck, dir, file = "deck.tex", overwrite = FALSE, png = FALSE) {
  if (!inherits(deck, "infereportR_deck")) .ir_abort("deck must be made by figure_deck()")
  if (missing(dir)) .ir_abort("dir must be given")
  .ir_check_string(dir, "dir")
  .ir_check_tex_file(file)
  .ir_check_flag(overwrite, "overwrite")
  .ir_check_flag(png, "png")
  folders <- .ir_bz_folders(.ir_bz_effective(deck$options), deck$figure_modes)
  jobs <- .ir_figure_jobs(deck$slides, .ir_deck_presets)
  tex <- .ir_deck_tex(deck)
  theme <- file.path(dir, "beamerthemeBleisatz.sty")
  targets <- c(file.path(dir, file), theme, .ir_figure_targets(jobs, dir, folders, png))
  .ir_check_targets(targets, overwrite)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  figs <- .ir_save_figures(jobs, dir, folders, png)
  file.copy(deck_theme_file("beamer"), theme, overwrite = TRUE)
  .ir_write_utf8(tex, file.path(dir, file))
  invisible(rbind(data.frame(file = c(file.path(dir, file), theme), type = c("tex", "theme"),
                             stringsAsFactors = FALSE), figs))
}
