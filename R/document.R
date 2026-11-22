# Bleisatz documents (bleisatz.cls): papers, reports and handouts from the
# same figures as the decks. Blocks keep their text as given; save_document()
# escapes it, saves the figures at their slot sizes and writes the .tex file.

.ir_document_layouts <- c("side", "full", "half", "margin")
.ir_document_presets <- c(side = "slide_side", full = "slide_full", half = "slide_half",
                          margin = "margin")

.ir_new_block <- function(type, ...) {
  structure(c(list(type = type), list(...)), class = "infereportR_block")
}

#' Blocks of a Bleisatz document
#'
#' These functions make the blocks that [figure_document()] sets, in order,
#' with the Bleisatz document class `bleisatz.cls`:
#'
#' | Block | LaTeX |
#' |---|---|
#' | `document_section(title, text)` | `\section{title}` and paragraphs |
#' | `document_text(text)` | paragraphs, one per string |
#' | `document_figure(layout = "side")` | `\sidefigure{file}{caption}[source]`: the caption beside the figure (86 mm) |
#' | `document_figure(layout = "full")` | `\fullfigure{file}{caption}[source]`: the caption below (134 mm) |
#' | `document_figure(layout = "half")` | `\sidefigure` with the 64 mm figure, so the caption gets the wider column |
#' | `document_figure(layout = "margin")` | `\marginfigure{file}{caption}[source]`: a 52 x 40 mm figure in the margin |
#' | `document_table(x, caption)` | a `table` float with booktabs rules, the caption above and `\tablenote` |
#' | `document_note(text)` | `\footnote{}` (a sidenote with `notes = "side"`), or `\marginnote{}` when `numbered = FALSE` |
#' | `document_newthought(opening, text)` | `\newthought{opening}` and the rest of the paragraph |
#' | `document_fullwidth(text)` | the `fullwidth` environment: text across column and margin |
#' | `document_epigraph(text, source)` | the `epigraph` environment |
#' | `document_sparkline(values)` | a `table` float of sparklines, as [slide_sparkline()] |
#'
#' A note attaches to the end of the block before it, as a footnote mark
#' does. Figures are saved by [save_document()] at the size of their slot
#' with [save_figure_set()]; see [figure_slide()] for `plot` and `name`, and
#' for how text is escaped (the same rules apply).
#'
#' `document_table()` sets a data.frame, such as the output of
#' [report_table()], as it is: column names become the head, numeric
#' columns are right-aligned and formatted with [format()], negative numbers
#' get a minus sign, and every cell is escaped. Format numbers first (for
#' example with [format_estimate()]) to control their decimals.
#'
#' @param title Section title, a single string.
#' @param text A character vector, one paragraph per string (for
#'   `document_note()` and `document_epigraph()` a single string).
#' @param plot A ggplot, a function `function(mode, transparent)` returning
#'   one, or `NULL` with a `name` defined by another block.
#' @param layout `"side"` (default), `"full"`, `"half"` or `"margin"`.
#' @param caption Caption, a single string. Required for figures and tables.
#' @param source Source line of a figure or attribution of an epigraph, or
#'   `NULL`.
#' @param label Cross-reference label (letters, digits, `: . - _`), or
#'   `NULL`; refer to it with `\ref{label}` in text.
#' @param name File name of the figure, without directory and extension; by
#'   default `"<nn>-<caption>"`.
#' @param x A data.frame.
#' @param note A note below the table (`\tablenote`), or `NULL`.
#' @param numbered If `TRUE` (default), a numbered footnote or sidenote;
#'   `FALSE` gives an unnumbered `\marginnote`.
#' @param opening The opening words of a new thought, set in small capitals.
#' @inheritParams figure_slide
#' @return An object of class `infereportR_block`.
#' @seealso [figure_document()], [save_document()], [render_document()]
#' @examples
#' document_section("Methods", "We simulated 240 patients.")
#' document_text(c("First paragraph.", "Second paragraph with 50% & more."))
#' document_table(data.frame(Group = c("A", "B"), n = c(12, 15)), caption = "Groups.")
#' document_note("Illustrative content.")
#' document_newthought("Late organ failure", "has replaced early haemorrhage.")
#' document_epigraph("Above all else show the data.", source = "Edward R. Tufte")
#' @name document_blocks
NULL

#' @rdname document_blocks
#' @export
document_section <- function(title, text = NULL) {
  if (missing(title)) .ir_abort("title must be given")
  .ir_check_string(title, "title")
  .ir_check_text(text, "text")
  .ir_new_block("section", title = title, text = text)
}

#' @rdname document_blocks
#' @export
document_text <- function(text) {
  if (missing(text)) .ir_abort("text must be given")
  .ir_check_text(text, "text", null = FALSE)
  .ir_new_block("text", text = text)
}

#' @rdname document_blocks
#' @export
document_figure <- function(plot = NULL, layout = c("side", "full", "half", "margin"), caption,
                            source = NULL, label = NULL, name = NULL) {
  layout <- .ir_check_choice(layout, .ir_document_layouts, "layout")
  if (missing(caption)) .ir_abort("caption must be given")
  .ir_check_string(caption, "caption")
  .ir_check_text(source, "source", single = TRUE)
  .ir_check_label(label)
  if (!is.null(name)) .ir_check_base_name(name)
  if (is.null(plot) && is.null(name)) {
    .ir_abort("a figure block needs a plot, or the name of a figure of another block")
  }
  if (!is.null(plot)) .ir_check_figure_plot(plot)
  .ir_new_block("figure", layout = layout, plot = plot, caption = caption, source = source,
                label = label, name = name)
}

#' @rdname document_blocks
#' @export
document_table <- function(x, caption, note = NULL, label = NULL) {
  if (!is.data.frame(x) || !ncol(x)) .ir_abort("x must be a data.frame with columns")
  if (missing(caption)) .ir_abort("caption must be given")
  .ir_check_string(caption, "caption")
  .ir_check_text(note, "note", single = TRUE)
  .ir_check_label(label)
  .ir_new_block("table", x = x, caption = caption, note = note, label = label)
}

#' @rdname document_blocks
#' @export
document_note <- function(text, numbered = TRUE) {
  if (missing(text)) .ir_abort("text must be given")
  .ir_check_text(text, "text", null = FALSE, single = TRUE)
  .ir_check_flag(numbered, "numbered")
  .ir_new_block("note", text = text, numbered = numbered)
}

#' @rdname document_blocks
#' @export
document_newthought <- function(opening, text = NULL) {
  if (missing(opening)) .ir_abort("opening must be given")
  .ir_check_string(opening, "opening")
  .ir_check_text(text, "text")
  .ir_new_block("newthought", opening = opening, text = text)
}

#' @rdname document_blocks
#' @export
document_fullwidth <- function(text) {
  if (missing(text)) .ir_abort("text must be given")
  .ir_check_text(text, "text", null = FALSE)
  .ir_new_block("fullwidth", text = text)
}

#' @rdname document_blocks
#' @export
document_epigraph <- function(text, source = NULL) {
  if (missing(text)) .ir_abort("text must be given")
  .ir_check_text(text, "text", null = FALSE, single = TRUE)
  .ir_check_text(source, "source", single = TRUE)
  .ir_new_block("epigraph", text = text, source = source)
}

#' @rdname document_blocks
#' @export
document_sparkline <- function(values, labels = names(values), caption, at = NULL,
                               band = NULL, range = NULL, marks = "both", width = "4.2em",
                               digits = 1L, header = c("", "", "Peak", "Last"), note = NULL,
                               label = NULL) {
  if (missing(caption)) .ir_abort("caption must be given")
  .ir_check_string(caption, "caption")
  spark <- .ir_sparkline_rows(values, labels, at, band, range, marks, width, digits, header)
  .ir_check_text(note, "note", single = TRUE)
  .ir_check_label(label)
  .ir_new_block("sparkline", spark = spark, caption = caption, note = note, label = label)
}

#' A Bleisatz document
#'
#' `figure_document()` collects blocks into a paper, report or handout set
#' with the Bleisatz document class `bleisatz.cls` (version 1.3, bundled with
#' the package; see [deck_theme_file()]). [save_document()] writes it as an
#' ordinary LaTeX document and [render_document()] compiles it. The text
#' block is 134 mm wide, so every slide figure fits at 100 %: full 134 mm,
#' side 86 mm, half 64 mm; the Tufte layout adds a 52 mm margin.
#'
#' **Class options.** Every option of the class is an argument; `NULL`
#' means the class default, and only the options given are written into
#' `\documentclass[...]{bleisatz}`. `palette`, `font`, `rules`,
#' `typography`, `emphasis`, `figures`, `progress` and `tufte` are those of
#' [figure_deck()]; documents add:
#'
#' | Argument | Values | Class default |
#' |---|---|---|
#' | `layout` | `"bleisatz"`, `"tufte"` (110 mm column, 52 mm margin, captions in the margin) | `bleisatz` |
#' | `notes` | `"foot"`, `"side"` (numbered sidenotes) | `foot` |
#' | `head` | `TRUE`, `FALSE` | running head |
#' | `paper` | `"a4"`, `"letter"` | `a4` |
#' | `justified` | `TRUE`, `FALSE` | ragged right |
#'
#' `tufte = TRUE` sets the Tufte layout, sidenotes, hairlines, Tufte
#' typography, italic emphasis, ET Book, the Tufte palette and figure set and
#' no progress marks; options given beside it win. Figures follow the
#' document as they follow a deck (see [figure_deck()]).
#'
#' **Bibliography.** With `bibliography`, the path to a `.bib` file,
#' [save_document()] copies the file next to the `.tex` file, loads biblatex
#' (`style=apa`, backend biber) with `\addbibresource` and prints the
#' references at the end. Cite with `\parencite{key}` or `\textcite{key}` in
#' any text; [render_document()] runs biber.
#'
#' @param blocks A list of blocks made by [document_section()] and the other
#'   block constructors.
#' @param title Title, a single string.
#' @param subtitle,shorttitle,author,institute,venue,date,correspondence,keywords
#'   Front matter: `NULL` or character vectors; several strings are set on
#'   separate lines. `shorttitle` is the running head.
#' @param abstract The abstract, `NULL` or a character vector of paragraphs.
#' @param paper `"a4"` (default) or `"letter"`.
#' @param justified If `TRUE`, justified text.
#' @param layout,notes Class options for the Tufte layout and sidenotes, see
#'   above; `NULL` (default) for the class default.
#' @param bibliography Path to a `.bib` file, or `NULL`.
#' @inheritParams figure_deck
#' @return An object of class `infereportR_document`: a list with the
#'   elements `blocks`, `meta`, `options`, `mode`, `figure_modes` and
#'   `bibliography`.
#' @seealso [document_section()], [save_document()], [render_document()],
#'   [figure_deck()], `vignette("slides-and-documents")`
#' @examples
#' doc <- figure_document(list(document_section("Introduction", "Why it matters."),
#'                             document_text("What we found.")),
#'                        title = "Write the main finding as a sentence",
#'                        author = c("A. Author", "Institute"), keywords = "three, keywords")
#' doc
#' @export
figure_document <- function(blocks, title, subtitle = NULL, shorttitle = NULL, author = NULL,
                            institute = NULL, venue = NULL, date = NULL,
                            correspondence = NULL, keywords = NULL, abstract = NULL,
                            mode = c("light", "dark"), palette = NULL, font = NULL,
                            rules = NULL, typography = NULL, emphasis = NULL, figures = NULL,
                            progress = NULL, head = NULL, layout = NULL, notes = NULL,
                            tufte = FALSE, paper = c("a4", "letter"), justified = FALSE,
                            total = FALSE, bibliography = NULL,
                            figure_modes = c("light", "dark", "light-transparent",
                                             "dark-transparent")) {
  if (missing(blocks)) .ir_abort("blocks must be given")
  if (inherits(blocks, "infereportR_block")) blocks <- list(blocks)
  if (!is.list(blocks) || !length(blocks) ||
      !all(vapply(blocks, inherits, logical(1L), "infereportR_block"))) {
    .ir_abort("blocks must be a non-empty list of blocks made by document_section() and friends")
  }
  if (missing(title)) .ir_abort("title must be given")
  .ir_check_string(title, "title")
  meta <- list(title = title, subtitle = subtitle, shorttitle = shorttitle, author = author,
               institute = institute, venue = venue, date = date,
               correspondence = correspondence, keywords = keywords, abstract = abstract)
  for (key in setdiff(names(meta), "title")) .ir_check_text(meta[[key]], key)
  mode <- .ir_check_choice(mode, .ir_modes, "mode")
  paper <- .ir_check_choice(paper, c("a4", "letter"), "paper")
  opts <- .ir_bz_check(list(palette = palette, font = font, rules = rules,
                            typography = typography, emphasis = emphasis, figures = figures,
                            progress = progress, head = head, layout = layout, notes = notes,
                            tufte = tufte, total = total, justified = justified,
                            paper = paper))
  if (!is.null(bibliography)) {
    .ir_check_string(bibliography, "bibliography")
    if (!grepl("[.]bib$", bibliography, ignore.case = TRUE) || !file.exists(bibliography)) {
      .ir_abort("bibliography must be the path to an existing .bib file")
    }
  }
  .ir_check_set_modes(figure_modes, .ir_set_modes)
  blocks <- .ir_name_figures(blocks)
  .ir_check_figure_modes(blocks, mode, figure_modes, opts)
  structure(list(blocks = unname(blocks), meta = meta, options = opts, mode = mode,
                 figure_modes = figure_modes, bibliography = bibliography),
            class = "infereportR_document")
}

.ir_label_tex <- function(label) if (is.null(label)) "" else sprintf("\\label{%s}", label)

# A data.frame as booktabs rows: numbers right-aligned, cells escaped.
.ir_table_tex <- function(x, font) {
  num <- vapply(x, is.numeric, logical(1L))
  cells <- lapply(seq_along(x), function(j) {
    v <- x[[j]]
    if (num[[j]]) {
      out <- format(v, trim = TRUE)
      out[is.na(v)] <- ""
      neg <- !is.na(v) & v < 0
      out <- .ir_tex(out, font)
      out[neg] <- paste0("$-$", sub("^-", "", out[neg]))
      out
    } else {
      v <- as.character(v)
      v[is.na(v)] <- ""
      .ir_tex(v, font)
    }
  })
  spec <- paste0("@{}", paste(ifelse(num, "r", "l"), collapse = ""), "@{}")
  head <- paste(.ir_tex(names(x), font), collapse = " & ")
  rows <- if (nrow(x)) {
    do.call(paste, c(cells, sep = " & "))
  } else {
    character()
  }
  c(sprintf("\\begin{tabular}{%s}", spec), "\\toprule", paste0(head, " \\\\"), "\\midrule",
    if (length(rows)) paste0(rows, " \\\\"), "\\bottomrule", "\\end{tabular}")
}

.ir_paragraphs <- function(text, font) {
  if (is.null(text)) return(character())
  p <- .ir_tex(text, font)
  unlist(lapply(seq_along(p), function(k) c(p[[k]], if (k < length(p)) "")))
}

.ir_block_tex <- function(b, font) {
  esc <- function(x) .ir_tex(x, font)
  switch(b$type,
    section = c(sprintf("\\section{%s}", esc(b$title)), .ir_paragraphs(b$text, font)),
    text = .ir_paragraphs(b$text, font),
    note = if (b$numbered) sprintf("\\footnote{%s}", esc(b$text)) else
      sprintf("\\marginnote{%s}", esc(b$text)),
    newthought = {
      p <- .ir_paragraphs(b$text, font)
      first <- sprintf("\\newthought{%s}%s", esc(b$opening),
                       if (length(p)) paste0(" ", p[[1L]]) else "")
      c(first, p[-1L])
    },
    fullwidth = c("\\begin{fullwidth}", .ir_paragraphs(b$text, font), "\\end{fullwidth}"),
    epigraph = c(sprintf("\\begin{epigraph}%s",
                         if (is.null(b$source)) "" else sprintf("[%s]", esc(b$source))),
                 esc(b$text), "\\end{epigraph}"),
    figure = {
      cmd <- switch(b$layout, side = "sidefigure", half = "sidefigure", full = "fullfigure",
                    margin = "marginfigure")
      src <- if (is.null(b$source)) "" else sprintf("%%\n  [%s]", esc(b$source))
      sprintf("\\%s{%s}{%s%s}%s", cmd, b$name, esc(b$caption), .ir_label_tex(b$label), src)
    },
    table = c("\\begin{table}[tbp]",
              sprintf("\\caption{%s%s}", esc(b$caption), .ir_label_tex(b$label)),
              .ir_table_tex(b$x, font),
              if (!is.null(b$note)) sprintf("\\tablenote{%s}", esc(b$note)),
              "\\end{table}"),
    sparkline = c("\\begin{table}[tbp]",
                  sprintf("\\caption{%s%s}", esc(b$caption), .ir_label_tex(b$label)),
                  sub("^  ", "", .ir_sparkline_table(b$spark, font)),
                  if (!is.null(b$note)) sprintf("\\tablenote{%s}", esc(b$note)),
                  "\\end{table}")
  )
}

# The lines of the .tex file of a document.
.ir_document_tex <- function(doc) {
  eff <- .ir_bz_effective(doc$options)
  font <- eff$font
  opts <- .ir_bz_option_string(doc$options, doc$mode)
  class <- if (length(opts)) {
    sprintf("\\documentclass[%s]{bleisatz}", paste(opts, collapse = ","))
  } else {
    "\\documentclass{bleisatz}"
  }
  engine <- if (font == "c64") "lualatex" else "pdflatex"
  bib <- if (!is.null(doc$bibliography)) basename(doc$bibliography)
  body <- character()
  for (b in doc$blocks) {
    lines <- .ir_block_tex(b, font)
    if (identical(b$type, "note") && length(body)) {
      # A note attaches to the end of the text before it.
      while (length(body) && !nzchar(body[[length(body)]])) body <- body[-length(body)]
      body[[length(body)]] <- paste0(body[[length(body)]], "%")
      body <- c(body, lines, "")
    } else {
      body <- c(body, lines, "")
    }
  }
  meta <- doc$meta
  c(sprintf("%% !TEX program = %s", engine),
    "% A Bleisatz document written by infereportR::save_document(); edit it freely.",
    if (!is.null(bib)) "% latexmk runs biber for the references.",
    if (font == "c64") c("% font=c64 needs LuaLaTeX and C64 Pro Mono (not bundled:",
                         "% https://style64.org/c64-truetype); without it IBM Plex Mono."),
    class,
    "\\usepackage[english]{babel}",
    "\\usepackage{csquotes}",
    if (!is.null(bib)) c("\\usepackage[backend=biber,style=apa]{biblatex}",
                         sprintf("\\addbibresource{%s}", bib)),
    "\\figurefolder{figures}",
    if (font == "c64") c("% Without C64 Pro Mono the theme falls back to font=mono and its",
                         "% \\figurefolder to figures/<mode>/; the C64 set stays in use:",
                         sprintf("\\graphicspath{{%s}}", .ir_bz_folders(eff, doc$figure_modes)$folder(doc$mode))),
    "",
    .ir_title_fields(meta, font, c("title", "subtitle", "shorttitle", "author", "institute",
                                   "venue", "date", "correspondence", "keywords")),
    "",
    "\\begin{document}",
    "\\maketitle",
    "",
    if (!is.null(meta$abstract)) c("\\begin{abstract}", .ir_paragraphs(meta$abstract, font),
                                   "\\end{abstract}", ""),
    body,
    if (!is.null(bib)) c("\\printbibliography", ""),
    "\\end{document}")
}

#' Write a Bleisatz document
#'
#' `save_document()` writes a document made by [figure_document()] into
#' `dir`: every figure, saved with [save_figure_set()] at the size of its
#' slot into the folders of `\figurefolder{figures}`, the bundled class file
#' `bleisatz.cls`, the bibliography (if any) and the `.tex` file. The `.tex`
#' file uses only the commands of the class, so it can be edited and compiled
#' like any other: with [render_document()], `latexmk -pdf document.tex`
#' (which runs biber as well), or on Overleaf.
#'
#' Nothing is written outside `dir`, which has no default and is created if
#' needed. Existing files are not overwritten unless `overwrite = TRUE`: if
#' any file the document would write exists, nothing is written and a
#' condition of class `infereportR_file_exists` is raised.
#'
#' @param doc A document made by [figure_document()].
#' @inheritParams save_deck
#' @return Invisibly, a data.frame with one row per written file and the
#'   columns `file` and `type` (`"tex"`, `"class"`, `"bibliography"`,
#'   `"figure-pdf"` or `"figure-png"`).
#' @seealso [figure_document()], [render_document()], [save_deck()]
#' @examples
#' doc <- figure_document(list(document_section("Results", "C5a peaked late."),
#'                             document_table(data.frame(Stratum = c("ISS 16-24", "ISS 25+"),
#'                                                       Peak = c(20.7, 31.1)),
#'                                            caption = "Median peak (ng/ml).")),
#'                        title = "Complement activation is a clock")
#' dir <- file.path(tempdir(), "document")
#' files <- save_document(doc, dir)
#' files
#' unlink(dir, recursive = TRUE)
#' @export
save_document <- function(doc, dir, file = "document.tex", overwrite = FALSE, png = FALSE) {
  if (!inherits(doc, "infereportR_document")) {
    .ir_abort("doc must be made by figure_document()")
  }
  if (missing(dir)) .ir_abort("dir must be given")
  .ir_check_string(dir, "dir")
  .ir_check_tex_file(file)
  .ir_check_flag(overwrite, "overwrite")
  .ir_check_flag(png, "png")
  folders <- .ir_bz_folders(.ir_bz_effective(doc$options), doc$figure_modes)
  jobs <- .ir_figure_jobs(doc$blocks, .ir_document_presets)
  tex <- .ir_document_tex(doc)
  cls <- file.path(dir, "bleisatz.cls")
  bib <- if (!is.null(doc$bibliography)) file.path(dir, basename(doc$bibliography))
  targets <- c(file.path(dir, file), cls, bib, .ir_figure_targets(jobs, dir, folders, png))
  .ir_check_targets(targets, overwrite)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  figs <- .ir_save_figures(jobs, dir, folders, png)
  file.copy(deck_theme_file("class"), cls, overwrite = TRUE)
  if (!is.null(bib) &&
      normalizePath(doc$bibliography, mustWork = FALSE) != normalizePath(bib, mustWork = FALSE)) {
    file.copy(doc$bibliography, bib, overwrite = TRUE)
  }
  .ir_write_utf8(tex, file.path(dir, file))
  written <- data.frame(file = c(file.path(dir, file), cls, bib),
                        type = c("tex", "class", if (!is.null(bib)) "bibliography"),
                        stringsAsFactors = FALSE)
  invisible(rbind(written, figs))
}
