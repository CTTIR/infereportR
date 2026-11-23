# LaTeX text for the Bleisatz decks and documents: escaping, ASCII file names,
# the palette commands and the theme and class options. Every text field of a
# slide or block is stored as given and escaped only when the .tex file is
# written, because the escaping depends on the font of the deck (C64 decks
# leave >=, <= and x to the theme).

# The ten characters LaTeX treats specially, and what stands for them in text.
.ir_tex_specials <- c(
  "\\" = "\\textbackslash{}", "{" = "\\{", "}" = "\\}", "&" = "\\&", "%" = "\\%",
  "$" = "\\$", "#" = "\\#", "_" = "\\_", "~" = "\\textasciitilde{}",
  "^" = "\\textasciicircum{}"
)

# Characters that pdfLaTeX's UTF-8 input does not set up: the comparison signs,
# the multiplication sign and the minus sign go in as maths. Names are set from
# a character vector (see .ir_cg_ascii_table for why).
.ir_tex_unicode <- stats::setNames(
  c("$\\geq$", "$\\leq$", "$\\times$", "$-$"),
  c("\u2265", "\u2264", "\u00d7", "\u2212")
)

# LaTeX that passes through the escaping unchanged: the palette commands of
# tex_red(), tex_green() and tex_muted(), sparklines from sparkline_tex(), the
# biblatex citation commands and cross-references.
.ir_tex_protected <- paste0(
  "\\\\(?:inred|ingreen|muted)\\{(?:[^{}]|\\{[^{}]*\\})*\\}",
  "|\\\\sparkline(?:\\[[^]]*\\])?\\{[^{}]*\\}",
  "|\\\\(?:paren|text|foot|side|full|auto)?cite[st]?\\*?(?:\\[[^]]*\\])*\\{[^{}]*\\}",
  "|\\\\(?:auto|eq|page)?ref\\{[^{}]*\\}"
)

# Escape the characters of one unprotected piece of text.
.ir_tex_plain <- function(s, font = "plex") {
  if (!nzchar(s)) return(s)
  chars <- strsplit(s, "", fixed = TRUE)[[1L]]
  hit <- match(chars, names(.ir_tex_specials))
  chars[!is.na(hit)] <- .ir_tex_specials[hit[!is.na(hit)]]
  if (font != "c64") {
    uni <- match(chars, names(.ir_tex_unicode))
    chars[!is.na(uni)] <- .ir_tex_unicode[uni[!is.na(uni)]]
  }
  # Line breaks inside a field are spaces; paragraphs are separate elements.
  chars[chars %in% c("\n", "\r", "\t")] <- " "
  paste(chars, collapse = "")
}

# Escape text for LaTeX. Elements of an I() vector go in verbatim; otherwise
# the protected commands pass through and the rest is escaped.
.ir_tex <- function(x, font = "plex") {
  if (is.null(x)) return(character())
  if (inherits(x, "AsIs")) return(enc2utf8(as.character(unclass(x))))
  x <- enc2utf8(as.character(x))
  vapply(x, function(s) {
    if (is.na(s)) return("")
    m <- gregexpr(.ir_tex_protected, s, perl = TRUE)[[1L]]
    if (m[[1L]] == -1L) return(.ir_tex_plain(s, font))
    starts <- as.integer(m)
    ends <- starts + attr(m, "match.length") - 1L
    out <- character()
    pos <- 1L
    for (k in seq_along(starts)) {
      if (starts[[k]] > pos) {
        out <- c(out, .ir_tex_plain(substr(s, pos, starts[[k]] - 1L), font))
      }
      out <- c(out, substr(s, starts[[k]], ends[[k]]))
      pos <- ends[[k]] + 1L
    }
    if (pos <= nchar(s)) out <- c(out, .ir_tex_plain(substr(s, pos, nchar(s)), font))
    paste(out, collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}

# Several lines of one field (author, venue) joined by LaTeX line breaks.
.ir_tex_lines <- function(x, font = "plex") paste(.ir_tex(x, font), collapse = "\\\\")

# ASCII transliteration for file names, the same in every locale (iconv's
# //TRANSLIT gives "?" under the C locale).
.ir_slug_table <- stats::setNames(
  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss", "a", "a", "a", "a", "a", "A", "A", "A",
    "e", "e", "e", "e", "E", "E", "i", "i", "i", "i", "o", "o", "o", "o", "O", "u", "u",
    "u", "c", "C", "n", "N", "ae", "oe", "ge", "le", "x", "u", "u"),
  c("\u00e4", "\u00f6", "\u00fc", "\u00c4", "\u00d6", "\u00dc", "\u00df", "\u00e0",
    "\u00e1", "\u00e2", "\u00e5", "\u00e3", "\u00c0", "\u00c1", "\u00c2", "\u00e8",
    "\u00e9", "\u00ea", "\u00eb", "\u00c8", "\u00c9", "\u00ec", "\u00ed", "\u00ee",
    "\u00ef", "\u00f2", "\u00f3", "\u00f4", "\u00f8", "\u00d8", "\u00f9", "\u00fa",
    "\u00fb", "\u00e7", "\u00c7", "\u00f1", "\u00d1", "\u00e6", "\u0153", "\u2265",
    "\u2264", "\u00d7", "\u00b5", "\u03bc")
)

# A file stem from a title: transliterated, lower case, words joined by "-",
# at most 40 characters.
.ir_slug <- function(x) {
  chars <- strsplit(enc2utf8(x), "", fixed = TRUE)[[1L]]
  hit <- match(chars, names(.ir_slug_table))
  chars[!is.na(hit)] <- .ir_slug_table[hit[!is.na(hit)]]
  keep <- vapply(chars, function(ch) {
    cp <- utf8ToInt(ch)
    length(cp) > 0L && all(!is.na(cp)) && all(cp < 128L)
  }, logical(1L), USE.NAMES = FALSE)
  s <- paste(chars[keep], collapse = "")
  s <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", s)
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("^-+|-+$", "", s)
  if (nchar(s) > 40L) s <- sub("-[^-]*$", "", substr(s, 1L, 41L))
  s <- gsub("^-+|-+$", "", substr(s, 1L, 40L))
  if (!nzchar(s)) "slide" else s
}

.ir_tex_colour <- function(x, command) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    .ir_abort("x must be a single string")
  }
  # Braces become text commands, so the result has only its own braces and
  # passes through the escaping of slide and block text.
  inner <- .ir_tex_plain(enc2utf8(x))
  inner <- gsub("\\{", "\\textbraceleft{}", inner, fixed = TRUE)
  inner <- gsub("\\}", "\\textbraceright{}", inner, fixed = TRUE)
  sprintf("\\%s{%s}", command, inner)
}

#' Palette commands of the Bleisatz theme as LaTeX
#'
#' `tex_red()`, `tex_green()` and `tex_muted()` return the LaTeX of the
#' Bleisatz palette commands `\inred{}`, `\ingreen{}` and `\muted{}` around
#' `x`, which is escaped for LaTeX first. They tie a number in the text to its
#' mark in a figure: under `palette = "tufte"` red and green are Tufte's
#' colours, `\muted{}` is the context grey of `palette = "grey"` and
#' `"tufte"`. Without colour in the palette the theme sets `\inred{}` in
#' italic and `\ingreen{}` in ink, so a text written for the Tufte palette
#' reads correctly in two values.
#'
#' The result can be pasted into any text of [figure_slide()],
#' [document_text()] and the other slide and block constructors: those escape
#' their text, but leave these commands, [sparkline_tex()] output, citation
#' commands such as `\parencite{key}` and `\ref{label}` as they are. It also
#' goes into hand-written LaTeX for a Bleisatz deck or document.
#'
#' @param x A single string, the text to mark.
#' @return A single string of LaTeX.
#' @seealso [figure_slide()], [document_text()], [sparkline_tex()]
#' @examples
#' tex_green("20.7")
#' tex_red("9.4")
#' tex_muted("ng/ml")
#' paste("C5a rose to", tex_green("20.7"), tex_muted("ng/ml"), "at six hours")
#' @export
tex_red <- function(x) .ir_tex_colour(x, "inred")

#' @rdname tex_red
#' @export
tex_green <- function(x) .ir_tex_colour(x, "ingreen")

#' @rdname tex_red
#' @export
tex_muted <- function(x) .ir_tex_colour(x, "muted")

# --- Theme and class options ---------------------------------------------

.ir_bz_values <- list(
  palette = c("onebit", "grey", "tufte"),
  font = c("plex", "mono", "c64", "etbb"),
  rules = c("heavy", "hair"),
  typography = c("bleisatz", "tufte"),
  emphasis = c("inverse", "italic"),
  figures = c("bleisatz", "tufte"),
  progress = c("bar", "dots", "none"),
  layout = c("bleisatz", "tufte"),
  notes = c("foot", "side")
)

# Validate the options of figure_deck() or figure_document(). NULL stays NULL
# (the theme's default); every other value is checked.
.ir_bz_check <- function(opts) {
  for (key in intersect(names(opts), names(.ir_bz_values))) {
    v <- opts[[key]]
    if (is.null(v)) next
    if (!is.character(v) || length(v) != 1L || is.na(v) || !v %in% .ir_bz_values[[key]]) {
      .ir_abort(sprintf("%s must be NULL or one of %s", key,
                        paste0("\"", .ir_bz_values[[key]], "\"", collapse = ", ")))
    }
  }
  for (key in intersect(names(opts), c("head", "sectionpages"))) {
    v <- opts[[key]]
    if (!is.null(v)) .ir_check_flag(v, key)
  }
  for (key in intersect(names(opts), c("tufte", "total", "justified"))) {
    .ir_check_flag(opts[[key]], key)
  }
  opts
}

# The option list of \usetheme or \documentclass: only what was asked for.
.ir_bz_option_string <- function(opts, mode) {
  out <- character()
  if (identical(mode, "dark")) out <- c(out, "dark")
  if (isTRUE(opts$total)) out <- c(out, "total")
  if (!is.null(opts$head)) out <- c(out, if (opts$head) "head" else "nohead")
  if (!is.null(opts$sectionpages)) {
    out <- c(out, if (opts$sectionpages) "sectionpages" else "nosectionpages")
  }
  for (key in c("progress", "font", "palette", "rules", "typography", "emphasis", "figures",
                "layout", "notes")) {
    if (!is.null(opts[[key]])) out <- c(out, paste0(key, "=", opts[[key]]))
  }
  if (isTRUE(opts$justified)) out <- c(out, "justified")
  if (identical(opts$paper, "letter")) out <- c(out, "letterpaper")
  if (isTRUE(opts$tufte)) out <- c(out, "tufte")
  out
}

# The value of an aspect as the theme resolves it: the option when given,
# otherwise Tufte's value under `tufte`, otherwise the Bleisatz default.
.ir_bz_effective <- function(opts) {
  tufte <- isTRUE(opts$tufte)
  pick <- function(key, plain, tuftes) {
    if (!is.null(opts[[key]])) opts[[key]] else if (tufte) tuftes else plain
  }
  list(palette = pick("palette", "onebit", "tufte"), font = pick("font", "plex", "etbb"),
       figures = pick("figures", "bleisatz", "tufte"))
}

# The figure folders of a deck or document, relative to its directory, as
# \figurefolder{figures} finds them: figures/<mode>/, figures/c64/<mode>/,
# or the Tufte set figures[/c64]/tufte/<mode><palette suffix>/.
.ir_bz_folders <- function(eff, figure_modes) {
  base <- if (eff$font == "c64") "figures/c64" else "figures"
  if (eff$figures == "tufte") {
    suffix <- switch(eff$palette, tufte = "", grey = "-grey", onebit = "-1bit")
    modes <- intersect(c("light", "dark"), figure_modes)
    list(dir = paste0(base, "/tufte"), modes = paste0(modes, suffix),
         palette = eff$palette, folder = function(mode) {
           paste0(base, "/tufte/", mode, suffix, "/")
         })
  } else {
    list(dir = base, modes = figure_modes, palette = eff$palette,
         folder = function(mode) paste0(base, "/", mode, "/"))
  }
}

.ir_opposite_mode <- function(mode) if (identical(mode, "dark")) "light" else "dark"

# Optional text fields: NULL or a character vector without NA.
.ir_check_text <- function(x, arg, null = TRUE, single = FALSE) {
  if (is.null(x)) {
    if (null) return(invisible(x))
    .ir_abort(sprintf("%s must be given", arg))
  }
  if (!is.character(x) || !length(x) || anyNA(x) || (single && length(x) != 1L)) {
    .ir_abort(sprintf("%s must be %s", arg,
                      if (single) "a single string" else "a character vector without NA"))
  }
  invisible(x)
}

# A figure: a ggplot, or a function(mode, transparent[, palette]) returning one.
.ir_check_figure_plot <- function(plot, arg = "plot") {
  if (!inherits(plot, "ggplot") && !is.function(plot)) {
    .ir_abort(sprintf("%s must be a ggplot or a function(mode, transparent) returning one",
                      arg))
  }
  invisible(plot)
}

.ir_as_build <- function(plot) {
  if (is.function(plot)) return(plot)
  function(mode, transparent) plot
}

# LaTeX labels: letters, digits and : . - _ only.
.ir_check_label <- function(label) {
  if (is.null(label)) return(invisible(label))
  .ir_check_string(label, "label")
  if (!grepl("^[A-Za-z0-9:._-]+$", label)) {
    .ir_abort("label must use only letters, digits and the characters : . - _")
  }
  invisible(label)
}

# Write lines as UTF-8, whatever the locale.
.ir_write_utf8 <- function(lines, path) {
  writeBin(charToRaw(paste0(paste(enc2utf8(lines), collapse = "\n"), "\n")), path)
  invisible(path)
}

# The file names a save will write, refused if any exists and overwrite is FALSE.
.ir_check_targets <- function(paths, overwrite) {
  if (!overwrite && any(file.exists(paths))) {
    .ir_abort(sprintf("refusing to overwrite existing files (use overwrite = TRUE): %s",
                      paste(utils::head(paths[file.exists(paths)], 5L), collapse = ", ")),
              code = "file_exists")
  }
  invisible(paths)
}

.ir_check_tex_file <- function(file) {
  .ir_check_base_name_ext(file, "file", "tex")
}

# A plain file name with the given extension, inside its folder.
.ir_check_base_name_ext <- function(x, arg, ext) {
  .ir_check_string(x, arg)
  if (grepl("[/\\\\]", x) || startsWith(x, "~") || startsWith(x, ".") ||
      !grepl(paste0("^[A-Za-z0-9._-]+[.]", ext, "$"), x)) {
    .ir_abort(sprintf("%s must be a file name ending in .%s, without a directory part",
                      arg, ext))
  }
  invisible(x)
}
