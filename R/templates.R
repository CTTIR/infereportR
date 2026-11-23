# The bundled Bleisatz files and the Quarto extension.

#' The bundled Bleisatz theme and class
#'
#' `deck_theme_file()` returns the path of a file bundled with the package:
#' the Beamer theme `beamerthemeBleisatz.sty` (`type = "beamer"`) or the
#' document class `bleisatz.cls` (`type = "class"`), both Bleisatz version
#' 1.3, unmodified. Copy the file next to a `.tex` file that uses it, as
#' [save_deck()], [save_document()], the R Markdown templates and the Quarto
#' extension do.
#'
#' The two files are distributed under the LaTeX Project Public License
#' (LPPL) 1.3c, not under the MIT licence of the package; their headers carry
#' the notice. See the file `LICENSE.md` of the package source.
#'
#' @param type `"beamer"` (default) or `"class"`.
#' @return The path of the file, a single string.
#' @seealso [save_deck()], [use_deck_quarto()]
#' @examples
#' deck_theme_file()
#' basename(deck_theme_file("class"))
#' head(readLines(deck_theme_file()), 8)
#' @export
deck_theme_file <- function(type = c("beamer", "class")) {
  type <- .ir_check_choice(type, c("beamer", "class"), "type")
  name <- switch(type, beamer = "beamerthemeBleisatz.sty", class = "bleisatz.cls")
  system.file("bleisatz", name, package = "infereportR", mustWork = TRUE)
}

# The files of the Quarto extension, from the package: the extension
# description, the two pandoc templates (shared with the R Markdown
# templates) and the Bleisatz files.
.ir_quarto_files <- function() {
  ext <- system.file("quarto", "bleisatz", package = "infereportR", mustWork = TRUE)
  rmd <- system.file("rmarkdown", "templates", package = "infereportR", mustWork = TRUE)
  c(`_extensions/bleisatz/_extension.yml` = file.path(ext, "_extensions", "bleisatz",
                                                      "_extension.yml"),
    `_extensions/bleisatz/bleisatz-beamer.latex` = file.path(rmd, "bleisatz", "skeleton",
                                                             "bleisatz-beamer.latex"),
    `_extensions/bleisatz/bleisatz-document.latex` = file.path(rmd, "bleisatz-document",
                                                               "skeleton",
                                                               "bleisatz-document.latex"),
    `_extensions/bleisatz/beamerthemeBleisatz.sty` = deck_theme_file("beamer"),
    `_extensions/bleisatz/bleisatz.cls` = deck_theme_file("class"))
}

.ir_use_quarto <- function(dir, template, target, overwrite) {
  if (missing(dir)) .ir_abort("dir must be given")
  .ir_check_string(dir, "dir")
  .ir_check_flag(overwrite, "overwrite")
  files <- c(.ir_quarto_files(),
             stats::setNames(system.file("quarto", "bleisatz", template,
                                         package = "infereportR", mustWork = TRUE), target))
  dest <- file.path(dir, names(files))
  .ir_check_targets(dest, overwrite)
  for (d in unique(dirname(dest))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(unname(files), dest, overwrite = TRUE)
  if (!all(ok)) .ir_abort("could not copy the Quarto extension", code = "unsupported")
  invisible(dest)
}

#' Quarto templates for Bleisatz decks and documents
#'
#' `use_deck_quarto()` and `use_document_quarto()` copy the Quarto extension
#' `bleisatz` into `dir/_extensions/bleisatz/`, with a starting document:
#' `deck.qmd` for a Beamer deck (format `bleisatz-beamer`) or `document.qmd`
#' for a paper, report or handout (format `bleisatz-pdf`). The extension
#' holds the bundled theme and class (see [deck_theme_file()]) and a short
#' pandoc template for each format, because pandoc's default LaTeX template
#' loads fonts, microtype and section settings that clash with the class.
#' Every theme and class option is listed, commented out, in the starting
#' document, as in the Bleisatz templates.
#'
#' Render with `quarto render deck.qmd`; Quarto is not needed to copy the
#' files. Existing files are not overwritten unless `overwrite = TRUE`
#' (condition class `infereportR_file_exists`).
#'
#' @param dir Directory to copy into, created if needed. Required.
#' @param overwrite If `TRUE`, replace existing files.
#' @return Invisibly, the paths of the copied files.
#' @seealso [deck_theme_file()], [figure_deck()], [figure_document()]
#' @examples
#' dir <- file.path(tempdir(), "quarto-deck")
#' files <- use_deck_quarto(dir)
#' basename(files)
#' unlink(dir, recursive = TRUE)
#' @export
use_deck_quarto <- function(dir, overwrite = FALSE) {
  .ir_use_quarto(dir, "template.qmd", "deck.qmd", overwrite)
}

#' @rdname use_deck_quarto
#' @export
use_document_quarto <- function(dir, overwrite = FALSE) {
  .ir_use_quarto(dir, "document.qmd", "document.qmd", overwrite)
}
