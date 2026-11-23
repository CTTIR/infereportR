# Optional compilation of Bleisatz decks and documents. LaTeX is never
# required: without it render_deck() and render_document() raise a classed
# condition that says what is missing.

# Wrapped so that tests can simulate a system with or without LaTeX.
.ir_which <- function(x) Sys.which(x)

.ir_tinytex_available <- function() {
  requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex())
}

# Run a program in the working directory; output and status, not an error.
.ir_run_tool <- function(cmd, args, quiet = TRUE) {
  out <- suppressWarnings(system2(cmd, args, stdout = if (quiet) TRUE else "",
                                  stderr = if (quiet) TRUE else ""))
  status <- attr(out, "status")
  if (is.null(status)) status <- if (is.numeric(out) && length(out) == 1L) out else 0L
  list(status = as.integer(status), output = if (is.character(out)) out else character())
}

.ir_tinytex_latexmk <- function(file, engine, bib, quiet) {
  tinytex::latexmk(file, engine = engine, bib_engine = if (bib) "biber" else "bibtex",
                   emulation = TRUE, clean = FALSE, install_packages = TRUE)
}

# The tail of the LaTeX log, for the error message.
.ir_log_tail <- function(log, output, n = 20L) {
  lines <- if (file.exists(log)) readLines(log, warn = FALSE) else output
  paste(utils::tail(lines, n), collapse = "\n")
}

.ir_render <- function(dir, file, engine, quiet, what) {
  if (missing(dir)) .ir_abort("dir must be given")
  .ir_check_string(dir, "dir")
  .ir_check_tex_file(file)
  .ir_check_flag(quiet, "quiet")
  path <- file.path(dir, file)
  if (!file.exists(path)) .ir_abort(sprintf("%s does not exist", path))
  tex <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (is.null(engine)) {
    preamble <- tex[grepl("\\\\(usetheme|documentclass)", tex)]
    engine <- if (any(grepl("font=c64", preamble, fixed = TRUE))) "lualatex" else "pdflatex"
  }
  engine <- .ir_check_choice(engine, c("pdflatex", "lualatex", "xelatex"), "engine")
  bib <- any(grepl("\\addbibresource", tex, fixed = TRUE))
  tools <- .ir_which(c("latexmk", engine, "biber"))
  has <- nzchar(tools)
  names(has) <- c("latexmk", "engine", "biber")
  stem <- sub("[.]tex$", "", file)
  pdf <- paste0(stem, ".pdf")
  log <- paste0(stem, ".log")

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  missing_biber <- function() {
    .ir_abort(sprintf(paste("render_%s() needs biber for the bibliography; install it",
                            "with your TeX distribution"), what), code = "missing_dependency")
  }
  failed <- function(step, res) {
    .ir_abort(sprintf("%s failed (status %d). End of the log:\n%s", step, res$status,
                      .ir_log_tail(log, res$output)), code = "render_failed")
  }
  if (has[["latexmk"]] || has[["engine"]]) {
    if (bib && !has[["biber"]]) missing_biber()
    if (has[["latexmk"]]) {
      flag <- switch(engine, pdflatex = "-pdf", lualatex = "-lualatex", xelatex = "-xelatex")
      res <- .ir_run_tool(tools[["latexmk"]], c(flag, "-interaction=nonstopmode", "-halt-on-error",
                                           "-file-line-error", file), quiet)
      if (res$status != 0L) failed("latexmk", res)
    } else {
      run_engine <- function() {
        res <- .ir_run_tool(tools[[engine]], c("-interaction=nonstopmode", "-halt-on-error", file),
                       quiet)
        if (res$status != 0L) failed(engine, res)
      }
      run_engine()
      if (bib) {
        res <- .ir_run_tool(tools[["biber"]], stem, quiet)
        if (res$status != 0L) failed("biber", res)
      }
      run_engine()
      if (bib) run_engine()
    }
  } else if (.ir_tinytex_available()) {
    res <- tryCatch({
      .ir_tinytex_latexmk(file, engine, bib, quiet)
      list(status = 0L, output = character())
    }, error = function(e) list(status = 1L, output = conditionMessage(e)))
    if (res$status != 0L) failed("tinytex::latexmk()", res)
  } else {
    .ir_abort(sprintf(paste("render_%s() needs LaTeX: latexmk or %s on the PATH, or TinyTeX",
                            "(tinytex::install_tinytex()). The .tex file is ready to",
                            "compile elsewhere, for example on Overleaf."), what, engine),
              code = "missing_dependency")
  }
  if (!file.exists(pdf)) {
    failed(engine, list(status = 1L, output = "no PDF was written"))
  }
  normalizePath(pdf)
}

#' Compile a Bleisatz deck or document
#'
#' `render_deck()` and `render_document()` compile a `.tex` file written by
#' [save_deck()] or [save_document()] (or any Bleisatz deck or document) in
#' `dir` and return the path of the PDF. LaTeX is optional and never needed
#' to write a deck or document.
#'
#' They use the first of these that is found:
#'
#' 1. `latexmk` on the PATH (`Sys.which()`), with pdfLaTeX (`-pdf`), or
#'    LuaLaTeX for `font=c64`; latexmk runs the engine as often as needed
#'    and biber for a bibliography;
#' 2. the engine itself (`pdflatex` or `lualatex`), run twice, with biber
#'    and a third run when the document has a bibliography;
#' 3. TinyTeX through [tinytex::latexmk()], when the 'tinytex' package and a
#'    TinyTeX installation are present (missing LaTeX packages are then
#'    installed).
#'
#' Without any of them, or without biber for a document with a
#' bibliography, they raise a condition of class
#' `infereportR_missing_dependency`. When a run fails they raise
#' `infereportR_render_failed`, with the end of the LaTeX log in the
#' message. The run happens in `dir`; the working directory is restored
#' afterwards.
#'
#' @param dir Directory that holds the `.tex` file.
#' @param file File name of the `.tex` file.
#' @param engine `NULL` (default) to choose from the file (`"lualatex"` for
#'   `font=c64`, otherwise `"pdflatex"`), or `"pdflatex"`, `"lualatex"` or
#'   `"xelatex"`.
#' @param quiet If `TRUE` (default), the LaTeX output is captured instead of
#'   shown.
#' @return The normalised path of the PDF file.
#' @seealso [save_deck()], [save_document()]
#' @examples
#' doc <- figure_document(list(document_text("One paragraph.")), title = "A short note")
#' dir <- file.path(tempdir(), "note")
#' save_document(doc, dir)
#' if (nzchar(Sys.which("latexmk")) && interactive()) {
#'   render_document(dir)
#' }
#' unlink(dir, recursive = TRUE)
#' @export
render_deck <- function(dir, file = "deck.tex", engine = NULL, quiet = TRUE) {
  .ir_render(dir, file, engine, quiet, "deck")
}

#' @rdname render_deck
#' @export
render_document <- function(dir, file = "document.tex", engine = NULL, quiet = TRUE) {
  .ir_render(dir, file, engine, quiet, "document")
}
