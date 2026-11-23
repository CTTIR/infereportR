# Helpers of the deck and document tests (test-deck.R, test-document.R,
# test-render.R, test-gallery-deck.R).

deck_plot <- function(mode, transparent) {
  ink <- if (mode == "dark") "white" else "black"
  ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) +
    ggplot2::geom_point(shape = 1, colour = ink) +
    theme_figure("slide", mode = mode, transparent = transparent) +
    ggplot2::labs(x = NULL, y = "Dried weight (g)")
}

# The three-slide deck of the specification (side, half with text, full),
# plus a section, a key figure and an inverse slide.
test_deck <- function(...) {
  figure_deck(list(
    section_slide("Results"),
    figure_slide(deck_plot, "Treatment 2 plants weigh most", caption = "Every plant.",
                 text = c(Rule = "Show the data."), source = "R datasets.", name = "side"),
    figure_slide(deck_plot, "Half slot", layout = "half", caption = "Half caption.",
                 text = "Text beside the figure.", name = "half"),
    figure_slide(deck_plot, "Full slot", layout = "full", caption = "Full caption.",
                 source = "R datasets.", name = "full"),
    keyfigure_slide("One number", "43 %", "of plants", "95 % CI 38 to 48, n = 30"),
    figure_slide(title = "Inverse", name = "side", inverse = TRUE, caption = "Again.")
  ), title = "Test deck", ...)
}

test_document <- function(...) {
  figure_document(list(
    document_section("Introduction", "Why it matters."),
    document_note("A note."),
    document_figure(deck_plot, caption = "Side figure.", label = "fig:side",
                    source = "R datasets.", name = "side"),
    document_figure(deck_plot, layout = "full", caption = "Full figure.", name = "full"),
    document_figure(deck_plot, layout = "half", caption = "Half figure.", name = "half"),
    document_table(data.frame(Group = c("a_b", "c"), n = c(12, -3.5)), caption = "Groups.",
                   note = "A note & more.", label = "tab:groups")
  ), title = "Test document", ...)
}

# A LaTeX installation with the packages the theme and class load.
latex_ready <- function(extra = character()) {
  if (!nzchar(Sys.which("latexmk")) && !nzchar(Sys.which("pdflatex"))) return(FALSE)
  if (!nzchar(Sys.which("kpsewhich"))) return(FALSE)
  need <- c("beamer.cls", "textpos.sty", "plex-sans.sty", "biblatex.sty", "mathastext.sty",
            extra)
  found <- suppressWarnings(system2("kpsewhich", need, stdout = TRUE, stderr = FALSE))
  length(found) == length(need)
}

# Number of pages of a PDF file: page objects, also inside compressed object
# streams (pdfTeX writes those), as in tufte_mediabox_mm().
pdf_pages <- function(file) {
  bytes <- readBin(file, "raw", file.size(file))
  starts <- grepRaw("stream\r?\n", bytes, all = TRUE)
  ends <- grepRaw("endstream", bytes, all = TRUE)
  texts <- rawToChar(bytes[bytes != as.raw(0)])
  for (s in starts) {
    e <- ends[ends > s][1]
    if (is.na(e)) next
    body <- bytes[(s + 7L):(e - 1L)]
    if (length(body) && body[1] == as.raw(10)) body <- body[-1]
    out <- tryCatch(memDecompress(body, type = "gzip"), error = function(err) raw())
    if (length(out)) texts <- c(texts, rawToChar(out[out != as.raw(0)]))
  }
  sum(vapply(texts, function(t) {
    m <- gregexpr("/Type\\s*/Page(?![s])", t, perl = TRUE, useBytes = TRUE)[[1]]
    sum(m > 0)
  }, numeric(1)))
}

# Mocks for .ir_which(): nothing found, or the given tools.
which_none <- function(x) stats::setNames(rep("", length(x)), x)
which_some <- function(found) {
  force(found)
  function(x) stats::setNames(ifelse(x %in% found, paste0("/usr/bin/", x), ""), x)
}

# A fake LaTeX run that writes the PDF (and a log) in the working directory.
fake_run <- function(status = 0L, calls = new.env()) {
  calls$cmd <- character()
  function(cmd, args, quiet = TRUE) {
    calls$cmd <- c(calls$cmd, paste(basename(cmd), paste(args, collapse = " ")))
    tex <- args[grepl("[.]tex$", args)]
    if (length(tex)) {
      stem <- sub("[.]tex$", "", tex)
      writeLines(c("This is a fake log.", "! Fake error."), paste0(stem, ".log"))
      if (status == 0L) writeLines("%PDF-1.5", paste0(stem, ".pdf"))
    }
    list(status = as.integer(status), output = "fake output")
  }
}
