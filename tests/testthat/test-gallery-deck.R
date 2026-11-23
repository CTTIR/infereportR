gallery_deck_tex <- function(...) {
  data <- .ir_gallery_data()
  .ir_deck_tex(figure_deck(.ir_gallery_slides(data), title = "Twelve figures, one rule each",
                           ...))
}

test_that("the gallery deck has the frames, slots and captions of gallery.tex", {
  tex <- gallery_deck_tex()
  caps <- gallery_captions()
  # title page, 12 figure frames, the modes, the inverse and the reuse frame
  expect_identical(sum(grepl("^\\\\begin\\{frame\\}", tex)), 16L)
  sections <- sub("^\\\\section\\{(.*)\\}$", "\\1", grep("^\\\\section", tex, value = TRUE))
  expect_identical(sections, c("Change over time", "Groups", "Relationships", "Effects",
                               "Character mode", "Modes"))
  for (i in seq_len(nrow(caps))) {
    nm <- caps$name[[i]]
    line <- grep(paste0("{", nm, "}"), tex, fixed = TRUE, value = TRUE)[[1]]
    pattern <- switch(caps$slot[[i]],
      side = "\\sidefigure[0.64]{\\includegraphics[width=\\linewidth]{",
      half = "    \\includegraphics[width=\\linewidth]{",
      full = "\\includegraphics[width=\\textwidth]{")
    expect_true(grepl(pattern, line, fixed = TRUE), info = nm)
    first_words <- .ir_tex(substr(caps$caption[[i]], 1, 30))
    expect_true(any(grepl(first_words, tex, fixed = TRUE)), info = nm)
  }
  frames <- .ir_gallery_frames
  for (t in frames$title) {
    expect_true(any(grepl(paste0("\\begin{frame}{", .ir_tex(t), "}"), tex, fixed = TRUE)),
                info = t)
  }
  # half frames: the figure in the 0.48 column, caption and rule in the 0.46 column
  expect_identical(sum(grepl("\\begin{column}{0.48\\textwidth}", tex, fixed = TRUE)), 4L)
  expect_identical(sum(grepl("\\figlabel", tex, fixed = TRUE)), 5L)
  expect_identical(sum(grepl("\\fullcaption{", tex, fixed = TRUE)), 2L)
  inv <- which(tex == "\\begin{inverse}")
  expect_length(inv, 1L)
  expect_identical(tex[[inv + 2L]], "  \\graphicspath{{figures/dark/}}")
  expect_true(any(grepl("{01-timecourse-direct-labels}", tex[inv:length(tex)], fixed = TRUE)))
  # the source line of every side and half frame
  expect_identical(sum(grepl("\\source{Simulated data (seed 20261117); figures:", tex,
                             fixed = TRUE)), 11L)
})

test_that("a dark gallery deck takes the light figures for the inverse frame", {
  tex <- gallery_deck_tex(mode = "dark", progress = "dots")
  expect_true("\\usetheme[dark,progress=dots]{Bleisatz}" %in% tex)
  expect_true("  \\graphicspath{{figures/light/}}" %in% tex)
})

test_that("the Tufte deck and handout follow the example-tufte and doc-tufte structure", {
  data <- .ir_gallery_data()
  deck <- figure_deck(.ir_tufte_slides(data), title = "T", tufte = TRUE)
  tex <- .ir_deck_tex(deck)
  expect_true("\\usetheme[tufte]{Bleisatz}" %in% tex)
  expect_identical(sum(grepl("^\\\\begin\\{frame\\}", tex)), 12L)
  expect_true(all(c("\\section{Question}", "\\section{Evidence}", "\\begin{inverse}",
                    "  \\begin{statement}", "  \\begin{measure}[0.8]") %in% tex))
  expect_true(any(grepl("\\sparkline[at={0,2,4,6,8,12,24,48}, marks=both]", tex,
                        fixed = TRUE)))
  expect_true(any(grepl("\\ingreen{", tex, fixed = TRUE)))
  expect_true(any(grepl("\\inred{", tex, fixed = TRUE)))
  for (nm in c("19-context-grey", "18-small-multiples", "17-slopegraph", "15-quartile-plot",
               "14-range-frame-dot-dash", "16-bars-white-grid")) {
    expect_true(any(grepl(paste0("{", nm, "}"), tex, fixed = TRUE)), info = nm)
  }
  doc <- figure_document(.ir_tufte_blocks(data), title = "T", tufte = TRUE)
  tex <- .ir_document_tex(doc)
  expect_true("\\documentclass[tufte]{bleisatz}" %in% tex)
  expect_true(all(c("\\begin{epigraph}[Edward R. Tufte]", "\\begin{fullwidth}") %in% tex))
  expect_true(any(grepl("^\\\\newthought\\{Late organ failure\\}", tex)))
  expect_true(any(grepl("^\\\\marginfigure\\{15-quartile-plot-margin\\}", tex)))
  expect_true(any(grepl("\\footnote{", tex, fixed = TRUE)))
  expect_true(any(grepl("\\marginnote{", tex, fixed = TRUE)))
  expect_true(any(grepl("^\\\\sidefigure\\{19-context-grey\\}", tex)))
  expect_true(any(grepl("^\\\\fullfigure\\{18-small-multiples\\}", tex)))
  expect_true(any(grepl("\\ref{fig:context}", tex, fixed = TRUE)))
})

test_that("figure_gallery_deck() checks its arguments", {
  expect_error(figure_gallery_deck(), class = "infereportR_invalid_input")
  dir <- withr::local_tempdir()
  expect_error(figure_gallery_deck(dir, set = "other"), class = "infereportR_invalid_input")
  expect_error(figure_gallery_deck(dir, render = NA), class = "infereportR_invalid_input")
  expect_error(figure_gallery_deck(dir, mode = "grey"), class = "infereportR_invalid_input")
})

test_that("figure_gallery_deck() writes the gallery deck and its twelve figures", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  files <- figure_gallery_deck(dir, figure_modes = c("light", "dark"))
  expect_identical(sum(files$type == "figure-pdf"), 24L)
  expect_true(file.exists(file.path(dir, "gallery.tex")))
  expect_setequal(sub("[.]pdf$", "", list.files(file.path(dir, "figures", "light"))),
                  gallery_captions()$name)
  expect_error(figure_gallery_deck(dir, figure_modes = c("light", "dark")),
               class = "infereportR_file_exists")
})

test_that("figure_gallery_deck(set = \"tufte\") writes the deck and the handout", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  files <- figure_gallery_deck(dir, set = "tufte", figure_modes = "light")
  expect_true(all(file.exists(file.path(dir, c("tufte-deck.tex", "tufte-handout.tex",
                                               "beamerthemeBleisatz.sty",
                                               "bleisatz.cls")))))
  expect_setequal(list.files(file.path(dir, "figures", "tufte", "light")),
                  paste0(c("19-context-grey", "18-small-multiples", "17-slopegraph",
                           "15-quartile-plot", "14-range-frame-dot-dash",
                           "16-bars-white-grid", "15-quartile-plot-margin"), ".pdf"))
  expect_false(anyDuplicated(files$file) > 0)
  expect_error(figure_gallery_deck(dir, set = "tufte", figure_modes = "light"),
               class = "infereportR_file_exists")
  mm <- tufte_mediabox_mm(file.path(dir, "figures", "tufte", "light",
                                    "15-quartile-plot-margin.pdf"))
  expect_lte(max(abs(mm - c(52, 40))), 25.4 / 72)
})

test_that("figure_gallery_deck(render = TRUE) compiles the deck, and the handout of the Tufte set", {
  calls <- character()
  written <- function(dir, file) data.frame(file = file.path(dir, file), type = "tex")
  # The files and their compilation are stand-ins: what is tested is the order
  # of the steps and the PDF paths returned.
  local_mocked_bindings(
    save_deck = function(deck, dir, file = "deck.tex", ...) {
      calls <<- c(calls, paste("save", file))
      written(dir, file)
    },
    save_document = function(doc, dir, file = "document.tex", ...) {
      calls <<- c(calls, paste("save", file))
      written(dir, file)
    },
    render_deck = function(dir, file = "deck.tex", ...) {
      calls <<- c(calls, paste("render", file))
      file.path(dir, sub("tex$", "pdf", file))
    },
    render_document = function(dir, file = "document.tex", ...) {
      calls <<- c(calls, paste("render", file))
      file.path(dir, sub("tex$", "pdf", file))
    })
  dir <- withr::local_tempdir()
  pdf <- figure_gallery_deck(dir, render = TRUE, figure_modes = c("light", "dark"))
  expect_identical(basename(pdf), "gallery.pdf")
  pdfs <- figure_gallery_deck(dir, set = "tufte", render = TRUE, figure_modes = "light")
  expect_identical(basename(pdfs), c("tufte-deck.pdf", "tufte-handout.pdf"))
  expect_identical(calls, c("save gallery.tex", "render gallery.tex", "save tufte-deck.tex",
                            "save tufte-handout.tex", "render tufte-deck.tex",
                            "render tufte-handout.tex"))
})
