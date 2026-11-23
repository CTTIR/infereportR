test_that("block constructors validate their input", {
  expect_error(document_section(), class = "infereportR_invalid_input")
  expect_error(document_section("S", text = 1), class = "infereportR_invalid_input")
  expect_error(document_text(), class = "infereportR_invalid_input")
  expect_error(document_text(NULL), class = "infereportR_invalid_input")
  expect_error(document_figure(deck_plot), class = "infereportR_invalid_input")
  expect_error(document_figure(caption = "C"), class = "infereportR_invalid_input")
  expect_error(document_figure(deck_plot, layout = "wide", caption = "C"),
               class = "infereportR_invalid_input")
  expect_error(document_figure(deck_plot, caption = "C", label = "a b"),
               class = "infereportR_invalid_input")
  expect_error(document_figure(1, caption = "C"), class = "infereportR_invalid_input")
  expect_error(document_table(list(a = 1), caption = "C"), class = "infereportR_invalid_input")
  expect_error(document_table(data.frame(a = 1)), class = "infereportR_invalid_input")
  expect_error(document_table(data.frame(a = 1), "C", note = 1),
               class = "infereportR_invalid_input")
  expect_error(document_note(), class = "infereportR_invalid_input")
  expect_error(document_note(c("a", "b")), class = "infereportR_invalid_input")
  expect_error(document_note("a", numbered = "yes"), class = "infereportR_invalid_input")
  expect_error(document_newthought(), class = "infereportR_invalid_input")
  expect_error(document_fullwidth(), class = "infereportR_invalid_input")
  expect_error(document_epigraph(), class = "infereportR_invalid_input")
  expect_error(document_epigraph("q", source = c("a", "b")),
               class = "infereportR_invalid_input")
  expect_error(document_sparkline(list(a = c(1, 2))), class = "infereportR_invalid_input")
  expect_error(document_sparkline(list(a = 1), caption = "C"),
               class = "infereportR_invalid_input")
})

test_that("figure_document() validates its arguments and is type-stable", {
  b <- document_text("x")
  expect_error(figure_document(), class = "infereportR_invalid_input")
  expect_error(figure_document(list(1), title = "D"), class = "infereportR_invalid_input")
  expect_error(figure_document(b), class = "infereportR_invalid_input")
  expect_error(figure_document(b, title = "D", keywords = 3),
               class = "infereportR_invalid_input")
  expect_error(figure_document(b, title = "D", paper = "a5"),
               class = "infereportR_invalid_input")
  expect_error(figure_document(b, title = "D", layout = "wide"),
               class = "infereportR_invalid_input")
  expect_error(figure_document(b, title = "D", notes = "end"),
               class = "infereportR_invalid_input")
  expect_error(figure_document(b, title = "D", justified = NA),
               class = "infereportR_invalid_input")
  expect_error(figure_document(b, title = "D", bibliography = "none.bib"),
               class = "infereportR_invalid_input")
  d <- figure_document(b, title = "D")
  expect_s3_class(d, "infereportR_document")
  expect_named(d, c("blocks", "meta", "options", "mode", "figure_modes", "bibliography"))
  expect_output(print(d), "Bleisatz document: 1 blocks")
  expect_output(print(b), "Bleisatz block")
  expect_identical(test_document()$blocks[[3]]$name, "side")
  f <- figure_document(document_figure(deck_plot, caption = "A caption"), title = "D")
  expect_identical(f$blocks[[1]]$name, "01-a-caption")
})

test_that("the document .tex uses the class's commands", {
  tex <- .ir_document_tex(test_document())
  expect_true("\\documentclass{bleisatz}" %in% tex)
  expect_true("\\figurefolder{figures}" %in% tex)
  expect_true("\\maketitle" %in% tex)
  expect_true("\\section{Introduction}" %in% tex)
  expect_true("Why it matters.%" %in% tex)
  expect_true("\\footnote{A note.}" %in% tex)
  expect_true("\\sidefigure{side}{Side figure.\\label{fig:side}}%\n  [R datasets.]" %in% tex)
  expect_true("\\fullfigure{full}{Full figure.}" %in% tex)
  expect_true("\\sidefigure{half}{Half figure.}" %in% tex)
  expect_true(all(c("\\begin{table}[tbp]", "\\caption{Groups.\\label{tab:groups}}",
                    "\\begin{tabular}{@{}lr@{}}", "\\toprule", "Group & n \\\\", "\\midrule",
                    "a\\_b & 12.0 \\\\", "c & $-$3.5 \\\\", "\\bottomrule",
                    "\\tablenote{A note \\& more.}") %in% tex))
  expect_false(any(grepl("biblatex", tex)))
})

test_that("class options, front matter and the bibliography reach the preamble", {
  bib <- withr::local_tempfile(fileext = ".bib")
  writeLines("@book{tufte2001, author = {Tufte, Edward R.}, title = {VDQI}, year = {2001}}",
             bib)
  d <- test_document(mode = "dark", progress = "dots", font = "mono", total = TRUE,
                     justified = TRUE, paper = "letter", head = FALSE, layout = "tufte",
                     notes = "side", palette = "tufte", figures = "tufte",
                     subtitle = "S", shorttitle = "Short", author = c("A", "Inst"),
                     institute = "I", venue = "V", date = "D", correspondence = "C",
                     keywords = "k1, k2", abstract = c("P1.", "P2."), bibliography = bib)
  tex <- .ir_document_tex(d)
  expect_true(paste0("\\documentclass[dark,total,nohead,progress=dots,font=mono,",
                     "palette=tufte,figures=tufte,layout=tufte,notes=side,justified,",
                     "letterpaper]{bleisatz}") %in% tex)
  expect_true(all(c("\\usepackage[backend=biber,style=apa]{biblatex}",
                    sprintf("\\addbibresource{%s}", basename(bib)), "\\printbibliography",
                    "\\subtitle{S}", "\\shorttitle{Short}", "\\author{A\\\\Inst}",
                    "\\correspondence{C}", "\\keywords{k1, k2}", "\\begin{abstract}", "P1.",
                    "", "P2.") %in% tex))
  expect_lt(which(tex == "\\printbibliography"), which(tex == "\\end{document}"))
  tex <- .ir_document_tex(test_document(tufte = TRUE))
  expect_true("\\documentclass[tufte]{bleisatz}" %in% tex)
  tex <- .ir_document_tex(test_document(font = "c64"))
  expect_identical(tex[[1]], "% !TEX program = lualatex")
})

test_that("Tufte blocks map to the class's commands", {
  blocks <- list(
    document_note("Opening note.", numbered = FALSE),
    document_newthought("Late organ failure", c("has replaced haemorrhage.", "Second.")),
    document_newthought("Alone"),
    document_figure(deck_plot, layout = "margin", caption = "Margin.", name = "m"),
    document_fullwidth(c("Wide one.", "Wide two.")),
    document_epigraph("Above all else show the data.", source = "Edward R. Tufte"),
    document_epigraph("No source."),
    document_text(paste("C5a rose to", tex_green("20.7"), sparkline_tex(c(9, 20, 9)))),
    document_note("Unnumbered.", numbered = FALSE),
    document_sparkline(stats::setNames(list(c(1, 2, 3)), "ISS \u2265 25"), caption = "Sparks.",
                       note = "Simulated.", label = "tab:sp")
  )
  tex <- .ir_document_tex(figure_document(blocks, title = "D", tufte = TRUE))
  expect_true(all(c("\\marginnote{Opening note.}",
                    "\\newthought{Late organ failure} has replaced haemorrhage.", "Second.",
                    "\\newthought{Alone}", "\\marginfigure{m}{Margin.}",
                    "\\begin{fullwidth}", "Wide one.", "\\end{fullwidth}",
                    "\\begin{epigraph}[Edward R. Tufte]", "\\begin{epigraph}",
                    "\\marginnote{Unnumbered.}", "\\caption{Sparks.\\label{tab:sp}}",
                    "\\tablenote{Simulated.}") %in% tex))
  expect_true(any(grepl("C5a rose to \\ingreen{20.7} \\sparkline{9.0, 20.0, 9.0}%", tex,
                        fixed = TRUE)))
  expect_true(any(grepl("ISS $\\geq$ 25 & \\sparkline[width=4.2em", tex, fixed = TRUE)))
})

test_that("document tables escape cells and handle missing values", {
  x <- data.frame(`a&b` = c("x_1", NA), value = c(NA, -2), check.names = FALSE,
                  stringsAsFactors = FALSE)
  rows <- .ir_table_tex(x, "plex")
  expect_true(all(c("a\\&b & value \\\\", "x\\_1 &  \\\\", " & $-$2 \\\\") %in% rows))
  expect_true("\\begin{tabular}{@{}lr@{}}" %in% .ir_table_tex(x[0, ], "plex"))
  r <- report_table(infercalcR::run_inference(
    infercalcR::method_spec("t_means_two_independent_welch"),
    x = pg_values("trt1"), y = pg_values("ctrl")))
  b <- document_table(r[, c("estimate_display", "interval_display")], caption = "Welch.")
  expect_true(any(grepl("estimate\\_display", .ir_block_tex(b, "plex"), fixed = TRUE)))
})

test_that("save_document() writes the .tex, the class, the bibliography and the figures", {
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  src <- withr::local_tempdir()
  bib <- file.path(src, "refs.bib")
  writeLines("@misc{a, title = {A}, year = {2020}}", bib)
  d1 <- test_document(bibliography = bib, figure_modes = "light")
  files <- save_document(d1, withr::local_tempdir())
  expect_setequal(files$type, c("tex", "class", "bibliography", "figure-pdf"))
  expect_identical(sum(files$type == "figure-pdf"), 3L)
  # Two modes, overwriting and a bibliography already in dir: not on CRAN,
  # where the twelve figure files above take long.
  skip_on_cran()
  d <- test_document(bibliography = bib, figure_modes = c("light", "dark"))
  files <- save_document(d, dir)
  expect_named(files, c("file", "type"))
  expected <- c("document.tex", "bleisatz.cls", "refs.bib",
                file.path("figures", rep(c("light", "dark"), 3),
                          paste0(rep(c("side", "full", "half"), each = 2), ".pdf")))
  expect_setequal(list.files(dir, recursive = TRUE), expected)
  expect_identical(unname(tools::md5sum(file.path(dir, "bleisatz.cls"))),
                   unname(tools::md5sum(deck_theme_file("class"))))
  for (slot in c("side", "half", "full")) {
    size <- figure_size(paste0("slide_", slot))
    mm <- tufte_mediabox_mm(file.path(dir, "figures", "light", paste0(slot, ".pdf")))
    expect_lte(abs(mm[[1]] - size$width), 25.4 / 72)
  }
  expect_error(save_document(d, dir), class = "infereportR_file_exists")
  expect_silent(save_document(d, dir, overwrite = TRUE))
  # A bibliography already in dir is not copied onto itself.
  d2 <- test_document(bibliography = file.path(dir, "refs.bib"), figure_modes = "light")
  expect_silent(save_document(d2, dir, file = "second.tex", overwrite = TRUE))
})

test_that("save_document() writes margin figures at 52 x 40 mm", {
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  d <- figure_document(document_figure(deck_plot, layout = "margin", caption = "M",
                                       name = "m"), title = "D", figure_modes = "light")
  save_document(d, dir)
  mm <- tufte_mediabox_mm(file.path(dir, "figures", "light", "m.pdf"))
  expect_lte(max(abs(mm - c(52, 40))), 25.4 / 72)
})

test_that("save_document() checks its arguments before writing", {
  dir <- withr::local_tempdir()
  expect_error(save_document(list(), dir), class = "infereportR_invalid_input")
  expect_error(save_document(test_document()), class = "infereportR_invalid_input")
  expect_error(save_document(test_document(), dir, file = "doc"),
               class = "infereportR_invalid_input")
  expect_length(list.files(dir), 0L)
})
