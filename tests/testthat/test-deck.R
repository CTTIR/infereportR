p_deck <- ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) +
  ggplot2::geom_point(shape = 1) + theme_figure("slide")

test_that("slide constructors validate their input", {
  expect_error(figure_slide(p_deck), class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, title = 1), class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", layout = "wide"), class = "infereportR_invalid_input")
  expect_error(figure_slide("not a plot", "T"), class = "infereportR_invalid_input")
  expect_error(figure_slide(title = "T"), class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", caption = c("a", "b")),
               class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", text = NA_character_),
               class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", source = 2), class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", name = "a/b"), class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", inverse = NA), class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", layout = "text", text = "x"),
               class = "infereportR_invalid_input")
  expect_error(figure_slide(title = "T", layout = "text"), class = "infereportR_invalid_input")
  expect_error(figure_slide(title = "T", layout = "text", text = "x", caption = "c"),
               class = "infereportR_invalid_input")
  expect_error(figure_slide(p_deck, "T", layout = "full", text = "x"),
               class = "infereportR_invalid_input")
  expect_error(section_slide(), class = "infereportR_invalid_input")
  expect_error(section_slide(c("a", "b")), class = "infereportR_invalid_input")
  expect_error(keyfigure_slide("T", "1", "what"), class = "infereportR_invalid_input")
  expect_error(keyfigure_slide("T", 1, "what", "n = 3"), class = "infereportR_invalid_input")
  expect_error(slide_sparkline(), class = "infereportR_invalid_input")
  expect_error(slide_sparkline("T", list(1)), class = "infereportR_invalid_input")
  expect_error(slide_sparkline("T", list(c(1, 2))), class = "infereportR_invalid_input")
  expect_error(slide_sparkline("T", list(a = c(1, 2)), header = "x"),
               class = "infereportR_invalid_input")
  expect_error(slide_sparkline("T", list(a = c(1, 2)), labels = c("a", "b")),
               class = "infereportR_invalid_input")
  expect_error(slide_sparkline("T", list(a = c(1, 2)), digits = 1:2),
               class = "infereportR_invalid_input")
})

test_that("slides and decks are type-stable", {
  s <- figure_slide(p_deck, "A claim")
  expect_s3_class(s, "infereportR_slide")
  expect_identical(s$layout, "side")
  expect_s3_class(section_slide("S"), "infereportR_slide")
  expect_s3_class(keyfigure_slide("K", "1", "w", "u"), "infereportR_slide")
  sp <- slide_sparkline("Sp", c(1, 2, 3), labels = "x")
  expect_identical(sp$spark$peak, "3.0")
  expect_identical(sp$spark$last, "3.0")
  d <- figure_deck(s, title = "Deck")
  expect_s3_class(d, "infereportR_deck")
  expect_named(d, c("slides", "meta", "options", "mode", "figure_modes"))
  expect_identical(d$slides[[1]]$name, "01-a-claim")
  expect_identical(d$mode, "light")
  expect_identical(d$figure_modes, .ir_set_modes)
  expect_output(print(d), "Bleisatz deck: 1 slides")
  expect_output(print(s), "side slide")
  expect_match(format(section_slide("S")), "section")
})

test_that("figure_deck() validates slides, fields, options and figure modes", {
  s <- figure_slide(p_deck, "A claim")
  expect_error(figure_deck(), class = "infereportR_invalid_input")
  expect_error(figure_deck(list(), title = "D"), class = "infereportR_invalid_input")
  expect_error(figure_deck(list("x"), title = "D"), class = "infereportR_invalid_input")
  expect_error(figure_deck(s), class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", author = 1), class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", mode = "grey"), class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", palette = "red"),
               class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", font = "comic"),
               class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", progress = TRUE),
               class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", head = "no"), class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", tufte = NA), class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", figure_modes = "light-grey"),
               class = "infereportR_invalid_input")
  expect_error(figure_deck(s, title = "D", figure_modes = "dark"),
               class = "infereportR_invalid_input")
  inv <- figure_slide(p_deck, "Inverse", inverse = TRUE)
  expect_error(figure_deck(inv, title = "D", figure_modes = "light"),
               class = "infereportR_invalid_input")
  expect_s3_class(figure_deck(figure_slide(title = "T", layout = "text", text = "x"),
                              title = "D", figure_modes = character()), "infereportR_deck")
})

test_that("figure names are unique, defined once and kept in one slot", {
  a <- figure_slide(p_deck, "A", name = "fig")
  expect_error(figure_deck(list(a, figure_slide(p_deck, "B", name = "fig")), title = "D"),
               class = "infereportR_invalid_input")
  expect_error(figure_deck(list(figure_slide(title = "B", name = "none")), title = "D"),
               class = "infereportR_invalid_input")
  expect_error(figure_deck(list(a, figure_slide(title = "B", name = "fig", layout = "full")),
                           title = "D"), class = "infereportR_invalid_input")
  d <- figure_deck(list(a, figure_slide(title = "B", name = "fig", inverse = TRUE)),
                   title = "D")
  expect_length(.ir_figure_jobs(d$slides, .ir_deck_presets), 1L)
})

test_that("the .tex file uses the theme's commands for every slot", {
  tex <- .ir_deck_tex(test_deck())
  expect_identical(tex[[3]], "\\documentclass[aspectratio=169,11pt]{beamer}")
  expect_true("\\usetheme{Bleisatz}" %in% tex)
  expect_true("\\figurefolder{figures}" %in% tex)
  expect_identical(sum(grepl("^\\\\begin\\{frame\\}", tex)), 6L)
  expect_identical(sum(tex == "\\end{frame}"), 6L)
  expect_true("\\section{Results}" %in% tex)
  expect_true(any(grepl("\\sidefigure[0.64]{\\includegraphics[width=\\linewidth]{side}}",
                        tex, fixed = TRUE)))
  expect_true(any(grepl("\\begin{column}{0.48\\textwidth}", tex, fixed = TRUE)))
  expect_true(any(grepl("\\begin{column}{0.46\\textwidth}", tex, fixed = TRUE)))
  expect_true(any(grepl("\\includegraphics[width=\\textwidth]{full}", tex, fixed = TRUE)))
  expect_true(any(grepl("\\fullcaption{Full caption. R datasets.}", tex, fixed = TRUE)))
  expect_true(any(grepl("\\figlabel", tex, fixed = TRUE)))
  expect_true(any(grepl("\\keyfigure{43 \\%}{of plants}", tex, fixed = TRUE)))
  inv <- which(tex == "\\begin{inverse}")
  expect_length(inv, 1L)
  expect_identical(tex[[inv + 2L]], "  \\graphicspath{{figures/dark/}}")
  expect_true(any(grepl("\\source{R datasets.}", tex, fixed = TRUE)))
  expect_true(any(grepl("\\smalllabel{Rule}", tex, fixed = TRUE)))
  expect_identical(tex[[length(tex)]], "\\end{document}")
})

test_that("theme options, title fields and the dark mode reach the preamble", {
  d <- test_deck(mode = "dark", subtitle = "Sub", author = c("A", "B"), institute = "I",
                 date = "1 May", venue = c("V", "City"), progress = "dots", font = "mono",
                 palette = "grey", rules = "hair", typography = "tufte", emphasis = "italic",
                 figures = "bleisatz", head = FALSE, sectionpages = FALSE, total = TRUE)
  tex <- .ir_deck_tex(d)
  expect_true(paste0("\\usetheme[dark,total,nohead,nosectionpages,progress=dots,font=mono,",
                     "palette=grey,rules=hair,typography=tufte,emphasis=italic,",
                     "figures=bleisatz]{Bleisatz}") %in% tex)
  expect_true(all(c("\\title{Test deck}", "\\subtitle{Sub}", "\\author{A\\\\B}",
                    "\\institute{I}", "\\date{1 May}", "\\venue{V\\\\City}") %in% tex))
  expect_true("  \\graphicspath{{figures/light/}}" %in% tex)
  tex <- .ir_deck_tex(test_deck(tufte = TRUE, palette = "onebit"))
  expect_true("\\usetheme[palette=onebit,tufte]{Bleisatz}" %in% tex)
  expect_true("  \\graphicspath{{figures/tufte/dark-1bit/}}" %in% tex)
  tex <- .ir_deck_tex(test_deck(font = "c64"))
  expect_identical(tex[[1]], "% !TEX program = lualatex")
  expect_true(any(grepl("C64 Pro Mono", tex, fixed = TRUE)))
  expect_true(any(grepl("{\\footnotesize ", tex, fixed = TRUE)))
})

test_that("text, statement and sparkline slides are set as the theme expects", {
  slides <- list(
    figure_slide(title = "One sentence", layout = "text", text = "A paragraph & more."),
    figure_slide(title = "A list", layout = "text", text = c("one", "two")),
    figure_slide(title = "Labels", layout = "text", text = c(light = "ink", dark = "paper")),
    .ir_new_slide("statement", title = "", inverse = TRUE, text = "One sentence."),
    slide_sparkline("Sparks", list(a = c(1, 3, 2)), text = paste("Up to", tex_green("3")),
                    source = "Simulated.")
  )
  tex <- .ir_deck_tex(figure_deck(slides, title = "D"))
  expect_true(all(c("  \\begin{measure}[0.8]", "    A paragraph \\& more.",
                    "    \\item one", "  \\smalllabel{light}\\par\\smallskip",
                    "\\begin{frame}[plain]", "  \\begin{statement}") %in% tex))
  expect_true(any(grepl("\\sparkline[width=5em, range={1,3}, marks=both]{1.0, 3.0, 2.0}",
                        tex, fixed = TRUE)))
  expect_true(any(grepl("& \\ingreen{3.0} & \\inred{2.0} \\\\", tex, fixed = TRUE)))
  expect_true(any(grepl("Up to \\ingreen{3}\\par", tex, fixed = TRUE)))
})

test_that("save_deck() writes the .tex, the theme and every figure at its slot size", {
  skip_if_not(cairo_pdf_works())
  skip_on_cran()
  dir <- withr::local_tempdir()
  files <- save_deck(test_deck(), dir)
  expect_s3_class(files, "data.frame")
  expect_named(files, c("file", "type"))
  modes <- c("light", "dark", "light-transparent", "dark-transparent")
  expected <- c("deck.tex", "beamerthemeBleisatz.sty",
                file.path("figures", rep(modes, 3), paste0(rep(c("side", "half", "full"),
                                                               each = 4), ".pdf")))
  expect_setequal(list.files(dir, recursive = TRUE), expected)
  expect_identical(sort(unique(files$type)), c("figure-pdf", "tex", "theme"))
  expect_identical(unname(tools::md5sum(file.path(dir, "beamerthemeBleisatz.sty"))),
                   unname(tools::md5sum(deck_theme_file())))
  tex <- readLines(file.path(dir, "deck.tex"), encoding = "UTF-8")
  expect_identical(tex, .ir_deck_tex(test_deck()))
  for (slot in c("side", "half", "full")) {
    size <- figure_size(paste0("slide_", slot))
    for (m in modes) {
      mm <- tufte_mediabox_mm(file.path(dir, "figures", m, paste0(slot, ".pdf")))
      expect_lte(abs(mm[[1]] - size$width), 25.4 / 72)
      expect_lte(abs(mm[[2]] - size$height), 25.4 / 72)
    }
  }
  expect_error(save_deck(test_deck(), dir), class = "infereportR_file_exists")
  again <- save_deck(test_deck(figure_modes = c("light", "dark")), dir, overwrite = TRUE,
                     png = TRUE)
  expect_true(all(file.exists(again$file)))
  expect_true("figure-png" %in% again$type)
})

test_that("save_deck() puts Tufte figures where \\figurefolder finds them", {
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  d <- figure_deck(figure_slide(deck_plot, "A claim", name = "a"), title = "D",
                   tufte = TRUE, palette = "grey", figure_modes = "light")
  files <- save_deck(d, dir)
  expect_setequal(list.files(dir, recursive = TRUE),
                  c("deck.tex", "beamerthemeBleisatz.sty",
                    file.path("figures", "tufte", "light-grey", "a.pdf")))
})

test_that("save_deck() checks its arguments before writing", {
  dir <- withr::local_tempdir()
  expect_error(save_deck(list(), dir), class = "infereportR_invalid_input")
  expect_error(save_deck(test_deck()), class = "infereportR_invalid_input")
  expect_error(save_deck(test_deck(), dir, file = "../deck.tex"),
               class = "infereportR_invalid_input")
  expect_error(save_deck(test_deck(), dir, overwrite = NA),
               class = "infereportR_invalid_input")
  expect_error(save_deck(test_deck(), dir, png = "yes"), class = "infereportR_invalid_input")
  expect_length(list.files(dir), 0L)
})

test_that("text slides check their labels and keep I() text verbatim", {
  bad <- stats::setNames(c("a", "b"), c("Rule", NA))
  expect_error(figure_slide(title = "T", layout = "text", text = bad),
               class = "infereportR_invalid_input")
  deck <- figure_deck(list(
    figure_slide(title = "Raw", layout = "text", text = I(c(Rule = "\\textbf{5 %} more"))),
    figure_slide(title = "Escaped", layout = "text", text = c(Rule = "5 % & more"))
  ), title = "D")
  tex <- .ir_deck_tex(deck)
  expect_true("  \\textbf{5 %} more\\par" %in% tex)
  expect_true("  5 \\% \\& more\\par" %in% tex)
  expect_identical(sum(tex == "  \\smalllabel{Rule}\\par\\smallskip"), 2L)
})

test_that("a full slide without a caption keeps its source line", {
  deck <- figure_deck(list(figure_slide(deck_plot, "Full", layout = "full", source = "Data.",
                                        name = "f")), title = "D")
  tex <- .ir_deck_tex(deck)
  at <- grep("\\includegraphics[width=\\textwidth]{f}", tex, fixed = TRUE)
  expect_length(at, 1L)
  expect_identical(tex[[at + 1L]], "  \\source{Data.}")
  expect_identical(tex[[at + 2L]], "\\end{frame}")
})

test_that("save_deck() writes a deck without figures as .tex and theme only", {
  deck <- figure_deck(list(section_slide("Plan"),
                           figure_slide(title = "Aims", layout = "text", text = c("a", "b")),
                           keyfigure_slide("Key", "12", "patients", "of 40")),
                      title = "D")
  dir <- withr::local_tempdir()
  files <- save_deck(deck, dir)
  expect_identical(files$type, c("tex", "theme"))
  expect_false(dir.exists(file.path(dir, "figures")))
  expect_true(all(file.exists(files$file)))
})

test_that("document figures without a name are named after their caption", {
  doc <- figure_document(list(
    document_figure(deck_plot, caption = "Weight by group"),
    document_figure(deck_plot, layout = "full", caption = "Weight again, ≥ 4 g")
  ), title = "D")
  tex <- .ir_document_tex(doc)
  expect_true(any(grepl("^\\\\sidefigure\\{01-weight-by-group\\}", tex)))
  expect_true(any(grepl("^\\\\fullfigure\\{02-weight-again-ge-4-g\\}", tex)))
})
