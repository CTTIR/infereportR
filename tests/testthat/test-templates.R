test_that("deck_theme_file() returns the bundled Bleisatz 1.3 files", {
  sty <- deck_theme_file()
  cls <- deck_theme_file("class")
  expect_identical(basename(sty), "beamerthemeBleisatz.sty")
  expect_identical(basename(cls), "bleisatz.cls")
  for (f in c(sty, cls)) {
    head <- readLines(f, n = 12L, encoding = "UTF-8")
    expect_true(any(grepl("Version 1.3", head, fixed = TRUE)))
    expect_true(any(grepl("LPPL 1.3c", head, fixed = TRUE)))
  }
  expect_true(any(grepl("\\ProvidesPackage{beamerthemeBleisatz}", readLines(sty),
                        fixed = TRUE)))
  expect_true(any(grepl("\\ProvidesClass{bleisatz}", readLines(cls), fixed = TRUE)))
  expect_error(deck_theme_file("pandoc"), class = "infereportR_invalid_input")
})

theme_options <- c("dark", "progress=dots", "font=mono", "total", "nohead", "palette=grey",
                   "rules=hair", "typography=tufte", "emphasis=italic", "figures=tufte",
                   "tufte")

test_that("the R Markdown templates draft the expected files", {
  skip_if_not_installed("rmarkdown")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  rmarkdown::draft("talk.Rmd", template = "bleisatz", package = "infereportR", edit = FALSE)
  expect_setequal(list.files("talk"), c("talk.Rmd", "bleisatz-beamer.latex"))
  rmd <- readLines(file.path("talk", "talk.Rmd"))
  yaml <- rmarkdown::yaml_front_matter(file.path("talk", "talk.Rmd"))
  out <- yaml$output[["rmarkdown::beamer_presentation"]]
  expect_identical(out$template, "bleisatz-beamer.latex")
  expect_identical(out$theme, "Bleisatz")
  expect_identical(yaml$aspectratio, 169L)
  expect_identical(yaml$fontsize, "11pt")
  for (o in c(theme_options, "nosectionpages")) {
    expect_true(any(grepl(paste0("# - ", o, " "), rmd, fixed = TRUE)), info = o)
  }
  expect_true(any(grepl("deck_theme_file(\"beamer\")", rmd, fixed = TRUE)))
  expect_true(any(grepl("figure_size(preset)", rmd, fixed = TRUE)))
  for (slot in c("\\sidefigure[0.64]", "{0.48\\textwidth}", "[width=\\textwidth]")) {
    expect_true(any(grepl(slot, rmd, fixed = TRUE)), info = slot)
  }

  rmarkdown::draft("paper.Rmd", template = "bleisatz-document", package = "infereportR",
                   edit = FALSE)
  expect_setequal(list.files("paper"), c("paper.Rmd", "bleisatz-document.latex"))
  yaml <- rmarkdown::yaml_front_matter(file.path("paper", "paper.Rmd"))
  expect_identical(yaml$documentclass, "bleisatz")
  expect_identical(yaml$`header-includes`, "\\figurefolder{figures}")
  expect_identical(yaml$output[["rmarkdown::pdf_document"]]$template,
                   "bleisatz-document.latex")
  rmd <- readLines(file.path("paper", "paper.Rmd"))
  for (o in c(theme_options, "justified", "letterpaper", "layout=tufte", "notes=side")) {
    expect_true(any(grepl(paste0("# - ", o, " "), rmd, fixed = TRUE)), info = o)
  }
  expect_true(any(grepl("citation_package: biblatex", rmd, fixed = TRUE)))
})

test_that("the pandoc templates set up the theme and class only", {
  beamer <- readLines(system.file("rmarkdown", "templates", "bleisatz", "skeleton",
                                  "bleisatz-beamer.latex", package = "infereportR"))
  expect_true(any(grepl("\\usetheme", beamer, fixed = TRUE)))
  expect_true(any(grepl("\\venue{", beamer, fixed = TRUE)))
  expect_false(any(grepl("lmodern|microtype|AtBeginSection", beamer[!startsWith(beamer, "%")])))
  doc <- readLines(system.file("rmarkdown", "templates", "bleisatz-document", "skeleton",
                               "bleisatz-document.latex", package = "infereportR"))
  expect_true(any(grepl("style=$if(biblio-style)$$biblio-style$$else$apa$endif$", doc,
                        fixed = TRUE)))
  expect_true(any(grepl("\\printbibliography", doc, fixed = TRUE)))
  expect_false(any(grepl("lmodern|microtype|geometry|secnumdepth", doc[!startsWith(doc, "%")])))
})

test_that("use_deck_quarto() and use_document_quarto() copy the extension", {
  dir <- withr::local_tempdir()
  files <- use_deck_quarto(dir)
  ext <- file.path("_extensions", "bleisatz")
  expect_setequal(list.files(dir, recursive = TRUE, all.files = TRUE),
                  c(file.path(ext, c("_extension.yml", "bleisatz-beamer.latex",
                                     "bleisatz-document.latex", "beamerthemeBleisatz.sty",
                                     "bleisatz.cls")), "deck.qmd"))
  expect_true(all(file.exists(files)))
  expect_identical(unname(tools::md5sum(file.path(dir, ext, "bleisatz.cls"))),
                   unname(tools::md5sum(deck_theme_file("class"))))
  expect_error(use_deck_quarto(dir), class = "infereportR_file_exists")
  expect_silent(use_deck_quarto(dir, overwrite = TRUE))
  use_document_quarto(dir, overwrite = TRUE)
  expect_true(file.exists(file.path(dir, "document.qmd")))
  expect_error(use_deck_quarto(), class = "infereportR_invalid_input")
  expect_error(use_document_quarto(dir, overwrite = "no"), class = "infereportR_invalid_input")

  skip_if_not_installed("rmarkdown")
  y <- rmarkdown::yaml_front_matter(file.path(dir, "deck.qmd"))
  expect_named(y$format, "bleisatz-beamer")
  y <- rmarkdown::yaml_front_matter(file.path(dir, "document.qmd"))
  expect_named(y$format, "bleisatz-pdf")
  for (o in c(theme_options, "layout=tufte", "notes=side")) {
    expect_true(any(grepl(paste0("# - ", o, " "), readLines(file.path(dir, "document.qmd")),
                          fixed = TRUE)), info = o)
  }
  wrapped <- withr::local_tempfile(fileext = ".md")
  writeLines(c("---", readLines(file.path(dir, ext, "_extension.yml")), "---"), wrapped)
  e <- rmarkdown::yaml_front_matter(wrapped)
  expect_identical(e$title, "Bleisatz")
  expect_setequal(names(e$contributes$formats), c("beamer", "pdf"))
  expect_identical(e$contributes$formats$beamer$theme, "Bleisatz")
  expect_identical(e$contributes$formats$beamer$`format-resources`, "beamerthemeBleisatz.sty")
  expect_identical(e$contributes$formats$pdf$documentclass, "bleisatz")
  expect_identical(e$contributes$formats$pdf$`format-resources`, "bleisatz.cls")
  expect_identical(e$contributes$formats$pdf$template, "bleisatz-document.latex")
})
