write_tex <- function(dir, file, lines) {
  writeLines(lines, file.path(dir, file))
  invisible(file.path(dir, file))
}

test_that("render_deck() raises missing_dependency without LaTeX", {
  dir <- withr::local_tempdir()
  write_tex(dir, "deck.tex", c("\\documentclass{beamer}", "\\usetheme{Bleisatz}"))
  local_mocked_bindings(.ir_which = which_none, .ir_tinytex_available = function() FALSE)
  old <- getwd()
  expect_error(render_deck(dir), class = "infereportR_missing_dependency")
  expect_error(render_document(dir, "deck.tex"), class = "infereportR_missing_dependency")
  expect_identical(getwd(), old)
})

test_that("render_document() needs biber for a bibliography", {
  dir <- withr::local_tempdir()
  write_tex(dir, "document.tex", c("\\documentclass{bleisatz}", "\\addbibresource{r.bib}"))
  local_mocked_bindings(.ir_which = which_some(c("latexmk", "pdflatex")),
                        .ir_tinytex_available = function() FALSE)
  expect_error(render_document(dir), class = "infereportR_missing_dependency")
})

test_that("render_*() check their arguments", {
  dir <- withr::local_tempdir()
  expect_error(render_deck(), class = "infereportR_invalid_input")
  expect_error(render_deck(dir), class = "infereportR_invalid_input")
  write_tex(dir, "deck.tex", "\\documentclass{beamer}")
  expect_error(render_deck(dir, file = "../deck.tex"), class = "infereportR_invalid_input")
  expect_error(render_deck(dir, engine = "context"), class = "infereportR_invalid_input")
  expect_error(render_deck(dir, quiet = NA), class = "infereportR_invalid_input")
})

test_that("render_deck() runs latexmk in dir with the engine of the deck", {
  dir <- withr::local_tempdir()
  write_tex(dir, "deck.tex", c("\\documentclass{beamer}", "\\usetheme[font=c64]{Bleisatz}"))
  calls <- new.env()
  local_mocked_bindings(.ir_which = which_some(c("latexmk", "lualatex", "pdflatex")),
                        .ir_run_tool = fake_run(0L, calls))
  old <- getwd()
  pdf <- render_deck(dir)
  expect_identical(getwd(), old)
  expect_identical(basename(pdf), "deck.pdf")
  expect_true(file.exists(pdf))
  expect_length(calls$cmd, 1L)
  expect_match(calls$cmd, "^latexmk -lualatex -interaction=nonstopmode")
  write_tex(dir, "plain.tex", c("\\documentclass{beamer}", "\\usetheme{Bleisatz}"))
  render_deck(dir, "plain.tex")
  expect_match(calls$cmd[[length(calls$cmd)]], "^latexmk -pdf ")
  render_deck(dir, "plain.tex", engine = "xelatex")
  expect_match(calls$cmd[[length(calls$cmd)]], "^latexmk -xelatex ")
})

test_that("without latexmk the engine runs twice, with biber for a bibliography", {
  dir <- withr::local_tempdir()
  write_tex(dir, "document.tex", c("\\documentclass{bleisatz}", "\\addbibresource{r.bib}"))
  calls <- new.env()
  local_mocked_bindings(.ir_which = which_some(c("pdflatex", "biber")),
                        .ir_run_tool = fake_run(0L, calls))
  render_document(dir)
  expect_identical(sub(" .*", "", calls$cmd), c("pdflatex", "biber", "pdflatex", "pdflatex"))
  write_tex(dir, "deck.tex", c("\\documentclass{beamer}", "\\usetheme{Bleisatz}"))
  calls$cmd <- character()
  render_deck(dir)
  expect_identical(sub(" .*", "", calls$cmd), c("pdflatex", "pdflatex"))
})

test_that("a failed run raises render_failed with the end of the log", {
  dir <- withr::local_tempdir()
  write_tex(dir, "deck.tex", c("\\documentclass{beamer}", "\\usetheme{Bleisatz}"))
  local_mocked_bindings(.ir_which = which_some("latexmk"), .ir_run_tool = fake_run(1L))
  err <- expect_error(render_deck(dir), class = "infereportR_render_failed")
  expect_s3_class(err, "infereportR_error")
  local_mocked_bindings(.ir_which = which_some("pdflatex"))
  expect_error(render_deck(dir), class = "infereportR_render_failed")
  write_tex(dir, "doc.tex", c("\\documentclass{bleisatz}", "\\addbibresource{r.bib}"))
  local_mocked_bindings(.ir_which = which_some(c("pdflatex", "biber")),
                        .ir_run_tool = function(cmd, args, quiet = TRUE) {
                          if (basename(cmd) == "biber") return(list(status = 2L, output = "x"))
                          list(status = 0L, output = character())
                        })
  expect_error(render_document(dir, "doc.tex"), class = "infereportR_render_failed")
  # A run that reports success but writes no PDF also fails.
  expect_error(render_deck(dir), class = "infereportR_render_failed")
})

test_that("TinyTeX is used when there is no LaTeX on the PATH", {
  dir <- withr::local_tempdir()
  write_tex(dir, "deck.tex", c("\\documentclass{beamer}", "\\usetheme{Bleisatz}"))
  seen <- new.env()
  local_mocked_bindings(.ir_which = which_none, .ir_tinytex_available = function() TRUE,
                        .ir_tinytex_latexmk = function(file, engine, bib, quiet) {
                          seen$engine <- engine
                          writeLines("%PDF", sub("tex$", "pdf", file))
                        })
  expect_identical(basename(render_deck(dir)), "deck.pdf")
  expect_identical(seen$engine, "pdflatex")
  local_mocked_bindings(.ir_tinytex_latexmk = function(...) stop("no package"))
  unlink(file.path(dir, "deck.pdf"))
  expect_error(render_deck(dir), class = "infereportR_render_failed")
})

test_that(".ir_run_tool() returns the status and output of a program", {
  res <- .ir_run_tool(file.path(R.home("bin"), "Rscript"), c("-e", shQuote("cat('hi')")))
  expect_identical(res$status, 0L)
  expect_true(any(grepl("hi", res$output)))
  res <- .ir_run_tool(file.path(R.home("bin"), "Rscript"), c("-e", shQuote("quit(status = 3)")))
  expect_identical(res$status, 3L)
})

test_that("render_deck() compiles the test deck with the right page count", {
  skip_on_cran()
  skip_if_not(latex_ready(), "no LaTeX with the Bleisatz packages")
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  save_deck(test_deck(figure_modes = c("light", "dark")), dir)
  pdf <- render_deck(dir)
  expect_true(file.exists(pdf))
  # title, section divider, side, half, full, key figure, inverse
  expect_identical(pdf_pages(pdf), 7)
})

test_that("render_document() compiles the test document with biber", {
  skip_on_cran()
  # The bibliography is set in the biblatex APA style (biblatex-apa).
  skip_if_not(latex_ready(c("apa.bbx", "apa.cbx")), "no LaTeX with the Bleisatz packages")
  skip_if(!nzchar(Sys.which("biber")), "no biber")
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  bib <- file.path(withr::local_tempdir(), "refs.bib")
  writeLines(c("@book{tufte2001, author = {Tufte, Edward R.}, year = {2001},",
               "  title = {The visual display of quantitative information},",
               "  publisher = {Graphics Press}}"), bib)
  d <- figure_document(list(document_text("As shown before \\parencite{tufte2001}."),
                            document_figure(deck_plot, caption = "A figure.", name = "f")),
                       title = "D", bibliography = bib, figure_modes = "light")
  save_document(d, dir)
  pdf <- render_document(dir)
  expect_true(file.exists(pdf))
  expect_gte(pdf_pages(pdf), 1)
})

test_that(".ir_run_tool() captures the output and exit status of a program", {
  # A shell, not Rscript: an R child process would hide these lines from
  # coverage tools that trace the package.
  sh <- Sys.which("sh")
  skip_if(!nzchar(sh), "no sh on the PATH")
  res <- .ir_run_tool(sh, c("-c", shQuote("echo compiled; exit 3")))
  expect_identical(res$status, 3L)
  expect_true("compiled" %in% res$output)
  ok <- .ir_run_tool(sh, c("-c", shQuote("echo done")))
  expect_identical(ok$status, 0L)
  # quiet = FALSE lets the output through and returns the status alone
  loud <- .ir_run_tool(sh, c("-c", shQuote("exit 4")), quiet = FALSE)
  expect_identical(loud$status, 4L)
  expect_identical(loud$output, character())
})

test_that("the LaTeX lookups report what the system has", {
  found <- .ir_which(c("no-such-latex-xyz", "sh"))
  expect_identical(unname(found[["no-such-latex-xyz"]]), "")
  expect_identical(unname(found[["sh"]]), unname(Sys.which("sh")))
  skip_if_not_installed("tinytex")
  local_mocked_bindings(is_tinytex = function(...) TRUE, .package = "tinytex")
  expect_true(.ir_tinytex_available())
  local_mocked_bindings(is_tinytex = function(...) FALSE, .package = "tinytex")
  expect_false(.ir_tinytex_available())
})

test_that("TinyTeX compiles with biber for a bibliography and keeps the aux files", {
  skip_if_not_installed("tinytex")
  seen <- new.env()
  local_mocked_bindings(latexmk = function(file, engine, bib_engine, emulation, clean,
                                           install_packages, ...) {
    seen$args <- list(file = file, engine = engine, bib_engine = bib_engine,
                      emulation = emulation, clean = clean,
                      install_packages = install_packages)
    sub("tex$", "pdf", file)
  }, .package = "tinytex")
  .ir_tinytex_latexmk("document.tex", "lualatex", bib = TRUE, quiet = TRUE)
  expect_identical(seen$args, list(file = "document.tex", engine = "lualatex",
                                   bib_engine = "biber", emulation = TRUE, clean = FALSE,
                                   install_packages = TRUE))
  .ir_tinytex_latexmk("deck.tex", "pdflatex", bib = FALSE, quiet = TRUE)
  expect_identical(seen$args$bib_engine, "bibtex")
  expect_identical(seen$args$engine, "pdflatex")
})
