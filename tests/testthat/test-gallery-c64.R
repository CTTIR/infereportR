# The C64 Tufte set: figure_gallery(set = "tufte", font = "c64"). C64 Pro Mono
# is not bundled, so the tests that draw with it skip without the font; the
# others simulate it with a mocked font lookup.

# Every label of a drawn figure (text grobs, axis labels, titles, strips).
grob_labels <- function(g) {
  out <- character()
  if (is.character(g$label)) out <- c(out, g$label)
  if (inherits(g, "gtable")) for (x in g$grobs) out <- c(out, grob_labels(x))
  for (x in g$children) out <- c(out, grob_labels(x))
  out
}

non_ascii <- function(x) x[grepl("[^\\x01-\\x7f]", x, perl = TRUE)]

test_that("figure_gallery() checks font and draws the C64 set for the Tufte set only", {
  expect_error(figure_gallery(tempdir(), set = "tufte", font = "amiga"),
               class = "infereportR_invalid_input")
  expect_error(figure_gallery(tempdir(), font = "c64"), class = "infereportR_invalid_input")
  expect_identical(nrow(figure_gallery(tempdir(), set = "tufte", font = "c64",
                                       modes = character())), 0L)
})

test_that("the C64 type sets family, sizes and colors and is restored afterwards", {
  local_mocked_bindings(.ir_has_font = function(family) TRUE)
  .ir_with_type("c64", {
    expect_identical(.ir_sans_family(), "C64 Pro Mono")
    expect_identical(.ir_condensed_family(), "C64 Pro Mono")
    expect_equal(.ir_label_pt(), 8 * 0.72)
    th <- theme_figure("slide")
    expect_equal(th$text$size, 9 * 0.72)
    expect_identical(th$text$family, "C64 Pro Mono")
    expect_identical(th$axis.title$family, "C64 Pro Mono")
    # an explicit base_size is kept
    expect_equal(theme_figure("slide", base_size = 9)$text$size, 9)
    light <- .ir_mode("light", palette = "tufte")
    dark <- .ir_mode("dark", palette = "grey")
    expect_identical(c(light$grey, light$red, light$green), c("#959595", "#9F4E44", "#5CAB5E"))
    expect_identical(dark$grey, "#959595")
    expect_identical(dark$red, dark$ink)
    expect_identical(.ir_mode("dark", palette = "tufte")$green, "#9AE29B")
    expect_identical(.ir_tufte_show(c("ISS 16-24", "ISS ≥ 25")),
                     c("ISS 16-24", "ISS >= 25"))
  })
  expect_false(.ir_type_c64())
  expect_identical(.ir_mode("light", palette = "tufte")$red, "#B3202A")
  expect_identical(.ir_tufte_show("ISS 16-24"), "ISS 16–24")
  expect_equal(.ir_label_pt(), 8)
  # restored after an error as well
  expect_error(.ir_with_type("c64", stop("boom")), "boom")
  expect_false(.ir_type_c64())
})

test_that("without C64 Pro Mono the type falls back to the mono family at Plex sizes", {
  local_mocked_bindings(.ir_has_font = function(family) FALSE)
  .ir_with_type("c64", {
    expect_identical(.ir_sans_family(), figure_font("mono"))
    expect_equal(.ir_label_pt(), 8)
    expect_equal(theme_figure("slide")$text$size, 9)
    # the C64 colors stay
    expect_identical(.ir_mode("light", palette = "grey")$grey, "#959595")
  })
})

test_that(".ir_c64_text() types out every character the font lacks", {
  x <- "a ≥ b ≤ c ± d × e − f – g — h · i …"
  expect_identical(.ir_c64_text(x), "a >= b <= c +/- d x e - f - g -- h / i ...")
  expect_identical(.ir_c64_text(character()), character())
})

test_that("every C64 Tufte figure is plain ASCII, one weight and in the C64 palette", {
  skip_on_cran()
  local_mocked_bindings(.ir_has_font = function(family) family == "C64 Pro Mono")
  d <- .ir_gallery_data()
  # the label walk finds the special characters of the Plex set
  plex <- draws_silently(.ir_tufte_build(3, d))
  expect_true(length(non_ascii(grob_labels(plex))) > 0)
  .ir_with_type("c64", {
    for (i in seq_len(nrow(.ir_tufte_captions))) {
      label <- .ir_tufte_captions$name[[i]]
      p <- .ir_tufte_build(i, d, palette = "tufte")
      labs <- grob_labels(draws_silently(p))
      expect_true(length(labs) > 0, label = label)
      expect_identical(non_ascii(labs), character(), label = label)
      th <- .ir_full_theme(p)
      for (el in c("text", "axis.text", "plot.title", "strip.text")) {
        e <- ggplot2::calc_element(el, th)
        if (inherits(e, "element_blank")) next
        expect_identical(e$family, "C64 Pro Mono", label = paste(label, el))
        expect_identical(e$face, "plain", label = paste(label, el))
      }
      expect_identical(check_figure(p, "slide", palette = "tufte")$status[3], "pass",
                       label = label)
    }
    # figures with axes: the y title above the axis, at the label size
    for (i in c(2, 3, 9)) {
      p <- .ir_tufte_build(i, d, palette = "tufte")
      expect_null(p$labels$y)
      expect_true(p$labels$title %in% c("IL-6 (pg/ml)", "C5a (ng/ml)"))
      expect_equal(ggplot2::calc_element("axis.text.x", .ir_full_theme(p))$size, 8 * 0.72)
    }
    # the canvas figures keep their axes blank
    th <- .ir_full_theme(.ir_tufte_build(5, d))
    expect_s3_class(ggplot2::calc_element("axis.text.y.left", th), "element_blank")
    expect_s3_class(ggplot2::calc_element("axis.title.x.bottom", th), "element_blank")
  })
})

test_that("the C64 Tufte set keeps to its palette in every folder", {
  skip_on_cran()
  d <- .ir_gallery_data()
  .ir_with_type("c64", {
    # one folder of each palette, in both modes
    for (folder in c("dark", "light-grey", "dark-1bit")) {
      args <- .ir_set_mode(folder, "tufte")
      allowed <- c(.ir_palette_colours(args$palette, c64 = TRUE), "transparent")
      for (i in seq_len(nrow(.ir_tufte_captions))) {
        b <- ggplot2::ggplot_build(.ir_tufte_build(i, d, mode = args$mode,
                                                   transparent = args$transparent,
                                                   palette = args$palette))
        cols <- unique(unlist(lapply(b$data, function(x) c(x$colour, x$fill))))
        cols <- toupper(cols[!is.na(cols)])
        expect_true(all(cols %in% toupper(allowed)),
                    label = paste(folder, .ir_tufte_captions$name[[i]]))
        if (args$palette == "onebit") expect_true(all(cols %in% c("#000000", "#FFFFFF")))
      }
    }
  })
})

test_that("figure_gallery(font = \"c64\") writes the nine files at slot size", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  skip_if_not(isTRUE(.ir_has_font("C64 Pro Mono")), "C64 Pro Mono is not installed")
  dir <- withr::local_tempdir()
  tab <- figure_gallery(dir, set = "tufte", font = "c64", modes = c("light", "dark-1bit"))
  expect_identical(nrow(tab), 18L)
  expect_true(all(file.exists(tab$file)))
  cap <- gallery_captions("tufte")
  slots <- cap[match(sub("[.]pdf$", "", basename(tab$file)), cap$name), ]
  for (i in seq_len(nrow(tab))) {
    tufte_near(tufte_mediabox_mm(tab$file[i]), c(slots$width_mm[i], slots$height_mm[i]),
               25.4 / 72)
  }
  expect_false(.ir_type_c64())
})

test_that("figure_gallery(font = \"c64\") falls back to the mono family without the font", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  local_mocked_bindings(.ir_has_font = function(family) FALSE)
  dir <- withr::local_tempdir()
  tab <- figure_gallery(dir, set = "tufte", font = "c64", modes = "light-grey")
  expect_identical(nrow(tab), 9L)
  expect_true(all(file.exists(tab$file)))
})

test_that("a C64 Tufte deck draws its figures in the C64 type into figures/c64/tufte", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  seen <- logical()
  # A stand-in figure keeps the test fast; what matters is the type it is built in.
  local_mocked_bindings(.ir_tufte_build = function(i, data, mode = "light", transparent = FALSE,
                                                   palette = "tufte") {
    seen <<- c(seen, .ir_type_c64())
    ggplot2::ggplot() + theme_figure("slide", mode = mode, palette = palette)
  })
  dir <- withr::local_tempdir()
  files <- figure_gallery_deck(dir, set = "tufte", mode = "dark", font = "c64",
                               figure_modes = "dark")
  expect_true(length(seen) > 0)
  expect_true(all(seen))
  expect_false(.ir_type_c64())
  figs <- files$file[files$type == "figure-pdf"]
  expect_true(length(figs) > 0)
  expect_true(all(grepl("figures/c64/tufte/dark/", figs, fixed = TRUE)))
  tex <- readLines(file.path(dir, "tufte-deck.tex"))
  expect_true(any(grepl("font=c64", tex, fixed = TRUE)))
  # a Plex deck does not switch the type
  seen <- logical()
  figure_gallery_deck(withr::local_tempdir(), set = "tufte", figure_modes = "light")
  expect_true(length(seen) > 0)
  expect_false(any(seen))
})
