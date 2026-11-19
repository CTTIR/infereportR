# Slopegraph, sparklines, sparkline_tex(), the palette folders of
# save_figure_set() and the Tufte gallery (figures 13-19).

slope_data <- data.frame(name = c("All", "Young", "Old", "Female", "Male"),
                         before = c(62, 67, 57, 71, 57), after = c(85, 78, 90, 85, 85))

text_layers <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
}

test_that("figure_slopegraph() spaces labels by value, at least gap apart", {
  skip_on_cran()
  p <- figure_slopegraph(slope_data, "name", "before", "after", highlight = "All",
                         titles = c("Before", "After"), note = "score", palette = "tufte")
  b <- ggplot2::ggplot_build(p)
  rng <- diff(range(c(slope_data$before, slope_data$after)))
  for (i in text_layers(p)[1:2]) {
    y <- sort(b$data[[i]]$y)
    expect_true(all(diff(y) >= 0.115 * rng - 1e-9))
    # The mean position is kept.
    v <- if (i == text_layers(p)[1]) slope_data$before else slope_data$after
    expect_equal(mean(y), mean(v))
  }
  left <- b$data[[text_layers(p)[1]]]
  expect_true(all(c("All  62", "Female  71") %in% left$label))
  seg <- b$data[[1]]
  expect_identical(nrow(seg), 5L)
  expect_identical(seg$colour[seg$y == 62], "#B3202A")
  expect_setequal(unique(seg$colour), c("#B3202A", "#999999"))
  expect_true(palette_ok_any(p, "tufte"))
  one <- ggplot2::ggplot_build(figure_slopegraph(slope_data, "name", "before", "after",
                                                 highlight = "All", mode = "dark"))
  expect_identical(unique(one$data[[1]]$colour), "#FFFFFF")
  expect_s3_class(draws_silently(p), "gtable")
  nt <- figure_slopegraph(slope_data, "name", "before", "after", titles = NULL, digits = 1)
  expect_true("All  62.0" %in% ggplot2::ggplot_build(nt)$data[[2]]$label)
})

test_that("figure_slopegraph() checks its arguments", {
  f <- function(...) figure_slopegraph(slope_data, "name", "before", "after", ...)
  expect_error(figure_slopegraph(slope_data[0, ], "name", "before", "after"),
               class = "infereportR_invalid_input")
  expect_error(figure_slopegraph(slope_data, "nm", "before", "after"),
               class = "infereportR_invalid_input")
  expect_error(figure_slopegraph(slope_data, "name", "name", "after"),
               class = "infereportR_invalid_input")
  expect_error(f(titles = "one"), class = "infereportR_invalid_input")
  expect_error(f(note = 1), class = "infereportR_invalid_input")
  expect_error(f(digits = 1.5), class = "infereportR_invalid_input")
  expect_error(f(digits = c(1, 2)), class = "infereportR_invalid_input")
  expect_error(f(gap = -1), class = "infereportR_invalid_input")
  expect_error(f(highlight = list("All")), class = "infereportR_invalid_input")
  expect_error(f(slot = "poster"), class = "infereportR_invalid_input")
  long <- slope_data
  long$name <- strrep("Very long subgroup name ", 3)
  expect_error(figure_slopegraph(long, "name", "before", "after", slot = "margin"),
               class = "infereportR_invalid_input")
  bad <- slope_data
  bad$after[1] <- NA
  expect_error(figure_slopegraph(bad, "name", "before", "after"),
               class = "infereportR_invalid_input")
})

spark_data <- function() {
  d <- expand.grid(x = c(0, 2, 4, 8), row = c("low", "high"), group = c("A", "B"),
                   stringsAsFactors = FALSE)
  d$y <- c(1, 5, 3, 2, 2, 7, 4, 3, 10, 30, 20, 15, 12, 40, 25, 20)
  d$lo <- d$y - 1
  d$hi <- d$y + 1
  d
}

test_that("figure_sparklines() marks and writes the last value and the extremes", {
  skip_on_cran()
  d <- spark_data()
  p <- figure_sparklines(d, "x", "y", "row", group = "group", lower = "lo", upper = "hi",
                         units = c(A = "mg", B = "kg"), digits = c(A = 1, B = 0),
                         marks = "both", extremes = "max", x_unit = " h",
                         titles = c(series = "Median", max = "Peak", last = "Last"),
                         palette = "tufte")
  b <- ggplot2::ggplot_build(p)
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
  expect_identical(geoms, c("GeomPolygon", "GeomPath", "GeomPoint", "GeomText", "GeomSegment"))
  expect_identical(length(unique(b$data[[2]]$group)), 4L)
  marks <- b$data[[3]]
  expect_identical(nrow(marks), 8L)
  expect_setequal(unique(marks$colour), c("#2E7D32", "#B3202A"))
  txt <- b$data[[4]]
  expect_true(all(c("5.0", "7.0", "2.0", "3.0", "30", "40", "15", "20") %in% txt$label))
  expect_true(all(c("2 h", "Peak", "Last", "Median", "mg", "kg") %in% txt$label))
  expect_identical(txt$colour[txt$label == "40"], "#2E7D32")
  expect_identical(txt$colour[txt$label == "20"], "#B3202A")
  expect_identical(unique(b$data[[1]]$fill), "#999999")
  # Rows of one group share a scale: the high row reaches the band top.
  expect_true(palette_ok_any(p, "tufte"))
  expect_s3_class(draws_silently(p), "gtable")
})

test_that("figure_sparklines() stays two-valued in onebit, with a dotted band", {
  skip_on_cran()
  d <- spark_data()
  p <- figure_sparklines(d, "x", "y", "row", group = "group", lower = "lo", upper = "hi",
                         marks = "both", mode = "dark")
  b <- ggplot2::ggplot_build(p)
  expect_identical(p$layers[[1]]$aes_params$linetype, "13")
  expect_identical(length(unique(b$data[[1]]$group)), 8L)
  cols <- unique(unlist(lapply(b$data, function(x) c(x$colour, x$fill))))
  expect_true(all(cols[!is.na(cols)] %in% c("#000000", "#FFFFFF")))
  expect_true(palette_ok_any(p, "onebit"))
  last <- b$data[[3]][b$data[[3]]$shape == 21, ]
  expect_identical(nrow(last), 4L)
  # No group: each row its own scale; minmax marks and numbers.
  q <- figure_sparklines(d[d$group == "A", ], "x", "y", "row", marks = "minmax")
  qb <- ggplot2::ggplot_build(q)
  expect_identical(nrow(qb$data[[2]]), 4L)
  expect_true(all(c("1", "5", "2", "7") %in% qb$data[[3]]$label))
  none <- figure_sparklines(d, "x", "y", "row", group = "group", marks = "none",
                            slot = "slide_half")
  expect_identical(length(none$layers), 2L)
  one <- figure_sparklines(d[d$row == "low" & d$group == "A", ], "x", "y", "row",
                           numbers = "last", extremes = "min", marks = "minmax")
  expect_s3_class(draws_silently(one), "gtable")
})

test_that("figure_sparklines() checks its arguments", {
  d <- spark_data()
  f <- function(...) figure_sparklines(d, "x", "y", "row", ...)
  expect_error(figure_sparklines(d[0, ], "x", "y", "row"), class = "infereportR_invalid_input")
  expect_error(figure_sparklines(d, "row", "y", "row"), class = "infereportR_invalid_input")
  expect_error(f(lower = "lo"), class = "infereportR_invalid_input")
  expect_error(f(marks = "all"), class = "infereportR_invalid_input")
  expect_error(f(numbers = "mean"), class = "infereportR_invalid_input")
  expect_error(f(units = 1), class = "infereportR_invalid_input")
  expect_error(f(x_unit = c("a", "b")), class = "infereportR_invalid_input")
  expect_error(f(titles = c(foo = "x")), class = "infereportR_invalid_input")
  expect_error(f(titles = "x"), class = "infereportR_invalid_input")
  expect_error(f(group = "group", digits = c(A = 1)), class = "infereportR_invalid_input")
  expect_error(f(digits = -1), class = "infereportR_invalid_input")
  d$y <- NA
  expect_error(figure_sparklines(d, "x", "y", "row"), class = "infereportR_invalid_input")
  wide <- spark_data()
  wide$row <- strrep("A very long row label ", 4)
  expect_error(figure_sparklines(wide, "x", "y", "row", slot = "margin"),
               class = "infereportR_invalid_input")
})

test_that("sparkline_tex() writes the theme's sparkline command", {
  x <- c(9.04, 16.9, 20.4, 20.7, 20.1, 17.7, 11.7, 9.4)
  expect_identical(
    sparkline_tex(x, at = c(0, 2, 4, 6, 8, 12, 24, 48), width = "5em", range = c(8, 32),
                  marks = "both"),
    paste0("\\sparkline[at={0,2,4,6,8,12,24,48}, width=5em, range={8,32}, marks=both]",
           "{9.0, 16.9, 20.4, 20.7, 20.1, 17.7, 11.7, 9.4}"))
  expect_identical(sparkline_tex(c(297, 518.4), digits = 0), "\\sparkline{297, 518}")
  expect_identical(sparkline_tex(c(1, 2), height = "1.5ex", band = c(0.5, 1.5)),
                   "\\sparkline[height=1.5ex, band={0.5,1.5}]{1.0, 2.0}")
  expect_error(sparkline_tex(1), class = "infereportR_invalid_input")
  expect_error(sparkline_tex(c(1, NA)), class = "infereportR_invalid_input")
  expect_error(sparkline_tex(1:2, at = c(2, 1)), class = "infereportR_invalid_input")
  expect_error(sparkline_tex(1:2, width = "4 em"), class = "infereportR_invalid_input")
  expect_error(sparkline_tex(1:2, band = c(2, 1)), class = "infereportR_invalid_input")
  expect_error(sparkline_tex(1:2, marks = "all"), class = "infereportR_invalid_input")
  expect_error(sparkline_tex(1:2, digits = 1:2), class = "infereportR_invalid_input")
})

test_that(".ir_format_fixed() keeps the decimals and drops a negative zero", {
  expect_identical(.ir_format_fixed(c(9.04, -0.01, NA, Inf, -Inf), 1),
                   c("9.0", "0.0", "unavailable", "Inf", "-Inf"))
  expect_identical(.ir_format_fixed(2.5, 0, decimal_mark = ","), "2")
  expect_error(.ir_format_fixed(1, 16), class = "infereportR_invalid_input")
  expect_error(.ir_format_fixed("1", 1), class = "infereportR_invalid_input")
})

test_that("save_figure_set() writes the palette folders and passes the palette", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  seen <- character()
  build <- function(mode, transparent, palette) {
    seen <<- c(seen, paste(mode, transparent, palette))
    ggplot2::ggplot(pts_tufte, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      theme_figure(mode = mode, transparent = transparent, palette = palette)
  }
  tab <- save_figure_set(build, "p", preset = "margin", dir = dir, modes = .ir_tufte_modes,
                         png = FALSE, palette = "tufte")
  expect_identical(tab$mode, .ir_tufte_modes)
  expect_true(all(file.exists(file.path(dir, .ir_tufte_modes, "p.pdf"))))
  expect_identical(seen, c("light FALSE tufte", "dark FALSE tufte", "light FALSE grey",
                           "dark FALSE grey", "light FALSE onebit", "dark FALSE onebit"))
  expect_identical(unique(tab$width_mm), 52)
  # A 0.2.0 build function without palette still works.
  old <- function(mode, transparent) {
    ggplot2::ggplot(pts_tufte, ggplot2::aes(x, y)) + ggplot2::geom_point()
  }
  expect_identical(nrow(save_figure_set(old, "q", preset = "margin", dir = dir,
                                        modes = "dark-1bit", png = FALSE)), 1L)
  expect_identical(.ir_set_mode("light-transparent", "grey"),
                   list(mode = "light", transparent = TRUE, palette = "grey"))
  expect_error(save_figure_set(old, "q", preset = "margin", dir = dir, palette = "x"),
               class = "infereportR_invalid_input")
  expect_error(save_figure_set(old, "q", preset = "margin", dir = dir, modes = "dark-2bit"),
               class = "infereportR_invalid_input")
})

test_that("gallery_captions(\"tufte\") lists the nine files of figures 13-19", {
  cap <- gallery_captions("tufte")
  expect_identical(nrow(cap), 9L)
  expect_identical(names(cap), names(gallery_captions()))
  expect_identical(cap$slot[cap$name == "14-range-frame-dot-dash-margin"], "margin")
  expect_identical(unique(cap$height_mm[cap$slot == "margin"]), 40)
  for (i in seq_len(nrow(cap))) {
    preset <- .ir_tufte_presets[[cap$slot[i]]]
    expect_identical(c(cap$width_mm[i], cap$height_mm[i]),
                     c(figure_size(preset)$width, figure_size(preset)$height))
  }
  expect_error(gallery_captions("c64"), class = "infereportR_invalid_input")
  expect_error(figure_gallery(tempdir(), set = "tufte", modes = "neon"),
               class = "infereportR_invalid_input")
  expect_error(figure_gallery(tempdir(), modes = "light-grey"),
               class = "infereportR_invalid_input")
  expect_identical(nrow(figure_gallery(tempdir(), modes = character(), set = "tufte")), 0L)
})

test_that("the Tufte figures reproduce the numbers of the reference", {
  skip_on_cran()
  d <- .ir_gallery_data()
  spark <- ggplot2::ggplot_build(.ir_tufte_13(d))
  txt <- spark$data[[4]]$label
  expect_true(all(c("20.7", "6 h", "9.4", "31.1", "8 h", "10.2", "578", "4 h", "301", "840",
                    "317", "115", "15", "203", "16") %in% txt))
  slope <- ggplot2::ggplot_build(.ir_tufte_17(d))
  left <- slope$data[[2]]$label
  right <- slope$data[[3]]$label
  expect_setequal(left, c("Female  71", "< 50 years  67", "ISS 16–24  63",
                          "All patients  62", "ISS ≥ 25  61", "Male  57",
                          "≥ 50 years  57"))
  expect_setequal(right, c("90  ≥ 50 years", "88  ISS ≥ 25", "85  Female",
                           "85  All patients", "85  Male", "81  ISS 16–24",
                           "78  < 50 years"))
  multiples <- ggplot2::ggplot_build(.ir_tufte_18(d))
  hours <- multiples$data[[4]]$label
  expect_identical(hours[order(multiples$data[[4]]$PANEL)],
                   paste(c(4, 4, 4, 6, 6, 6, 6, 6, 8, 12, 4, 6, 6, 6, 6, 8, 8, 8, 12, 24), "h"))
})

test_that("every Tufte figure keeps to its palette in every folder", {
  skip_on_cran()
  d <- .ir_gallery_data()
  for (folder in .ir_tufte_modes) {
    args <- .ir_set_mode(folder, "tufte")
    allowed <- .ir_palette_colours(args$palette)
    for (i in seq_along(.ir_tufte_builders)) {
      p <- .ir_tufte_builders[[i]](d, mode = args$mode, transparent = args$transparent,
                                   palette = args$palette,
                                   narrow = .ir_tufte_captions$slot[i] == "margin")
      label <- paste(folder, .ir_tufte_captions$name[i])
      expect_true(palette_ok_any(p, args$palette), label = label)
      if (args$palette == "onebit") {
        b <- ggplot2::ggplot_build(p)
        cols <- unique(unlist(lapply(b$data, function(x) c(x$colour, x$fill))))
        cols <- toupper(cols[!is.na(cols)])
        expect_true(all(cols %in% c("#000000", "#FFFFFF")), label = label)
      }
    }
  }
})

test_that("figure_gallery(set = \"tufte\") writes 9 figures x 6 folders at slot size", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  withr::local_seed(11)
  before <- get(".Random.seed", envir = globalenv())
  tab <- figure_gallery(dir, set = "tufte")
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(nrow(tab), 54L)
  expect_identical(unique(tab$mode), .ir_tufte_modes)
  expect_true(all(file.exists(tab$file)))
  cap <- gallery_captions("tufte")
  slots <- cap[match(sub("[.]pdf$", "", basename(tab$file)), cap$name), ]
  for (i in seq_len(nrow(tab))) {
    tufte_near(tufte_mediabox_mm(tab$file[i]), c(slots$width_mm[i], slots$height_mm[i]), 25.4 / 72)
  }
})
