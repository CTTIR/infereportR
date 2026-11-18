# Modes across the package (SPEC amendment 01, A2 and D14).

all_modes <- list(
  light = list(mode = "light", transparent = FALSE),
  dark = list(mode = "dark", transparent = FALSE),
  "light-transparent" = list(mode = "light", transparent = TRUE),
  "dark-transparent" = list(mode = "dark", transparent = TRUE)
)

hex <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(character())
  rgba <- grDevices::col2rgb(v, alpha = TRUE)
  out <- grDevices::rgb(rgba[1, ], rgba[2, ], rgba[3, ], maxColorValue = 255)
  out[rgba[4, ] == 0] <- NA_character_
  unique(out)
}

# Atomic colour values of a built column; patterns are returned separately.
colour_values <- function(x) {
  if (is.list(x)) {
    x <- x[!vapply(x, function(v) inherits(v, "GridPattern"), logical(1))]
    x <- unlist(x, use.names = FALSE)
  }
  hex(x)
}

# The grob tree of one pattern tile (grid keeps it behind a function).
tile_kids <- function(pat) get("grob", envir = environment(pat$f))$children

patterns_of <- function(b) {
  out <- list()
  for (d in b$data) {
    if (is.list(d$fill)) {
      out <- c(out, Filter(function(v) inherits(v, "GridPattern"), d$fill))
    }
  }
  out
}

# Every built colour is the mode's ink (lines, text, marks) or, for fills,
# ink or the mode's background; NA everywhere is allowed.
expect_mode_colours <- function(p, m) {
  b <- ggplot2::ggplot_build(p)
  for (d in b$data) {
    if (!is.null(d$colour)) {
      expect_true(all(colour_values(d$colour) %in% c(m$ink, NA)))
    }
    if (!is.null(d$fill)) {
      expect_true(all(colour_values(d$fill) %in% c(m$ink, m$knock, NA)))
    }
  }
  invisible(b)
}

two_values <- function(p) {
  cf <- check_figure(p)
  cf$status[cf$check == "two_values"]
}

line_plot <- function(mode, transparent) {
  d <- data.frame(time = rep(1:5, 2), score = c(1, 2, 3, 3.5, 4, 1.2, 2.2, 3.1, 3.4, 4),
                  arm = rep(c("Treatment", "Control"), each = 5))
  ggplot2::ggplot(d, ggplot2::aes(time, score, colour = arm, linetype = arm, label = arm)) +
    ggplot2::geom_line(show.legend = FALSE) +
    geom_endlabel(mode = mode, transparent = transparent) +
    scale_colour_ink(mode = mode) +
    scale_linetype_figure() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Time (days)", y = "Score (points)") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent)
}

hatch_plot <- function(mode, transparent) {
  ink <- .ip_mode(mode)$ink
  d <- data.frame(group = factor(rep(c("A", "B", "C"), each = 3)),
                  outcome = factor(rep(c("None", "Single", "Multiple"), 3),
                                   levels = c("None", "Single", "Multiple")),
                  n = c(52, 9, 3, 47, 15, 9, 24, 17, 16))
  ggplot2::ggplot(d, ggplot2::aes(group, n, fill = outcome)) +
    ggplot2::geom_col(position = "fill", colour = ink, width = 0.62) +
    stat_stacklabel(mode = mode, transparent = transparent) +
    scale_fill_hatch(patterns = c("open", "hatch", "solid"), mode = mode,
                     transparent = transparent) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Group", y = "Patients (%)") +
    theme_figure("slide", legend = "none", mode = mode, transparent = transparent)
}

compare_plot <- function(mode, transparent) {
  ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) +
    inferplot("slide", compare = welch, reference = "ctrl", adjust = "holm",
              omnibus = "anova_one_way", mode = mode, transparent = transparent) +
    ggplot2::labs(x = "Group", y = "Dried weight (g)")
}

forest_plot <- function(mode, transparent) {
  spec <- infercalcR::method_spec(welch)
  res <- list(
    "All" = infercalcR::run_inference(spec, x = pg_values("trt2"), y = pg_values("ctrl")),
    "trt1" = infercalcR::run_inference(spec, x = pg_values("trt1"), y = pg_values("ctrl"))
  )
  figure_forest(res, xlab = "Difference (g)", mode = mode, transparent = transparent)
}

family_plots <- list(line = line_plot, hatch = hatch_plot, compare = compare_plot,
                     forest = forest_plot)

text_layers <- function(fig) {
  which(vapply(fig$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
}

cf_source <- function() {
  d <- data.frame(hour = c(0, 2, 4, 8, 12, 24), level = c(9, 14, 19, 17, 13, 10))
  ggplot2::ggplot(d, ggplot2::aes(hour, level)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Level", x = "Hours", y = "Level")
}

# Resolver ---------------------------------------------------------------------

test_that("the resolver gives the A2 values for the four modes", {
  expect_identical(.ip_mode("light"), list(ink = "#000000", paper = "#FFFFFF",
                                           knock = "#FFFFFF"))
  expect_identical(.ip_mode("dark"), list(ink = "#FFFFFF", paper = "#000000",
                                          knock = "#000000"))
  expect_identical(.ip_mode("light", TRUE), list(ink = "#000000", paper = "#FFFFFF",
                                                 knock = NA_character_))
  expect_identical(.ip_mode("dark", TRUE), list(ink = "#FFFFFF", paper = "#000000",
                                                knock = NA_character_))
})

test_that("only R/modes.R compares against a mode", {
  dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(dir), "package sources not available")
  files <- setdiff(list.files(dir, pattern = "[.][Rr]$", full.names = TRUE),
                   file.path(dir, "modes.R"))
  branch <- "(==|!=)\\s*[\"']dark[\"']|[\"']dark[\"']\\s*(==|!=)|dark\\s*="
  hits <- character()
  for (f in files) {
    code <- readLines(f, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    if (any(grepl(branch, code))) hits <- c(hits, basename(f))
  }
  expect_identical(hits, character())
})

# Theme ------------------------------------------------------------------------

test_that("theme backgrounds follow the mode", {
  for (nm in names(all_modes)) {
    a <- all_modes[[nm]]
    m <- .ip_mode(a$mode, a$transparent)
    th <- theme_figure(mode = a$mode, transparent = a$transparent)
    for (el in c("plot.background", "panel.background", "legend.background")) {
      fill <- ggplot2::calc_element(el, th)$fill
      if (a$transparent) expect_true(is.na(fill)) else expect_identical(fill, m$paper)
    }
    for (el in c("axis.line.x.bottom", "axis.ticks.y.left", "axis.text.x.bottom",
                 "axis.title.y.left", "legend.text", "plot.title")) {
      expect_identical(ggplot2::calc_element(el, th)$colour, m$ink)
    }
  }
  dark <- theme_figure(mode = "dark")
  expect_identical(ggplot2::calc_element("plot.background", dark)$fill, "#000000")
  expect_identical(ggplot2::calc_element("panel.background", dark)$fill, "#000000")
  expect_identical(ggplot2::calc_element("legend.background", dark)$fill, "#000000")
  expect_error(theme_figure(mode = "grey"), class = "inferplotR_invalid_input")
  expect_error(theme_figure(transparent = NA), class = "inferplotR_invalid_input")
})

test_that("the geom element carries the mode's ink and background", {
  for (nm in names(all_modes)) {
    a <- all_modes[[nm]]
    m <- .ip_mode(a$mode, a$transparent)
    el <- ggplot2::calc_element("geom", theme_figure(mode = a$mode, transparent = a$transparent))
    skip_if(is.null(el), "ggplot2 has no geom theme element")
    expect_identical(el@ink, m$ink)
    expect_identical(el@accent, m$ink)
    expect_identical(el@paper, m$knock)
  }
})

# Hatch -----------------------------------------------------------------------

test_that("hatch patterns follow the mode and lose their background when transparent", {
  for (nm in names(all_modes)) {
    a <- all_modes[[nm]]
    m <- .ip_mode(a$mode, a$transparent)
    expect_identical(hatch_pattern("solid", mode = a$mode, transparent = a$transparent), m$ink)
    expect_identical(hatch_pattern("open", mode = a$mode, transparent = a$transparent), m$knock)
    for (type in c("hatch", "dots", "crosshatch", "vertical")) {
      pat <- hatch_pattern(type, mode = a$mode, transparent = a$transparent)
      kids <- tile_kids(pat)
      if (a$transparent) {
        expect_s3_class(kids[[1]], "null")
      } else {
        expect_identical(kids[[1]]$gp$fill, m$paper)
      }
      marks <- kids[[2]]$gp
      expect_identical(if (type == "dots") marks$fill else marks$col, m$ink)
    }
  }
  expect_error(hatch_pattern("hatch", mode = "sepia"), class = "inferplotR_invalid_input")
  expect_error(scale_fill_hatch(transparent = "yes"), class = "inferplotR_invalid_input")
})

# Built data per family ------------------------------------------------------

test_that("every family draws only the mode's ink and background", {
  for (fam in names(family_plots)) {
    for (nm in names(all_modes)) {
      a <- all_modes[[nm]]
      m <- .ip_mode(a$mode, a$transparent)
      p <- family_plots[[fam]](a$mode, a$transparent)
      b <- expect_mode_colours(p, m)
      for (pat in patterns_of(b)) {
        first <- tile_kids(pat)[[1]]
        if (a$transparent) expect_s3_class(first, "null") else expect_false(inherits(first, "null"))
      }
      expect_identical(two_values(p), "pass")
      expect_no_warning(draws_silently(p))
    }
  }
})

test_that("inferplot passes the mode to every component", {
  p <- compare_plot("dark", TRUE)
  b <- ggplot2::ggplot_build(p)
  for (d in b$data) expect_identical(unique(hex(d$colour)), "#FFFFFF")
  expect_true(is.na(ggplot2::calc_element("plot.background", p$theme)$fill))
  q <- ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) +
    inferplot(correlation = "correlation_pearson", means = FALSE, points = "plain",
              mode = "dark")
  for (d in ggplot2::ggplot_build(q)$data) expect_identical(unique(hex(d$colour)), "#FFFFFF")
  fit <- ggplot2::ggplot(mt, ggplot2::aes(wt, mpg)) +
    stat_correlation(method = "correlation_pearson", fit = TRUE, mode = "dark")
  expect_identical(unique(hex(layer_data_of(fit, 1)$colour)), "#FFFFFF")
  expect_identical(unique(hex(layer_data_of(fit, 2)$colour)), "#FFFFFF")
  expect_error(inferplot(mode = "dark", transparent = 1), class = "inferplotR_invalid_input")
})

test_that("every layer accepts mode and transparent", {
  m <- .ip_mode("dark")
  layers <- list(
    stat_means(mode = "dark", transparent = TRUE),
    stat_compare(method = welch, reference = "ctrl", adjust = "holm", mode = "dark",
                 transparent = TRUE),
    stat_omnibus(method = "anova_one_way", mode = "dark", transparent = TRUE)
  )
  for (l in layers) {
    p <- ggplot2::ggplot(PlantGrowth, ggplot2::aes(group, weight)) +
      ggplot2::geom_point(colour = m$ink) + l
    expect_identical(unique(hex(layer_data_of(p, 2)$colour)), m$ink)
  }
  expect_error(stat_means(mode = "grey"), class = "inferplotR_invalid_input")
  expect_error(stat_paired(method = "t_means_paired", transparent = NA),
               class = "inferplotR_invalid_input")
  expect_error(stat_ecdf_guides(transparent = "no"), class = "inferplotR_invalid_input")
  expect_error(geom_endlabel(mode = "dim"), class = "inferplotR_invalid_input")
  expect_identical(scale_colour_ink(mode = "dark")$palette(2), rep("#FFFFFF", 2))
})

# Character figures -------------------------------------------------------------

test_that("char_figure bw follows the mode", {
  for (nm in names(all_modes)) {
    a <- all_modes[[nm]]
    m <- .ip_mode(a$mode, a$transparent)
    fig <- char_figure(cf_source(), mode = a$mode, transparent = a$transparent)
    expect_mode_colours(fig, m)
    th <- fig$theme
    expect_identical(th$plot.background$fill, m$knock)
    expect_identical(th$panel.background$fill, m$knock)
    expect_identical(two_values(fig), "pass")
  }
})

test_that("char_figure c64 keeps its colours, and only its glyphs when transparent", {
  c64 <- unname(.ip_c64)
  for (nm in names(all_modes)) {
    a <- all_modes[[nm]]
    fig <- char_figure(cf_source(), style = "c64", mode = a$mode, transparent = a$transparent)
    b <- ggplot2::ggplot_build(fig)
    used <- unique(unlist(lapply(b$data, function(d) c(colour_values(d$colour),
                                                       colour_values(d$fill)))))
    backgrounds <- c(fig$theme$plot.background$fill, fig$theme$panel.background$fill)
    if (a$transparent) {
      expect_identical(used, "#6C5EB5")
      expect_true(all(is.na(backgrounds)))
    } else {
      expect_setequal(c(used, backgrounds), c64)
      expect_identical(backgrounds, c("#6C5EB5", "#352879"))
    }
    # c64 is the documented exception of the two-values rule.
    expect_identical(two_values(fig), "fail")
  }
})

test_that("char_figure c64bw keeps its screen; transparent is glyphs only", {
  for (nm in names(all_modes)) {
    a <- all_modes[[nm]]
    m <- .ip_mode(a$mode, a$transparent)
    fig <- char_figure(cf_source(), style = "c64bw", slot = "slide_half", mode = a$mode,
                       transparent = a$transparent)
    rects <- which(vapply(fig$layers, function(l) inherits(l$geom, "GeomRect") &&
                            !inherits(l$geom, "GeomTile"), logical(1)))
    glyphs <- layer_data_of(fig, text_layers(fig))
    if (a$transparent) {
      expect_length(rects, 0L)
      expect_identical(unique(glyphs$colour), m$ink)
      expect_true(is.na(fig$theme$plot.background$fill))
      expect_true(is.na(fig$theme$panel.background$fill))
    } else {
      expect_length(rects, 2L)
      expect_identical(unique(glyphs$colour), "#FFFFFF")
      expect_identical(layer_data_of(fig, rects[[2]])$fill, "#000000")
      tile <- tile_kids(layer_data_of(fig, rects[[1]])$fill[[1]])
      expect_identical(tile[[1]]$gp$fill, m$paper)
      expect_identical(tile[[2]]$gp$fill, m$ink)
      expect_identical(fig$theme$plot.background$fill, m$paper)
    }
    expect_identical(two_values(fig), "pass")
    # Same size in every mode, so the files are interchangeable.
    expect_identical(char_size(fig)[c("width", "height")], list(width = 64, height = 44))
  }
})

# Export -------------------------------------------------------------------------

png_colour_type <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, "raw", 26L)
  # Signature (8), chunk length (4), "IHDR" (4), width (4), height (4),
  # bit depth (1), colour type (1).
  list(width = sum(as.integer(bytes[17:20]) * 256^(3:0)),
       height = sum(as.integer(bytes[21:24]) * 256^(3:0)),
       type = as.integer(bytes[26L]))
}

test_that("save_figure writes a transparent background on request", {
  skip_if_not_installed("ragg")
  p <- line_plot("dark", TRUE)
  f <- withr::local_tempfile(fileext = ".png")
  save_figure(p, f, preset = "slide_half", dpi = 100, transparent = TRUE)
  expect_identical(png_colour_type(f)$type, 6L)
  expect_error(save_figure(p, f, bg = "#000000", transparent = TRUE),
               class = "inferplotR_invalid_input")
  expect_error(save_figure(p, f, transparent = "yes"), class = "inferplotR_invalid_input")
})

test_that("save_figure_set writes the four mode folders with identical names", {
  skip_if_not_installed("ragg")
  skip_if_not(cairo_pdf_works(), "cairo_pdf() does not work here")
  dir <- withr::local_tempdir()
  calls <- list()
  build <- function(mode, transparent) {
    calls[[length(calls) + 1L]] <<- list(mode = mode, transparent = transparent)
    compare_plot(mode, transparent)
  }
  out <- save_figure_set(build, "plant-growth", preset = "slide_half", dir = dir, dpi = 100)
  expect_identical(names(out), c("mode", "file", "format", "width_mm", "height_mm"))
  expect_identical(nrow(out), 8L)
  expect_identical(unique(out$mode), names(all_modes))
  expect_identical(calls, unname(all_modes))
  expect_true(all(file.exists(out$file)))
  expect_identical(unique(basename(out$file)), c("plant-growth.pdf", "plant-growth.png"))
  expect_setequal(basename(dirname(out$file)), names(all_modes))
  expect_true(all(normalizePath(dirname(dirname(out$file))) == normalizePath(dir)))
  expect_identical(sort(list.files(dir, recursive = TRUE)), sort(
    file.path(rep(names(all_modes), each = 2), c("plant-growth.pdf", "plant-growth.png"))))
  expect_identical(unique(out$width_mm), 64)
  expect_identical(unique(out$height_mm), 44)
  pngs <- out[out$format == "png", , drop = FALSE]
  for (i in seq_len(nrow(pngs))) {
    info <- png_colour_type(pngs$file[i])
    # The device truncates or rounds to whole pixels.
    expect_lt(abs(info$width - 64 / 25.4 * 100), 1)
    expect_lt(abs(info$height - 44 / 25.4 * 100), 1)
    if (endsWith(pngs$mode[i], "-transparent")) expect_identical(info$type, 6L)
  }
})

test_that("save_figure_set is type-stable and stays inside dir", {
  dir <- withr::local_tempdir()
  build <- function(mode, transparent) line_plot(mode, transparent)
  none <- save_figure_set(build, "fig", preset = "slide_half", dir = dir, modes = character())
  expect_identical(nrow(none), 0L)
  expect_identical(vapply(none, class, ""), c(mode = "character", file = "character",
                                              format = "character", width_mm = "numeric",
                                              height_mm = "numeric"))
  expect_identical(list.files(dir, recursive = TRUE, all.files = TRUE), character())
  bad <- list(list(name = "../fig"), list(name = "sub/fig"), list(name = ".."),
              list(name = "fig.pdf"), list(name = "~fig"), list(modes = "sepia"),
              list(modes = c("dark", "dark")), list(preset = "poster"), list(png = NA),
              list(dpi = 0), list(build = "fig"))
  for (b in bad) {
    args <- utils::modifyList(list(build = build, name = "fig", preset = "slide_half",
                                   dir = dir), b)
    expect_error(do.call(save_figure_set, args), class = "inferplotR_invalid_input")
  }
  expect_error(save_figure_set(build, "fig", dir = dir), class = "inferplotR_invalid_input")
  expect_error(save_figure_set(function(mode, transparent) 1, "fig", "slide_half", dir = dir,
                               modes = "light"),
               class = "inferplotR_invalid_input")
  expect_identical(list.files(dir, recursive = TRUE), character())
})
