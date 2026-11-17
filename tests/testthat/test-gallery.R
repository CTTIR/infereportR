# Gallery parity (amendment D15, A6): data, oracle numbers, files, two values
# and the random state.

# Absolute agreement within tol (the oracle prints six decimals).
expect_near <- function(object, expected, tol) {
  expect_lte(max(abs(unname(object) - expected)), tol)
}

gallery_numbers <- function(v) {
  as.numeric(regmatches(v, gregexpr("-?[0-9]+(\\.[0-9]+)?", v))[[1]])
}

test_that(".ip_with_seed() restores an existing seed and removes a new one", {
  withr::local_seed(42)
  before <- get(".Random.seed", envir = globalenv())
  a <- .ip_with_seed(1, stats::runif(2))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(a, .ip_with_seed(1, stats::runif(2)))
  withr::with_preserve_seed({
    rm(".Random.seed", envir = globalenv())
    .ip_with_seed(1, stats::runif(1))
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })
})

test_that(".ip_gallery_data() returns the nine simulated tables", {
  d <- .ip_gallery_data()
  expect_identical(names(d), c("cohort", "long", "med", "il6", "counts", "sc", "fp", "mk", "pl"))
  expect_identical(nrow(d$cohort), 128L)
  expect_identical(nrow(d$il6), 72L)
  expect_identical(nrow(d$sc), 80L)
  expect_identical(nrow(d$fp), 240L)
  expect_identical(dim(d$mk), c(90L, 5L))
  expect_identical(nrow(d$pl), 14L)
  expect_identical(d, .ip_gallery_data())
  expect_identical(nrow(.ip_gallery_captions), 12L)
  expect_length(.ip_gallery_builders, 12L)
})

test_that("figure 02 reproduces the means, intervals and Holm-adjusted p of the oracle", {
  p <- .ip_gallery_02(.ip_gallery_data())
  rep <- figure_report(p)
  means <- rep[rep$method_id == "t_mean_one_sample", ]
  groups <- c("ISS < 16", "ISS 16-24", "ISS \u2265 25")
  expect_identical(nrow(means), 3L)
  for (i in seq_along(groups)) {
    want <- gallery_numbers(gallery_value("02", paste0("mean_ci:", groups[i])))
    got <- unlist(means[i, c("estimate", "conf.low", "conf.high", "n")])
    expect_near(got, want, 1e-6)
  }
  cmp <- rep[rep$method_id == "t_means_two_independent_welch", ]
  for (g in groups[2:3]) {
    contrast <- paste(g, "minus", groups[1])
    expect_identical(cmp$p_display[cmp$contrast == contrast],
                     gallery_value("02", paste0("p_adjusted_display:", contrast)))
  }
})

test_that("figure 04 reproduces the correlation of the oracle", {
  p <- .ip_gallery_04(.ip_gallery_data())
  rep <- figure_report(p)
  res <- inference_results(p)
  expect_identical(nrow(rep), 1L)
  got <- paste0("r = ", rep$estimate_display, " ", rep$interval_display, " | ",
                rep$p_display, ", n = ", infercalcR::result_data(res[[1]])$n)
  expect_identical(got, gallery_value("04", "label"))
  # The figure uses the two-line label with n, so it equals the oracle string
  # (the oracle writes the line break as " | ").
  label <- ggplot2::layer_data(p, 2L)$label
  expect_identical(label, sub(" | ", "\n", gallery_value("04", "label"), fixed = TRUE))
})

test_that("figure 05 reproduces the medians of the oracle", {
  p <- .ip_gallery_05(.ip_gallery_data())
  d <- ggplot2::layer_data(p, 2L)
  drop <- d[d$part == "drop", ]
  oracle <- strsplit(strsplit(gallery_value("05", "median_peak_h"), "; ")[[1]], "=")
  expect_equal(drop$x, as.numeric(vapply(oracle, `[`, "", 2)), tolerance = 1e-4)
})

test_that("figure 06 reproduces the seven forest rows of the oracle", {
  p <- .ip_gallery_06(.ip_gallery_data())
  b <- ggplot2::ggplot_build(p)
  marks <- b$data[[3]]
  n_layer <- b$data[[5]]
  est_text <- b$data[[6]]
  labels <- b$data[[4]]$label
  expect_identical(length(labels), 7L)
  for (i in seq_along(labels)) {
    v <- gallery_value("06", paste0("row:", labels[i]))
    expect_identical(est_text$label[i], sub("^.* text=", "", v))
    expect_near(marks$x[i], gallery_numbers(sub(" text=.*$", "", v))[2], 1e-6)
    expect_identical(n_layer$label[i], sub("^n=([0-9]+) .*$", "\\1", v))
  }
  # The axis runs from -30 to 60 with a break at -20, as in the reference.
  expect_equal(b$layout$panel_params[[1]]$x.range, c(-30, 60))
  breaks <- b$layout$panel_params[[1]]$x$get_breaks()
  expect_identical(as.numeric(breaks[!is.na(breaks)]), c(-20, 0, 20, 40))
  expect_true(all(c(b$data[[4]]$x, b$data[[5]]$x) < -30))
  expect_true(all(b$data[[6]]$x > 60))
})

test_that("caption 08 puts the circles above the diagonal and the values below", {
  cap <- .ip_gallery_captions$caption[.ip_gallery_captions$name == "08-correlation-matrix"]
  expect_true(grepl("above the diagonal, circle", cap, fixed = TRUE))
  expect_true(grepl("mirrored below the diagonal", cap, fixed = TRUE))
  # The figure agrees. The diagonal runs from top left to bottom right
  # (x + y = 6 for five variables): circles (layer 1) lie above it, values
  # (layer 2) below.
  p8 <- .ip_gallery_08(.ip_gallery_data())
  circles <- ggplot2::layer_data(p8, 1L)
  values <- ggplot2::layer_data(p8, 2L)
  expect_true(all(circles$x + circles$y > 6))
  expect_true(all(values$x + values$y < 6))
})

test_that("figures 08 and 09 reproduce the matrix and the paired result of the oracle", {
  d <- .ip_gallery_data()
  p8 <- .ip_gallery_08(d)
  res <- inference_results(p8)
  values <- ggplot2::layer_data(p8, 2L)
  vars <- colnames(d$mk)
  pairs <- utils::combn(5, 2)
  for (k in seq_len(ncol(pairs))) {
    key <- paste0("rho:", vars[pairs[1, k]], "-", vars[pairs[2, k]])
    want <- strsplit(gallery_value("08", key), " display=", fixed = TRUE)[[1]]
    expect_near(infercalcR::result_data(res[[k]])$estimate, as.numeric(want[1]), 1e-6)
    expect_identical(values$label[k], want[2])
  }
  rep9 <- figure_report(.ip_gallery_09(d))
  expect_identical(paste(rep9$estimate_display, rep9$interval_display, rep9$p_display),
                   gallery_value("09", "paired"))
})

test_that("figures 01, 06 and 12 build and pass the two-value audit (light mode)", {
  d <- .ip_gallery_data()
  for (f in list(.ip_gallery_01, .ip_gallery_06, .ip_gallery_12)) {
    p <- f(d)
    expect_no_warning(draws_silently(p))
    chk <- check_figure(p, "slide")
    expect_identical(chk$status[chk$check == "two_values"], "pass")
  }
})

test_that("figure_gallery() checks its arguments and returns a typed empty table", {
  expect_error(figure_gallery(modes = "sepia"), class = "inferplotR_invalid_input")
  expect_error(figure_gallery(png = NA), class = "inferplotR_invalid_input")
  expect_error(figure_gallery(dir = 1), class = "inferplotR_invalid_input")
  empty <- figure_gallery(withr::local_tempdir(), modes = character())
  expect_identical(empty, .ip_set_empty())
})

# The page size of a PDF in mm. Cairo writes the page dictionary in a
# compressed object stream (PDF 1.7), so each FlateDecode stream is inflated
# with base R before the /MediaBox is matched.
mediabox_mm <- function(file) {
  bytes <- readBin(file, "raw", file.size(file))
  starts <- grepRaw("stream\r?\n", bytes, all = TRUE, value = FALSE)
  ends <- grepRaw("endstream", bytes, all = TRUE)
  texts <- rawToChar(bytes[!bytes %in% as.raw(0)], multiple = FALSE)
  texts <- c(texts, unlist(lapply(starts, function(s) {
    e <- ends[ends > s][1]
    if (is.na(e)) return(character())
    body <- bytes[(s + 7L):(e - 1L)]
    if (body[1] == as.raw(10)) body <- body[-1]
    out <- tryCatch(memDecompress(body, type = "gzip"), error = function(err) raw())
    if (!length(out)) return(character())
    rawToChar(out[out != as.raw(0)])
  })))
  hit <- regmatches(texts, regexpr("/MediaBox *\\[[^]]*\\]", texts, useBytes = TRUE))
  v <- as.numeric(strsplit(trimws(gsub("[^0-9. ]", " ", hit[[1]])), " +")[[1]])
  (v[3:4] - v[1:2]) * 25.4 / 72
}

test_that("figure_gallery() writes 48 PDFs at their slot size, leaving the seed alone", {
  skip_on_cran()
  skip_if_not(cairo_pdf_works())
  dir <- withr::local_tempdir()
  withr::local_seed(7)
  before <- get(".Random.seed", envir = globalenv())
  tab <- figure_gallery(dir)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(names(tab), c("mode", "file", "format", "width_mm", "height_mm"))
  expect_identical(nrow(tab), 48L)
  expect_true(all(file.exists(tab$file)))
  expect_identical(sort(unique(tab$mode)), sort(.ip_set_modes))
  slots <- .ip_gallery_captions[match(sub("[.]pdf$", "", basename(tab$file)),
                                      .ip_gallery_captions$name), ]
  expect_equal(tab$width_mm, slots$width_mm)
  for (i in seq_len(nrow(tab))) {
    # Cairo writes the MediaBox in whole points (243 x 124 for 86 x 44 mm), so
    # the page can be up to one point (0.353 mm) smaller than the slot. This
    # 1 pt tolerance was approved by the owner (2026-11-17, STATE.md Q9).
    expect_near(mediabox_mm(tab$file[i]), c(slots$width_mm[i], slots$height_mm[i]),
                25.4 / 72)
  }
  # Without a seed before, there is none after.
  withr::with_preserve_seed({
    rm(".Random.seed", envir = globalenv())
    figure_gallery(dir, modes = "light-transparent")
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })
})

# Figure 11 (C64 colors) fails two_values in every mode, light, dark and both
# transparent modes: the c64 style keeps its Pepto colors by design. This
# documented exception was approved by the owner (2026-11-17, STATE.md Q9).
test_that("every figure passes the two-value audit in every mode except 11 (C64 colors)", {
  skip_on_cran()
  d <- .ip_gallery_data()
  for (folder in .ip_set_modes) {
    args <- .ip_set_mode(folder)
    for (i in seq_along(.ip_gallery_builders)) {
      p <- .ip_gallery_builders[[i]](d, mode = args$mode, transparent = args$transparent)
      chk <- check_figure(p, "slide")
      status <- chk$status[chk$check == "two_values"]
      want <- if (i == 11L) "fail" else "pass"
      expect_identical(status, want, label = paste(folder, .ip_gallery_captions$name[i]))
    }
  }
})
