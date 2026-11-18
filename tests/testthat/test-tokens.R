test_that("colour tokens are the two values and the Pepto pair", {
  expect_identical(.ip_ink, "#000000")
  expect_identical(.ip_paper, "#FFFFFF")
  expect_identical(unname(.ip_c64[["background"]]), "#352879")
  expect_identical(unname(.ip_c64[["foreground"]]), "#6C5EB5")
})

test_that("size presets match the specification", {
  expected <- data.frame(
    preset = c("journal_column", "journal_page", "slide_full", "slide_side", "slide_half"),
    width = c(85, 174, 134, 86, 64), height = c(70, 100, 44, 44, 44),
    stringsAsFactors = FALSE)
  expect_identical(.ip_presets, expected)
})

test_that("sequences have the specified lengths and values", {
  expect_identical(.ip_linetypes, c("solid", "22", "13", "1343", "73", "2262"))
  expect_identical(.ip_shapes, c(16, 1, 15, 0, 17, 2, 18, 5))
  expect_identical(.ip_hatches,
                   c("solid", "open", "hatch", "dots", "crosshatch", "backhatch"))
  expect_true(all(c("horizontal", "vertical") %in% .ip_hatch_types))
  for (cs in c("ascii", "unicode")) {
    expect_length(.ip_glyphs[[cs]]$points, 8L)
    expect_length(.ip_glyphs[[cs]]$lines, 8L)
    expect_length(.ip_glyphs[[cs]]$fills, 8L)
  }
})

test_that(".ip_lw converts points to linewidth", {
  expect_equal(.ip_lw(0.4) * ggplot2::.pt * 72.27 / 96, 0.4, tolerance = 1e-12)
  expect_equal(.ip_lw(1.1) * ggplot2::.pt * 72.27 / 96, 1.1, tolerance = 1e-12)
})

test_that(".ip_round rounds half up", {
  expect_identical(.ip_round(c(0.5, 1.5, 2.5)), c(1, 2, 3))
  expect_identical(.ip_round(c(-0.5, 0.49, 2.51)), c(0, 0, 3))
})

test_that("amendment 01 tokens have the measured values", {
  expect_identical(.ip_hatch_spacing, 1.4)
  expect_identical(.ip_label_size_pt, 8)
  expect_identical(c(.ip_bracket_first, .ip_bracket_step), c(0.10, 0.20))
  expect_identical(.ip_means_offset, 0.28)
  expect_identical(.ip_halftone_spacing, 0.75)
  expect_identical(c(.ip_endlabel_gap, .ip_endlabel_nudge), c(1.15, 2.5))
  expect_identical(unname(.ip_screen), c("#000000", "#FFFFFF"))
})
