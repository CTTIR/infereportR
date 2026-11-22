test_that("all ten LaTeX special characters are escaped", {
  expect_identical(.ir_tex("&"), "\\&")
  expect_identical(.ir_tex("%"), "\\%")
  expect_identical(.ir_tex("$"), "\\$")
  expect_identical(.ir_tex("#"), "\\#")
  expect_identical(.ir_tex("_"), "\\_")
  expect_identical(.ir_tex("{"), "\\{")
  expect_identical(.ir_tex("}"), "\\}")
  expect_identical(.ir_tex("~"), "\\textasciitilde{}")
  expect_identical(.ir_tex("^"), "\\textasciicircum{}")
  expect_identical(.ir_tex("\\"), "\\textbackslash{}")
  expect_identical(.ir_tex("50% & a_b {c} #1 $2 ~^\\"),
                   paste0("50\\% \\& a\\_b \\{c\\} \\#1 \\$2 \\textasciitilde{}",
                          "\\textasciicircum{}\\textbackslash{}"))
})

test_that("escaping is vectorised and keeps plain text", {
  expect_identical(.ir_tex(c("plain", "a&b", NA)), c("plain", "a\\&b", ""))
  expect_identical(.ir_tex(NULL), character())
  expect_identical(.ir_tex(""), "")
  expect_identical(.ir_tex("line\nbreak"), "line break")
})

test_that("non-ASCII passes through; comparison, times and minus signs become maths", {
  expect_identical(.ir_tex("M\u00fcller \u00e9t\u00e9 \u00b5g 16\u201324"),
                   "M\u00fcller \u00e9t\u00e9 \u00b5g 16\u201324")
  expect_identical(.ir_tex("ISS \u2265 25"), "ISS $\\geq$ 25")
  expect_identical(.ir_tex("\u2264 \u00d7 \u2212"), "$\\leq$ $\\times$ $-$")
  # C64 decks leave the signs to the theme, which types them out.
  expect_identical(.ir_tex("ISS \u2265 25", font = "c64"), "ISS \u2265 25")
  expect_identical(Encoding(.ir_tex("\u00e4")), "UTF-8")
})

test_that("palette, sparkline, citation and reference commands pass through", {
  s <- paste("rose to", tex_green("20.7"), "then", tex_red("9.4 & less"),
             tex_muted("ng/ml"), sparkline_tex(c(1, 2, 3), at = c(0, 2, 4)),
             "\\parencite[p. 3]{tufte2001}", "\\ref{fig:a} 5%")
  out <- .ir_tex(s)
  expect_match(out, "\\ingreen{20.7}", fixed = TRUE)
  expect_match(out, "\\inred{9.4 \\& less}", fixed = TRUE)
  expect_match(out, "\\muted{ng/ml}", fixed = TRUE)
  expect_match(out, "\\sparkline[at={0,2,4}]{1.0, 2.0, 3.0}", fixed = TRUE)
  expect_match(out, "\\parencite[p. 3]{tufte2001}", fixed = TRUE)
  expect_match(out, "\\ref{fig:a} 5\\%", fixed = TRUE)
  expect_identical(.ir_tex(tex_red("a{b}")), tex_red("a{b}"))
  expect_identical(.ir_tex(I("\\textbf{raw} & co")), "\\textbf{raw} & co")
})

test_that("tex_red(), tex_green() and tex_muted() return escaped LaTeX", {
  expect_identical(tex_red("9.4"), "\\inred{9.4}")
  expect_identical(tex_green("20.7"), "\\ingreen{20.7}")
  expect_identical(tex_muted("50%"), "\\muted{50\\%}")
  expect_identical(tex_red("{x}"), "\\inred{\\textbraceleft{}x\\textbraceright{}}")
  expect_error(tex_red(1), class = "infereportR_invalid_input")
  expect_error(tex_green(c("a", "b")), class = "infereportR_invalid_input")
  expect_error(tex_muted(NA_character_), class = "infereportR_invalid_input")
})

test_that("file stems are ASCII in every locale", {
  expect_identical(.ir_slug("Treatment 2 plants weigh most"), "treatment-2-plants-weigh-most")
  expect_identical(.ir_slug("\u00c4rger \u2265 5 \u00d7 m\u00fc\u00df"), "aerger-ge-5-x-muess")
  expect_identical(.ir_slug("\u03b1\u03b2\u03b3"), "slide")
  expect_identical(.ir_slug("!!!"), "slide")
  long <- .ir_slug(paste(rep("word", 20), collapse = " "))
  expect_lte(nchar(long), 40L)
  expect_false(grepl("-$", long))
  expect_true(all(utf8ToInt(.ir_slug("\u00e9t\u00e9 \u00e0 Z\u00fcrich")) < 128L))
})

test_that("theme options are written only when given", {
  expect_identical(.ir_bz_option_string(list(), "light"), character())
  expect_identical(.ir_bz_option_string(list(total = TRUE, head = FALSE, sectionpages = TRUE,
                                             progress = "dots", font = "mono",
                                             palette = "grey", tufte = TRUE), "dark"),
                   c("dark", "total", "nohead", "sectionpages", "progress=dots", "font=mono",
                     "palette=grey", "tufte"))
  expect_identical(.ir_bz_option_string(list(head = TRUE, sectionpages = FALSE,
                                             justified = TRUE, paper = "letter",
                                             layout = "tufte", notes = "side"), "light"),
                   c("head", "nosectionpages", "layout=tufte", "notes=side", "justified",
                     "letterpaper"))
  expect_error(.ir_bz_check(list(palette = "red")), class = "infereportR_invalid_input")
  expect_error(.ir_bz_check(list(head = "yes")), class = "infereportR_invalid_input")
  expect_error(.ir_bz_check(list(tufte = NA)), class = "infereportR_invalid_input")
})

test_that("the effective aspects and figure folders follow the theme", {
  e <- .ir_bz_effective(list())
  expect_identical(e, list(palette = "onebit", font = "plex", figures = "bleisatz"))
  e <- .ir_bz_effective(list(tufte = TRUE))
  expect_identical(e, list(palette = "tufte", font = "etbb", figures = "tufte"))
  e <- .ir_bz_effective(list(tufte = TRUE, palette = "grey", font = "c64"))
  f <- .ir_bz_folders(e, .ir_set_modes)
  expect_identical(f$dir, "figures/c64/tufte")
  expect_identical(f$modes, c("light-grey", "dark-grey"))
  expect_identical(f$folder("dark"), "figures/c64/tufte/dark-grey/")
  f <- .ir_bz_folders(.ir_bz_effective(list(figures = "tufte")), c("light", "dark"))
  expect_identical(f$modes, c("light-1bit", "dark-1bit"))
  f <- .ir_bz_folders(.ir_bz_effective(list(tufte = TRUE)), "light")
  expect_identical(f$modes, "light")
  expect_identical(f$palette, "tufte")
  f <- .ir_bz_folders(.ir_bz_effective(list()), .ir_set_modes)
  expect_identical(f$modes, .ir_set_modes)
  expect_identical(f$folder("light"), "figures/light/")
})

test_that("file names and labels are checked", {
  expect_error(.ir_check_tex_file("../deck.tex"), class = "infereportR_invalid_input")
  expect_error(.ir_check_tex_file("deck.pdf"), class = "infereportR_invalid_input")
  expect_error(.ir_check_tex_file(".deck.tex"), class = "infereportR_invalid_input")
  expect_silent(.ir_check_tex_file("my-deck_1.tex"))
  expect_error(.ir_check_label("fig one"), class = "infereportR_invalid_input")
  expect_silent(.ir_check_label("fig:one.two-3_4"))
})
