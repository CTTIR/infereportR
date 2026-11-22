# infereportR 0.2.0.9000

Development version of 0.3.0.

## Bleisatz slide decks and documents

* New slide constructors `figure_slide()` (side, half, full and text
  layouts, inverse frames), `section_slide()`, `keyfigure_slide()` and
  `slide_sparkline()`, collected by `figure_deck()` into a Bleisatz Beamer
  deck. `save_deck()` writes the deck into a directory that must be given:
  every figure saved with `save_figure_set()` at the size of its slot, in the
  deck's palette and in the mode folders that the theme's
  `\figurefolder{figures}` finds, the theme file and a `.tex` file that uses
  only the theme's own commands. It refuses to overwrite existing files
  unless `overwrite = TRUE` (condition class `infereportR_file_exists`).
* New document blocks `document_section()`, `document_text()`,
  `document_figure()` (side, full, half and margin slots),
  `document_table()`, `document_note()`, `document_newthought()`,
  `document_fullwidth()`, `document_epigraph()` and `document_sparkline()`,
  collected by `figure_document()` and written by `save_document()` with the
  Bleisatz document class, optionally with a biblatex bibliography (APA
  style, biber).
* `figure_deck()` and `figure_document()` expose every option of Bleisatz
  1.3 (`palette`, `font`, `rules`, `typography`, `emphasis`, `figures`,
  `progress`, `head`, `sectionpages`, `layout`, `notes`, `tufte`, plus the
  mode, `total`, paper size and justification); `NULL` keeps the theme
  default, and only the options given are written.
* `render_deck()` and `render_document()` compile with latexmk, pdfLaTeX or
  LuaLaTeX, or TinyTeX, when available; LaTeX is never required. Without it
  they raise `infereportR_missing_dependency`, and a failed run raises
  `infereportR_render_failed` with the end of the log.
* `figure_gallery_deck()` writes the twelve gallery figures as the deck of
  the theme's `gallery.tex`, and with `set = "tufte"` the Tufte set as a
  deck and a handout in the structure of the theme's Tufte samples.
* `tex_red()`, `tex_green()` and `tex_muted()` return the theme's palette
  commands as LaTeX. Slide and block text is escaped for LaTeX, leaving these
  commands, `sparkline_tex()` output, citations and `\ref{}` intact.
* `deck_theme_file()` returns the bundled theme or class file. The Bleisatz
  1.3 files `beamerthemeBleisatz.sty` and `bleisatz.cls` are included
  unmodified under the LaTeX Project Public License 1.3c; the package stays
  MIT licensed (see `LICENSE.md`).
* R Markdown templates "infereportR slides (Bleisatz)" and "infereportR
  document (Bleisatz)" with short pandoc templates of their own, and a Quarto
  extension with the formats `bleisatz-beamer` and `bleisatz-pdf`, copied by
  `use_deck_quarto()` and `use_document_quarto()`.
* New vignette "Slides and documents". 'tinytex' joins Suggests.

## Tufte options for figures (Bleisatz 1.3)

* Every figure function takes `palette = "onebit"` (default, unchanged: two
  values), `"grey"` (plus one grey for context) or `"tufte"` (plus Tufte's red
  and green): `theme_figure()`, the layers, the builders, `report_figure()`,
  `scale_colour_ink()`, `save_figure_set()` and `check_figure()`. The colors
  come from the one mode resolver; red and green are lighter in dark mode.
* `scale_colour_palette()` maps levels to the palette roles ink, grey, red,
  green and paper.
* New Tufte figure types: `geom_rangeframe()` and
  `theme_figure(axes = "range")` (range frames), `geom_dotdash()` (dot-dash
  axes), `stat_quartiles()` (quartile plot), `theme_figure(grid = "paper")`
  and `geom_papergrid()` (a grid in the paper color through the bars),
  `figure_slopegraph()` (slopegraph as a table-graphic),
  `figure_sparklines()` and `sparkline_tex()` (sparklines in figures and in
  Bleisatz LaTeX), and `stat_context()` (context in grey, the summary in ink).
* `figure_size("margin")`: the 52 x 40 mm margin slot of the Tufte handout.
* `figure_font("etbb")`: ET Book when installed, else `"serif"`.
* `check_figure(palette = )`: the `two_values` row is now the palette rule;
  with the default palette it is unchanged.
* `save_figure_set()` writes the folders `light-grey`, `dark-grey`,
  `light-1bit` and `dark-1bit`, and passes `palette` to a `build` function
  that accepts it.
* `figure_gallery(set = "tufte")` writes the Tufte set, figures 13-19 with two
  margin variants, in six folders; `gallery_captions(set = "tufte")` returns
  its captions.
* New vignette "Tufte options".

# infereportR 0.2.0

## The figure layer joins infereportR

* Merged the figure package inferplotR 0.1.0 into infereportR, with its
  history. Its exported functions keep their names except two: the bundle
  `inferplot()` is now `report_figure()` (same arguments), to match the
  `report_*` family, and `figure_report()` is now `figure_table()`.
  `report_figure()` adds layers that draw a figure; `figure_table()` returns
  the results of a figure as one table. The separate repository
  CTTIR/inferplotR is retired.
* Renamed the figure conditions to `infereportR_<code>` errors and warnings,
  with the classes `infereportR_error` and `infereportR_warning`, and the
  result ids to `"infereportR:..."`.
* Added the class `infereportR_error` to the errors of the reporting
  functions, next to their `infereportR_<code>` class; errors of the figure
  layer also keep the underlying condition as `parent`. The change is
  additive: a reporting error without an underlying condition has the same
  fields as in 0.1.0.
* Changed the imports to ggplot2 (3.5.0 or later), infercalcR, grid,
  grDevices, stats and utils; 0.1.0 imported infercalcR and utils and
  suggested ggplot2. Reporting therefore no longer runs without ggplot2, and
  `stat_inference()` and `annotate_inference()` no longer check for an
  optional backend. Added ragg, systemfonts, testthat and withr to Suggests.
* Changed the inference layers to take their display strings from
  `report_table()` and `format_estimate()` of the same package.
* Kept DESCRIPTION without a `Remotes` field, as in 0.1.0; the GitHub
  Actions workflows install infercalcR from GitHub.
* Changed the title to "Tables, Text and Figures for Statistical Results".
* Added four vignettes, "Figures: theme, scales, labels and export",
  "Inference layers", "Character mode" and "The figure gallery", next to the
  three of 0.1.0 ("Getting started with infereportR", "Comparison families,
  adjusted p-values and plot annotations" and "Descriptive tables"). Added a
  section on figures with `report_figure()` to "Getting started", and a
  drawing of the same family with `stat_compare()` next to `stat_inference()`
  to "Comparison families", with when to use which.
* Added a `dpi` argument to `figure_gallery()` for its PNG files (300 by
  default).
* Removed the default `dir = "figures"` of `save_figure_set()`: `dir` is now
  required, so the function never writes to the working directory unless
  asked to. Pass `dir = "figures"` for the folder layout of a Bleisatz deck.
* Added `gallery_captions()`, which returns the file names, slots, sizes and
  captions of the twelve gallery figures; the help page of
  `figure_gallery()` and the vignette "The figure gallery" referred to an
  internal object for them.
* Changed `stat_omnibus()` to set a label at the left or right panel edge
  1 mm in from the axis, and `stat_ecdf_guides()` to place a curve label
  clear of the curve over the label's own height.
* Fixed the ASCII transliteration of `char_grid()` and the subgroup labels
  of gallery figure 06 for a package installed under a non-UTF-8 locale:
  their names were written as argument tags such as `"\u2265" = ">="`, which
  R stores as `"<U+2265>"` in such a locale. The names are now set with
  `stats::setNames()` and tested to be the real characters.
* Added testthat (edition 3) tests for the figure layer under
  `tests/testthat/`; the reporting tests stay plain `tests/*.R` scripts. The
  quality gate `tools/gate.R` runs both. On CRAN only a fast subset of the
  figure tests runs.

## What the figure layer brings (inferplotR 0.1.0)

* Added the inference layers `stat_means()` (group means with confidence
  intervals), `stat_compare()` (pairwise comparisons with a named two-sample
  procedure and a declared adjustment, drawn as stacked brackets),
  `stat_omnibus()` (one-way analysis of means, classic or Welch, or
  Kruskal-Wallis), `stat_correlation()` (Pearson, Spearman or Kendall, on one
  or two lines, with an optional fitted line and band) and `stat_paired()`
  (paired designs as a slopegraph). Every layer runs once per panel, needs an
  explicitly named procedure and takes all numbers from infercalcR.
  `report_figure()` adds the theme, the scales, the observations, the means
  and the named tests in one call.
* Added the figure builders `figure_forest()` and
  `figure_correlation_matrix()`, and `stat_stacklabel()` and
  `stat_ecdf_guides()`, which label stacked bars and empirical distribution
  functions instead of a legend.
* Added `inference_results()`, which returns the infercalcR results behind
  a figure, and `figure_table()` (`figure_report()` in inferplotR), which
  binds their `report_table()` rows into one table with the layer and panel
  of each row.
* Added the theme `theme_figure()` for APA papers and Bleisatz
  slides with all text between 8 and 14 pt and `figure_font()` for IBM Plex;
  `scale_linetype_figure()`, `scale_shape_figure()`, `scale_colour_ink()`,
  `scale_fill_hatch()` and `hatch_pattern()`, which replace color, and
  `geom_endlabel()`, which labels lines at their last point.
* Added modes and export: every drawing function takes `mode = "light"` or
  `"dark"` and `transparent`; `figure_size()` holds journal and slide-slot
  presets; `save_figure()`, `save_figure_set()` and `figure_gallery()` write
  single figures, mode sets and the twelve Bleisatz example figures.
* Added `check_figure()`, which audits a figure against the APA figure rules
  and the black-and-white rule.
* Added the character mode: `char_grid()` redraws a ggplot on a character
  grid, `char_figure()` renders it in black and white or as a Commodore 64
  screen and `char_size()` gives the matching file size.

# infereportR 0.1.0

* Removed the adapter for precomputed DescrTab2 tables (`import_descrtab2()`,
  `render_descrtab2()`), and DescrTab2 is no longer suggested.
  DescrTab2 inspired the suite and is acknowledged and cited; the tests it
  computes come from other packages, which infercalcR uses as optional
  originals next to its own base-R implementations.
* Added acknowledgments and citations to the README and every vignette:
  DescrTab2 (Institute of Medical Biometry, Heidelberg University), which
  deeply inspired the suite, the Methodenberatung pages of the University of
  Zurich and G*Power; the README also cites R and the packages used.
* Added three vignettes, "Getting started with infereportR", "Comparison
  families, adjusted p-values and plot annotations" and "Descriptive
  tables", and a pkgdown site at <https://cttir.github.io/infereportR/>,
  built and deployed to GitHub Pages by a GitHub Actions workflow.
* Made the repository public. The README describes installation from
  GitHub, and the R CMD check workflow reads infercalcR with the standard
  workflow token instead of a separate access token.
* Prepared for a first CRAN submission, which has to wait until
  infercalcR is on CRAN.
* Changed the maintainer address to raban.heller@uni-ulm.de.
* Dropped the `Remotes` field from DESCRIPTION, which now requires
  infercalcR 0.1.0 or later, has a fuller Description and declares
  `Language: en-US`. Added `inst/WORDLIST` with the technical terms used in
  the documentation.
* Changed the GitHub Actions check to install infercalcR explicitly, to
  add R-devel and R oldrel-1 on Linux and to explain what is missing when it
  cannot read infercalcR.
* Added a CTTIR v5 hex logo in the statistical inference color shared with
  the other inference packages; `tools/branding/render.R` draws it in R.
* Added plain tables, deterministic text and descriptions through infercalcR's
  shared result contract, with full-precision analytical fields and common
  formatting across displays.
* Added explicit-coordinate ggplot inference layers, facet-key checks,
  coordinate guards, and preserved adjustment-family metadata.
* Added installed-package base-R core and backend integration checks.
