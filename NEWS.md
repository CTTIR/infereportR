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
