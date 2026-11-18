# infereportR 0.2.0

## The figure layer joins infereportR

* The figure package inferplotR 0.1.0 is merged into infereportR, with its
  history. Its exported functions keep their names, except the bundle
  `inferplot()`, which is now `report_figure()` (same arguments) to match the
  `report_*` family, and `figure_report()`, which is now `figure_table()`. Its conditions are `infereportR_<code>` errors and
  warnings (with the classes `infereportR_error` and `infereportR_warning`),
  and its result ids start with `"infereportR:"`. The separate repository
  CTTIR/inferplotR is retired.
* ggplot2 (3.5.0 or later), grid, grDevices and stats are imported; reporting
  no longer runs without ggplot2. `stat_inference()` and
  `annotate_inference()` therefore no longer check for an optional backend.
  ragg, systemfonts, testthat and withr are suggested.
* The inference layers take their display strings from `report_table()` and
  `format_estimate()` of the same package; `Remotes` lists infercalcR only.
* Three vignettes keep their place ("Getting started with infereportR",
  "Comparison families, adjusted p-values and plot annotations" and
  "Descriptive tables"); four join them: "Figures: theme, scales, labels and
  export", "Inference layers", "Character mode" and "The figure gallery".
  "Getting started" gains a section on figures with `report_figure()`, and
  "Comparison families" draws the same family with `stat_compare()` next to
  `stat_inference()` and explains when to use which.
* The figure tests use testthat (edition 3) under `tests/testthat/`; the
  reporting tests stay plain `tests/*.R` scripts. The quality gate
  `tools/gate.R` runs both.

## What the figure layer brings (inferplotR 0.1.0)

* Inference layers: `stat_means()` (group means with confidence intervals),
  `stat_compare()` (pairwise comparisons with a named two-sample procedure and
  a declared adjustment, drawn as stacked brackets), `stat_omnibus()` (one-way
  analysis of means, classic or Welch, or Kruskal-Wallis), `stat_correlation()`
  (Pearson, Spearman or Kendall, on one or two lines, with an optional fitted
  line and band) and `stat_paired()` (paired designs as a slopegraph). Every
  layer runs once per panel, needs an explicitly named procedure and takes all
  numbers from infercalcR. `report_figure()` adds the theme, the scales, the
  observations, the means and the named tests in one call.
* Figure builders: `figure_forest()` and `figure_correlation_matrix()`;
  `stat_stacklabel()` and `stat_ecdf_guides()` label stacked bars and
  empirical distribution functions instead of a legend.
* Results: `inference_results()` returns the infercalcR results behind a
  figure and `figure_table()` (`figure_report()` in inferplotR) binds their `report_table()` rows into one
  table with the layer and panel of each row.
* Theme, scales and labels: `theme_figure()` for APA papers and Bleisatz
  slides with all text between 8 and 14 pt and `figure_font()` for IBM Plex;
  `scale_linetype_figure()`, `scale_shape_figure()`, `scale_colour_ink()`,
  `scale_fill_hatch()` and `hatch_pattern()` replace color;
  `geom_endlabel()` labels lines at their last point.
* Modes and export: every drawing function takes `mode = "light"` or
  `"dark"` and `transparent`; `figure_size()` holds journal and slide-slot
  presets; `save_figure()`, `save_figure_set()` and `figure_gallery()` write
  single figures, mode sets and the twelve Bleisatz example figures.
* `check_figure()` audits a figure against the APA figure rules and the
  black-and-white rule.
* Character mode: `char_grid()` redraws a ggplot on a character grid;
  `char_figure()` renders it in black and white or as a Commodore 64 screen
  and `char_size()` gives the matching file size.

# infereportR 0.1.0

* The adapter for precomputed DescrTab2 tables (`import_descrtab2()`,
  `render_descrtab2()`) is removed, and DescrTab2 is no longer suggested.
  DescrTab2 inspired the suite and is acknowledged and cited; the tests it
  computes come from other packages, which infercalcR uses as optional
  originals next to its own base-R implementations.
* The README and every vignette now acknowledge and cite DescrTab2
  (Institute of Medical Biometry, Heidelberg University), which deeply
  inspired the suite, the Methodenberatung pages of the University of
  Zurich and G*Power; the README also cites R and the packages used.
* Added three vignettes, "Getting started with infereportR", "Comparison
  families, adjusted p-values and plot annotations" and "Descriptive
  tables", and a pkgdown site at <https://cttir.github.io/infereportR/>,
  built and deployed to GitHub Pages by a GitHub Actions workflow.
* The repository is public. The README describes installation from GitHub,
  and the R CMD check workflow reads infercalcR with the standard workflow
  token instead of a separate access token.
* Prepared for a first CRAN submission, which has to wait until
  infercalcR is on CRAN.
* The maintainer address is now raban.heller@uni-ulm.de.
* DESCRIPTION drops the `Remotes` field, requires infercalcR 0.1.0 or
  later, gains a fuller Description and declares `Language: en-US`;
  `inst/WORDLIST` records the technical terms used in the documentation.
* The GitHub Actions check installs infercalcR explicitly, adds R-devel and R oldrel-1 on Linux, and explains what is
  missing when it cannot read infercalcR.
* Add a CTTIR v5 hex logo in the statistical inference color shared with
  the other inference packages; `tools/branding/render.R` draws it in R.
* Added plain tables, deterministic text and descriptions through infercalcR's
  shared result contract, with full-precision analytical fields and common
  formatting across displays.
* Added explicit-coordinate ggplot inference layers, facet-key checks,
  coordinate guards, and preserved adjustment-family metadata.
* Added installed-package base-R core and backend integration checks.
