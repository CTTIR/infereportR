# inferplotR 0.1.0

First release.

## Inference layers

* `stat_means()` draws group means with confidence intervals from the
  one-sample t procedure of infercalcR.
* `stat_compare()` compares x groups pairwise with a named two-sample procedure
  (Welch, Student, Mann-Whitney or Brunner-Munzel), adjusts the comparison
  family as declared with `infercalcR::adjust_inference()` and draws brackets,
  stacked by `first` and `step` with `headroom` above the top one so no label
  is clipped.
* `stat_omnibus()` labels a one-way analysis of means (classic or Welch) or a
  Kruskal-Wallis test; degrees of freedom are not shown, because infereportR
  0.1.0 has no display string for them.
* `stat_correlation()` labels a Pearson, Spearman or Kendall correlation, on
  one line or, with `label = "estimate_ci_p_n"`, on two lines with n, and,
  with `fit = TRUE`, draws the fitted line and pointwise confidence band of
  infercalcR's linear prediction model. A missing interval (Spearman and
  Kendall in infercalcR 0.1.0) is left out of the label.
* `stat_paired()` draws a paired design with two levels as a slopegraph, one
  line per unit (the new `id` aesthetic), and labels the mean change from a
  paired t or signed-rank procedure.
* Every layer runs once per panel, takes all numbers from infercalcR and all
  display strings from infereportR, and needs an explicitly named procedure.
  With more than one comparison the adjustment must be declared.
* `inferplot()` adds the theme, the black-and-white scales, the observations,
  the means and the named tests to a plot in one call; the means sit beside
  the jittered observations.

## Figure builders

* `figure_forest()` draws a forest plot from a named list of infercalcR
  results: label, n and estimate columns, markers with area proportional to n
  and a dotted null line.
* `figure_correlation_matrix()` runs one named correlation per pair of
  variables and draws circles above and values below the diagonal.
* `stat_stacklabel()` names stacked-bar categories beside the last or first
  bar instead of a legend.
* `stat_ecdf_guides()` adds median guide lines and curve labels to
  `ggplot2::stat_ecdf()`.
* The builders keep their results, so `inference_results()` and
  `figure_report()` work on their figures.

## Results

* `inference_results()` returns the infercalcR results behind a figure.
* `figure_report()` binds their `infereportR::report_table()` rows into one
  table with the layer and panel of each row.
* Precomputed results can be drawn on the same figures with
  `infereportR::stat_inference()`.

## Theme, scales and labels

* `theme_figure()` is a complete black-and-white theme for APA papers
  (`"paper"`) and Bleisatz slides (`"slide"`), with all text between 8 and
  14 pt; `figure_font()` finds IBM Plex when it is installed and falls back to
  R's generic families.
* `scale_linetype_figure()`, `scale_shape_figure()` and `scale_colour_ink()`
  replace color with line types, point shapes and black ink.
* `scale_fill_hatch()` and `hatch_pattern()` fill areas with solid, open,
  hatched, dotted and crosshatched grid patterns.
* `geom_endlabel()` labels lines at their last point instead of a legend and
  pushes labels apart at draw time so they never overlap.

## Modes

* Every function that draws takes `mode = "light"` or `"dark"` and
  `transparent`: black on white, white on black, or either without any
  background.
* `save_figure_set()` writes one figure in all four modes into
  `<dir>/<mode>/`, with the same file name in every folder, so one
  `\graphicspath` line switches a Bleisatz deck.
* `figure_gallery()` writes the twelve Bleisatz example figures with the
  package API: `Rscript -e 'inferplotR::figure_gallery("figures")'`.

## Export and APA check

* `figure_size()` holds size presets for journal columns and the Bleisatz
  slide slots (134, 86 and 64 mm wide, 44 mm high); `save_figure()` writes
  PDF, PNG, SVG or TIFF at those sizes, with a transparent background on
  request.
* `check_figure()` audits a figure against the APA figure rules (text size,
  font, axis labels, embedded title, legend case) and the black-and-white rule.

## Character mode

* `char_grid()` redraws a single-panel ggplot on a monospaced character grid in
  ASCII or Unicode, as is or in upper case, writes end labels right of the
  plot and transliterates text to ASCII.
* `char_figure()` renders a grid as a ggplot of text, in black and white, as
  a Commodore 64 screen or as that screen in two values (`"c64bw"`), and with
  `slot` fills a slide slot; `char_size()` gives the matching file size.

## Documentation

* A README, four vignettes ("Getting started with inferplotR", "Inference
  layers", "Character mode" and "The figure gallery"), a pkgdown site at
  <https://cttir.github.io/inferplotR/> and a `CITATION` file.
