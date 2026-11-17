# inferplotR 0.1.0

First release.

## Inference layers

* `stat_means()` draws group means with confidence intervals from the
  one-sample t procedure of infercalcR.
* `stat_compare()` compares x groups pairwise with a named two-sample procedure
  (Welch, Student, Mann-Whitney or Brunner-Munzel), adjusts the comparison
  family as declared with `infercalcR::adjust_inference()` and draws brackets.
* `stat_omnibus()` labels a one-way analysis of means (classic or Welch) or a
  Kruskal-Wallis test; degrees of freedom are not shown, because infereportR
  0.1.0 has no display string for them.
* `stat_correlation()` labels a Pearson, Spearman or Kendall correlation and,
  with `fit = TRUE`, draws the fitted line and pointwise confidence band of
  infercalcR's linear prediction model.
* Every layer runs once per panel, takes all numbers from infercalcR and all
  display strings from infereportR, and needs an explicitly named procedure.
  With more than one comparison the adjustment must be declared.
* `inferplot()` adds the theme, the black-and-white scales, the observations,
  the means and the named tests to a plot in one call.

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
* `geom_endlabel()` labels lines at their last point instead of a legend.

## Export and APA check

* `figure_size()` holds size presets for journal columns and Bleisatz slide
  slots; `save_figure()` writes PDF, PNG, SVG or TIFF at those sizes.
* `check_figure()` audits a figure against the APA figure rules (text size,
  font, axis labels, embedded title, legend case) and the black-and-white rule.

## Character mode

* `char_grid()` redraws a single-panel ggplot on a monospaced character grid in
  ASCII or Unicode, as is or in upper case.
* `char_figure()` renders a grid as a ggplot of text, in black and white or as
  a Commodore 64 screen; `char_size()` gives the matching file size.

## Documentation

* A README, three vignettes ("Getting started with inferplotR", "Inference
  layers" and "Character mode"), a pkgdown site at
  <https://cttir.github.io/inferplotR/> and a `CITATION` file.
