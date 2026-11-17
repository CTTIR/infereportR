# inferplotR <picture class="cttir-readme-logo"><source class="cttir-logo-dark-source" media="(prefers-color-scheme: dark)" srcset="man/figures/logo-dark.png" /><img src="man/figures/logo.png" align="right" alt="inferplotR logo" width="120" height="139" /></picture>

inferplotR is the figure layer of the CTTIR inference suite. It is a ggplot2
extension that you add with `+` like a theme, but its layers compute their
statistics through **infercalcR** and label them through **infereportR**, and the
full results stay retrievable from the figure. The figures are strictly black
and white, follow the APA figure rules for papers and the Bleisatz grid for
slides, and can be redrawn on a character grid in 1980s home-computer style.

| Package | Role |
|---|---|
| infercalcR | computes results (estimates, intervals, p-values, provenance) |
| inferguidR | plans analyses (power, procedure choice) |
| infereportR | formats results as tables, text and ggplot annotations |
| **inferplotR** | **draws black-and-white ggplot2 figures whose inference layers compute through infercalcR and label through infereportR** |

inferplotR never computes or formats a statistic itself. Every estimate,
interval, test statistic, p-value and adjustment comes from infercalcR, and
every number in a label is an infereportR display string.

## Installation

R 4.1.0 or later is required. The packages are not on CRAN yet. Install
infercalcR, infereportR and inferplotR from GitHub, in that order:

```r
install.packages("remotes")
remotes::install_github("CTTIR/infercalcR")
remotes::install_github("CTTIR/infereportR")
remotes::install_github("CTTIR/inferplotR", build_vignettes = TRUE)
```

The documentation site, with the reference and three vignettes, is at
<https://cttir.github.io/inferplotR/>. "Getting started with inferplotR" covers
the theme, scales, labels, export and the APA check; "Inference layers" and
"Character mode" cover the rest.

## One line on top of ggplot

```r
library(ggplot2)
library(inferplotR)

p <- ggplot(PlantGrowth, aes(group, weight)) +
  inferplot(compare = "t_means_two_independent_welch", reference = "ctrl",
            adjust = "holm", omnibus = "anova_one_way") +
  labs(x = "Group", y = "Dried weight (g)")
p
```

`inferplot()` returns a plain list of ggplot2 components: the figure theme, the
black-and-white scales, the observations as open circles, the group means with
95% confidence intervals and the tests you name. Here those are a one-way
analysis of means and Welch t-tests of each treatment against the control,
Holm-adjusted as one family. No test is added unless its procedure is named,
and with more than one comparison the adjustment must be declared (`"none"` is
allowed). The same layers are available one by one as `stat_means()`,
`stat_compare()`, `stat_omnibus()` and `stat_correlation()`. Each panel of a
faceted plot is its own analysis.

The results behind the labels can be read back from the figure:

```r
names(inference_results(p))
#> [1] "inferplotR:means:t_mean_one_sample:panel1:ctrl"
#> [2] "inferplotR:means:t_mean_one_sample:panel1:trt1"
#> [3] "inferplotR:means:t_mean_one_sample:panel1:trt2"
#> [4] "inferplotR:omnibus:anova_one_way:panel1"
#> [5] "inferplotR:compare:t_means_two_independent_welch:panel1"

figure_report(p)[, c("figure_layer", "contrast", "estimate_display", "p_display")]
#>   figure_layer        contrast estimate_display p_display
#> 1            2            <NA>             5.03 p < 0.001
#> 2            2            <NA>             4.66 p < 0.001
#> 3            2            <NA>             5.53 p < 0.001
#> 4            3            <NA>      unavailable p = 0.016
#> 5            4 trt1 minus ctrl           -0.371 p = 0.250
#> 6            4 trt2 minus ctrl            0.494 p = 0.096
```

`inference_results()` returns the infercalcR result objects, so they can be
validated or passed to `infereportR::report_text()`. `figure_report()` binds
their `infereportR::report_table()` rows into one table, with the layer and
panel each row came from, and reports adjusted p-values where an adjustment was
made. Results computed elsewhere can still be drawn on the same figure with
`infereportR::stat_inference()`.

## Black and white by design

```r
chicks <- aggregate(weight ~ Diet + Time, data = ChickWeight, FUN = mean)
chicks$Diet <- paste("Diet", chicks$Diet)

q <- ggplot(chicks, aes(Time, weight, linetype = Diet, label = Diet)) +
  geom_line(show.legend = FALSE) +
  geom_endlabel() +
  scale_linetype_figure() +
  coord_cartesian(clip = "off") +
  theme_figure("paper", legend = "none") +
  theme(plot.margin = margin(2, 16, 2, 2, unit = "mm")) +
  labs(x = "Days since birth", y = "Mean weight (g)")

save_figure(q, file.path(tempdir(), "chicks.pdf"), preset = "journal_column")
save_figure(q + theme_figure("slide", legend = "none") +
              theme(plot.margin = margin(1, 16, 1, 1, unit = "mm")),
            file.path(tempdir(), "chicks-slide.png"), preset = "slide_side")
```

- `theme_figure()` is a complete theme with two targets. `"paper"` follows the
  APA figure rules: all text between 8 and 14 pt, one font family, the legend
  inside the panel. `"slide"` matches the Bleisatz Beamer theme: 1-bit, 1 mm
  margins and IBM Plex when it is installed. There is no gray anywhere; grid
  lines, when requested, are dotted black lines.
- `scale_linetype_figure()`, `scale_shape_figure()` and `scale_colour_ink()`
  tell groups apart by line type and point shape instead of color.
  `scale_fill_hatch()` and `hatch_pattern()` fill areas with solid, open,
  hatched, dotted and crosshatched grid patterns, without any extra package.
  The scales never recycle values; too many groups is an error.
- `geom_endlabel()` labels each line at its last point instead of a legend.
- `figure_size()` holds size presets: journal single and double columns (85 and
  174 mm wide) and the Bleisatz slots `slide_full`, `slide_side` and
  `slide_half` (134, 86 and 64 mm wide). `save_figure()` writes PDF (with
  `cairo_pdf()`), PNG, SVG or TIFF at a preset or at an explicit size.

## APA check

`check_figure()` audits a plot against the APA Style figure rules and the
package's black-and-white rule and returns one row per check:

```r
check_figure(ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red"))[, 1:3]
#>            check     source status
#> 1      text_size      APA 7   pass
#> 2    font_family      APA 7   pass
#> 3     two_values inferplotR   fail
#> 4    axis_labels      APA 7   note
#> 5 embedded_title      APA 7   pass
#> 6    legend_case      APA 7   pass
```

The `detail` column names the offenders: here the red points, the gray panel
and the axis labels `wt` and `mpg`, which give no unit. The checks cover text
size (8 to 14 pt), sans serif fonts, black and white only, labeled axes with
units, no title inside the image and title-case legends. The figure number,
title and note belong to the manuscript and are outside the audit.

## Character mode

`char_grid()` redraws a single-panel ggplot on a monospaced character grid:

```r
d <- aggregate(weight ~ Time, data = ChickWeight, FUN = mean)
g <- ggplot(d, aes(Time, weight)) +
  geom_line() +
  geom_point() +
  labs(x = "Days since birth", y = "Mean weight (g)")
char_grid(g, width = 48, height = 14)
```

```
Mean weight (g)
    |                                       *
200 +                                    /*/
    |                                 /*/
    |                             /*//
150 +                          /*/
    |                      /*//
100 +               /*---*/
    |            /*/
 50 +    /*---*//
    |  */
    +--+--------+--------+-------+--------+----
       0        5        10      15       20
                  Days since birth
```

`charset = "unicode"` draws box-drawing lines and block fills, and
`case = "upper"` writes all text in capitals. `char_figure()` turns the grid
back into a ggplot of glyphs, either 1-bit black on white or, with
`style = "c64"`, as the screen of a Commodore 64 after power-on: light blue
characters on a blue screen inside a light blue border, ending with `READY.`
and a cursor. `char_size()` gives the file size at which the glyphs line up,
for `save_figure()`.

## Verification

`tools/gate.R` is the quality gate for every change. Its `quick` mode checks that
the documentation is in sync with the roxygen comments, scans `R/` for banned
patterns (color literals outside the design tokens, non-ASCII characters,
`sapply()`, `T`/`F`, `library()` calls), scans tracked files and commit
messages for attribution lines and runs the testthat suite without failures,
errors or warnings. The `full` mode adds `R CMD check --as-cran` with zero
errors, warnings and notes, vignettes included. The `release` mode adds line
coverage of at least 90% and the CRAN incoming checks. GitHub Actions runs
R CMD check on Linux (R release, devel and oldrel-1), Windows and macOS, and a
minimal lane on R 4.1 without suggested packages.

The tests show that:

- every number drawn by an inference layer equals a direct infercalcR call on
  the same data (means, comparison families with their adjustment, omnibus
  tests, correlations and the fitted line), and every label is an infereportR
  display string, never `ns`;
- procedures and adjustments must be explicit, and errors have classes and keep
  the infercalcR condition as parent;
- facets give one analysis per panel, and `inference_results()` and
  `figure_report()` return what the layers computed;
- the theme resolves all text to 8 to 14 pt and uses only black and white; the
  scales follow the specified sequences and refuse to recycle; hatched plots
  render to PDF and PNG; the size presets have the specified sizes;
- `check_figure()` passes conforming figures and names the offenders in
  non-conforming ones;
- character grids are deterministic (snapshot tests of fixed plots), and
  `char_figure()` and `char_size()` agree on the grid size in both styles.

## Acknowledgments and sources

inferplotR is part of an inference suite with infercalcR, inferguidR and
infereportR. It builds on the work of others, which we gratefully acknowledge
and cite here.

### Rules and tokens

The text-size, font, legend, axis and title rules of the theme and of
`check_figure()` follow the APA Style page on figures [1]. The slide target
shares its 1-bit tokens (rules, hairlines, text width) with the Bleisatz Beamer
theme [7]. Text uses IBM Plex [5] when it is installed; no font is bundled. The
two colors of the C64 style are the VIC-II "blue" and "light blue" of the
Pepto palette by Philip Timmermann [4].

### Software

inferplotR is written in R [3] and extends ggplot2 [2]; its ggproto objects
were designed from the public ggplot2 extension guide [9]. Hatch fills use the
grid patterns of R 4.1.0 and later. All statistics come from infercalcR and all
display strings from infereportR [8]. Use `citation("inferplotR")` for this
package and `citation()` or `citation("<package>")` for R and the packages you
used.

### Related work

Several packages showed what figures for statistical reports can look like and
inspired this one [6]: the `theme_apa()` themes of jtools and papaja, the
pattern fills of fillpattern and ggpattern, the comparison brackets of ggsignif
and of `stat_compare_means()` in ggpubr, the statistics in figures of
ggstatsplot, and the terminal rendering of ggplots by `ggplotcli()` in plotcli.
They are credited as inspiration only. inferplotR does not import them, and
every function was written from scratch: no code, tests, documentation or
examples of theirs were copied (clean-room). inferplotR differs from them in
that it runs only explicitly named procedures, takes every number and display
string from infercalcR and infereportR, keeps the results retrievable from the
figure, shares its 1-bit tokens with a Beamer theme, audits figures against the
APA rules and renders character mode as a figure.

### References

1. American Psychological Association. *Figures*. APA Style, 7th ed. <https://apastyle.apa.org/style-grammar-guidelines/tables-figures/figures>.
2. Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer, New York. <https://doi.org/10.1007/978-3-319-24277-4>.
3. R Core Team. *R: A Language and Environment for Statistical Computing*. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>. Grid patterns since R 4.1.0.
4. Timmermann P ("Pepto"). VIC-II color analysis (Pepto palette), as shipped with the VICE emulator in `pepto-pal.vpl`: blue `35 28 79`, light blue `6C 5E B5`. <https://sources.debian.org/src/vice/3.9%2Bdfsg-1/data/CBM-II/pepto-pal.vpl>.
5. IBM. *IBM Plex* typeface. SIL Open Font License. <https://github.com/IBM/plex>.
6. Related work, credited as inspiration only: jtools and papaja (`theme_apa()`), fillpattern and ggpattern (pattern fills), ggsignif and ggpubr (`stat_compare_means()`, comparison brackets), ggstatsplot (statistics in figures), plotcli (`ggplotcli()`, terminal rendering of ggplots).
7. CTTIR. *Bleisatz* Beamer theme, the slide counterpart whose tokens this package shares.
8. Heller R. *infercalcR* and *infereportR*, R packages. <https://github.com/CTTIR/infercalcR>, <https://github.com/CTTIR/infereportR>.
9. ggplot2 authors. *Extending ggplot2*. Vignette of the ggplot2 package: `vignette("extending-ggplot2", package = "ggplot2")`.
