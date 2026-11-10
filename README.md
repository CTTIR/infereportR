# infereportR <picture class="cttir-readme-logo"><source class="cttir-logo-dark-source" media="(prefers-color-scheme: dark)" srcset="man/figures/logo-dark.png" /><img src="man/figures/logo.png" align="right" alt="infereportR logo" width="120" height="139" /></picture>

Tables, manuscript-ready values and ggplot annotations from finalized statistical
results. Numerical inference belongs to **infercalcR**; guidance and prospective
planning belong to **inferguidR**. Plain reporting requires infercalcR and
R-distributed packages. It does not require inferguidR, ggplot2, a table framework
or testthat.

Install the exact infercalcR source candidate first, then this package with
`R CMD INSTALL`. These development repositories may require authenticated access;
no CRAN availability is implied.

## Tables and text

```r
library(infercalcR)
library(infereportR)

# A finalized result can be rendered without observations or a model object.
r <- new_inference_result(
  data.frame(row_id = "A-B", variable_id = "outcome", component = "contrast",
    estimand = "mean difference", contrast = "A minus B", estimate = -1.2,
    conf.low = -2.1, conf.high = -0.3, conf.level = 0.95,
    interval.method = "Welch t", interval.scale = "difference",
    interval.scope = "pointwise", p.value = 0.012, status = "ok"),
  list(method_id = "welch_example", result_id = "study-1"))
report_table(r)
report_text(r)
report_text(r, template = "{contrast}: {estimate} {ci}; {p}")
```

The table retains analytical precision in its original columns. Display values
have separate `_display` columns; `p_selected` and `p_selection` identify the
chosen stored p-value. `report_metadata()` returns provenance, population and
complete-family adjustment metadata. `p = "adjusted"` errors when adjusted
values are absent. Filter by `rows` only after adjustment has been performed on
the full declared family in infercalcR.

A tiny p-value or stored zero displays a threshold, never `p = 0`. Missing and
unresolved results cannot become `ns`. Full text retains the supplied estimand,
contrast, interval policy, confidence level, sample size and status. It makes no
clinical or causal interpretation. The same formatters supply tables, text and
plot labels. Decimal-mark changes affect presentation only.

## Descriptive summaries

```r
d <- data.frame(outcome = c(1, 2, NA, 4), group = c("A", "A", "B", "B"))
report_descriptive(d, group = "group", units = c(outcome = "mm"))
```

The raw-data route delegates to `infercalcR::describe_data()` and performs no
hypothesis test. Ordered factors remain categorical. Output includes group and
variable denominators, missing counts, labels and units. To add already computed
inference, supply `inference`, an explicit `mapping` data.frame with
`variable_id`, `result_id`, `row_id`, and an `inference_population` label. At most
one inference row per variable is supported. Descriptive and inferential
populations remain separately labeled.

## ggplot annotations

```r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  d <- data.frame(group = rep(c("A", "B"), each = 3), value = c(1, 3, 4, 2, 5, 6))
  p <- ggplot2::ggplot(d, ggplot2::aes(group, value)) + ggplot2::geom_point()
  positions <- data.frame(row_id = "A-B", xmin = 1, xmax = 2, y_position = 7)
  p + stat_inference(r, positions, label = "estimate_ci_p")
}
```

Positions must match selected stable row IDs exactly. Named `group1`/`group2`
endpoints require `groups` in the plotted factor-level order; changing visual
order does not reverse the scientific contrast. Numeric endpoints support
explicit dodge positions. Omnibus annotations use `x`, `y_position` and
`type = "text"`. Nonsignificant rows are visible by default.

All coordinates and offsets are in original data units. A `y_position` of 100
maps to 2 under `scale_y_log10()` once. Cartesian plots and `coord_flip()` are
supported. For horizontal labels use `text_hjust = -0.1, text_vjust = 0.5`
and choose adequate scale expansion. `annotate_inference(plot, ...)` checks an existing coordinate system;
later-added coordinates cannot be guarded. Arbitrary nonlinear coordinates,
automatic placement and automatic dodge/free-scale inference are outside scope.

Facet-specific results require `facet_vars`, with identical nonmissing keys in
stored result rows and positions. Missing declared keys error instead of
broadcasting. A list of layers cannot infer undeclared subgroup meaning or
validate arbitrary facets added later; maintain this explicit mapping contract.

## Precomputed DescrTab2 objects

The optional adapter is pinned to **DescrTab2 2.1.26**, upstream commit
`f157380e7abcb7964612a480e09e59a45e7a01be` (source SHA-256
`354157ec6b7ceef2cb328395e7fd20b0f13dff7715525284c5deacd2ba076f27`).
[CRAN archived DescrTab2](https://CRAN.R-project.org/package=DescrTab2) on
2025-10-18. Use the pinned upstream source and its declared dependencies for this
optional lane; installation is never attempted by package functions.

```r
# native is an already computed DescrTab2::descr(...) result.
# imported <- import_descrtab2(native)
# render_descrtab2(imported, "table")
# render_descrtab2(imported, "text")
# render_descrtab2(imported, "html")
```

`descr()` can calculate tests even with `print_p = FALSE`. This adapter never
calls it during import or rendering. It invokes the public native numeric
print method and formats stored cells, avoiding custom native formatting and
reshaping callbacks. Row-level group membership, input options and presentation callbacks are removed
while aggregate counts and levels remain. Native method labels and numbers are retained;
there is no normalized inference conversion. In the pinned version, native
`CI_upper`/`CI_lower` column names reverse the stored CI-vector order. They are
labeled `native_CI_first`/`native_CI_second` to avoid misleading upper/lower
headings, with original names retained in metadata; no normalized inference
conversion is claimed.
Use the shared result route for normalized estimates and intervals.

## Verification

All new tests are ordinary `tests/*.R` scripts against installed packages.
They cover precision, interval/p-value semantics, no-computation guards,
explicit populations/joins, actual ggplot builds and drawing, and the pinned
native import/render route. `INFER_SUITE_FULL=true` requires ggplot2 and
DescrTab2; absence fails that lane. Minimal-library runs test explicit missing
feature errors and label unexecuted integrations as SKIP. Skips do not establish
backend support. Set `INFER_SUITE_PLOT_DIR` and `INFER_SUITE_REPORT_DIR` to retain
plot/native-output artifacts during verification.
