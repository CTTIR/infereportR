# Branding

`render.R` draws the infereportR logo, a CTTIR v5 hex, entirely in R with the shared
CTTIR renderer bundled in `R/` and `design_tokens.csv` (from CTTIR/.github
`tools/branding`). The motif is a results table beside two bars joined by a significance bracket. The colour is the statistical
inference family (`methods`, #56B4E9), shared with inferguidR and infercalcR.

```sh
Rscript tools/branding/render.R /path/to/review   # review copy
Rscript tools/branding/render.R --apply           # update man/figures
```

`--apply` writes `man/figures/logo.svg` and `logo.png` (light), `logo-dark.svg`
and `logo-dark.png`, `logo-mono.svg` and the artwork-only `logo-symbol.svg`.
The name is set in the installed JetBrains Mono ExtraBold, the closest cut to
the supplied v5 hex names, so rendering needs that font. Re-rendering
produces identical files.
