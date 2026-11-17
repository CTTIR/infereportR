# Branding

`render.R` draws the inferplotR logo, a CTTIR v5 hex, entirely in R with the shared
CTTIR renderer bundled in `R/` and `design_tokens.csv` (copied unchanged from
infereportR, which takes them from CTTIR/.github `tools/branding`). The motif is
two bars, one solid and one diagonally hatched (the 1-bit fill idea), joined by a
comparison bracket (the inference layer), with a row of character-mode glyphs
under the axis. The colour is the statistical inference family (`methods`,
#56B4E9), shared with infercalcR, inferguidR and infereportR.

```sh
Rscript tools/branding/render.R /path/to/review   # review copy
Rscript tools/branding/render.R --apply           # update man/figures
```

`--apply` writes `man/figures/logo.svg` and `logo.png` (light), `logo-dark.svg`
and `logo-dark.png`, `logo-mono.svg` and the artwork-only `logo-symbol.svg`.
The name is set in the installed JetBrains Mono ExtraBold, so rendering needs
that font. Re-rendering produces identical files.
