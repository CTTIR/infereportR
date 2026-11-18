# Branding

`render.R` builds the inferplotR logo with hexmakR (>= 0.5.0) on the inferguidR
identity, so the frame, the `methods` family palette (#56B4E9), the name font and
the proportions match the other packages of the CTTIR inference suite. The motif
is two bars, one solid and one diagonally hatched (the 1-bit fill idea), joined
by a comparison bracket (the inference layer), with a row of character-mode
glyphs under the axis. It is drawn from hexmakR parts, coloured from the
design's own palette in each mode.

```sh
Rscript tools/branding/render.R /path/to/review   # review copy
Rscript tools/branding/render.R --apply           # update man/figures
```

`--apply` writes `man/figures/logo.svg` and `logo.png` (light), `logo-dark.svg`
and `logo-dark.png`, `logo-mono.svg` and the artwork-only `logo-symbol.svg`.
Favicons come from `pkgdown::build_favicons()`.
