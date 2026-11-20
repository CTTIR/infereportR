#!/usr/bin/env Rscript
# Renders the light PNGs of the Tufte set (figures 13-19) shown in
# vignettes/tufte.Rmd at 96 dpi. Run from the package root with the package
# installed (or after devtools::load_all()):
#   Rscript tools/gallery/render-tufte-png.R
dir <- file.path(tempdir(), "tufte")
files <- infereportR::figure_gallery(dir, modes = "light", png = TRUE, dpi = 96, set = "tufte")
png <- files[files$format == "png", ]
target <- file.path("vignettes", "figures", "tufte", "light")
dir.create(target, recursive = TRUE, showWarnings = FALSE)
stopifnot(all(file.copy(png$file, target, overwrite = TRUE)))
unlink(dir, recursive = TRUE)
writeLines(sprintf("%d PNG files written to %s", nrow(png), target))
