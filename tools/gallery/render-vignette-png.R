#!/usr/bin/env Rscript
# Renders the light and dark PNGs of the twelve gallery figures shown in
# vignettes/figure-gallery.Rmd. Run from the package root with the package
# installed (or after devtools::load_all()):
#   Rscript tools/gallery/render-vignette-png.R
# The images are written by figure_gallery(png = TRUE, dpi = 150), the same
# call a deck would use, and only the PNG files are copied into the vignette.
dir <- file.path(tempdir(), "gallery")
files <- infereportR::figure_gallery(dir, modes = c("light", "dark"), png = TRUE, dpi = 150)
png <- files[files$format == "png", ]
for (mode in unique(png$mode)) {
  target <- file.path("vignettes", "figures", "gallery", mode)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(png$file[png$mode == mode], target, overwrite = TRUE)
  stopifnot(all(ok))
}
unlink(dir, recursive = TRUE)
writeLines(sprintf("%d PNG files written to vignettes/figures/gallery/", nrow(png)))
