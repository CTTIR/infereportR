#!/usr/bin/env Rscript
# Renders the light and dark PNGs of the twelve gallery figures shown in
# vignettes/figure-gallery.Rmd. Run from the package root with the package
# installed (or after devtools::load_all()):
#   Rscript tools/gallery/render-vignette-png.R
# The images are written by figure_gallery(png = TRUE, dpi = 120), the same
# call a deck would use, and only the PNG files are copied into the vignette.
# An image without color (all but the C64 screen) is then stored as an 8-bit
# grayscale PNG with the 'png' package, which keeps every pixel and halves the
# file size, so the vignette HTML stays small.
dir <- file.path(tempdir(), "gallery")
files <- infereportR::figure_gallery(dir, modes = c("light", "dark"), png = TRUE, dpi = 120)
png <- files[files$format == "png", ]
to_gray <- function(file) {
  a <- png::readPNG(file)
  if (length(dim(a)) != 3L || dim(a)[3] < 3L) return(FALSE)
  if (dim(a)[3] == 4L && any(a[, , 4] < 1)) return(FALSE)
  if (max(abs(a[, , 1] - a[, , 2]), abs(a[, , 2] - a[, , 3])) > 0) return(FALSE)
  png::writePNG(a[, , 1], file)
  TRUE
}
gray <- 0L
for (mode in unique(png$mode)) {
  target <- file.path("vignettes", "figures", "gallery", mode)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(png$file[png$mode == mode], target, overwrite = TRUE)
  stopifnot(all(ok))
  if (requireNamespace("png", quietly = TRUE)) {
    for (f in file.path(target, basename(png$file[png$mode == mode]))) gray <- gray + to_gray(f)
  }
}
unlink(dir, recursive = TRUE)
writeLines(sprintf("%d PNG files written to vignettes/figures/gallery/ (%d stored as grayscale)",
                   nrow(png), gray))
