# The inferplotR logo: a CTTIR v5 hex drawn entirely in R with the shared CTTIR
# renderer in R/ (primitives, palette, tokens). Statistical inference family
# (`methods`), shared with infercalcR, inferguidR and infereportR.
# Motif: two bars, one solid and one diagonally hatched (the 1-bit fill idea),
# joined by a comparison bracket (the inference layer), with a row of
# character-mode glyphs under the axis.
#
#   Rscript tools/branding/render.R /path/to/review   # review copy
#   Rscript tools/branding/render.R --apply           # update man/figures
#
# Requires the installed JetBrains Mono ExtraBold for the name: the supplied
# v5 names are heavier than the bundled SemiBold cut, and ExtraBold is the
# closest measured match.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args)) stop("Supply an output directory or --apply.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
branding <- dirname(normalizePath(script, mustWork = TRUE))
repository <- dirname(dirname(branding))
Sys.setenv(CTTIR_FONTCONFIG = "off")
source(file.path(branding, "R", "cttir_hex.R"))
if (!capabilities("cairo")) stop("Cairo graphics are required.")

installed <- systemfonts::system_fonts()
if (!any(installed$family == "JetBrains Mono" & installed$style == "ExtraBold")) {
  stop("Install JetBrains Mono ExtraBold.")
}
name_font <- "JetBrains Mono ExtraBold"
cttir_name <- function(name, y = .049, max_width = .90, size = 34, colour = "#212121",
                       font = name_font) {
  repeat {
    g <- grid::textGrob(name, gp = grid::gpar(fontfamily = name_font, fontsize = size))
    if (grid::convertWidth(grid::grobWidth(g), "npc", valueOnly = TRUE) <= max_width ||
        size <= 12) break
    size <- size - .25
  }
  grid::grid.text(name, .5, y, gp = grid::gpar(fontfamily = name_font, fontsize = size,
                                               col = colour))
  invisible(size)
}

inferplotR_motif <- function(s, small = FALSE) {
  g <- cttir_primitives(s, small)
  a <- s$mid; p <- s$paper; l <- s$light; d <- s$deep
  base <- .30
  # Solid bar.
  g$B(.37, base + .15, .17, .30, a, r = .015, shadow = TRUE)
  # Hatched bar: paper fill, outline and diagonal lines clipped to the bar.
  x0 <- .545; x1 <- .715; y1 <- base + .44
  if (small) {
    g$B((x0 + x1) / 2, (base + y1) / 2, x1 - x0, y1 - base, l, r = .015, shadow = TRUE)
  } else {
    g$B((x0 + x1) / 2, (base + y1) / 2, x1 - x0, y1 - base, p, r = .015, col = a, lwd = 4,
        shadow = TRUE)
    step <- .055
    for (k in seq(-(x1 - x0), y1 - base, by = step)) {
      # Line y - base = (x - x0) + k, clipped to the rectangle.
      xs <- c(max(x0, x0 - k), min(x1, x0 + (y1 - base) - k))
      if (xs[2] - xs[1] > .01) g$L(xs, base + (xs - x0) + k, a, 3)
    }
  }
  # Axis.
  g$L(c(.22, .78), c(base, base), d, if (small) 5 else 4)
  # Comparison bracket.
  top <- y1 + .07
  g$L(c(.37, .37, .63, .63), c(top - .045, top, top, top - .045), d, if (small) 6 else 5)
  if (!small) {
    grid::grid.text("* - / *", .5, base - .075,
                    gp = grid::gpar(fontfamily = name_font, fontsize = 17, col = a))
  }
}
v5_motif <- cttir_motif
cttir_motif <- function(name, s, small = FALSE) {
  if (name == "inferplotR") inferplotR_motif(s, small) else v5_motif(name, s, small)
}

output <- if (identical(args, "--apply")) tempfile("inferplotR-logo-") else args
dir.create(output, recursive = TRUE, showWarnings = FALSE)
font <- cttir_token("type", "brand_font")
for (mode in c("light", "dark", "mono")) for (layout in c("hex", "lockup")) {
  stem <- paste0(layout, if (mode == "light") "" else paste0("-", mode))
  d <- cttir_dimensions(layout)
  draw <- function() cttir_draw("inferplotR", "methods", mode, layout)
  cttir_device(function() grDevices::svg(file.path(output, paste0(stem, ".svg")),
    width = d$width, height = d$height, bg = "transparent", family = font), draw)
  cttir_device(function() grDevices::png(file.path(output, paste0(stem, ".png")),
    width = 1600L, height = 1848L, res = 1600 / d$width, bg = "transparent",
    type = "cairo", antialias = "gray"), draw)
}

if (identical(args, "--apply")) {
  targets <- c(
    "lockup.svg" = "man/figures/logo.svg",
    "lockup.png" = "man/figures/logo.png",
    "lockup-dark.svg" = "man/figures/logo-dark.svg",
    "lockup-dark.png" = "man/figures/logo-dark.png",
    "lockup-mono.svg" = "man/figures/logo-mono.svg",
    "hex.svg" = "man/figures/logo-symbol.svg")
  dir.create(file.path(repository, "man", "figures"), showWarnings = FALSE)
  for (from in names(targets)) {
    if (!file.copy(file.path(output, from), file.path(repository, targets[[from]]),
                   overwrite = TRUE)) stop("Could not update ", targets[[from]])
  }
  unlink(output, recursive = TRUE)
  message("Updated the inferplotR logo in man/figures.")
} else message("inferplotR logo rendered into ", normalizePath(output))
