# The infereportR logo: a CTTIR v5 hex drawn entirely in R with the shared CTTIR
# renderer in R/ (primitives, palette, tokens). Statistical inference family
# (`methods`), shared with inferguidR and infercalcR.
# Motif: a results table beside two bars joined by a significance bracket,
# the finished values a report carries.
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

infereportR_motif <- function(s, small = FALSE) {
  g <- cttir_primitives(s, small)
  a <- s$mid; p <- s$paper; l <- s$light; d <- s$deep
  g$B(.35, .51, .42, .58, p, r = .03, shadow = TRUE)
  g$B(.35, .755, .42, .09, d, r = .025)
  rows <- c(.63, .52, .41, .30)
  for (y in rows) {
    g$B(.245, y, .14, .055, a, r = .012)
    g$B(.435, y, .14, .055, if (y == .52) d else cttir_mix(a, p, .45), r = .012)
  }
  if (!small) for (y in c(.575, .465, .355)) g$L(c(.16, .54), c(y, y), cttir_alpha(d, .35), 1.2)
  g$L(c(.61, .89), c(.19, .19), l, if (small) 5 else 4)
  g$B(.685, .31, .10, .24, a, r = .015, shadow = TRUE)
  g$B(.815, .395, .10, .41, l, r = .015, shadow = TRUE)
  g$L(c(.685, .685, .815, .815), c(.69, .745, .745, .69), l, if (small) 6 else 5)
  if (!small) for (t in c(90, 210, 330)) {
    r <- .04; x <- .75; y <- .83
    g$L(x + c(-1, 1) * r * cos(t * pi / 180), y + c(-1, 1) * r * sin(t * pi / 180), a, 6)
  }
}
v5_motif <- cttir_motif
cttir_motif <- function(name, s, small = FALSE) {
  if (name == "infereportR") infereportR_motif(s, small) else v5_motif(name, s, small)
}

output <- if (identical(args, "--apply")) tempfile("infereportR-logo-") else args
dir.create(output, recursive = TRUE, showWarnings = FALSE)
font <- cttir_token("type", "brand_font")
for (mode in c("light", "dark", "mono")) for (layout in c("hex", "lockup")) {
  stem <- paste0(layout, if (mode == "light") "" else paste0("-", mode))
  d <- cttir_dimensions(layout)
  draw <- function() cttir_draw("infereportR", "methods", mode, layout)
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
  message("Updated the infereportR logo in man/figures.")
} else message("infereportR logo rendered into ", normalizePath(output))
