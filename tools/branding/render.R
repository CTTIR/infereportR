# The inferplotR logo, built with hexmakR (>= 0.5.0) on the inferguidR identity,
# so frame, palette, name font and proportions match the CTTIR v5 `methods`
# family of the inference suite. The motif: two bars, one solid and one
# diagonally hatched (the 1-bit fill idea), joined by a comparison bracket (the
# inference layer), with a row of character-mode glyphs under the axis.
#
#   Rscript tools/branding/render.R /path/to/review   # review copy
#   Rscript tools/branding/render.R --apply           # update man/figures
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args)) stop("Supply an output directory or --apply.")
if (!requireNamespace("hexmakR", quietly = TRUE) ||
    utils::packageVersion("hexmakR") < "0.5.0") stop("Install hexmakR >= 0.5.0.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
repository <- dirname(dirname(dirname(normalizePath(script, mustWork = TRUE))))

inferplotR_logo <- function(mode, layout) {
  d <- hexmakR::hex_design("inferguidR", mode = mode, layout = layout, label = "inferplotR",
                           archive = FALSE)
  s <- hexmakR::hex_palette(d)
  # Strokes need contrast with the face: deep on light faces, light on dark.
  stroke <- if (mode == "dark") s$light else s$deep
  # Same footprint as the inferguidR compass: centred near y = 0.59 in the
  # lockup, at the frame centre in the artwork-only hex.
  dy <- if (layout == "hex") -.09 else 0
  base <- .45 + dy
  add <- function(d, type, ...) hexmakR::hex_add_part(d, type, ...)
  line <- function(d, x, y, col, lwd) add(d, "line", x = x, y = y, fill = "transparent",
                                          col = col, lwd = lwd)
  # Solid bar.
  d <- add(d, "rectangle", x = .41, y = base + .10, width = .13, height = .20,
           fill = s$mid, col = "transparent")
  # Hatched bar: face fill (open), outline and clipped diagonal hatch lines.
  x0 <- .525; x1 <- .655; top <- base + .30
  d <- add(d, "rectangle", x = (x0 + x1) / 2, y = (base + top) / 2, width = x1 - x0,
           height = top - base, fill = s$face, col = s$mid, lwd = 7)
  for (k in seq(-(x1 - x0) + .015, top - base - .015, by = .045)) {
    xs <- c(max(x0, x0 - k), min(x1, x0 + (top - base) - k))
    if (diff(xs) > .012) d <- line(d, xs, base + (xs - x0) + k, s$mid, 5)
  }
  # Axis and comparison bracket.
  d <- line(d, c(.30, .70), c(base, base), stroke, 8)
  b <- top + .055
  d <- line(d, c(.41, .41, .59, .59), c(b - .04, b, b, b - .04), stroke, 8)
  # Character-mode glyphs "* - / *" under the axis, drawn as strokes.
  g <- base - .055; h <- .018
  star <- function(d, cx) {
    for (a in c(90, 30, -30)) {
      r <- a * pi / 180
      d <- line(d, cx + c(-1, 1) * h * cos(r), g + c(-1, 1) * h * sin(r), s$mid, 5)
    }
    d
  }
  d <- star(d, .41)
  d <- line(d, c(.468, .502), c(g, g), s$mid, 5)
  d <- line(d, c(.538, .562), c(g - h, g + h), s$mid, 5)
  star(d, .62)
}

output <- if (identical(args, "--apply")) tempfile("inferplotR-logo-") else args
dir.create(output, recursive = TRUE, showWarnings = FALSE)
for (mode in c("light", "dark", "mono")) for (layout in c("hex", "lockup")) {
  stem <- paste0(layout, if (mode == "light") "" else paste0("-", mode))
  d <- inferplotR_logo(mode, layout)
  hexmakR::hex_export(d, file.path(output, paste0(stem, ".svg")))
  hexmakR::hex_export(d, file.path(output, paste0(stem, ".png")), width = 1600L)
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
