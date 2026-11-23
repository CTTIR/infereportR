# The single mode resolver. Light mode draws black on white, dark mode white on
# black; transparent modes leave the background empty. No other code branches
# on the mode: it takes its colours from here.
#   ink     foreground (lines, text, glyphs)
#   paper   background colour of the mode
#   knock   background fill actually drawn: paper, or NA when transparent
#   grey    the context grey; in "onebit" the ink (drawn as a hairline)
#   red     Tufte's red ("tufte" only; ink otherwise)
#   green   Tufte's green ("tufte" only; ink otherwise)
#   palette the palette name, for the few places that draw a 1-bit substitute
# Grey, red and green are the VIC-II colors with c64 = TRUE, which is the
# default while the C64 type is in use (.ir_with_type("c64", ...)).

.ir_modes <- c("light", "dark")

.ir_mode <- function(mode = "light", transparent = FALSE, palette = "onebit",
                     c64 = .ir_type_c64()) {
  mode <- .ir_check_choice(mode, .ir_modes, "mode")
  .ir_check_flag(transparent, "transparent")
  palette <- .ir_check_choice(palette, .ir_palettes, "palette")
  .ir_check_flag(c64, "c64")
  pair <- switch(mode,
    light = c(ink = .ir_ink, paper = .ir_paper),
    dark = c(ink = .ir_paper, paper = .ir_ink)
  )
  ink <- unname(pair[["ink"]])
  font <- if (c64) "c64" else "plex"
  grey <- if (palette == "onebit") ink else unname(.ir_grey[[font]])
  i <- match(mode, .ir_modes)
  red <- if (palette == "tufte") .ir_red[[font]][[i]] else ink
  green <- if (palette == "tufte") .ir_green[[font]][[i]] else ink
  list(ink = ink, paper = unname(pair[["paper"]]),
       knock = if (transparent) NA_character_ else unname(pair[["paper"]]),
       grey = grey, red = red, green = green, palette = palette)
}

# The colours a palette may use in a mode (check_figure()'s palette rule).
.ir_palette_colours <- function(palette = "onebit", c64 = FALSE) {
  out <- c(.ir_ink, .ir_paper)
  for (mode in .ir_modes) {
    m <- .ir_mode(mode, palette = palette, c64 = c64)
    out <- c(out, m$grey, m$red, m$green)
  }
  unique(out)
}
