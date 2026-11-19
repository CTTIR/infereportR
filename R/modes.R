# The single mode resolver. Light mode draws black on white, dark mode white on
# black; transparent modes leave the background empty. No other code branches
# on the mode: it takes ink, paper and knock from here.
#   ink   foreground (lines, text, glyphs)
#   paper background colour of the mode
#   knock background fill actually drawn: paper, or NA when transparent

.ir_modes <- c("light", "dark")

.ir_mode <- function(mode = "light", transparent = FALSE) {
  mode <- .ir_check_choice(mode, .ir_modes, "mode")
  .ir_check_flag(transparent, "transparent")
  pair <- switch(mode,
    light = c(ink = .ir_ink, paper = .ir_paper),
    dark = c(ink = .ir_paper, paper = .ir_ink)
  )
  list(ink = unname(pair[["ink"]]), paper = unname(pair[["paper"]]),
       knock = if (transparent) NA_character_ else unname(pair[["paper"]]))
}
