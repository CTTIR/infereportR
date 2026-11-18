# The single mode resolver. Light mode draws black on white, dark mode white on
# black; transparent modes leave the background empty. No other code branches
# on the mode: it takes ink, paper and knock from here.
#   ink   foreground (lines, text, glyphs)
#   paper background colour of the mode
#   knock background fill actually drawn: paper, or NA when transparent

.ip_modes <- c("light", "dark")

.ip_mode <- function(mode = "light", transparent = FALSE) {
  mode <- .ip_check_choice(mode, .ip_modes, "mode")
  .ip_check_flag(transparent, "transparent")
  pair <- switch(mode,
    light = c(ink = .ip_ink, paper = .ip_paper),
    dark = c(ink = .ip_paper, paper = .ip_ink)
  )
  list(ink = unname(pair[["ink"]]), paper = unname(pair[["paper"]]),
       knock = if (transparent) NA_character_ else unname(pair[["paper"]]))
}
