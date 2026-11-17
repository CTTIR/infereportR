# Design tokens: the single source of truth for every colour, line weight and
# size literal in the package (SPEC section 3). No other file in R/ may hold a
# colour literal; the quality gate checks this.

.ip_ink <- "#000000"
.ip_paper <- "#FFFFFF"
# VIC-II "blue" and "light blue" from the Pepto palette.
.ip_c64 <- c(background = "#352879", foreground = "#6C5EB5")

# Line weights in points.
.ip_rule_heavy <- 1.1
.ip_rule_hair <- 0.4
.ip_axis_line <- 0.5
.ip_data_line <- 0.8

# Lengths in millimetres, text sizes in points.
.ip_tick_length <- 1.1
.ip_text_min_pt <- 8
.ip_text_max_pt <- 14
.ip_base_size <- 9
# In-plot text (end labels, brackets, annotations); axis text stays at base.
.ip_label_size_pt <- 8
# 1.4 mm reads better at slide size; paper figures may pass 1.6.
.ip_hatch_spacing <- 1.4
.ip_halftone_spacing <- 0.75
.ip_endlabel_nudge <- 2.5
.ip_slide_text_width <- 134
.ip_char_width_em <- 0.6

# Unitless factors: brackets as fractions of the panel's y range, the means
# offset in x units, the end-label gap in text heights.
.ip_bracket_first <- 0.10
.ip_bracket_step <- 0.20
.ip_means_offset <- 0.28
.ip_endlabel_gap <- 1.15

# The C64 black-and-white screen, fixed in every mode.
.ip_screen <- c(background = "#000000", foreground = "#FFFFFF")

.ip_linetypes <- c("solid", "22", "13", "1343", "73", "2262")
.ip_shapes <- c(16, 1, 15, 0, 17, 2, 18, 5)
.ip_hatches <- c("solid", "open", "hatch", "dots", "crosshatch", "backhatch")
.ip_hatch_types <- c(.ip_hatches, "horizontal", "vertical")

.ip_presets <- data.frame(
  preset = c("journal_column", "journal_page", "slide_full", "slide_side", "slide_half"),
  width = c(85, 174, 134, 86, 64),
  # Slide slots: the Bleisatz frame body below a two-line title, minus one
  # source line, measured at 44 mm.
  height = c(70, 100, 44, 44, 44),
  stringsAsFactors = FALSE
)

# Character-mode glyph tables (SPEC section 4.10). Unicode glyphs are escaped so
# the source stays ASCII.
.ip_glyphs <- list(
  ascii = list(
    points = c("*", "o", "+", "x", "#", "@", "%", "&"),
    line1 = c(horizontal = "-", rising = "/", vertical = "|", falling = "\\"),
    lines = c(".", ":", "+", "~", "=", "^", "'", "\""),
    fills = c("#", "/", "=", ":", "%", "+", "x", "o"),
    cursor = "_"
  ),
  unicode = list(
    points = c("\u25cf", "\u25cb", "\u25a0", "\u25a1", "\u25b2", "\u25b3", "\u25c6", "\u25c7"),
    line1 = c(horizontal = "\u2500", rising = "\u2571", vertical = "\u2502",
              falling = "\u2572"),
    lines = c(".", ":", "+", "~", "=", "^", "'", "\""),
    fills = c("\u2588", "\u2593", "\u2592", "\u2591", "%", "+", "x", "o"),
    cursor = "\u2588"
  )
)

# Points to ggplot2 linewidth: linewidth L draws L * .pt lwd units of 1/96 inch.
.ip_lw <- function(pt) pt * 96 / (72.27 * ggplot2::.pt)

# Half-up rounding; round() rounds half to even, which is not position-stable.
.ip_round <- function(v) floor(v + 0.5)
