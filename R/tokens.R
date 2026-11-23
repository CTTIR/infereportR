# Design tokens: the single source of truth for every colour, line weight and
# size literal in the package (SPEC section 3). No other file in R/ may hold a
# colour literal; the quality gate checks this.

.ir_ink <- "#000000"
.ir_paper <- "#FFFFFF"
# The background of transparent exports (save_figure(transparent = TRUE)).
.ir_transparent <- "transparent"
# VIC-II "blue" and "light blue" from the Pepto palette.
.ir_c64 <- c(background = "#352879", foreground = "#6C5EB5")

# Line weights in points.
.ir_rule_heavy <- 1.1
.ir_rule_hair <- 0.4
.ir_axis_line <- 0.5
.ir_data_line <- 0.8

# Lengths in millimetres, text sizes in points.
.ir_tick_length <- 1.1
.ir_text_min_pt <- 8
.ir_text_max_pt <- 14
.ir_base_size <- 9
# In-plot text (end labels, brackets, annotations); axis text stays at base.
.ir_label_size_pt <- 8
# 1.4 mm reads better at slide size; paper figures may pass 1.6.
.ir_hatch_spacing <- 1.4
.ir_halftone_spacing <- 0.75
.ir_endlabel_nudge <- 2.5
.ir_slide_text_width <- 134
.ir_char_width_em <- 0.6

# Unitless factors: brackets as fractions of the panel's y range, the means
# offset in x units, the end-label gap in text heights.
.ir_bracket_first <- 0.10
.ir_bracket_step <- 0.20
.ir_means_offset <- 0.28
.ir_endlabel_gap <- 1.15
# Inset in mm of a label placed at a panel edge (-Inf or Inf), so a corner
# label never touches the axis line or the panel border.
.ir_edge_inset <- 1

# Opt-in palettes beyond the two values (Bleisatz 1.3, Tufte options). "grey"
# adds the one grey for context; "tufte" adds red (first/last values, the one
# series to watch) and green (highs and lows), lighter on black paper. The
# C64 variants are the VIC-II grey, red and green of the Pepto palette.
.ir_palettes <- c("onebit", "grey", "tufte")
.ir_grey <- c(plex = "#999999", c64 = "#959595")
# Red and green per mode, in the order of .ir_modes (light, then dark).
.ir_red <- list(plex = c("#B3202A", "#E5484D"), c64 = c("#9F4E44", "#CB7E75"))
.ir_green <- list(plex = c("#2E7D32", "#5DB760"), c64 = c("#5CAB5E", "#9AE29B"))

# Tufte figure weights in points: thin series lines, context lines, the 1-bit
# hairline that stands in for the grey, the casing that separates a summary
# line from its context, and the paper-coloured grid through bars.
.ir_thin_line <- 0.6
.ir_context_line <- 0.3
.ir_fine_line <- 0.15
.ir_casing_line <- 2.6
.ir_paper_grid <- 0.9
# Dash length of geom_dotdash() in mm.
.ir_dotdash_length <- 1.3

# The C64 black-and-white screen, fixed in every mode.
.ir_screen <- c(background = "#000000", foreground = "#FFFFFF")

# The C64 type of the Tufte set (Bleisatz figures/c64/tufte/): C64 Pro Mono,
# not bundled, at 0.72 x the Plex size, so that its capitals are as tall as
# those of Plex. The characters the font lacks are typed out, glyph by glyph
# (two vectors, as names would not survive a C locale).
.ir_c64_family <- "C64 Pro Mono"
.ir_c64_scale <- 0.72
.ir_c64_glyphs <- c("≥", "≤", "±", "×", "−", "–", "—",
                    "·", "…")
.ir_c64_typed <- c(">=", "<=", "+/-", "x", "-", "-", "--", "/", "...")

.ir_linetypes <- c("solid", "22", "13", "1343", "73", "2262")
.ir_shapes <- c(16, 1, 15, 0, 17, 2, 18, 5)
.ir_hatches <- c("solid", "open", "hatch", "dots", "crosshatch", "backhatch")
.ir_hatch_types <- c(.ir_hatches, "horizontal", "vertical")

.ir_presets <- data.frame(
  preset = c("journal_column", "journal_page", "slide_full", "slide_side", "slide_half",
             "margin"),
  width = c(85, 174, 134, 86, 64, 52),
  # Slide slots: the Bleisatz frame body below a two-line title, minus one
  # source line, measured at 44 mm. The margin slot is the 52 mm margin of the
  # Tufte handout layout of bleisatz.cls.
  height = c(70, 100, 44, 44, 44, 40),
  stringsAsFactors = FALSE
)

# Character-mode glyph tables (SPEC section 4.10). Unicode glyphs are escaped so
# the source stays ASCII.
.ir_glyphs <- list(
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
.ir_lw <- function(pt) pt * 96 / (72.27 * ggplot2::.pt)

# Half-up rounding; round() rounds half to even, which is not position-stable.
.ir_round <- function(v) floor(v + 0.5)
