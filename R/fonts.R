.ir_font_cache <- new.env(parent = emptyenv())

.ir_has_font <- function(family) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(FALSE)
  fonts <- tryCatch(systemfonts::system_fonts(), error = function(e) NULL)
  if (is.null(fonts) || !nrow(fonts)) return(FALSE)
  family %in% fonts$family
}

#' Font family for figures
#'
#' Returns the font family that the figure layer uses for figure text: IBM Plex Sans
#' or IBM Plex Mono when the font is installed and reported by 'systemfonts',
#' otherwise the generic R families `"sans"` or `"mono"`. The function never
#' fails on a missing font. Fonts are not bundled and nothing is registered.
#'
#' IBM Plex ships with TeX Live (`fonts/opentype/ibm/plex`). To make it
#' available to R through fontconfig, copy the Sans, Sans Condensed and Mono
#' OTF files to `~/.fonts` and run `fc-cache -f`; detection uses 'systemfonts'.
#'
#' `type = "etbb"` is the serif of the Tufte option of Bleisatz
#' (`font=etbb`): it returns `"ETbb"` or `"ET Book"`, whichever 'systemfonts'
#' reports, otherwise the generic `"serif"`. ETbb ships with TeX Live
#' (`texmf-dist/fonts/opentype/public/etbb/`, the `ETbb-*.otf` files); install
#' them for the system as above. A serif face fails the APA sans-serif rule
#' of [check_figure()] (`font_family`), so use it for Tufte handouts and decks
#' set in ET Book, not for journal figures.
#'
#' The lookup is cached for the session; `reset = TRUE` clears the cache, for
#' example after installing a font.
#'
#' @param type `"sans"` (default), `"mono"` or `"etbb"`.
#' @param reset If `TRUE`, clear the cached lookup before looking up again.
#' @return A single string, a font family name.
#' @seealso `theme_figure()` and `char_figure()` use this family by default.
#' @examples
#' figure_font()
#' figure_font("mono")
#' figure_font("etbb")
#' @export
figure_font <- function(type = c("sans", "mono", "etbb"), reset = FALSE) {
  type <- .ir_check_choice(type, c("sans", "mono", "etbb"), "type")
  .ir_check_flag(reset, "reset")
  if (reset) rm(list = ls(.ir_font_cache), envir = .ir_font_cache)
  cached <- .ir_font_cache[[type]]
  if (!is.null(cached)) return(cached)
  if (type == "etbb") {
    found <- Filter(function(f) isTRUE(.ir_has_font(f)), c("ETbb", "ET Book"))
    family <- if (length(found)) found[[1L]] else "serif"
  } else {
    plex <- c(sans = "IBM Plex Sans", mono = "IBM Plex Mono")[[type]]
    family <- if (isTRUE(.ir_has_font(plex))) plex else type
  }
  assign(type, family, envir = .ir_font_cache)
  family
}

# The type of the figure text. Plex by default; while the C64 Tufte set is
# drawn (figure_gallery(font = "c64") and the C64 gallery deck), the text
# family, the text sizes, the face and the palette colors of every figure
# function switch to the C64 type, and back afterwards. The state holds the
# resolved type: list(c64, real, family, scale), or NULL for Plex.
.ir_type_state <- new.env(parent = emptyenv())

# Resolved once per use: C64 Pro Mono when 'systemfonts' reports it, else
# the figure's mono family at the Plex sizes, as char_figure() falls back.
.ir_resolve_type <- function(font) {
  if (!identical(font, "c64")) return(NULL)
  real <- isTRUE(.ir_has_font(.ir_c64_family))
  list(c64 = TRUE, real = real, family = if (real) .ir_c64_family else figure_font("mono"),
       scale = if (real) .ir_c64_scale else 1)
}

.ir_type <- function() {
  ty <- .ir_type_state$type
  if (is.null(ty)) list(c64 = FALSE, real = FALSE, family = NULL, scale = 1) else ty
}

.ir_type_c64 <- function() isTRUE(.ir_type_state$type$c64)

# Evaluates code with the type of `font` ("plex" or "c64"), restoring the
# previous type on exit, also after an error.
.ir_with_type <- function(font, code) {
  old <- .ir_type_state$type
  .ir_type_state$type <- .ir_resolve_type(font)
  on.exit(.ir_type_state$type <- old, add = TRUE)
  force(code)
}

# The sans family of figure text in the current type.
.ir_sans_family <- function() {
  if (.ir_type_c64()) .ir_type_state$type$family else figure_font("sans")
}

# In-plot label size in points in the current type.
.ir_label_pt <- function() .ir_label_size_pt * .ir_type()$scale

# Text the C64 type cannot set: every character it lacks typed out. Matched
# by code point, so it works in a C locale too.
.ir_c64_text <- function(x) {
  vapply(x, function(s) {
    cp <- if (is.na(s)) NA_integer_ else utf8ToInt(enc2utf8(s))
    if (anyNA(cp)) return(s)
    hit <- match(cp, .ir_c64_glyphs)
    if (all(is.na(hit))) return(s)
    parts <- intToUtf8(cp, multiple = TRUE)
    parts[!is.na(hit)] <- .ir_c64_typed[hit[!is.na(hit)]]
    paste(parts, collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}
