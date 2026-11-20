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
