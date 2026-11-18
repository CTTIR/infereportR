.ip_font_cache <- new.env(parent = emptyenv())

.ip_has_font <- function(family) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(FALSE)
  fonts <- tryCatch(systemfonts::system_fonts(), error = function(e) NULL)
  if (is.null(fonts) || !nrow(fonts)) return(FALSE)
  family %in% fonts$family
}

#' Font family for figures
#'
#' Returns the font family that inferplotR uses for figure text: IBM Plex Sans
#' or IBM Plex Mono when the font is installed and reported by 'systemfonts',
#' otherwise the generic R families `"sans"` or `"mono"`. The function never
#' fails on a missing font. Fonts are not bundled and nothing is registered.
#'
#' IBM Plex ships with TeX Live (`fonts/opentype/ibm/plex`). To make it
#' available to R through fontconfig, copy the Sans, Sans Condensed and Mono
#' OTF files to `~/.fonts` and run `fc-cache -f`; detection uses 'systemfonts'.
#'
#' The lookup is cached for the session; `reset = TRUE` clears the cache, for
#' example after installing a font.
#'
#' @param type `"sans"` (default) or `"mono"`.
#' @param reset If `TRUE`, clear the cached lookup before looking up again.
#' @return A single string, a font family name.
#' @seealso `theme_figure()` and `char_figure()` use this family by default.
#' @examples
#' figure_font()
#' figure_font("mono")
#' @export
figure_font <- function(type = c("sans", "mono"), reset = FALSE) {
  type <- .ip_check_choice(type, c("sans", "mono"), "type")
  .ip_check_flag(reset, "reset")
  if (reset) rm(list = ls(.ip_font_cache), envir = .ip_font_cache)
  cached <- .ip_font_cache[[type]]
  if (!is.null(cached)) return(cached)
  plex <- c(sans = "IBM Plex Sans", mono = "IBM Plex Mono")[[type]]
  family <- if (isTRUE(.ip_has_font(plex))) plex else type
  assign(type, family, envir = .ip_font_cache)
  family
}
