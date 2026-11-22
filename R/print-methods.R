# print, format and as.character methods for the figure layer's objects. This is the
# only file in R/ that may write to the console.

#' Print, format or coerce a character grid
#'
#' Methods for the `infereportR_chargrid` objects returned by [char_grid()].
#'
#' @param x An `infereportR_chargrid` object.
#' @param ... Unused; for compatibility with the generics.
#' @return `format()` and `as.character()` return a character vector of `height`
#'   lines of `width` characters each. `print()` writes those lines to the console
#'   and returns `x` invisibly.
#' @seealso [char_grid()]
#' @name chargrid-methods
#' @examples
#' p <- ggplot2::ggplot(data.frame(x = 1:5, y = c(2, 4, 3, 5, 4)),
#'                      ggplot2::aes(x, y)) + ggplot2::geom_line()
#' g <- char_grid(p, width = 32, height = 10)
#' format(g)
#' print(g)
NULL

#' @rdname chargrid-methods
#' @export
format.infereportR_chargrid <- function(x, ...) {
  unname(apply(x$chars, 1L, paste0, collapse = ""))
}

#' @rdname chargrid-methods
#' @export
as.character.infereportR_chargrid <- function(x, ...) {
  format(x)
}

#' @rdname chargrid-methods
#' @export
print.infereportR_chargrid <- function(x, ...) {
  cat(paste0(format(x), "\n"), sep = "")
  invisible(x)
}

# Decks and documents print one line per slide or block.
.ir_item_line <- function(s) {
  what <- switch(s$type,
    figure = sprintf("%s slide%s", s$layout, if (!is.null(s$name)) paste0(" [", s$name, "]")
                     else ""),
    section = "section", keyfigure = "key figure", sparkline = "sparklines",
    statement = "statement", text = "text", note = "note", newthought = "new thought",
    fullwidth = "full width", epigraph = "epigraph", table = "table", s$type)
  if (identical(s$type, "figure") && is.null(s$title)) {
    what <- sprintf("%s figure [%s]", s$layout, s$name)
  }
  label <- if (!is.null(s$title)) s$title else if (!is.null(s$caption)) s$caption else ""
  sprintf("%-28s %s%s", what, label, if (isTRUE(s$inverse)) " (inverse)" else "")
}

.ir_options_line <- function(x) {
  opts <- .ir_bz_option_string(x$options, x$mode)
  if (length(opts)) paste(opts, collapse = ",") else "(theme defaults)"
}

#' @export
format.infereportR_deck <- function(x, ...) {
  c(sprintf("<Bleisatz deck: %d slides> %s", length(x$slides), x$meta$title),
    sprintf("options: %s; figures in %s", .ir_options_line(x),
            paste(x$figure_modes, collapse = ", ")),
    vapply(x$slides, function(s) paste0("  ", .ir_item_line(s)), character(1L)))
}

#' @export
print.infereportR_deck <- function(x, ...) {
  cat(paste0(format(x), "\n"), sep = "")
  invisible(x)
}

#' @export
format.infereportR_document <- function(x, ...) {
  c(sprintf("<Bleisatz document: %d blocks> %s", length(x$blocks), x$meta$title),
    sprintf("options: %s; figures in %s%s", .ir_options_line(x),
            paste(x$figure_modes, collapse = ", "),
            if (!is.null(x$bibliography)) paste0("; bibliography ", basename(x$bibliography))
            else ""),
    vapply(x$blocks, function(s) paste0("  ", .ir_item_line(s)), character(1L)))
}

#' @export
print.infereportR_document <- function(x, ...) {
  cat(paste0(format(x), "\n"), sep = "")
  invisible(x)
}

#' @export
format.infereportR_slide <- function(x, ...) paste0("<Bleisatz slide> ", .ir_item_line(x))

#' @export
print.infereportR_slide <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
format.infereportR_block <- function(x, ...) paste0("<Bleisatz block> ", .ir_item_line(x))

#' @export
print.infereportR_block <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}
