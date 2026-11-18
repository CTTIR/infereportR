# print, format and as.character methods for inferplotR objects. This is the
# only file in R/ that may write to the console.

#' Print, format or coerce a character grid
#'
#' Methods for the `inferplotR_chargrid` objects returned by [char_grid()].
#'
#' @param x An `inferplotR_chargrid` object.
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
format.inferplotR_chargrid <- function(x, ...) {
  unname(apply(x$chars, 1L, paste0, collapse = ""))
}

#' @rdname chargrid-methods
#' @export
as.character.inferplotR_chargrid <- function(x, ...) {
  format(x)
}

#' @rdname chargrid-methods
#' @export
print.inferplotR_chargrid <- function(x, ...) {
  cat(paste0(format(x), "\n"), sep = "")
  invisible(x)
}
