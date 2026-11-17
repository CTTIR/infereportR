#' @keywords internal
#' @importFrom utils globalVariables
"_PACKAGE"

utils::globalVariables(character())

# Until the theme, export and inference phases land, some declared Imports have
# no use yet; this keeps R CMD check from flagging them. Removed once unused.
.ip_imports_used <- function() {
  list(grid::unit, grDevices::cairo_pdf, stats::setNames)
}
