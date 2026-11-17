#' @keywords internal
#' @importFrom utils globalVariables
"_PACKAGE"

utils::globalVariables(character())

# The skeleton has no feature code yet; this keeps R CMD check from flagging
# declared Imports as unused until the phases that use them land.
.ip_imports_used <- function() {
  list(ggplot2::ggplot, grid::unit, grDevices::cairo_pdf, stats::setNames,
       infercalcR::run_inference, infereportR::report_table)
}
