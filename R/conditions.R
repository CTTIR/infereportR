# Classed base R conditions, as in the rest of the infer suite. Tests assert
# the class, never the message text. One .ir_abort() serves the reporting
# functions and the figure layer: every error carries infereportR_<code>
# (infereportR_invalid_input, infereportR_missing_dependency, ...) plus
# infereportR_error; the figure layer passes the underlying condition as
# `parent`.

.ir_abort <- function(message, code = "invalid_input", parent = NULL) {
  stop(structure(list(message = message, call = NULL, parent = parent),
                 class = c(paste0("infereportR_", code), "infereportR_error", "error",
                           "condition")))
}

.ir_warn <- function(message, code) {
  warning(structure(list(message = message, call = NULL),
                    class = c(paste0("infereportR_", code), "infereportR_warning", "warning",
                              "condition")))
}
