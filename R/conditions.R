# Classed base R conditions, as in the rest of the infer suite. Tests assert
# the class, never the message text. One .ir_abort() serves the reporting
# functions and the figure layer: every error carries infereportR_<code>
# (infereportR_invalid_input, infereportR_unsupported, ...) plus
# infereportR_error; the figure layer passes the underlying condition as
# `parent`. Without a parent the condition has exactly the fields of
# infereportR 0.1.0 (message and call).

.ir_abort <- function(message, code = "invalid_input", parent = NULL) {
  fields <- list(message = message, call = NULL)
  if (!is.null(parent)) fields$parent <- parent
  stop(structure(fields,
                 class = c(paste0("infereportR_", code), "infereportR_error", "error",
                           "condition")))
}

.ir_warn <- function(message, code) {
  warning(structure(list(message = message, call = NULL),
                    class = c(paste0("infereportR_", code), "infereportR_warning", "warning",
                              "condition")))
}
