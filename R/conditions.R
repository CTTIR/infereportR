# Classed base R conditions, as in the rest of the infer suite. Tests assert
# the class, never the message text.

.ip_abort <- function(message, code = "invalid_input", parent = NULL) {
  stop(structure(list(message = message, call = NULL, parent = parent),
                 class = c(paste0("inferplotR_", code), "inferplotR_error", "error",
                           "condition")))
}

.ip_warn <- function(message, code) {
  warning(structure(list(message = message, call = NULL),
                    class = c(paste0("inferplotR_", code), "inferplotR_warning", "warning",
                              "condition")))
}
