# Argument validators. Every exported function checks its arguments with these
# before any ggplot2 call, so bad input fails early with a classed condition.

.ip_check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .ip_abort(sprintf("%s must be a single non-empty string", arg))
  }
  invisible(x)
}

.ip_check_number <- function(x, arg, min = -Inf, max = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    .ip_abort(sprintf("%s must be a single finite number", arg))
  }
  if (x < min || x > max) {
    .ip_abort(sprintf("%s must be between %s and %s", arg, format(min), format(max)))
  }
  invisible(x)
}

.ip_check_count <- function(x, arg, min = 0) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x != floor(x)) {
    .ip_abort(sprintf("%s must be a single whole number", arg))
  }
  if (x < min) {
    .ip_abort(sprintf("%s must be at least %s", arg, format(min)))
  }
  as.integer(x)
}

.ip_check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .ip_abort(sprintf("%s must be TRUE or FALSE", arg))
  }
  invisible(x)
}

.ip_check_choice <- function(x, choices, arg) {
  # A missing argument arrives as the full default vector, as with match.arg().
  if (identical(x, choices)) return(choices[[1L]])
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    .ip_abort(sprintf("%s must be one of %s", arg,
                      paste0("\"", choices, "\"", collapse = ", ")))
  }
  x
}

.ip_check_plot <- function(x, arg = "plot") {
  # inherits() covers both the S3 ggplot class and the S7 class of ggplot2 4.x.
  if (!inherits(x, "ggplot")) {
    .ip_abort(sprintf("%s must be a ggplot object", arg))
  }
  invisible(x)
}
