.ir_abort <- function(message, code = "invalid_input") {
  stop(structure(list(message = message, call = NULL),
    class = c(paste0("infereportR_", code), "error", "condition")))
}
.ir_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.ir_digits <- function(digits, decimal_mark) {
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits != floor(digits) || digits < 1 || digits > 15) .ir_abort("digits must be an integer from 1 to 15")
  if (!.ir_string(decimal_mark) || !decimal_mark %in% c(".", ",")) .ir_abort("decimal_mark must be '.' or ','")
}
#' Format values without changing stored numbers
#' @export
format_estimate <- function(x, digits = 3L, decimal_mark = ".", missing = "unavailable") {
  .ir_digits(digits, decimal_mark)
  if (!is.numeric(x) || !.ir_string(missing)) .ir_abort("x must be numeric and missing must be a nonempty string")
  vapply(x, function(v) {
    if (is.na(v)) return(missing)
    if (is.infinite(v)) return(if (v > 0) "Inf" else "-Inf")
    format(v, digits = digits, trim = TRUE, scientific = NA, decimal.mark = decimal_mark)
  }, "", USE.NAMES = FALSE)
}
#' Format p-values with a declared display threshold
#' @export
format_p <- function(x, digits = 3L, decimal_mark = ".", prefix = TRUE) {
  .ir_digits(digits, decimal_mark)
  if (!is.numeric(x) || any(is.nan(x)) || any(!is.na(x) & (!is.finite(x) | x < 0 | x > 1))) .ir_abort("p-values must be NA or numbers in [0,1]")
  if (!is.logical(prefix) || length(prefix) != 1L || is.na(prefix)) .ir_abort("prefix must be TRUE or FALSE")
  threshold <- 10^-digits
  floor_text <- formatC(threshold, format = "f", digits = digits, decimal.mark = decimal_mark)
  vapply(x, function(v) {
    if (is.na(v)) return(if (prefix) "p unavailable" else "unavailable")
    # Zero may be numerical underflow; never imply a mathematically zero probability.
    if (v < threshold) return(paste0(if (prefix) "p " else "", "< ", floor_text))
    paste0(if (prefix) "p = " else "", formatC(v, format = "f", digits = digits, decimal.mark = decimal_mark))
  }, "", USE.NAMES = FALSE)
}
#' Format stored confidence endpoints
#' @export
format_interval <- function(low, high, digits = 3L, decimal_mark = ".") {
  .ir_digits(digits, decimal_mark)
  if (!is.numeric(low) || !is.numeric(high) || length(low) != length(high) ||
      any(!is.na(low) & !is.na(high) & low > high)) .ir_abort("interval endpoints must be equally sized numeric vectors in increasing order")
  out <- paste0("[", format_estimate(low, digits, decimal_mark), ", ", format_estimate(high, digits, decimal_mark), "]")
  out[is.na(low) | is.na(high)] <- "interval unavailable"
  out
}
.ir_rows <- function(d, rows) {
  if (is.null(rows)) return(seq_len(nrow(d)))
  if (!is.character(rows) || !length(rows) || anyNA(rows) || anyDuplicated(rows) || any(!rows %in% d$row_id)) .ir_abort("rows must identify distinct stored row_id values")
  match(rows, d$row_id)
}
.ir_p <- function(d, p) {
  field <- switch(p, reported = "p.reported", raw = "p.value", adjusted = "p.adjusted")
  if (is.null(field)) .ir_abort("p must be reported, raw or adjusted")
  value <- d[[field]]
  if (p == "adjusted" && anyNA(value)) .ir_abort("requested adjusted p-values are unavailable for one or more selected rows")
  value
}
.ir_unavailable <- function(d) {
  out <- d$status != "ok"
  if ("decision" %in% names(d)) out <- out | (!is.na(d$decision) & d$decision == "unresolved")
  out
}
.ir_labels <- function(table, label, template = NULL) {
  if (!is.null(template)) {
    if (!.ir_string(template)) .ir_abort("template must be one nonempty string")
    fields <- list(estimate = table$estimate_display, ci = table$interval_display,
      p = table$p_display, statistic = table$statistic_display, method = table$method_id,
      estimand = table$estimand, contrast = table$contrast, status = table$status,
      n = format_estimate(table$n))
    tokens <- regmatches(template, gregexpr("\\{[^{}]*\\}", template))[[1L]]
    keys <- sub("}$", "", sub("^\\{", "", tokens))
    if (any(!keys %in% names(fields)) || grepl("[{}]", gsub("\\{[^{}]*\\}", "", template))) .ir_abort("template contains an unsupported field or malformed braces")
    ans <- rep(template, nrow(table))
    for (key in unique(keys)) {
      val <- fields[[key]]; val[is.na(val)] <- "unavailable"
      ans <- vapply(seq_len(nrow(table)), function(i) gsub(paste0("{",key,"}"), val[i], ans[i], fixed=TRUE), "")
    }
  } else {
    if (!.ir_string(label) || !label %in% c("p", "estimate_ci", "estimate_ci_p", "stars")) .ir_abort("unknown label preset")
    ans <- switch(label, p = table$p_display,
      estimate_ci = paste(table$estimate_display, table$interval_display),
      estimate_ci_p = paste0(table$estimate_display, " ", table$interval_display, "; ", table$p_display),
      stars = {
        v <- table$p_selected
        z <- rep("unavailable", length(v)); ok <- !is.na(v)
        z[ok] <- ifelse(v[ok] <= .0001, "****", ifelse(v[ok] <= .001, "***", ifelse(v[ok] <= .01, "**", ifelse(v[ok] <= .05, "*", "ns"))))
        z
      })
  }
  bad <- .ir_unavailable(table)
  status <- table$status
  if ("decision" %in% names(table)) status[!is.na(table$decision) & table$decision=="unresolved"] <- "unresolved"
  ans[bad] <- paste0("unavailable (", status[bad], ")")
  if (identical(label,"stars") && "decision" %in% names(table)) {
    unresolved <- !is.na(table$decision) & table$decision == "unresolved"
    ans[unresolved] <- "unavailable (unresolved)"
    conflict <- !is.na(table$decision) & table$decision == "do_not_reject" & !is.na(table$p_selected) & table$p_selected <= .05
    ans[conflict] <- "decision: do_not_reject"
    conflict <- !is.na(table$decision) & table$decision == "reject" & !is.na(table$p_selected) & table$p_selected > .05
    ans[conflict] <- "decision: reject"
  }
  ans
}
