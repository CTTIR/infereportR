# The single adapter to infercalcR (numbers) and infereportR (display strings).
# inferplotR never computes or formats a statistic itself.

.ip_methods <- list(
  means = "t_mean_one_sample",
  compare = c("t_means_two_independent_welch", "t_means_two_independent_student",
              "rank_sum_mann_whitney", "brunner_munzel"),
  omnibus = c("anova_one_way", "anova_one_way_welch", "kruskal_wallis"),
  correlation = c("correlation_pearson", "correlation_spearman", "correlation_kendall")
)

.ip_check_method <- function(method, set, arg = "method") {
  if (is.null(method) || (is.character(method) && length(method) == 1L && !is.na(method) &&
                          !nzchar(method))) {
    .ip_abort(sprintf(
      "%s is missing: choose the procedure explicitly; inferplotR never selects a test from the data",
      arg))
  }
  .ip_check_string(method, arg)
  allowed <- .ip_methods[[set]]
  if (!method %in% allowed) {
    .ip_abort(sprintf(
      "%s = \"%s\" is not supported here; use one of %s (see infercalcR::method_catalogue())",
      arg, method, paste0("\"", allowed, "\"", collapse = ", ")), code = "unsupported")
  }
  method
}

.ip_spec <- function(method, alternative = NULL, conf.level = NULL) {
  args <- list(method_id = method)
  if (!is.null(alternative)) args$alternative <- alternative
  if (!is.null(conf.level)) args$conf.level <- conf.level
  .ip_wrap(do.call(infercalcR::method_spec, args), "infercalcR::method_spec()")
}

.ip_run <- function(spec, ..., result_id) {
  .ip_wrap(infercalcR::run_inference(spec, ..., result_id = result_id),
           "infercalcR::run_inference()")
}

.ip_display <- function(result, p = c("raw", "adjusted"), digits = 3L, p_digits = 3L) {
  p <- .ip_check_choice(p, c("raw", "adjusted"), "p")
  .ip_wrap(infereportR::report_table(result, p = p, digits = digits, p_digits = p_digits),
           "infereportR::report_table()")
}

# Re-raise any condition from the suite packages as calculation_failed, keeping
# the original as parent; never swallow it.
.ip_wrap <- function(expr, what) {
  tryCatch(expr, error = function(e) {
    .ip_abort(sprintf("%s failed: %s", what, conditionMessage(e)),
              code = "calculation_failed", parent = e)
  })
}
