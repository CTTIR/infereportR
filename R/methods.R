# The single adapter of the figure layer to infercalcR (numbers) and to the
# reporting functions of this package (display strings). The figure layer
# never computes or formats a statistic itself.

.ir_methods <- list(
  means = "t_mean_one_sample",
  compare = c("t_means_two_independent_welch", "t_means_two_independent_student",
              "rank_sum_mann_whitney", "brunner_munzel"),
  omnibus = c("anova_one_way", "anova_one_way_welch", "kruskal_wallis"),
  correlation = c("correlation_pearson", "correlation_spearman", "correlation_kendall"),
  paired = c("t_means_paired", "signed_rank_wilcoxon")
)

.ir_check_method <- function(method, set, arg = "method") {
  if (is.null(method) || (is.character(method) && length(method) == 1L && !is.na(method) &&
                          !nzchar(method))) {
    .ir_abort(sprintf(
      "%s is missing: choose the procedure explicitly; infereportR never selects a test from the data",
      arg))
  }
  .ir_check_string(method, arg)
  allowed <- .ir_methods[[set]]
  if (!method %in% allowed) {
    .ir_abort(sprintf(
      "%s = \"%s\" is not supported here; use one of %s (see infercalcR::method_catalogue())",
      arg, method, paste0("\"", allowed, "\"", collapse = ", ")), code = "unsupported")
  }
  method
}

.ir_spec <- function(method, alternative = NULL, conf.level = NULL) {
  args <- list(method_id = method)
  if (!is.null(alternative)) args$alternative <- alternative
  if (!is.null(conf.level)) args$conf.level <- conf.level
  .ir_wrap(do.call(infercalcR::method_spec, args), "infercalcR::method_spec()")
}

.ir_run <- function(spec, ..., result_id) {
  .ir_wrap(infercalcR::run_inference(spec, ..., result_id = result_id),
           "infercalcR::run_inference()")
}

.ir_display <- function(result, p = c("raw", "adjusted"), digits = 3L, p_digits = 3L) {
  p <- .ir_check_choice(p, c("raw", "adjusted"), "p")
  .ir_wrap(report_table(result, p = p, digits = digits, p_digits = p_digits),
           "report_table()")
}

# Re-raise any condition from the suite packages as calculation_failed, keeping
# the original as parent; never swallow it.
.ir_wrap <- function(expr, what) {
  tryCatch(expr, error = function(e) {
    .ir_abort(sprintf("%s failed: %s", what, conditionMessage(e)),
              code = "calculation_failed", parent = e)
  })
}
