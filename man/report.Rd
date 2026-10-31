\name{report_table}
\alias{report_table}
\alias{report_text}
\alias{report_metadata}
\title{Tables and neutral text from finalized results}
\description{Read the shared public result accessors and preserve stored numerical quantities and metadata.}
\usage{
report_table(x, rows = NULL, p = c("reported", "raw", "adjusted"),
  digits = 3L, p_digits = 3L, decimal_mark = ".")
report_text(x, rows = NULL, p = c("reported", "raw", "adjusted"),
  digits = 3L, p_digits = 3L, decimal_mark = ".",
  style = c("full", "p", "estimate_ci", "estimate_ci_p", "stars"),
  template = NULL)
report_metadata(x)
}
\arguments{
\item{x}{A finalized result supporting \code{infercalcR::result_data()} and \code{result_metadata()}; for \code{report_metadata}, a reporting output or native import.}
\item{rows}{Unique stored row IDs to display, or NULL for all.}
\item{p}{Select stored reported, raw or adjusted p-values. An adjusted request fails if a selected value is absent.}
\item{digits, p_digits}{Display precision for estimates and p-values.}
\item{decimal_mark}{Either a dot or comma.}
\item{style}{Full neutral text or a compact label preset.}
\item{template}{Optional string containing approved fields in braces: estimate, ci, p, statistic, method, estimand, contrast, status, n. This is substitution only, without evaluation.}
}
\value{\code{report_table} returns an ordinary data.frame. Original analytical columns retain full precision, with additional columns ending in \code{_display}, and \code{p_selected}/\code{p_selection}. Metadata is an attribute accessed by \code{report_metadata}. \code{report_text} returns a named character vector with the same metadata.}
\details{Filtering changes presentation only; metadata retains the entire adjustment family. No test, model, interval or adjustment is recomputed. Full text preserves estimand, orientation, interval method/scale/scope, confidence level, sample size and unavailable status when supplied. It does not assign clinical meaning or infer equivalence from nonsignificance. Numerical optimization bounds are not interpreted as confidence intervals. Stars use thresholds .05, .01, .001 and .0001; missing or unresolved results never acquire stars or \code{ns}. A stored decision conflicting with these display thresholds is shown explicitly. Compact labels for non-ok rows show unavailable status.}
\examples{
x <- infercalcR::new_inference_result(
  data.frame(row_id = "contrast", estimate = 1.2, conf.low = 0.2,
    conf.high = 2.2, conf.level = 0.95, p.value = 0.021,
    estimand = "mean difference", status = "ok"),
  list(method_id = "example", result_id = "study-1"))
report_table(x)
report_text(x)
}
