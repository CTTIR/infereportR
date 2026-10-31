\name{report_descriptive}
\alias{report_descriptive}
\title{Describe selected variables with explicit populations}
\description{Render stored descriptions or ask infercalcR to compute descriptions without hypothesis tests. Supplied inference is joined only by explicit stable identifiers.}
\usage{
report_descriptive(x, variables = NULL, group = NULL, labels = NULL,
  units = NULL, types = NULL, inference = NULL, mapping = NULL,
  inference_population = NULL, p = c("reported", "raw", "adjusted"),
  digits = 3L, p_digits = 3L, decimal_mark = ".")
}
\arguments{
\item{x}{A raw data.frame or a finalized descriptive result.}
\item{variables}{Selected raw-data column names, default all except the grouping variable.}
\item{group}{An optional grouping column name.}
\item{labels, units}{Optional vectors named by variable ID.}
\item{types}{Named vector of \code{continuous} or \code{categorical}. Factors, including ordered factors, remain categorical by default.}
\item{inference}{Optional finalized common result or list of results with distinct result IDs.}
\item{mapping}{A data.frame with variable_id, result_id and row_id. At most one inference row per descriptive variable is supported. Every mapping entry must refer to an existing result and variable.}
\item{inference_population}{Required nonempty label when inference is supplied, documenting its analysis population separately from descriptive denominators.}
\item{p}{Which stored p-value to display.}
\item{digits, p_digits, decimal_mark}{Display settings as in \code{report_table}.}
}
\value{An ordinary data.frame with full-precision descriptions, n, n.missing, n.total, group/level labels and display columns. No p-value columns exist by default. Optional joined inference fields have an \code{inference_} prefix. Metadata retains descriptive and inferential populations separately.}
\details{The raw route delegates to \code{infercalcR::describe_data}. Categorical percentages use nonmissing denominators and missing groups remain explicit. No automatic hypothesis test is selected. The join never matches on display labels, and cannot infer that two differently labelled analysis populations are identical. Group-specific multiple tests per variable require separate explicitly scoped reports.}
\examples{
d <- data.frame(value = c(1, 2, NA, 4), group = c("A", "A", "B", "B"))
report_descriptive(d, group = "group", units = c(value = "mm"))
}
