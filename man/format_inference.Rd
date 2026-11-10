\name{format_inference}
\alias{format_estimate}
\alias{format_interval}
\alias{format_p}
\title{Format stored numerical values}
\description{Common display rules used by tables, text and plot annotations. No numerical inference is performed.}
\usage{
format_estimate(x, digits = 3L, decimal_mark = ".", missing = "unavailable")
format_interval(low, high, digits = 3L, decimal_mark = ".")
format_p(x, digits = 3L, decimal_mark = ".", prefix = TRUE)
}
\arguments{
\item{x}{Numeric values; p-values must be NA or in [0,1].}
\item{digits}{An integer from 1 to 15. Significant digits for estimates; decimal places and threshold for p-values.}
\item{decimal_mark}{Either \code{"."} or \code{","}; presentation only.}
\item{missing}{Display for missing estimates.}
\item{low, high}{Equally sized stored interval endpoints. Infinities are retained.}
\item{prefix}{Include \code{p =} or \code{p <} when TRUE.}
}
\value{A character vector.}
\details{P-values below \code{10^-digits}, including stored zero, display a strict upper threshold. Missing p-values never display as nonsignificant. A stored zero may represent numerical underflow, and is not labeled as a mathematically zero probability. Intervals are formatted without calculation or changes of scale.}
\examples{
format_p(c(0.03, 1e-20, 0, NA_real_))
format_interval(c(-Inf, 0.1), c(2, 0.5))
}
