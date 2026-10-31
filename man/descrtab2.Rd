\name{import_descrtab2}
\alias{import_descrtab2}
\alias{render_descrtab2}
\title{Import and render precomputed native DescrTab2 results}
\description{An optional, pinned adapter for native DescrList objects that have already been computed.}
\usage{
import_descrtab2(x, source_version = "2.1.26")
render_descrtab2(x, format = c("table", "text", "html"))
}
\arguments{
\item{x}{For import, an already computed native DescrList. For render, the import result.}
\item{source_version}{Declared producing version; must match the verified installed 2.1.26.}
\item{format}{Native numerical table, plain text, or HTML.}
}
\value{Import returns a descrtab2_import holding native results and method metadata. Render returns a plain data.frame or character output, with reporting metadata.}
\details{Requires DescrTab2 2.1.26 from upstream commit f157380e7abcb7964612a480e09e59a45e7a01be. DescrTab2 was archived on CRAN on 2025-10-18 and is never a mandatory dependency. Its native descr function computes tests even when print_p is FALSE; this adapter never calls descr or sig_test.

Rendering uses the public registered print method with print_format="numeric" and silent=TRUE, then formats the stored cells locally. This avoids native custom formatting/reshaping callbacks and does not provide arbitrary native layout customizations. Row-level group membership, analysis input options and presentation callbacks are removed on import while levels and aggregate group sizes remain. Input observations are never requested.

The native numbers and test labels are preserved. It is not converted into a shared inference result: native fields alone do not reliably establish every estimand, orientation and interval policy. In particular the pinned native numeric renderer uses CI_upper/CI_lower column names in the reverse order of the stored CI vector; the adapter gives them neutral native_CI_first/native_CI_second names, retaining the original headings in report metadata. It does not reinterpret them as a normalized inference result. Prefer the shared result route for normalized inferential reporting. Imports are trusted native R objects, not an untrusted serialized-data format.}
\seealso{\url{https://imbi-heidelberg.github.io/DescrTab2/reference/print.DescrList.html}}
