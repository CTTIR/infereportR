.ir_descrtab2 <- function() {
  .ir_require("DescrTab2")
  version <- as.character(utils::packageVersion("DescrTab2"))
  if (!identical(version,"2.1.26")) .ir_abort(paste0("DescrTab2 import/render is verified for pinned version 2.1.26; installed version is ",version),"unsupported")
  version
}
#' Import an already computed native DescrTab2 object
#' @export
import_descrtab2 <- function(x, source_version = "2.1.26") {
  version <- .ir_descrtab2()
  if (!identical(source_version,version)) .ir_abort("source_version must match the verified installed DescrTab2 version")
  if (!inherits(x,"DescrList") || !is.list(x) || !is.list(x$variables) || !length(x$variables) || !is.list(x$format) || !is.list(x$group)) .ir_abort("x must be a precomputed native DescrTab2 DescrList")
  # A native object retains row-level group membership. Renderers need only levels.
  native <- x[c("input_facts","group","variables","var_names","format")]
  class(native) <- c("DescrList","list")
  native$group <- native$group[c("var","name","labels","levels","lengths")]
  n_group_rows <- length(native$group$var)
  if (!is.null(native$group$var)) native$group$var <- factor(character(),levels=levels(native$group$var))
  # Finalized numerical rendering needs neither analysis options/subject IDs
  # nor function closures, which might themselves retain input observations.
  native$format <- native$format[c("options","test_abbreviations")]
  native$variables <- lapply(native$variables,function(v) {
    v <- v[c("results","test_list","variable_name","variable_levels","variable_options","variable_lengths")]
    v$variable_options <- v$variable_options[c("label","format_options")]
    v$test_list <- v$test_list[c("p","test_name","CI_name","CI")]
    v
  })
  metadata <- list(backend="DescrTab2",version=version,source_version=source_version,
    source_commit="f157380e7abcb7964612a480e09e59a45e7a01be",
    scope="Native precomputed import/render; no normalized estimand or confidence-interval mapping is asserted.",
    input_facts=x$input_facts,group=x$group[c("name","labels","levels","lengths")],
    tests=lapply(native$variables,function(v) v$test_list),
    row_level_group_memberships_removed=n_group_rows,
    rendering="Registered public print method; no analysis functions are called by this adapter.")
  structure(list(native=native,metadata=metadata),class="descrtab2_import")
}
#' Render a precomputed native DescrTab2 import
#' @export
render_descrtab2 <- function(x, format = c("table","text","html")) {
  .ir_descrtab2()
  format <- match.arg(format)
  if (!inherits(x,"descrtab2_import") || !inherits(x$native,"DescrList") || !identical(x$metadata$version,"2.1.26")) .ir_abort("x must be an import_descrtab2 result")
  # The documented numeric print route does not invoke user-supplied formatting
  # or reshaping callbacks. Text and HTML are built from these finalized cells.
  rendered <- print(x$native, print_format="numeric", silent=TRUE)
  if (!is.data.frame(rendered$tibble)) .ir_abort("unexpected native numeric renderer shape","unsupported")
  table <- as.data.frame(rendered$tibble,stringsAsFactors=FALSE)
  native_names <- names(table)
  # Do not publish misleading upper/lower headings from the pinned renderer.
  names(table)[names(table)=="CI_upper"] <- "native_CI_first"
  names(table)[names(table)=="CI_lower"] <- "native_CI_second"
  if (format=="table") out <- table else {
    display <- table
    for (name in names(display)) {
      if (is.numeric(display[[name]])) display[[name]] <- if (name=="p") format_p(display[[name]],prefix=FALSE) else format_estimate(display[[name]])
      else display[[name]] <- as.character(display[[name]])
      display[[name]][is.na(display[[name]])] <- ""
    }
    if (format=="text") out <- utils::capture.output(print(display,row.names=FALSE,right=FALSE)) else {
      escape <- function(z) {
        z <- gsub("&","&amp;",z,fixed=TRUE)
        z <- gsub("<","&lt;",z,fixed=TRUE)
        z <- gsub(">","&gt;",z,fixed=TRUE)
        gsub('"',"&quot;",z,fixed=TRUE)
      }
      head <- paste0("<tr>",paste0("<th>",escape(names(display)),"</th>",collapse=""),"</tr>")
      body <- vapply(seq_len(nrow(display)),function(i) paste0("<tr>",paste0("<td>",escape(unlist(display[i,],use.names=FALSE)),"</td>",collapse=""),"</tr>"),"")
      out <- paste0("<table><thead>",head,"</thead><tbody>",paste(body,collapse=""),"</tbody></table>")
    }
  }
  metadata <- x$metadata
  metadata$native_column_names <- structure(native_names,names=names(table))
  attr(out,"inference_metadata") <- metadata
  out
}
