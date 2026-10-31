#' Tabulate finalized inference values
#' @export
report_table <- function(x, rows = NULL, p = c("reported", "raw", "adjusted"),
                         digits = 3L, p_digits = 3L, decimal_mark = ".") {
  p <- match.arg(p)
  d <- as.data.frame(infercalcR::result_data(x), stringsAsFactors=FALSE)
  metadata <- infercalcR::result_metadata(x)
  d <- d[.ir_rows(d, rows), , drop = FALSE]
  d$p_selected <- .ir_p(d, p)
  d$p_selection <- rep(p, nrow(d))
  d$result_id <- rep(metadata$result_id, nrow(d))
  d$method_id <- rep(metadata$method_id, nrow(d))
  d$variant_id <- rep(metadata$variant_id, nrow(d))
  d$estimate_display <- format_estimate(d$estimate, digits, decimal_mark)
  d$interval_display <- format_interval(d$conf.low, d$conf.high, digits, decimal_mark)
  d$statistic_display <- format_estimate(d$statistic, digits, decimal_mark)
  d$p_display <- format_p(d$p_selected, p_digits, decimal_mark)
  d$confidence_display <- ifelse(is.na(d$conf.level), "confidence level unavailable",
    paste0(format_estimate(100*d$conf.level, digits, decimal_mark), "%"))
  attr(d, "inference_metadata") <- metadata
  attr(d, "display_settings") <- list(p=p, digits=digits, p_digits=p_digits, decimal_mark=decimal_mark)
  rownames(d) <- NULL
  d
}
#' Retrieve reporting provenance and complete-family metadata
#' @export
report_metadata <- function(x) {
  if (inherits(x, "descrtab2_import")) return(x$metadata)
  metadata <- attr(x, "inference_metadata", exact=TRUE)
  if (is.null(metadata)) .ir_abort("object has no reporting metadata")
  metadata
}
#' Render deterministic neutral text from finalized results
#' @export
report_text <- function(x, rows = NULL, p = c("reported", "raw", "adjusted"),
                        digits = 3L, p_digits = 3L, decimal_mark = ".",
                        style = c("full", "p", "estimate_ci", "estimate_ci_p", "stars"), template = NULL) {
  p <- match.arg(p); style <- match.arg(style)
  d <- report_table(x, rows, p, digits, p_digits, decimal_mark)
  if (style != "full" || !is.null(template)) {
    out <- .ir_labels(d, if (style == "full") "estimate_ci_p" else style, template)
  } else {
    out <- vapply(seq_len(nrow(d)), function(i) {
      z <- d[i,,drop=FALSE]
      identity <- c(z$method_id, z$estimand, z$contrast)
      identity <- identity[!is.na(identity) & nzchar(identity)]
      text <- paste(identity, collapse="; ")
      if (!is.na(z$estimate)) text <- paste0(text, ": estimate ", z$estimate_display)
      if (!is.na(z$conf.low) && !is.na(z$conf.high)) {
        policy <- c(z$interval.method,z$interval.scale,z$interval.scope)
        policy <- policy[!is.na(policy) & nzchar(policy)]
        text <- paste0(text, "; ", z$confidence_display, " interval ", z$interval_display,
          if (length(policy)) paste0(" (",paste(policy,collapse="; "),")") else "")
      }
      if (!is.na(z$statistic)) text <- paste0(text,"; statistic ",z$statistic_display)
      if (!is.na(z$p_selected) || identical(report_metadata(d)$kind,"inference")) {
        text <- paste0(text,"; ",z$p_display," (",p,")")
        if (p != "raw" && !is.na(z$p.method) && z$p.method != "none") text <- paste0(text,"; recorded adjustment: ",z$p.method)
      }
      if (!is.na(z$n)) text <- paste0(text,"; n = ",format_estimate(z$n,15L))
      if (!is.na(z$n.excluded)) text <- paste0(text,"; excluded = ",format_estimate(z$n.excluded,15L))
      if (z$status != "ok") text <- paste0(text,"; status ",z$status,
        if (!is.na(z$reason) && nzchar(z$reason)) paste0(" (",z$reason,")") else "")
      if ("decision" %in% names(z) && !is.na(z$decision)) text <- paste0(text,"; decision ",z$decision)
      paste0(text,".")
    }, "")
  }
  names(out) <- d$row_id
  attr(out,"inference_metadata") <- report_metadata(d)
  out
}
#' Describe data with explicit denominators and optional supplied inference
#' @export
report_descriptive <- function(x, variables = NULL, group = NULL, labels = NULL, units = NULL, types = NULL,
                               inference = NULL, mapping = NULL, inference_population = NULL,
                               p = c("reported", "raw", "adjusted"), digits = 3L, p_digits = 3L, decimal_mark = ".") {
  p <- match.arg(p)
  if (is.data.frame(x)) {
    if (is.null(variables)) variables <- setdiff(names(x), group)
    x <- infercalcR::describe_data(x, variables, group, labels, units, types)
  } else if (!is.null(variables) || !is.null(group) || !is.null(labels) || !is.null(units) || !is.null(types)) .ir_abort("selection/label arguments apply only to raw-data descriptions")
  if (!identical(infercalcR::result_metadata(x)$kind,"descriptive")) .ir_abort("x must be raw data or a descriptive result")
  d <- report_table(x, p="reported", digits=digits, p_digits=p_digits, decimal_mark=decimal_mark)
  d[c("p.value","p.adjusted","p.reported","p_selected","p_selection","p_display","p.method","family_id","family_size")] <- NULL
  if (is.null(inference)) {
    if (!is.null(mapping) || !is.null(inference_population)) .ir_abort("mapping and inference_population require supplied inference")
    return(d)
  }
  if (!.ir_string(inference_population)) .ir_abort("label the supplied inferential population explicitly with inference_population")
  if (inherits(inference,"inference_result")) inference <- list(inference)
  if (!is.list(inference) || !length(inference)) .ir_abort("inference must be a result or list of finalized results")
  tables <- lapply(inference,function(z) report_table(z,p=p,digits=digits,p_digits=p_digits,decimal_mark=decimal_mark))
  ids <- vapply(tables,function(z) report_metadata(z)$result_id,"")
  if (anyDuplicated(ids)) .ir_abort("supplied results require distinct result_id values")
  if (!is.data.frame(mapping) || anyDuplicated(names(mapping)) || !all(c("variable_id","result_id","row_id") %in% names(mapping)) || !nrow(mapping)) .ir_abort("mapping requires variable_id, result_id and row_id columns")
  for (nm in c("variable_id","result_id","row_id")) if (!is.character(mapping[[nm]]) || anyNA(mapping[[nm]]) || any(!nzchar(mapping[[nm]]))) .ir_abort("mapping identifiers must be nonmissing strings")
  if (anyDuplicated(mapping$variable_id) || any(!mapping$variable_id %in% d$variable_id) || any(!mapping$result_id %in% ids)) .ir_abort("mapping must assign at most one supplied inference row per known variable")
  joined <- lapply(seq_len(nrow(mapping)),function(i) {
    t <- tables[[match(mapping$result_id[i],ids)]]
    if (!mapping$row_id[i] %in% t$row_id) .ir_abort("mapping refers to a missing inference row_id")
    z <- t[match(mapping$row_id[i],t$row_id),,drop=FALSE]
    if (!is.na(z$variable_id) && z$variable_id != mapping$variable_id[i]) .ir_abort("mapping variable_id conflicts with stored inference identity")
    z
  })
  fields <- c("result_id","row_id","p.value","p.adjusted","p.reported","p_selected","p_display","p_selection","status","reason","n","n.excluded","family_id","family_size","estimand","contrast")
  for (field in fields) {
    prototype <- joined[[1L]][[field]]
    values <- rep(if (is.numeric(prototype)) NA_real_ else NA_character_,nrow(d))
    for (i in seq_len(nrow(mapping))) values[d$variable_id == mapping$variable_id[i]] <- joined[[i]][[field]]
    d[[paste0("inference_",field)]] <- values
  }
  d$inference_population <- ifelse(d$variable_id %in% mapping$variable_id,inference_population,NA_character_)
  metadata <- attr(d,"inference_metadata")
  metadata$supplied_inference <- lapply(tables,report_metadata)
  metadata$inference_mapping <- mapping
  metadata$inference_population <- inference_population
  attr(d,"inference_metadata") <- metadata
  d
}
