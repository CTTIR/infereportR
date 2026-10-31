.ir_require <- function(package) {
  if (!requireNamespace(package, quietly=TRUE)) .ir_abort(paste0("Install the optional '",package,"' package before requesting this feature."),"missing_dependency")
}
utils::globalVariables(c(".ir_x", ".ir_xend", ".ir_y", ".ir_yend", ".ir_label"))
#' Add explicit-coordinate annotation layers for stored inference
#' @export
stat_inference <- function(x, positions, rows = NULL, label = c("p","estimate_ci","estimate_ci_p","stars"),
                           p = c("reported","raw","adjusted"), groups = NULL, facet_vars = NULL,
                           template = NULL, digits = 3L, p_digits = 3L, decimal_mark = ".",
                           unavailable = c("label","error","warn"), tip_length = 0.03,
                           label_offset = 0, text_size = 3.5, colour = "black", linewidth = 0.4,
                           text_hjust = 0.5, text_vjust = -0.4) {
  .ir_require("ggplot2")
  label <- match.arg(label); p <- match.arg(p); unavailable <- match.arg(unavailable)
  d <- report_table(x, rows, p, digits, p_digits, decimal_mark)
  if (!nrow(d)) .ir_abort("annotations require at least one selected result row")
  if (!is.data.frame(positions) || anyDuplicated(names(positions)) || !all(c("row_id","y_position") %in% names(positions))) .ir_abort("positions must contain row_id and y_position")
  if (!is.character(positions$row_id) || anyNA(positions$row_id) || anyDuplicated(positions$row_id) || !setequal(positions$row_id,d$row_id)) .ir_abort("positions require exactly one row per selected result row_id")
  pos <- positions[match(d$row_id,positions$row_id),,drop=FALSE]
  if (!is.numeric(pos$y_position) || any(!is.finite(pos$y_position))) .ir_abort("y_position must contain finite original-data-unit coordinates")
  for (value in list(tip_length,label_offset,text_size,linewidth,text_hjust,text_vjust)) if (!is.numeric(value) || length(value)!=1L || !is.finite(value)) .ir_abort("position offsets, text_size and linewidth must be finite numeric scalars")
  if (tip_length<0 || text_size<=0 || linewidth<0 || !.ir_string(colour)) .ir_abort("invalid annotation appearance")
  if (is.null(facet_vars)) facet_vars <- report_metadata(d)$facet_vars
  if (is.null(facet_vars)) facet_vars <- character()
  declared <- report_metadata(d)$facet_vars
  if (!is.null(declared) && !setequal(facet_vars,declared)) .ir_abort("facet_vars must match the result's declared facet contract")
  if (!is.character(facet_vars) || anyNA(facet_vars) || anyDuplicated(facet_vars) || any(!nzchar(facet_vars)) || any(!facet_vars %in% names(pos))) .ir_abort("every declared facet key must be supplied in positions")
  for (key in facet_vars) {
    if (anyNA(pos[[key]])) .ir_abort("facet keys cannot be missing")
    if (!key %in% names(d)) .ir_abort("facet keys must also exist in the stored result rows")
    if (!identical(as.character(pos[[key]]),as.character(d[[key]]))) .ir_abort("position facet keys conflict with stored result rows")
  }
  named <- all(c("group1","group2") %in% names(pos))
  numeric <- all(c("xmin","xmax") %in% names(pos))
  if (named && numeric) .ir_abort("supply named group endpoints or numeric endpoints, not both")
  if (!"type" %in% names(pos)) pos$type <- if (named || numeric) "bracket" else "text"
  if (!is.character(pos$type) || anyNA(pos$type) || any(!pos$type %in% c("bracket","text"))) .ir_abort("position type must be bracket or text")
  bracket <- pos$type == "bracket"
  if (any(bracket & !is.na(d$component) & d$component == "omnibus")) .ir_abort("omnibus tests require text-only annotations")
  a <- pos[,facet_vars,drop=FALSE]
  a$row_id <- d$row_id
  a$.ir_x <- a$.ir_xend <- rep(NA_real_,nrow(d))
  if (any(bracket)) {
    if (named) {
      if (!is.character(groups) || length(groups)<2L || anyNA(groups) || anyDuplicated(groups) || any(!nzchar(groups))) .ir_abort("named endpoints require groups in the plot's explicit discrete-axis order")
      if (!is.character(pos$group1) || !is.character(pos$group2) || anyNA(pos$group1[bracket]) || anyNA(pos$group2[bracket]) || any(!pos$group1[bracket] %in% groups) || any(!pos$group2[bracket] %in% groups)) .ir_abort("unknown or missing named group endpoint")
      a$.ir_x[bracket] <- match(pos$group1[bracket],groups)
      a$.ir_xend[bracket] <- match(pos$group2[bracket],groups)
    } else if (numeric) {
      if (!is.numeric(pos$xmin) || !is.numeric(pos$xmax)) .ir_abort("xmin and xmax must be numeric")
      a$.ir_x[bracket] <- pos$xmin[bracket]; a$.ir_xend[bracket] <- pos$xmax[bracket]
    } else .ir_abort("brackets require named group endpoints or explicit numeric xmin/xmax")
    if (any(!is.finite(a$.ir_x[bracket])) || any(!is.finite(a$.ir_xend[bracket])) || any(a$.ir_x[bracket] == a$.ir_xend[bracket])) .ir_abort("bracket endpoints must be finite and distinct")
  }
  if (any(!bracket)) {
    if (!"x" %in% names(pos) || !is.numeric(pos$x) || any(!is.finite(pos$x[!bracket]))) .ir_abort("text-only annotations require finite numeric x coordinates")
    a$.ir_x[!bracket] <- a$.ir_xend[!bracket] <- pos$x[!bracket]
  }
  a$.ir_y <- pos$y_position
  a$.ir_yend <- a$.ir_y
  a$.ir_label <- .ir_labels(d,label,template)
  bad <- .ir_unavailable(d) | (label %in% c("p","stars","estimate_ci_p") & is.na(d$p_selected))
  if (any(bad)) {
    if (unavailable=="error") .ir_abort("selected inference includes unavailable or unresolved rows")
    if (unavailable=="warn") warning("Unavailable inference retained as an explicit annotation.",call.=FALSE)
  }
  layers <- list()
  if (any(bracket)) {
    seg <- a[bracket,,drop=FALSE]
    tip1 <- tip2 <- seg
    tip1$.ir_xend <- tip1$.ir_x
    tip2$.ir_x <- tip2$.ir_xend
    tip1$.ir_yend <- tip2$.ir_yend <- seg$.ir_y-tip_length
    seg <- rbind(seg,tip1,tip2)
    layers[[length(layers)+1L]] <- ggplot2::geom_segment(data=seg,
      mapping=ggplot2::aes(x=.ir_x,xend=.ir_xend,y=.ir_y,yend=.ir_yend),
      inherit.aes=FALSE, colour=colour, linewidth=linewidth, show.legend=FALSE)
  }
  txt <- a
  txt$.ir_x <- (a$.ir_x+a$.ir_xend)/2
  txt$.ir_y <- a$.ir_y+label_offset
  layers[[length(layers)+1L]] <- ggplot2::geom_text(data=txt,
    mapping=ggplot2::aes(x=.ir_x,y=.ir_y,label=.ir_label),inherit.aes=FALSE,
    colour=colour,size=text_size,hjust=text_hjust,vjust=text_vjust,show.legend=FALSE)
  attr(layers,"annotation_data") <- a
  attr(layers,"inference_metadata") <- report_metadata(d)
  layers
}
#' Guard existing plot coordinates before adding inference layers
#' @export
annotate_inference <- function(plot, x, positions, ...) {
  .ir_require("ggplot2")
  if (!inherits(plot,"ggplot")) .ir_abort("plot must be a ggplot object")
  if (!inherits(plot$coordinates,c("CoordCartesian","CoordFlip"))) .ir_abort("only Cartesian coordinates and coord_flip are supported; use scale transformations for log axes")
  plot + stat_inference(x,positions,...)
}
