.ip_cache <- new.env(parent = emptyenv())

# Every .ip_result of a plot with the layer and panel it came from, in layer
# then panel order, de-duplicated by result id.
.ip_collect <- function(plot) {
  .ip_check_plot(plot)
  built <- ggplot2::ggplot_build(plot)
  results <- list()
  layer <- integer()
  panel <- character()
  for (i in seq_along(built$data)) {
    d <- built$data[[i]]
    if (!all(c(".ip_result", ".ip_result_id") %in% names(d)) || !nrow(d)) next
    ord <- order(as.integer(d$PANEL), seq_len(nrow(d)))
    for (j in ord) {
      id <- d$.ip_result_id[[j]]
      if (id %in% names(results)) next
      results[[id]] <- d$.ip_result[[j]]
      layer <- c(layer, i)
      panel <- c(panel, as.character(d$PANEL[[j]]))
    }
  }
  list(results = results, layer = layer, panel = panel)
}

#' Inference results behind a figure
#'
#' Builds the plot once and returns every 'infercalcR' result computed by its
#' inference layers ([stat_means()], [stat_compare()], [stat_omnibus()],
#' [stat_correlation()]), so the numbers behind each label can be inspected,
#' validated and reported.
#'
#' @param plot A ggplot.
#' @return A named list of 'infercalcR' result objects, named by their result
#'   ids (`"inferplotR:<stat>:<method>:panel<PANEL>"`, plus the x level for
#'   [stat_means()]), in layer then panel order and without duplicates. A plot
#'   without inference layers gives `list()`.
#' @seealso [figure_report()], [infercalcR::result_data()],
#'   [infereportR::report_text()]
#' @examples
#' library(ggplot2)
#' p <- ggplot(PlantGrowth, aes(group, weight)) +
#'   stat_omnibus(method = "anova_one_way")
#' res <- inference_results(p)
#' names(res)
#' infercalcR::result_data(res[[1]])[, c("statistic", "df", "p.value")]
#' @export
inference_results <- function(plot) {
  .ip_collect(plot)$results
}

.ip_report_prototype <- function() {
  proto <- .ip_cache$report_prototype
  if (is.null(proto)) {
    r <- .ip_run(.ip_spec("t_means_two_independent_welch"), x = c(1, 2, 3), y = c(2, 3, 5),
                 result_id = "inferplotR:prototype")
    proto <- .ip_display(r)[0L, , drop = FALSE]
    .ip_cache$report_prototype <- proto
  }
  proto
}

#' Figure results as one report table
#'
#' Calls [infereportR::report_table()] on every result of
#' [inference_results()] and binds the rows, adding the layer and panel each
#' result came from.
#'
#' @param plot A ggplot.
#' @param p `"auto"` (default) reports adjusted p-values where a result has
#'   an adjustment and raw ones otherwise; `"raw"` and `"adjusted"` force one
#'   kind for every result.
#' @param digits,p_digits Significant digits for estimates and p-values,
#'   passed to [infereportR::report_table()].
#' @return A data.frame with the leading columns `figure_layer` (integer) and
#'   `figure_panel` (character), followed by the columns of
#'   [infereportR::report_table()]. Without results, a 0-row data.frame with
#'   the same columns.
#' @seealso [inference_results()]
#' @examples
#' library(ggplot2)
#' p <- ggplot(PlantGrowth, aes(group, weight)) +
#'   stat_compare(method = "t_means_two_independent_welch", reference = "ctrl",
#'                adjust = "holm")
#' figure_report(p)[, c("contrast", "estimate_display", "p_display")]
#' @export
figure_report <- function(plot, p = c("auto", "raw", "adjusted"), digits = 3L,
                          p_digits = 3L) {
  .ip_check_plot(plot)
  p <- .ip_check_choice(p, c("auto", "raw", "adjusted"), "p")
  digits <- .ip_check_count(digits, "digits", min = 1)
  p_digits <- .ip_check_count(p_digits, "p_digits", min = 1)
  found <- .ip_collect(plot)
  proto <- .ip_report_prototype()
  lead <- function(d, layer, panel) {
    cbind(data.frame(figure_layer = rep(as.integer(layer), nrow(d)),
                     figure_panel = rep(as.character(panel), nrow(d)),
                     stringsAsFactors = FALSE), d)
  }
  if (!length(found$results)) return(lead(proto, integer(), character()))
  tables <- lapply(seq_along(found$results), function(i) {
    r <- found$results[[i]]
    kind <- p
    if (kind == "auto") {
      kind <- if (any(!is.na(infercalcR::result_data(r)$p.adjusted))) "adjusted" else "raw"
    }
    d <- .ip_display(r, p = kind, digits = digits, p_digits = p_digits)
    # Status-only results may lack optional columns; align with the prototype.
    for (nm in setdiff(names(proto), names(d))) d[[nm]] <- rep(proto[[nm]][NA_integer_], nrow(d))
    lead(d[, names(proto), drop = FALSE], found$layer[[i]], found$panel[[i]])
  })
  out <- do.call(rbind, tables)
  rownames(out) <- NULL
  out
}
