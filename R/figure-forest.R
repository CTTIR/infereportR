# Helpers shared by the figure builders --------------------------------------

# IBM Plex Sans Condensed when 'systemfonts' reports it, else the figure font.
.ip_condensed_family <- function() {
  if (isTRUE(.ip_has_font("IBM Plex Sans Condensed"))) return("IBM Plex Sans Condensed")
  figure_font("sans")
}

# Width in mm of each string, set in the given font. Measured on an
# off-screen device so the user's current device is never touched; ragg when
# available (it resolves system fonts as the PNG export does), else a null
# PDF device.
.ip_text_width_mm <- function(labels, family, fontsize, fontface = 1L) {
  labels <- as.character(labels)
  if (!length(labels)) return(numeric())
  current <- grDevices::dev.cur()
  file <- NULL
  if (requireNamespace("ragg", quietly = TRUE)) {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, width = 400, height = 400, res = 72)
  } else {
    grDevices::pdf(NULL)
  }
  on.exit({
    grDevices::dev.off()
    if (current > 1L) grDevices::dev.set(current)
    if (!is.null(file)) unlink(file)
  }, add = TRUE)
  grid::pushViewport(grid::viewport(gp = grid::gpar(fontfamily = family, fontsize = fontsize,
                                                    fontface = fontface)))
  vapply(labels, function(s) {
    grid::convertWidth(grid::stringWidth(s), "mm", valueOnly = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

# A stat that attaches the builder's results to its rows: the row index
# arrives as the `ip_row` aesthetic, the results as a parameter. This is how
# a builder figure carries `.ip_result`, so inference_results() and
# figure_report() work on it.
.ip_stat_results <- ggplot2::ggproto("StatInferplotResults", ggplot2::Stat,
  required_aes = c("ip_row"),
  compute_panel = function(data, scales, results, ids) {
    i <- data$ip_row
    data$.ip_result <- unname(results[i])
    data$.ip_result_id <- unname(ids[i])
    data
  }
)

.ip_method_id <- function(result) {
  meta <- tryCatch(infercalcR::result_metadata(result), error = function(e) NULL)
  id <- meta$method_id
  if (is.character(id) && length(id) == 1L && !is.na(id)) id else NA_character_
}

# One row of a result: the row selected by row_id, else the only row.
.ip_forest_row <- function(result, row, name) {
  d <- .ip_wrap(infercalcR::result_data(result), "infercalcR::result_data()")
  if (!is.null(row)) d <- d[!is.na(d$row_id) & d$row_id == row, , drop = FALSE]
  if (nrow(d) != 1L) {
    .ip_abort(sprintf(
      "result \"%s\" gives %d rows%s; figure_forest() needs exactly one (select it with row)",
      name, nrow(d), if (is.null(row)) "" else sprintf(" for row \"%s\"", row)))
  }
  d
}

#' Forest plot of single-contrast results
#'
#' Draws one row per 'infercalcR' result: a square marker at the estimate
#' (area proportional to n), its confidence interval as a line, a dotted
#' vertical line at the null value, and three text columns, label | n | plot
#' | estimate text. Nothing is computed here: estimates, intervals, null value
#' and n come from [infercalcR::result_data()], and the right-hand text is
#' `paste(estimate_display, interval_display)` from
#' [infereportR::report_table()]. Any 'infercalcR' method whose result has a
#' contrast row with an estimate and interval can be shown, for example
#' `"t_means_two_independent_welch"` or `"t_means_paired"`.
#'
#' The row `overall` is drawn on top, in bold, separated from the other rows
#' by a hairline; the header row is set in condensed bold. The text columns
#' are text layers at x positions outside the axis range, drawn with
#' `coord_cartesian(clip = "off")`. Their widths are measured with
#' [grid::stringWidth()] in the figure font at 8 pt, and the plot margins
#' and positions follow from these widths and the width of `slot`.
#'
#' @param results A named list of 'infercalcR' results, for example from a
#'   loop of [infercalcR::run_inference()] calls or from
#'   [inference_results()]. Each must give exactly one row after `row`
#'   selection.
#' @param labels Row labels; the names of `results` by default. Must be
#'   unique.
#' @param n `NULL` (default: `result_data()$n`) or a numeric vector, one per
#'   result.
#' @param row `NULL` (each result has a single row), one `row_id` used for
#'   every result, or one `row_id` per result.
#' @param null_value `NULL` (default: the common `null.value` of the
#'   results) or a number; `NA` draws no null line.
#' @param xlab The x-axis title; `NULL` for none.
#' @param header Three column headings, or `NULL` for no header row.
#' @param overall Index of the overall row (drawn on top in bold), or `NULL`
#'   for none.
#' @param mode `"light"` (black on white) or `"dark"` (white on black).
#' @param transparent If `TRUE`, no background is drawn.
#' @param base_size Base font size of the axis text in points.
#' @param slot A preset of [figure_size()] whose width the figure is laid out
#'   for; `"slide_full"` (134 mm) by default. Save the figure at that width.
#' @return A ggplot. Its marker layer carries `.ip_result`, so
#'   [inference_results()] returns the results and [figure_report()] reports
#'   them.
#' @seealso [figure_correlation_matrix()], [inference_results()]
#' @importFrom ggplot2 .data
#' @examples
#' welch <- infercalcR::method_spec("t_means_two_independent_welch")
#' groups <- split(PlantGrowth$weight, PlantGrowth$group)
#' res <- list(
#'   "trt1 vs ctrl" = infercalcR::run_inference(welch, x = groups$trt1, y = groups$ctrl),
#'   "trt2 vs ctrl" = infercalcR::run_inference(welch, x = groups$trt2, y = groups$ctrl)
#' )
#' figure_forest(res, xlab = "Difference in weight (g)", overall = NULL,
#'               header = c("Contrast", "n", "Difference [95% CI]"))
#' @export
figure_forest <- function(results, labels = names(results), n = NULL, row = NULL,
                          null_value = NULL, xlab = NULL,
                          header = c("Subgroup", "n", "Difference [95% CI]"), overall = 1L,
                          mode = "light", transparent = FALSE, base_size = 9,
                          slot = "slide_full") {
  m <- .ip_mode(mode, transparent)
  if (!is.list(results) || inherits(results, "inference_result") || !length(results)) {
    .ip_abort("results must be a non-empty list of infercalcR results")
  }
  k <- length(results)
  if (is.null(labels) || !is.character(labels) || length(labels) != k || anyNA(labels) ||
      anyDuplicated(labels)) {
    .ip_abort("labels must be unique strings, one per result (or results must be named)")
  }
  if (!is.null(n) && (!is.numeric(n) || length(n) != k)) {
    .ip_abort("n must be NULL or a numeric vector with one value per result")
  }
  if (!is.null(row)) {
    if (!is.character(row) || !length(row) %in% c(1L, k) || anyNA(row)) {
      .ip_abort("row must be NULL, one row_id or one row_id per result")
    }
    row <- rep(row, length.out = k)
  }
  if (!is.null(null_value) && (!is.numeric(null_value) || length(null_value) != 1L)) {
    .ip_abort("null_value must be NULL or a single number (NA for no line)")
  }
  xlab <- .ip_check_optional_string(xlab, "xlab")
  if (!is.null(header) && (!is.character(header) || length(header) != 3L || anyNA(header))) {
    .ip_abort("header must be NULL or three strings")
  }
  if (!is.null(overall)) overall <- .ip_check_count(overall, "overall", min = 1)
  if (!is.null(overall) && overall > k) .ip_abort("overall must index one of the results")
  .ip_check_number(base_size, "base_size")
  slot <- .ip_check_choice(slot, .ip_presets$preset, "slot")

  rows <- lapply(seq_len(k), function(i) {
    r <- results[[i]]
    if (!inherits(r, "inference_result")) {
      .ip_abort(sprintf("results[[%d]] is not an infercalcR result", i))
    }
    d <- .ip_forest_row(r, row[i], labels[i])
    shown <- .ip_display(r)
    shown <- shown[match(d$row_id, shown$row_id), , drop = FALSE]
    data.frame(estimate = d$estimate, low = d$conf.low, high = d$conf.high,
               null = if (is.null(d$null.value)) NA_real_ else d$null.value,
               n = if (is.null(n)) d$n else n[i],
               text = paste(shown$estimate_display, shown$interval_display),
               method = .ip_method_id(r),
               stringsAsFactors = FALSE)
  })
  fd <- do.call(rbind, rows)
  fd$label <- labels
  fd$ip_row <- seq_len(k)
  if (is.null(null_value)) {
    nulls <- unique(fd$null[!is.na(fd$null)])
    if (length(nulls) > 1L) {
      .ip_abort("the results have different null values; set null_value")
    }
    null_value <- if (length(nulls)) nulls else NA_real_
  }

  # Rows from the top: the overall row, a gap with the hairline, the rest.
  others <- setdiff(seq_len(k), overall)
  fd$y <- NA_real_
  fd$y[others] <- rev(seq_along(others))
  top <- length(others)
  if (!is.null(overall)) {
    fd$y[overall] <- top + 1.6
    rule_y <- top + 0.8
    top <- top + 1.6
  }
  fd$face <- ifelse(seq_len(k) %in% overall, 2L, 1L)
  header_y <- top + 1.1

  # Axis range: every interval and the null value, with 4 % room.
  span <- range(c(fd$low, fd$high, fd$estimate, null_value), finite = TRUE)
  if (!all(is.finite(span))) .ip_abort("the results have no finite estimate or interval")
  if (diff(span) == 0) span <- span + c(-1, 1)
  xr <- span + c(-1, 1) * 0.04 * diff(span)

  # Text columns, measured in the figure font at the label size.
  sans <- figure_font("sans")
  cond <- .ip_condensed_family()
  fs <- .ip_label_size_pt
  n_text <- format(fd$n, trim = TRUE)
  w <- function(s, family = sans, face = 1L) {
    if (!length(s)) return(0)
    max(.ip_text_width_mm(s, family, fs, face))
  }
  head_w <- if (is.null(header)) c(0, 0, 0) else
    vapply(header, function(h) w(h, cond, 2L), numeric(1), USE.NAMES = FALSE)
  label_w <- max(w(fd$label[fd$face == 1L]), w(fd$label[fd$face == 2L], face = 2L), head_w[1])
  n_w <- max(w(n_text), head_w[2])
  text_w <- max(w(fd$text), head_w[3])
  gap <- w("MM")
  left_mm <- label_w + gap + n_w + gap
  right_mm <- gap + text_w
  edge <- 1
  width <- .ip_presets$width[.ip_presets$preset == slot]
  panel_mm <- width - left_mm - right_mm - 2 * edge
  if (panel_mm <= 0) {
    .ip_abort(sprintf("the text columns (%.0f mm) leave no room for the plot in slot \"%s\"",
                      left_mm + right_mm, slot))
  }
  per_mm <- diff(xr) / panel_mm
  x_label <- xr[1] - left_mm * per_mm
  x_n <- xr[1] - gap * per_mm
  x_text <- xr[2] + gap * per_mm
  x_end <- xr[2] + right_mm * per_mm

  size <- fs / ggplot2::.pt
  ink <- m$ink
  p <- ggplot2::ggplot(fd, ggplot2::aes(x = .data$estimate, y = .data$y))
  if (is.finite(null_value)) {
    p <- p + ggplot2::geom_vline(xintercept = null_value, linetype = "13",
                                 linewidth = .ip_lw(.ip_rule_hair), colour = ink)
  }
  p <- p +
    ggplot2::geom_segment(ggplot2::aes(x = .data$low, xend = .data$high, yend = .data$y),
                          linewidth = .ip_lw(0.6), colour = ink, na.rm = TRUE) +
    ggplot2::layer(stat = .ip_stat_results, geom = "point", position = "identity",
                   mapping = ggplot2::aes(size = .data$n, ip_row = .data$ip_row),
                   params = list(results = results, ids = .ip_forest_ids(fd), shape = 15,
                                 colour = ink, na.rm = TRUE),
                   show.legend = FALSE, inherit.aes = TRUE) +
    ggplot2::geom_text(ggplot2::aes(x = x_label, label = .data$label, fontface = .data$face),
                       hjust = 0, family = sans, size = size, colour = ink) +
    ggplot2::geom_text(ggplot2::aes(x = x_n, label = n_text), hjust = 1, family = sans,
                       size = size, colour = ink) +
    ggplot2::geom_text(ggplot2::aes(x = x_text, label = .data$text), hjust = 0,
                       family = sans, size = size, colour = ink)
  if (!is.null(header)) {
    hd <- data.frame(x = c(x_label, x_n, x_text), y = header_y, label = header,
                     hjust = c(0, 1, 0), stringsAsFactors = FALSE)
    p <- p + ggplot2::geom_text(
      data = hd, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                              hjust = .data$hjust),
      family = cond, fontface = "bold", size = size, colour = ink, inherit.aes = FALSE)
  }
  if (!is.null(overall)) {
    rd <- data.frame(x = x_label, xend = x_end, y = rule_y)
    p <- p + ggplot2::geom_segment(
      data = rd, ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$y),
      linewidth = .ip_lw(.ip_rule_hair), colour = ink, inherit.aes = FALSE)
  }
  ylim <- c(0.4, (if (is.null(header)) top else header_y) + 0.5)
  p +
    ggplot2::scale_size_area(max_size = 2.4, guide = "none") +
    ggplot2::coord_cartesian(xlim = xr, ylim = ylim, expand = FALSE, clip = "off") +
    ggplot2::labs(x = xlab, y = NULL) +
    theme_figure("slide", base_size = base_size, mode = mode, transparent = transparent) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(edge, right_mm + edge, edge, left_mm + edge,
                                                 unit = "mm"))
}

.ip_forest_ids <- function(fd) {
  method <- ifelse(is.na(fd$method), "result", fd$method)
  vapply(seq_len(nrow(fd)), function(i) {
    .ip_result_id("forest", method[i], 1L, fd$label[i])
  }, character(1))
}
