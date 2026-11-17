# Correlation matrix without colour (amendment D11).

# Circle diameter in mm for |estimate| = 1 (scale_size_area maximum).
.ip_matrix_circle <- 6

# One correlation. When infercalcR stops instead of returning a status (for
# example a constant variable or too few complete cases), the pair is kept as
# an infercalcR result with status "undefined" and infercalcR's message as the
# reason, so the cell shows a cross and figure_report() says why.
.ip_matrix_run <- function(spec, frame, id) {
  attempt <- tryCatch(infercalcR::run_inference(spec, data = frame, result_id = id),
                      error = function(e) e)
  if (!inherits(attempt, "error")) return(attempt)
  rows <- data.frame(row_id = paste0(spec$method_id, ":1"), component = "contrast",
                     estimand = "correlation", n = nrow(frame), status = "undefined",
                     reason = paste("infercalcR:", conditionMessage(attempt)),
                     stringsAsFactors = FALSE)
  .ip_wrap(infercalcR::new_inference_result(rows, list(method_id = spec$method_id,
                                                       result_id = id)),
           "infercalcR::new_inference_result()")
}

# The cross drawn for a pair without a usable estimate: the multiplication
# sign, or "x" where the session cannot write UTF-8.
.ip_cross_glyph <- function() {
  if (isTRUE(l10n_info()[["UTF-8"]])) "\u00d7" else "x"
}

#' Correlation matrix without colour
#'
#' Runs one explicitly named correlation procedure of 'infercalcR' for every
#' pair of variables and draws the matrix in two values. Above the diagonal,
#' a circle per pair has an area proportional to |estimate| (the scale runs
#' from 0 to 1, 6 mm at 1); it is filled for a positive and open for a
#' negative estimate. Below the diagonal, the mirrored cell shows the
#' 'infereportR' `estimate_display` at `digits` significant digits. The
#' diagonal names the variables in condensed bold.
#'
#' Pairs are taken in the order of `utils::combn(vars, 2)`. Each pair uses
#' its complete cases with `id = seq_len(n)` and one
#' [infercalcR::run_inference()] call. A pair whose result is not `ok`
#' (including a call that 'infercalcR' refuses, such as a constant variable
#' or too few cases; Pearson needs at least 20) is drawn as a cross at the
#' circle position and its value cell stays empty; the status and reason are
#' in [figure_report()].
#'
#' Accepted method ids: `"correlation_pearson"`, `"correlation_spearman"`,
#' `"correlation_kendall"`.
#'
#' @param data A data frame.
#' @param vars Names of at least two numeric columns; `NULL` (default) takes
#'   every numeric column.
#' @param method The 'infercalcR' method id. Required.
#' @param digits Significant digits of the displayed values.
#' @param labels Variable labels for the diagonal; `vars` by default.
#' @param mode `"light"` (black on white) or `"dark"` (white on black).
#' @param transparent If `TRUE`, no background is drawn.
#' @param base_size Base font size in points.
#' @return A ggplot with `coord_fixed()`. Its value layer carries
#'   `.ip_result`, so [inference_results()] returns one result per pair.
#' @seealso [stat_correlation()] for a single pair with a scatter plot,
#'   [figure_forest()]
#' @examples
#' figure_correlation_matrix(mtcars, vars = c("mpg", "wt", "hp", "qsec"),
#'                           method = "correlation_spearman")
#' @export
figure_correlation_matrix <- function(data, vars = NULL, method, digits = 2L, labels = NULL,
                                      mode = "light", transparent = FALSE, base_size = 9) {
  method <- .ip_check_method(if (missing(method)) NULL else method, "correlation")
  m <- .ip_mode(mode, transparent)
  if (!is.data.frame(data)) .ip_abort("data must be a data frame")
  if (is.null(vars)) vars <- names(data)[vapply(data, is.numeric, logical(1))]
  if (!is.character(vars) || length(vars) < 2L || anyNA(vars) || anyDuplicated(vars)) {
    .ip_abort("vars must name at least two different columns")
  }
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars)) {
    .ip_abort(sprintf("vars not in data: %s", paste(missing_vars, collapse = ", ")))
  }
  if (!all(vapply(data[vars], is.numeric, logical(1)))) {
    .ip_abort("every column in vars must be numeric")
  }
  digits <- .ip_check_count(digits, "digits", min = 1)
  if (is.null(labels)) labels <- vars
  if (!is.character(labels) || length(labels) != length(vars) || anyNA(labels)) {
    .ip_abort("labels must be one string per variable")
  }
  .ip_check_number(base_size, "base_size")

  k <- length(vars)
  pairs <- utils::combn(k, 2L)
  spec <- .ip_spec(method)
  cells <- lapply(seq_len(ncol(pairs)), function(p) {
    i <- pairs[1L, p]
    j <- pairs[2L, p]
    x <- data[[vars[i]]]
    y <- data[[vars[j]]]
    ok <- is.finite(x) & is.finite(y)
    frame <- data.frame(id = seq_len(sum(ok)), x = x[ok], y = y[ok])
    id <- .ip_result_id("correlation_matrix", method, 1L, paste(vars[i], vars[j], sep = "-"))
    result <- .ip_matrix_run(spec, frame, id)
    res <- infercalcR::result_data(result)[1L, , drop = FALSE]
    shown <- .ip_display(result, digits = digits)[1L, , drop = FALSE]
    usable <- identical(res$status, "ok") && is.finite(res$estimate)
    list(cell = data.frame(i = i, j = j, id = id,
                           estimate = if (usable) res$estimate else NA_real_,
                           text = if (usable) shown$estimate_display else "",
                           usable = usable, stringsAsFactors = FALSE),
         result = result)
  })
  cm <- do.call(rbind, lapply(cells, function(cl) cl$cell))
  results <- lapply(cells, function(cl) cl$result)
  cm$ip_row <- seq_len(nrow(cm))
  # Positions: variable i on the diagonal at (i, k + 1 - i); the pair (i, j)
  # has its circle above the diagonal and its value mirrored below.
  cm$cx <- cm$j
  cm$cy <- k + 1 - cm$i
  cm$tx <- cm$i
  cm$ty <- k + 1 - cm$j
  circles <- cm[cm$usable, , drop = FALSE]
  circles$size <- abs(circles$estimate)
  circles$shape <- ifelse(circles$estimate > 0, 16, 1)
  crosses <- cm[!cm$usable, , drop = FALSE]
  diag <- data.frame(x = seq_len(k), y = k + 1 - seq_len(k), label = labels,
                     stringsAsFactors = FALSE)

  size <- .ip_label_size_pt / ggplot2::.pt
  sans <- figure_font("sans")
  ink <- m$ink
  p <- ggplot2::ggplot()
  if (nrow(circles)) {
    p <- p + ggplot2::geom_point(
      data = circles, ggplot2::aes(x = .data$cx, y = .data$cy, size = .data$size,
                                   shape = .data$shape),
      colour = ink, stroke = .ip_lw(0.6) * ggplot2::.pt / ggplot2::.stroke * 2)
  }
  if (nrow(crosses)) {
    p <- p + ggplot2::geom_text(
      data = crosses, ggplot2::aes(x = .data$cx, y = .data$cy), label = .ip_cross_glyph(),
      family = sans, size = size, colour = ink)
  }
  p <- p +
    ggplot2::layer(stat = .ip_stat_results, geom = "text", position = "identity", data = cm,
                   mapping = ggplot2::aes(x = .data$tx, y = .data$ty, label = .data$text,
                                          ip_row = .data$ip_row),
                   params = list(results = results, ids = cm$id, family = sans,
                                 size = size, colour = ink),
                   show.legend = FALSE, inherit.aes = FALSE) +
    ggplot2::geom_text(data = diag, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                       family = .ip_condensed_family(), fontface = "bold", size = size,
                       colour = ink) +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_size_area(max_size = .ip_matrix_circle, limits = c(0, 1), guide = "none") +
    ggplot2::coord_fixed(xlim = c(0.5, k + 0.5), ylim = c(0.5, k + 0.5), expand = FALSE,
                         clip = "off") +
    # theme_void() with the mode's ink and backgrounds (none when transparent).
    ggplot2::theme_void(base_size = base_size, base_family = sans) +
    ggplot2::theme(text = ggplot2::element_text(colour = ink),
                   plot.background = ggplot2::element_rect(fill = m$knock, colour = NA),
                   panel.background = ggplot2::element_rect(fill = m$knock, colour = NA))
  p
}
