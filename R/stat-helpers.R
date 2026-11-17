# Helpers shared by the inference stats. Validation runs in the constructors,
# so a bad layer fails when it is created, not when the plot is printed.

.ip_reserved_args <- c("x", "y", "group", "data", "mu0", "result_id", "spec")

.ip_check_method_args <- function(method_args, arg = "method_args") {
  if (!is.list(method_args) || is.data.frame(method_args)) {
    .ip_abort(sprintf("%s must be a named list", arg))
  }
  if (!length(method_args)) return(invisible(method_args))
  nm <- names(method_args)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
    .ip_abort(sprintf("%s must be a list with unique, non-empty names", arg))
  }
  reserved <- intersect(nm, .ip_reserved_args)
  if (length(reserved)) {
    .ip_abort(sprintf("%s must not set %s; the layer supplies the data arguments", arg,
                      paste0("\"", reserved, "\"", collapse = ", ")))
  }
  invisible(method_args)
}

.ip_check_alternative <- function(x, arg = "alternative") {
  if (is.null(x)) return(NULL)
  .ip_check_choice(x, c("two.sided", "greater", "less"), arg)
}

.ip_check_conf_level <- function(x, arg = "conf.level") {
  if (is.null(x)) return(NULL)
  .ip_check_number(x, arg, min = 0, max = 1)
  if (x <= 0 || x >= 1) .ip_abort(sprintf("%s must be strictly between 0 and 1", arg))
  x
}

.ip_check_optional_number <- function(x, arg, min = -Inf, max = Inf) {
  if (is.null(x)) return(NULL)
  .ip_check_number(x, arg, min = min, max = max)
}

.ip_check_optional_string <- function(x, arg) {
  if (is.null(x)) return(NULL)
  .ip_check_string(x, arg)
}

# Positions may be -Inf/Inf (panel edges), so only NA is rejected here.
.ip_check_position <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    .ip_abort(sprintf("%s must be a single number (-Inf and Inf mean the panel edges)", arg))
  }
  x
}

.ip_result_id <- function(stat, method, panel, level = NULL) {
  id <- sprintf("inferplotR:%s:%s:panel%s", stat, method, as.character(panel))
  if (!is.null(level)) id <- paste0(id, ":", level)
  id
}

.ip_finite_xy <- function(data) {
  data[is.finite(data$x) & is.finite(data$y), , drop = FALSE]
}

.ip_attach_result <- function(out, result, id) {
  out$.ip_result <- rep(list(result), nrow(out))
  out$.ip_result_id <- rep(id, nrow(out))
  out
}

.ip_is_discrete <- function(scale) {
  !is.null(scale) && isTRUE(scale$is_discrete())
}

# Text parameters for label layers: the mode's ink, the in-plot label size
# (8 pt) and the figure font unless set.
.ip_text_params <- function(dots, text_size, family, ink = .ip_ink) {
  text_size <- .ip_check_optional_number(text_size, "text_size", min = 0)
  family <- .ip_check_optional_string(family, "family")
  params <- list(size = if (is.null(text_size)) .ip_label_size_pt / ggplot2::.pt else text_size,
                 family = if (is.null(family)) figure_font("sans") else family)
  if (!"colour" %in% names(dots) && !"color" %in% names(dots)) params$colour <- ink
  params
}

# A layer whose stat also learns the plot's y label (used as variable_id of a
# comparison family). Layer$setup_layer() is the one step of the build that
# sees the plot. The label is read cheaply: an explicit labs(y = ) first, else
# the text of the y mapping; building the plot's labels here would be slow.
.ip_labelled_layer <- function(layer) {
  ggplot2::ggproto(NULL, layer,
    setup_layer = function(self, data, plot) {
      data <- ggplot2::ggproto_parent(layer, self)$setup_layer(data, plot)
      self$stat_params$y_label <- .ip_y_label(plot, self$computed_mapping)
      data
    }
  )
}

.ip_y_label <- function(plot, mapping) {
  ok <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  explicit <- tryCatch(plot$labels$y, error = function(e) NULL)
  if (ok(explicit)) return(explicit)
  y <- mapping$y
  if (inherits(y, "formula") && length(y) >= 2L) {
    text <- paste(deparse(unclass(y)[[length(y)]]), collapse = " ")
    if (ok(text)) return(text)
  }
  "y"
}

# ggplot2's Stat$compute_layer() turns errors in compute_panel() into a
# "Computation failed" warning and an empty layer. The inference stats must
# never swallow a failure (or a missing declaration), so they run the panels
# themselves and let classed conditions propagate.
.ip_compute_layer <- function(self, data, params, layout) {
  params <- params[intersect(names(params), self$parameters(FALSE))]
  panels <- split(data, data$PANEL, drop = TRUE)
  out <- lapply(panels, function(d) {
    scales <- layout$get_scales(d$PANEL[1L])
    do.call(self$compute_panel, c(list(data = d, scales = scales), params))
  })
  out <- do.call(rbind, unname(out))
  rownames(out) <- NULL
  out
}
