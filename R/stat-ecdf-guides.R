# Reading guides for empirical distribution functions (amendment D13).

# Horizontal offset of the curve labels as a fraction of the x range, and of
# the guide label from the panel's left edge in npc.
.ip_ecdf_label_offset <- 0.025

.ip_ecdf_empty <- function() {
  data.frame(x = double(), y = double(), part = character(), label = character(),
             hjust = double(), group = integer(), stringsAsFactors = FALSE)
}

# The name of a group: the first non-positional aesthetic that is constant
# within the group (a label mapping first), else the group number.
.ip_ecdf_group_name <- function(cell) {
  for (nm in c("label", "linetype", "colour", "shape", "fill", "alpha")) {
    v <- cell[[nm]]
    if (!is.null(v) && !is.list(v) && length(unique(v)) == 1L) return(as.character(v[1L]))
  }
  as.character(cell$group[1L])
}

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatEcdfGuides <- ggplot2::ggproto("StatEcdfGuides", ggplot2::Stat,
  required_aes = c("x"),
  compute_panel = function(data, scales, probs = 0.5, guide_label = "median",
                           curve_labels = NULL) {
    data <- data[is.finite(data$x), , drop = FALSE]
    if (!nrow(data)) return(.ip_ecdf_empty())
    groups <- sort(unique(data$group))
    q <- function(g, p) {
      stats::quantile(data$x[data$group == g], p, type = 1, names = FALSE)
    }
    if (!is.null(curve_labels) && length(curve_labels) != length(groups)) {
      .ip_abort(sprintf("curve_labels needs one height per group (%d), not %d",
                        length(groups), length(curve_labels)))
    }
    rows <- list()
    # The guide line across the panel and its label, one per prob.
    rows$guide <- data.frame(x = -Inf, y = probs, part = "guide",
                             label = c(guide_label, rep("", length(probs) - 1L)), hjust = 0,
                             group = -1L, stringsAsFactors = FALSE)
    # Per group a drop line from 0 to each prob at the type-1 quantile.
    rows$drop <- do.call(rbind, lapply(groups, function(g) {
      data.frame(x = vapply(probs, function(p) q(g, p), numeric(1)), y = probs,
                 part = "drop", label = "", hjust = 0, group = g, stringsAsFactors = FALSE)
    }))
    if (!is.null(curve_labels)) {
      span <- diff(range(data$x))
      rows$curve <- do.call(rbind, lapply(seq_along(groups), function(k) {
        g <- groups[k]
        h <- curve_labels[k]
        here <- vapply(groups, function(o) q(o, h), numeric(1))
        # The leftmost curve at this height takes its label on the left.
        leftmost <- here[k] <= min(here)
        data.frame(x = here[k] + (if (leftmost) -1 else 1) * .ip_ecdf_label_offset * span,
                   y = h, part = "curve",
                   label = .ip_ecdf_group_name(data[data$group == g, , drop = FALSE]),
                   hjust = if (leftmost) 1 else 0, group = g, stringsAsFactors = FALSE)
      }))
    }
    out <- do.call(rbind, unname(rows))
    out$PANEL <- data$PANEL[1L]
    rownames(out) <- NULL
    out
  }
)

#' @rdname inferplotR-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomEcdfGuides <- ggplot2::ggproto("GeomEcdfGuides", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(colour = .ip_ink, alpha = NA,
                             size = .ip_label_size_pt / ggplot2::.pt, family = "",
                             guide_family = ""),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord) {
    if (!nrow(data)) return(grid::nullGrob())
    ink <- ggplot2::alpha(data$colour[1L], data$alpha[1L])
    hair <- grid::gpar(col = ink, lwd = .ip_rule_hair * 96 / 72.27, lty = "13",
                       lineend = "butt")
    text_gp <- function(family) {
      if (is.na(family)) family <- ""
      grid::gpar(col = ink, fontsize = data$size[1L] * ggplot2::.pt, fontfamily = family)
    }
    t <- coord$transform(data, panel_params)
    zero <- coord$transform(data.frame(x = data$x, y = 0), panel_params)$y
    grobs <- list()
    guide <- t$part == "guide"
    if (any(guide)) {
      grobs$guide <- grid::segmentsGrob(0, t$y[guide], 1, t$y[guide],
                                        default.units = "native", gp = hair)
      lab <- guide & nzchar(t$label)
      if (any(lab)) {
        grobs$guide_label <- grid::textGrob(
          t$label[lab], x = .ip_ecdf_label_offset, y = grid::unit(t$y[lab], "native"),
          hjust = 0, vjust = -0.3, default.units = "npc",
          gp = text_gp(data$guide_family[1L]))
      }
    }
    drop <- t$part == "drop"
    if (any(drop)) {
      grobs$drop <- grid::segmentsGrob(t$x[drop], zero[drop], t$x[drop], t$y[drop],
                                       default.units = "native", gp = hair)
    }
    curve <- t$part == "curve"
    if (any(curve)) {
      grobs$curve <- grid::textGrob(t$label[curve], x = t$x[curve], y = t$y[curve],
                                    hjust = t$hjust[curve], vjust = 0.5,
                                    default.units = "native",
                                    gp = text_gp(data$family[1L]))
    }
    do.call(grid::grobTree, c(grobs, list(name = "inferplotR.ecdf_guides")))
  }
)

#' Median lines and curve labels for an empirical distribution function
#'
#' A companion to [ggplot2::stat_ecdf()]. At each of `probs` it draws a
#' dotted hairline across the panel and, per group, a dotted drop line from
#' 0 up to that proportion at the group's type-1 quantile
#' (`stats::quantile(type = 1)`, the inverse of the step function that
#' `stat_ecdf()` draws). `guide_label` names the first line at its left end,
#' in condensed type. These are geometry: no number is printed. If a median
#' is to be reported as a number, take it from 'infercalcR'.
#'
#' With `curve_labels`, one cumulative proportion per group in level order,
#' each group's name is written beside its curve at that height: left of the
#' curve that is leftmost at that height (`hjust = 1`), right of the others
#' (`hjust = 0`), offset by 2.5 % of the x range. The name is the group's
#' `label`, `linetype`, `colour`, `shape`, `fill` or `alpha` value, the first
#' that is mapped.
#'
#' @param mapping,data,inherit.aes As in [ggplot2::layer()]. Needs `x` and
#'   the grouping of the `stat_ecdf()` layer.
#' @param ... Further parameters passed to the geom, such as `colour`.
#' @param probs Proportions for the guide lines; 0.5 (the median) by default.
#' @param guide_label Text at the left end of the first guide line;
#'   `"median"` by default, `""` for none.
#' @param curve_labels `NULL` (no curve labels) or one cumulative proportion
#'   per group.
#' @param mode `"light"` (default, black ink) or `"dark"` (white ink); match
#'   the mode of [theme_figure()].
#' @param transparent If `TRUE`, the layer draws no background fill (open
#'   marks stay empty); accepted by every layer of the package.
#' @return A ggplot2 layer. Its computed data has the columns `x`, `y`,
#'   `part` (`"guide"`, `"drop"` or `"curve"`), `label` and `hjust`.
#' @seealso [ggplot2::stat_ecdf()]
#' @examples
#' library(ggplot2)
#' d <- data.frame(hours = c(rlnorm(70, log(6), 0.25), rlnorm(60, log(8.5), 0.25)),
#'                 group = rep(c("Early", "Late"), c(70, 60)))
#' ggplot(d, aes(hours, linetype = group)) +
#'   stat_ecdf(geom = "step", pad = FALSE) +
#'   stat_ecdf_guides(curve_labels = c(0.85, 0.35)) +
#'   scale_linetype_figure() +
#'   labs(x = "Hours to peak (h)", y = "Cumulative proportion") +
#'   theme_figure("slide", legend = "none")
#' @export
stat_ecdf_guides <- function(mapping = NULL, data = NULL, ..., probs = 0.5,
                             guide_label = "median", curve_labels = NULL, mode = "light",
                             transparent = FALSE, inherit.aes = TRUE) {
  if (!is.numeric(probs) || !length(probs) || anyNA(probs) || any(probs <= 0) ||
      any(probs >= 1)) {
    .ip_abort("probs must be proportions strictly between 0 and 1")
  }
  if (!is.character(guide_label) || length(guide_label) != 1L || is.na(guide_label)) {
    .ip_abort("guide_label must be a single string (\"\" for none)")
  }
  if (!is.null(curve_labels) && (!is.numeric(curve_labels) || !length(curve_labels) ||
                                 anyNA(curve_labels) || any(curve_labels <= 0) ||
                                 any(curve_labels >= 1))) {
    .ip_abort("curve_labels must be NULL or proportions strictly between 0 and 1")
  }
  ink <- .ip_mode(mode, transparent)$ink
  dots <- list(...)
  params <- c(list(probs = probs, guide_label = guide_label, curve_labels = curve_labels,
                   family = figure_font("sans"), guide_family = .ip_condensed_family()),
              dots)
  if (!"colour" %in% names(dots) && !"color" %in% names(dots)) params$colour <- ink
  ggplot2::layer(stat = StatEcdfGuides, data = data, mapping = mapping, geom = GeomEcdfGuides,
                 position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
                 params = params)
}
