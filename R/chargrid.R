#' Redraw a ggplot on a character grid
#'
#' `char_grid()` redraws a single-panel Cartesian ggplot on a monospaced character
#' grid, in the manner of 1980s home-computer screens. The output is deterministic:
#' the same plot and arguments always give the same grid.
#'
#' Supported layers, matched by geom class (subclasses included):
#'
#' * `GeomRect` (`geom_rect()`, `geom_bar()`, `geom_col()`, `geom_tile()`):
#'   cells filled with the fill glyph of the series;
#' * `GeomRibbon` (`geom_ribbon()`, `geom_area()`, `geom_density()`): the area
#'   between `ymin` and `ymax`, filled;
#' * `GeomSmooth` (`geom_smooth()`): the fitted line only, no band;
#' * `GeomPath` (`geom_path()`, `geom_line()`, `geom_step()`): connected
#'   segments; `geom_line()` is sorted by x;
#' * `GeomSegment` (`geom_segment()`, `geom_curve()`): straight segments;
#' * `GeomErrorbar`, `GeomLinerange`, `GeomPointrange`: vertical or horizontal
#'   ranges, with caps for error bars and a point for point ranges;
#' * `GeomHline`, `GeomVline`: reference lines written into blank cells only;
#' * `GeomPoint`: the point glyph of the series;
#' * `GeomText`, `GeomLabel`: the label string, placed by `hjust`.
#'
#' Every other layer (for example `geom_boxplot()` or `geom_violin()`) is skipped
#' with one warning of class `inferplotR_skipped_layer` that names the skipped
#' geom classes. Layers are drawn in order, later layers overwrite earlier ones,
#' and the axes are drawn last.
#'
#' Series are told apart by glyph. The series key comes from the first discrete,
#' non-position scale among `fill`, `colour`, `shape` and `linetype` with at least
#' two breaks; without such a scale every layer uses the first glyph and no legend
#' is shown. The first series draws lines with slope glyphs (`-`, `/`, `|` and a backslash),
#' chosen for cells twice as tall as wide; later series use `.`, `:`, `+` and so on.
#'
#' The grid has, from the top: the title, the y-axis label, the plot rows, the
#' x axis, the tick labels, the x-axis label, the legend and the caption. Use
#' `char_figure()` to turn the grid into a ggplot of text for saving.
#'
#' @param plot A ggplot with one panel and `coord_cartesian()` (or
#'   `coord_fixed()`). Facets, `coord_flip()`, `coord_polar()`, `coord_trans()`
#'   and `coord_sf()` raise an error of class `inferplotR_unsupported`.
#' @param width Grid width in characters, a whole number of at least 24.
#' @param height Grid height in characters, a whole number of at least 10.
#' @param charset `"ascii"` (default) or `"unicode"` for box-drawing line glyphs,
#'   geometric point shapes and block fills.
#' @param case `"asis"` (default) keeps labels as they are; `"upper"` writes every
#'   string (title, labels, tick labels, legend) in upper case. Glyphs are not
#'   changed.
#' @return An object of class `inferplotR_chargrid`: a list with `chars` (a
#'   character matrix of `height` rows and `width` columns, one glyph per cell),
#'   `width`, `height`, `charset` and `legend` (a data frame with columns `key`
#'   (integer), `symbol` and `label` (character); zero rows when there is no
#'   series key). `print()` writes the grid, `format()` and `as.character()`
#'   return its lines.
#' @seealso `char_figure()` and `char_size()` to render and save a grid;
#'   [ggplot2::ggplot_build()] for the built data the grid is drawn from.
#' @examples
#' d <- data.frame(hour = c(0, 2, 4, 8, 12, 24), level = c(9, 14, 19, 17, 13, 10))
#' p <- ggplot2::ggplot(d, ggplot2::aes(hour, level)) +
#'   ggplot2::geom_line() +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Level over time", x = "Hours", y = "Level")
#' char_grid(p, width = 48, height = 16)
#' @export
char_grid <- function(plot, width = 64L, height = 20L, charset = c("ascii", "unicode"),
                      case = c("asis", "upper")) {
  .ip_check_plot(plot)
  width <- .ip_check_count(width, "width", min = 24)
  height <- .ip_check_count(height, "height", min = 10)
  charset <- .ip_check_choice(charset, c("ascii", "unicode"), "charset")
  case <- .ip_check_choice(case, c("asis", "upper"), "case")

  b <- ggplot2::ggplot_build(plot)
  if (nrow(b$layout$layout) != 1L) {
    .ip_abort("plot must have exactly one panel for char_grid(); remove facets",
              code = "unsupported")
  }
  coord <- b$layout$coord
  bad_coord <- c("CoordFlip", "CoordTransform", "CoordTrans", "CoordPolar", "CoordRadial",
                 "CoordSf")
  if (!inherits(coord, "CoordCartesian") || inherits(coord, bad_coord)) {
    .ip_abort("plot must use a plain Cartesian coordinate system for char_grid()",
              code = "unsupported")
  }

  pp <- b$layout$panel_params[[1L]]
  labs <- .ip_cg_labels(plot)
  up <- function(s) if (identical(case, "upper")) toupper(s) else s
  xb <- .ip_cg_breaks(pp$x)
  yb <- .ip_cg_breaks(pp$y)
  xb$label <- up(xb$label)
  yb$label <- up(yb$label)

  ks <- .ip_cg_key_scale(b)
  legend_labels <- if (is.null(ks)) character() else up(ks$labels)
  legend_rows <- .ip_cg_legend_wrap(nchar(legend_labels, type = "chars") + 2L, width)

  lay <- .ip_cg_layout(width, height, title = !is.null(labs$title), ylab = !is.null(labs$y),
                       xlab = !is.null(labs$x), caption = !is.null(labs$caption),
                       legend_rows = length(legend_rows),
                       ytick_width = max(c(nchar(yb$label, type = "chars"), 1L)))
  map <- .ip_cg_map(lay, pp$x.range, pp$y.range)

  cv <- list(m = matrix(" ", nrow = height, ncol = width), lay = lay, col = map$col,
             row = map$row, charset = charset, glyphs = .ip_glyphs[[charset]],
             sym = rep(NA_character_, length(legend_labels)))

  layers <- b$plot$layers
  skipped <- character()
  for (i in seq_along(layers)) {
    d <- b$data[[i]]
    geom <- layers[[i]]$geom
    kind <- .ip_cg_kind(geom)
    if (is.na(kind)) {
      skipped <- c(skipped, class(geom)[[1L]])
      next
    }
    if (!nrow(d)) next
    if (identical(kind, "step")) d <- .ip_cg_stairs(d, layers[[i]])
    keys <- .ip_cg_keys(d, ks)
    cv <- switch(kind,
      rect = .ip_cg_draw_rect(cv, d, keys),
      ribbon = .ip_cg_draw_ribbon(cv, d, keys),
      smooth = .ip_cg_draw_path(cv, d, keys, sort_x = TRUE),
      line = .ip_cg_draw_path(cv, d, keys, sort_x = TRUE),
      step = .ip_cg_draw_path(cv, d, keys, sort_x = FALSE),
      path = .ip_cg_draw_path(cv, d, keys, sort_x = FALSE),
      segment = .ip_cg_draw_segment(cv, d, keys),
      range = .ip_cg_draw_range(cv, d, keys, geom),
      hline = .ip_cg_draw_refline(cv, d, "y"),
      vline = .ip_cg_draw_refline(cv, d, "x"),
      point = .ip_cg_draw_point(cv, d, keys),
      text = .ip_cg_draw_text(cv, d, up)
    )
  }

  m <- .ip_cg_axes(cv$m, lay, xb, yb)
  m <- .ip_cg_titles(m, lay, labs, up)
  sym <- cv$sym
  if (length(sym)) {
    unseen <- is.na(sym)
    sym[unseen] <- .ip_cg_cycle(cv$glyphs$points, which(unseen))
  }
  legend <- data.frame(key = seq_along(legend_labels), symbol = sym, label = legend_labels,
                       stringsAsFactors = FALSE)
  m <- .ip_cg_legend(m, lay, legend, legend_rows)

  if (length(skipped)) {
    .ip_warn(sprintf("char_grid() skipped layers it cannot draw: %s",
                     paste(unique(skipped), collapse = ", ")), code = "skipped_layer")
  }
  structure(list(chars = m, width = width, height = height, charset = charset,
                 legend = legend),
            class = "inferplotR_chargrid")
}

# Plot labels as single strings, or NULL when absent. get_labs() resolves the
# labels on ggplot2 >= 3.5.2; older versions keep them in plot$labels.
.ip_cg_labels <- function(plot) {
  labs <- if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ggplot2::get_labs(plot)
  } else {
    plot$labels
  }
  out <- list()
  for (nm in c("title", "x", "y", "caption")) out[nm] <- list(.ip_cg_text(labs[[nm]]))
  out
}

.ip_cg_text <- function(x) {
  if (is.null(x) || inherits(x, "waiver") || !length(x)) return(NULL)
  if (is.expression(x)) x <- x[[1L]]
  s <- if (is.language(x)) paste(deparse(x), collapse = "") else as.character(x)[[1L]]
  if (is.na(s) || !nzchar(s)) NULL else s
}

# Break positions (npc) and labels of a view scale, inside [0, 1] only.
.ip_cg_breaks <- function(vs) {
  pos <- as.numeric(vs$break_positions())
  lab <- as.character(vs$get_labels())
  n <- min(length(pos), length(lab))
  pos <- pos[seq_len(n)]
  lab <- lab[seq_len(n)]
  keep <- is.finite(pos) & pos >= 0 & pos <= 1 & !is.na(lab)
  data.frame(pos = pos[keep], label = lab[keep], stringsAsFactors = FALSE)
}

# Greedy wrap of legend entries of the given widths, 3 blanks apart, into rows
# of at most `width` characters. Returns a list of index vectors, one per row.
.ip_cg_legend_wrap <- function(widths, width) {
  rows <- list()
  cur <- integer()
  used <- 0L
  for (i in seq_along(widths)) {
    need <- if (length(cur)) used + 3L + widths[[i]] else widths[[i]]
    if (length(cur) && need > width) {
      rows[[length(rows) + 1L]] <- cur
      cur <- i
      used <- widths[[i]]
    } else {
      cur <- c(cur, i)
      used <- need
    }
  }
  if (length(cur)) rows[[length(rows) + 1L]] <- cur
  rows
}

# Rows and columns of every region (SPEC 4.10, layout steps 1-4).
.ip_cg_layout <- function(width, height, title = FALSE, ylab = FALSE, xlab = FALSE,
                          caption = FALSE, legend_rows = 0L, ytick_width = 1L) {
  top <- 1L
  title_row <- NA_integer_
  ylab_row <- NA_integer_
  if (title) {
    title_row <- 1L
    top <- 3L
  }
  if (ylab) {
    ylab_row <- top
    top <- top + 1L
  }
  bottom <- 2L + as.integer(xlab) + as.integer(legend_rows) + as.integer(caption)
  r0 <- top
  r1 <- as.integer(height) - bottom
  yw <- max(as.integer(ytick_width), 1L)
  ac <- yw + 2L
  c0 <- ac + 1L
  c1 <- as.integer(width) - 1L
  ph <- r1 - r0 + 1L
  pw <- c1 - c0 + 1L
  if (ph < 4L || pw < 10L) {
    .ip_abort(sprintf(
      "a %d x %d grid is too small for the labels; plot area would be %d x %d (needs 10 x 4)",
      as.integer(width), as.integer(height), pw, ph), code = "too_small")
  }
  list(width = as.integer(width), height = as.integer(height), title_row = title_row,
       ylab_row = ylab_row, r0 = r0, r1 = r1, c0 = c0, c1 = c1, ac = ac, yw = yw, ph = ph,
       pw = pw, axis_row = r1 + 1L, tick_row = r1 + 2L,
       xlab_row = if (xlab) r1 + 3L else NA_integer_,
       legend_row = r1 + 3L + as.integer(xlab),
       caption_row = if (caption) as.integer(height) else NA_integer_)
}

# Data-to-cell mapping closures. A degenerate range maps to the middle.
.ip_cg_map <- function(lay, xr, yr) {
  frac <- function(v, r) {
    d <- diff(r)
    if (!is.finite(d) || d == 0) return(rep(0.5, length(v)))
    (as.numeric(v) - r[[1L]]) / d
  }
  force(lay)
  list(
    col = function(x) lay$c0 + .ip_round(frac(x, xr) * (lay$pw - 1L)),
    row = function(y) lay$r1 - .ip_round(frac(y, yr) * (lay$ph - 1L))
  )
}

# The series key scale: the first discrete non-position scale (fill, colour,
# shape, linetype) with at least two breaks; NULL when there is none.
.ip_cg_key_scale <- function(b) {
  for (aes in c("fill", "colour", "shape", "linetype")) {
    sc <- b$plot$scales$get_scales(aes)
    if (is.null(sc) || !isTRUE(sc$is_discrete())) next
    br <- sc$get_breaks()
    br <- br[!is.na(br)]
    if (length(br) < 2L) next
    return(list(aes = aes, mapped = sc$map(br),
                labels = as.character(sc$get_labels(br))))
  }
  NULL
}

# Key per row: the index of the row's value among the mapped breaks. Rows of a
# layer without the key aesthetic (or with an unmapped value) get NA and draw
# with key 1, but do not set a legend symbol.
.ip_cg_keys <- function(d, ks) {
  n <- nrow(d)
  if (is.null(ks)) return(rep(1L, n))
  v <- d[[ks$aes]]
  if (is.null(v)) return(rep(NA_integer_, n))
  if (is.list(v) || is.list(ks$mapped)) {
    mapped <- as.list(ks$mapped)
    return(vapply(seq_len(n), function(i) {
      hit <- which(vapply(mapped, identical, logical(1), v[[i]]))
      if (length(hit)) hit[[1L]] else NA_integer_
    }, integer(1)))
  }
  match(as.character(v), as.character(ks$mapped))
}

.ip_cg_cycle <- function(set, key) {
  key <- ifelse(is.na(key), 1L, key)
  set[(as.integer(key) - 1L) %% length(set) + 1L]
}

# Slope glyph for a segment of dc columns and dr rows (rows grow downwards).
# Cells are twice as tall as wide, hence dr * 2. NA for a zero-length segment.
.ip_cg_glyph_slope <- function(dc, dr, charset = "ascii") {
  if (dc == 0 && dr == 0) return(NA_character_)
  a <- atan2(-dr * 2, dc) * 180 / pi
  if (a <= -90) a <- a + 180
  if (a > 90) a <- a - 180
  g <- .ip_glyphs[[charset]]$line1
  if (abs(a) < 22.5) {
    g[["horizontal"]]
  } else if (abs(a) > 67.5) {
    g[["vertical"]]
  } else if (a > 0) {
    g[["rising"]]
  } else {
    g[["falling"]]
  }
}

# Cells of a straight line, n = max(|dc|, |dr|, 1) steps with half-up rounding.
.ip_cg_line <- function(ca, ra, cb, rb) {
  n <- max(abs(cb - ca), abs(rb - ra), 1)
  t <- seq.int(0, n) / n
  cells <- data.frame(r = .ip_round(ra + (rb - ra) * t), c = .ip_round(ca + (cb - ca) * t))
  cells[!duplicated(cells), , drop = FALSE]
}

# Write glyphs into cells inside the plot rectangle; others are dropped.
.ip_cg_set <- function(cv, r, c, g, blank_only = FALSE) {
  lay <- cv$lay
  g <- rep_len(g, length(r))
  ok <- is.finite(r) & is.finite(c) & r >= lay$r0 & r <= lay$r1 & c >= lay$c0 & c <= lay$c1
  if (!any(ok)) return(cv)
  idx <- cbind(as.integer(r[ok]), as.integer(c[ok]))
  g <- g[ok]
  if (blank_only) {
    free <- cv$m[idx] == " "
    idx <- idx[free, , drop = FALSE]
    g <- g[free]
  }
  cv$m[idx] <- g
  cv
}

# Remember the first glyph drawn for each key, for the legend.
.ip_cg_note <- function(cv, key, glyph) {
  if (length(key) != 1L || is.na(key) || key > length(cv$sym) || is.na(glyph)) return(cv)
  if (is.na(cv$sym[[key]])) cv$sym[[key]] <- glyph
  cv
}

.ip_cg_point_glyph <- function(cv, key) .ip_cg_cycle(cv$glyphs$points, key)
.ip_cg_fill_glyph <- function(cv, key) .ip_cg_cycle(cv$glyphs$fills, key)

# Draw one segment between two cells with the line glyph of the key.
.ip_cg_segment <- function(cv, ca, ra, cb, rb, key) {
  if (!all(is.finite(c(ca, ra, cb, rb)))) return(cv)
  k <- if (is.na(key)) 1L else key
  g <- if (k == 1L) {
    .ip_cg_glyph_slope(cb - ca, rb - ra, cv$charset)
  } else {
    .ip_cg_cycle(cv$glyphs$lines, k - 1L)
  }
  if (is.na(g)) g <- .ip_cg_point_glyph(cv, k)
  cells <- .ip_cg_line(ca, ra, cb, rb)
  cv <- .ip_cg_note(cv, key, g)
  .ip_cg_set(cv, cells$r, cells$c, g)
}

.ip_cg_kind <- function(geom) {
  table <- list(rect = "GeomRect", ribbon = "GeomRibbon", smooth = "GeomSmooth",
                line = "GeomLine", step = "GeomStep", path = "GeomPath",
                segment = "GeomSegment",
                range = c("GeomErrorbar", "GeomLinerange", "GeomPointrange"),
                hline = "GeomHline", vline = "GeomVline", point = "GeomPoint",
                text = c("GeomText", "GeomLabel"))
  for (kind in names(table)) if (inherits(geom, table[[kind]])) return(kind)
  NA_character_
}

.ip_cg_draw_rect <- function(cv, d, keys) {
  lay <- cv$lay
  for (j in seq_len(nrow(d))) {
    cs <- cv$col(c(d$xmin[[j]], d$xmax[[j]]))
    rs <- cv$row(c(d$ymin[[j]], d$ymax[[j]]))
    if (anyNA(cs) || anyNA(rs)) next
    cs <- pmin(pmax(cs, lay$c0), lay$c1)
    rs <- pmin(pmax(rs, lay$r0), lay$r1)
    g <- .ip_cg_fill_glyph(cv, keys[[j]])
    cells <- expand.grid(r = seq(min(rs), max(rs)), c = seq(min(cs), max(cs)))
    cv <- .ip_cg_note(cv, keys[[j]], g)
    cv <- .ip_cg_set(cv, cells$r, cells$c, g)
  }
  cv
}

.ip_cg_draw_ribbon <- function(cv, d, keys) {
  if (is.null(d$ymin) || is.null(d$ymax)) return(cv)
  lay <- cv$lay
  grp <- if (is.null(d$group)) rep(1L, nrow(d)) else d$group
  for (part in .ip_cg_parts(keys, grp)) {
    e <- d[part, , drop = FALSE]
    key <- keys[part[[1L]]]
    e <- e[order(e$x), , drop = FALSE]
    e <- e[is.finite(e$x) & is.finite(e$ymin) & is.finite(e$ymax), , drop = FALSE]
    g <- .ip_cg_fill_glyph(cv, key)
    for (j in seq_len(max(nrow(e) - 1L, 0L))) {
      ca <- cv$col(e$x[[j]])
      cb <- cv$col(e$x[[j + 1L]])
      for (cc in seq(ca, cb)) {
        t <- if (cb == ca) 0 else (cc - ca) / (cb - ca)
        lo <- e$ymin[[j]] + (e$ymin[[j + 1L]] - e$ymin[[j]]) * t
        hi <- e$ymax[[j]] + (e$ymax[[j + 1L]] - e$ymax[[j]]) * t
        rs <- pmin(pmax(cv$row(c(lo, hi)), lay$r0), lay$r1)
        rows <- seq(min(rs), max(rs))
        cv <- .ip_cg_note(cv, key, g)
        cv <- .ip_cg_set(cv, rows, rep(cc, length(rows)), g)
      }
    }
  }
  cv
}

# Row indices split by key and group; unmatched keys (NA) form their own series.
.ip_cg_parts <- function(keys, grp) {
  split(seq_along(keys), list(ifelse(is.na(keys), 0L, keys), grp), drop = TRUE)
}

# geom_step(): expand the points into stairs before connecting them.
.ip_cg_stairs <- function(d, layer) {
  dir <- layer$geom_params$direction
  vh <- identical(dir, "vh")
  grp <- if (is.null(d$group)) rep(1L, nrow(d)) else d$group
  out <- lapply(split(seq_len(nrow(d)), grp), function(idx) {
    e <- d[idx, , drop = FALSE]
    n <- nrow(e)
    if (n < 2L) return(e)
    take <- rep(seq_len(n), each = 2L)[-1L]
    s <- e[take, , drop = FALSE]
    pos <- seq(2L, nrow(s), by = 2L)
    if (vh) {
      s$x[pos] <- e$x[seq_len(n - 1L)]
    } else {
      s$y[pos] <- e$y[seq_len(n - 1L)]
    }
    s
  })
  do.call(rbind, unname(out))
}

.ip_cg_draw_path <- function(cv, d, keys, sort_x) {
  grp <- if (is.null(d$group)) rep(1L, nrow(d)) else d$group
  for (part in .ip_cg_parts(keys, grp)) {
    if (sort_x) part <- part[order(d$x[part])]
    key <- keys[[part[[1L]]]]
    cs <- cv$col(d$x[part])
    rs <- cv$row(d$y[part])
    if (length(part) == 1L) {
      cv <- .ip_cg_segment(cv, cs, rs, cs, rs, key)
      next
    }
    for (j in seq_len(length(part) - 1L)) {
      cv <- .ip_cg_segment(cv, cs[[j]], rs[[j]], cs[[j + 1L]], rs[[j + 1L]], key)
    }
  }
  cv
}

.ip_cg_draw_segment <- function(cv, d, keys) {
  cs <- cv$col(d$x)
  rs <- cv$row(d$y)
  ce <- cv$col(d$xend)
  re <- cv$row(d$yend)
  for (j in seq_len(nrow(d))) {
    cv <- .ip_cg_segment(cv, cs[[j]], rs[[j]], ce[[j]], re[[j]], keys[[j]])
  }
  cv
}

.ip_cg_draw_range <- function(cv, d, keys, geom) {
  vert <- cv$glyphs$line1[["vertical"]]
  horiz <- cv$glyphs$line1[["horizontal"]]
  caps <- inherits(geom, "GeomErrorbar")
  flipped <- isTRUE(any(d$flipped_aes)) ||
    (!is.null(d$xmin) && !is.null(d$xmax) && is.null(d$ymin))
  for (j in seq_len(nrow(d))) {
    key <- keys[[j]]
    if (!flipped) {
      cx <- cv$col(d$x[[j]])
      rs <- cv$row(c(d$ymin[[j]], d$ymax[[j]]))
      if (anyNA(c(cx, rs))) next
      rows <- seq(min(rs), max(rs))
      cv <- .ip_cg_note(cv, key, vert)
      cv <- .ip_cg_set(cv, rows, rep(cx, length(rows)), vert)
      if (caps) {
        span <- .ip_cg_cap_span(cv$col(c(d$xmin[[j]], d$xmax[[j]])), cx)
        for (r in unique(rs)) cv <- .ip_cg_set(cv, rep(r, length(span)), span, horiz)
      }
    } else {
      r <- cv$row(d$y[[j]])
      cs <- cv$col(c(d$xmin[[j]], d$xmax[[j]]))
      if (anyNA(c(r, cs))) next
      cols <- seq(min(cs), max(cs))
      cv <- .ip_cg_note(cv, key, horiz)
      cv <- .ip_cg_set(cv, rep(r, length(cols)), cols, horiz)
      if (caps) {
        span <- if (is.null(d$ymin) || is.null(d$ymax)) {
          r
        } else {
          rr <- cv$row(c(d$ymin[[j]], d$ymax[[j]]))
          if (anyNA(rr)) r else seq(min(rr), max(rr))
        }
        for (cx in unique(cs)) cv <- .ip_cg_set(cv, span, rep(cx, length(span)), vert)
      }
    }
    if (inherits(geom, "GeomPointrange")) {
      cv <- .ip_cg_set(cv, cv$row(d$y[[j]]), cv$col(d$x[[j]]), .ip_cg_point_glyph(cv, key))
    }
  }
  cv
}

# Error-bar caps span the bar's width, but at least one cell either side.
.ip_cg_cap_span <- function(cs, cx) {
  if (anyNA(cs)) return(seq(cx - 1L, cx + 1L))
  seq(min(min(cs), cx - 1L), max(max(cs), cx + 1L))
}

.ip_cg_draw_refline <- function(cv, d, axis) {
  lay <- cv$lay
  if (identical(axis, "y")) {
    g <- cv$glyphs$line1[["horizontal"]]
    for (r in unique(cv$row(d$yintercept))) {
      cols <- seq(lay$c0, lay$c1)
      cv <- .ip_cg_set(cv, rep(r, length(cols)), cols, g, blank_only = TRUE)
    }
  } else {
    g <- cv$glyphs$line1[["vertical"]]
    for (cx in unique(cv$col(d$xintercept))) {
      rows <- seq(lay$r0, lay$r1)
      cv <- .ip_cg_set(cv, rows, rep(cx, length(rows)), g, blank_only = TRUE)
    }
  }
  cv
}

.ip_cg_draw_point <- function(cv, d, keys) {
  rs <- cv$row(d$y)
  cs <- cv$col(d$x)
  for (j in seq_len(nrow(d))) {
    g <- .ip_cg_point_glyph(cv, keys[[j]])
    if (is.finite(rs[[j]]) && is.finite(cs[[j]])) cv <- .ip_cg_note(cv, keys[[j]], g)
    cv <- .ip_cg_set(cv, rs[[j]], cs[[j]], g)
  }
  cv
}

.ip_cg_hjust <- function(h) {
  if (is.null(h)) return(0.5)
  if (is.numeric(h)) return(ifelse(is.finite(h), h, 0.5))
  h <- as.character(h)
  out <- rep(0.5, length(h))
  out[h %in% c("left", "inward")] <- 0
  out[h %in% c("right", "outward")] <- 1
  num <- suppressWarnings(as.numeric(h))
  out[is.finite(num)] <- num[is.finite(num)]
  out
}

.ip_cg_draw_text <- function(cv, d, up) {
  if (is.null(d$label)) return(cv)
  labels <- up(as.character(d$label))
  hj <- rep_len(.ip_cg_hjust(d$hjust), nrow(d))
  rs <- cv$row(d$y)
  cs <- cv$col(d$x)
  for (j in seq_len(nrow(d))) {
    s <- labels[[j]]
    if (is.na(s) || !is.finite(rs[[j]]) || !is.finite(cs[[j]])) next
    start <- cs[[j]] - .ip_round(hj[[j]] * (nchar(s, type = "chars") - 1L))
    cv$m <- .ip_cg_put(cv$m, rs[[j]], start, s)
  }
  cv
}

# Write a string cell by cell from (r, c); cells off the grid are dropped.
.ip_cg_put <- function(m, r, c, s) {
  if (is.na(r) || r < 1L || r > nrow(m) || is.na(s) || !nzchar(s)) return(m)
  ch <- strsplit(s, "")[[1L]]
  cc <- c + seq_along(ch) - 1L
  ok <- cc >= 1L & cc <= ncol(m)
  m[r, cc[ok]] <- ch[ok]
  m
}

.ip_cg_axes <- function(m, lay, xb, yb) {
  m[seq(lay$r0, lay$r1), lay$ac] <- "|"
  m[lay$axis_row, seq(lay$c0, lay$c1)] <- "-"
  m[lay$axis_row, lay$ac] <- "+"
  for (j in seq_len(nrow(yb))) {
    r <- lay$r1 - .ip_round(yb$pos[[j]] * (lay$ph - 1L))
    m[r, lay$ac] <- "+"
    lab <- yb$label[[j]]
    m <- .ip_cg_put(m, r, lay$ac - 1L - nchar(lab, type = "chars"), lab)
  }
  last_end <- -Inf
  for (j in seq_len(nrow(xb))) {
    cx <- lay$c0 + .ip_round(xb$pos[[j]] * (lay$pw - 1L))
    m[lay$axis_row, cx] <- "+"
    lab <- xb$label[[j]]
    n <- nchar(lab, type = "chars")
    start <- cx - (n - 1L) %/% 2L
    if (n > 0L && start > last_end + 1L) {
      m <- .ip_cg_put(m, lay$tick_row, start, lab)
      last_end <- start + n - 1L
    }
  }
  m
}

.ip_cg_titles <- function(m, lay, labs, up) {
  if (!is.null(labs$title)) m <- .ip_cg_put(m, lay$title_row, 1L, up(labs$title))
  if (!is.null(labs$y)) m <- .ip_cg_put(m, lay$ylab_row, 1L, up(labs$y))
  if (!is.null(labs$x)) {
    s <- up(labs$x)
    start <- max(1L, lay$c0 + (lay$pw - nchar(s, type = "chars")) %/% 2L)
    m <- .ip_cg_put(m, lay$xlab_row, start, s)
  }
  if (!is.null(labs$caption)) m <- .ip_cg_put(m, lay$caption_row, 1L, up(labs$caption))
  m
}

.ip_cg_legend <- function(m, lay, legend, rows) {
  for (i in seq_along(rows)) {
    entries <- paste(legend$symbol[rows[[i]]], legend$label[rows[[i]]])
    m <- .ip_cg_put(m, lay$legend_row + i - 1L, 1L, paste(entries, collapse = "   "))
  }
  m
}
