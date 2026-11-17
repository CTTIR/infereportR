#' Figure size presets
#'
#' Returns the width and height of a named size preset in millimetres. The
#' journal presets are common single- and double-column widths; APA style
#' itself prescribes no physical figure widths, so check the target journal's
#' author guidelines. The slide presets match the slots of the Bleisatz slide
#' frame (text width 134 mm). Their height of 44 mm is the frame body below a
#' two-line title, minus one source line.
#'
#' | preset | width | height | meaning |
#' |---|---|---|---|
#' | `journal_column` | 85 | 70 | common single-column width |
#' | `journal_page` | 174 | 100 | common double-column width |
#' | `slide_full` | 134 | 44 | Bleisatz frame body, full width |
#' | `slide_side` | 86 | 44 | Bleisatz side figure (0.64 x 134 mm) |
#' | `slide_half` | 64 | 44 | Bleisatz two-column slot (0.48 x 134 mm) |
#'
#' @param preset One of `"journal_column"` (default), `"journal_page"`,
#'   `"slide_full"`, `"slide_side"` or `"slide_half"`.
#' @return A list with the numeric elements `width` and `height` and the
#'   string `units = "mm"`.
#' @seealso [save_figure()], which uses these presets.
#' @examples
#' figure_size()
#' figure_size("slide_half")
#' @export
figure_size <- function(preset = c("journal_column", "journal_page", "slide_full",
                                   "slide_side", "slide_half")) {
  preset <- .ip_check_choice(preset, .ip_presets$preset, "preset")
  row <- .ip_presets[.ip_presets$preset == preset, , drop = FALSE]
  list(width = row$width[[1L]], height = row$height[[1L]], units = "mm")
}

# Wrapped so that tests can simulate a build without Cairo.
.ip_cairo_available <- function() isTRUE(capabilities("cairo"))

.ip_ragg_available <- function() requireNamespace("ragg", quietly = TRUE)

.ip_check_size <- function(x, arg) {
  if (is.null(x)) return(invisible(x))
  .ip_check_number(x, arg, min = 0)
  if (x <= 0) .ip_abort(sprintf("%s must be a positive number", arg))
  invisible(x)
}

#' Save a figure at a preset size
#'
#' Writes a ggplot to a PDF, PNG, SVG or TIFF file at one of the sizes of
#' [figure_size()], or at an explicit width and height, with a white
#' background, or none with `transparent = TRUE`. The device is chosen from
#' the file extension:
#'
#' * `.pdf`: [grDevices::cairo_pdf()], which embeds fonts and supports pattern
#'   fills;
#' * `.png`: the `"png"` device of [ggplot2::ggsave()], which uses 'ragg' when
#'   it is installed;
#' * `.svg`: [grDevices::svg()];
#' * `.tiff`: the `"tiff"` device of [ggplot2::ggsave()].
#'
#' `transparent = TRUE` writes with `bg = "transparent"`: a PNG written by
#' 'ragg' keeps its alpha channel (RGBA) and a PDF has no page background.
#' Only areas the plot itself leaves empty become transparent, so use it with
#' `theme_figure(transparent = TRUE)` or a figure builder called with
#' `transparent = TRUE`; [save_figure_set()] does this for every mode.
#'
#' PDF and SVG output need an R build with Cairo; PNG and TIFF need it only
#' when 'ragg' is not installed. Without it the function fails with an error of
#' class `inferplotR_device_unsupported`.
#'
#' @param plot A ggplot object.
#' @param file Path of the output file. The extension (`pdf`, `png`, `svg` or
#'   `tiff`, in any case) selects the device.
#' @param preset Size preset, see [figure_size()]. Defaults to
#'   `"journal_column"` (85 x 70 mm).
#' @param width,height Width and height in millimetres. Each one given
#'   overrides the corresponding value of the preset.
#' @param dpi Resolution in dots per inch for PNG and TIFF output.
#' @param bg Background colour. `NULL` means white paper, or no background
#'   with `transparent = TRUE`; `bg` and `transparent = TRUE` are exclusive.
#' @param transparent If `TRUE`, the device background is transparent.
#' @return `file`, invisibly.
#' @seealso [figure_size()] for the presets, [save_figure_set()] for all four
#'   modes at once, [ggplot2::ggsave()], which does the writing.
#' @examples
#' p <- ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 1, 3)), ggplot2::aes(x, y)) +
#'   ggplot2::geom_line()
#' out <- save_figure(p, file.path(tempdir(), "figure.png"), dpi = 150)
#' file.exists(out)
#' unlink(out)
#'
#' # A dark figure without background, for a dark slide.
#' q <- p + theme_figure("slide", mode = "dark", transparent = TRUE)
#' out <- save_figure(q, file.path(tempdir(), "figure-dark.png"), preset = "slide_half",
#'                    dpi = 150, transparent = TRUE)
#' unlink(out)
#' @export
save_figure <- function(plot, file, preset = "journal_column", width = NULL, height = NULL,
                        dpi = 600, bg = NULL, transparent = FALSE) {
  .ip_check_plot(plot, "plot")
  .ip_check_string(file, "file")
  formats <- c("pdf", "png", "svg", "tiff")
  ext <- if (grepl("[.][^./\\\\]+$", file)) tolower(sub(".*[.]", "", file)) else ""
  if (!ext %in% formats) {
    .ip_abort("file must end in .pdf, .png, .svg or .tiff")
  }
  size <- figure_size(.ip_check_choice(preset, .ip_presets$preset, "preset"))
  .ip_check_size(width, "width")
  .ip_check_size(height, "height")
  .ip_check_number(dpi, "dpi", min = 0)
  if (dpi <= 0) .ip_abort("dpi must be a positive number")
  .ip_check_flag(transparent, "transparent")
  if (!is.null(bg)) {
    .ip_check_string(bg, "bg")
    if (transparent) .ip_abort("give bg or transparent = TRUE, not both")
  }
  if (is.null(bg)) bg <- if (transparent) .ip_transparent else .ip_paper
  if (!is.null(width)) size$width <- width
  if (!is.null(height)) size$height <- height

  needs_cairo <- ext %in% c("pdf", "svg") || !.ip_ragg_available()
  if (needs_cairo && !.ip_cairo_available()) {
    .ip_abort(sprintf("saving .%s files needs an R build with Cairo support", ext),
              code = "device_unsupported")
  }
  device <- switch(ext,
    pdf = grDevices::cairo_pdf,
    svg = grDevices::svg,
    png = "png",
    tiff = "tiff"
  )
  ggplot2::ggsave(filename = file, plot = plot, device = device, width = size$width,
                  height = size$height, units = "mm", dpi = dpi, bg = bg)
  invisible(file)
}
