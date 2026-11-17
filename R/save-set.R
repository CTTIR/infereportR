# save_figure_set(): one figure in the four mode folders of a Bleisatz deck
# (SPEC amendment 01, D14).

.ip_set_modes <- c("light", "dark", "light-transparent", "dark-transparent")

# A folder name of .ip_set_modes as the arguments of a build function.
.ip_set_mode <- function(folder) {
  list(mode = sub("-transparent$", "", folder),
       transparent = endsWith(folder, "-transparent"))
}

.ip_set_empty <- function() {
  data.frame(mode = character(), file = character(), format = character(),
             width_mm = double(), height_mm = double(), stringsAsFactors = FALSE)
}

# A base name that stays inside its folder: no directory part, no "." or "..",
# no home-directory tilde and no file extension of its own.
.ip_check_base_name <- function(name, arg = "name") {
  .ip_check_string(name, arg)
  if (grepl("[/\\\\]", name) || name %in% c(".", "..") || startsWith(name, "~")) {
    .ip_abort(sprintf("%s must be a file name without a directory part", arg))
  }
  if (grepl("[.](pdf|png|svg|tiff)$", name, ignore.case = TRUE)) {
    .ip_abort(sprintf("%s must be given without a file extension", arg))
  }
  invisible(name)
}

#' Save a figure in light, dark and transparent modes
#'
#' Writes one figure in up to four modes, each into its own folder below
#' `dir`, with the same file name in every folder:
#'
#' ```
#' figures/light/<name>.pdf               figures/light/<name>.png
#' figures/dark/<name>.pdf                figures/dark/<name>.png
#' figures/light-transparent/<name>.pdf   figures/light-transparent/<name>.png
#' figures/dark-transparent/<name>.pdf    figures/dark-transparent/<name>.png
#' ```
#'
#' This is the folder convention of Bleisatz decks: a deck includes figures
#' by name only (`\includegraphics{<name>}`), so one line in the preamble,
#' such as `\graphicspath{{figures/dark/}}` for a deck in
#' `\usetheme[dark]{Bleisatz}`, switches every figure of the deck at once.
#'
#' `build` is called once per mode, as `build(mode = , transparent = )` with
#' `mode` `"light"` or `"dark"` and `transparent` `TRUE` or `FALSE`, and must
#' return a ggplot. Pass both on to [theme_figure()], [inferplot()], the
#' layers and builders of the package, or [char_figure()]. Each figure is
#' written with [save_figure()] at the size of `preset`: a PDF (Cairo, fonts
#' embedded) and, with `png = TRUE`, a PNG ('ragg' when installed) at `dpi`.
#' The transparent modes are saved with `transparent = TRUE`, so their PNG
#' files keep an alpha channel and their PDF files have no page background.
#'
#' Nothing is written outside `dir`: `name` must be a plain file name, and the
#' folders are created as needed. Every argument is checked before `build` is
#' first called.
#'
#' @param build A function with the arguments `mode` and `transparent` that
#'   returns a ggplot.
#' @param name File name without directory and extension, used in every mode
#'   folder.
#' @param preset Size preset, see [figure_size()]. Required.
#' @param dir Directory that holds the mode folders; `"figures"` by default.
#' @param modes Mode folders to write, any of `"light"`, `"dark"`,
#'   `"light-transparent"` and `"dark-transparent"` (all four by default).
#'   `character()` writes nothing.
#' @param png If `TRUE` (default), write a PNG beside each PDF.
#' @param dpi Resolution of the PNG files in dots per inch.
#' @return A data.frame with one row per written file and the columns `mode`
#'   (the folder name), `file` (the path), `format` (`"pdf"` or `"png"`),
#'   `width_mm` and `height_mm`. With `modes = character()` it has 0 rows and
#'   the same columns.
#' @seealso [save_figure()] for a single file, [theme_figure()] for the modes,
#'   [figure_size()] for the presets.
#' @examples
#' library(ggplot2)
#' build <- function(mode, transparent) {
#'   ggplot(PlantGrowth, aes(group, weight)) +
#'     inferplot("slide", mode = mode, transparent = transparent) +
#'     labs(x = "Group", y = "Dried weight (g)")
#' }
#' dir <- file.path(tempdir(), "figures")
#' if (capabilities("cairo")) {
#'   files <- save_figure_set(build, "plant-growth", preset = "slide_half", dir = dir,
#'                            dpi = 150)
#'   files
#'   list.files(dir, recursive = TRUE)
#' }
#' unlink(dir, recursive = TRUE)
#' @export
save_figure_set <- function(build, name, preset, dir = "figures",
                            modes = c("light", "dark", "light-transparent", "dark-transparent"),
                            png = TRUE, dpi = 300) {
  if (!is.function(build)) {
    .ip_abort("build must be a function(mode, transparent) that returns a ggplot")
  }
  if (missing(name)) .ip_abort("name must be given")
  .ip_check_base_name(name)
  if (missing(preset)) .ip_abort("preset must be given")
  preset <- .ip_check_choice(preset, .ip_presets$preset, "preset")
  .ip_check_string(dir, "dir")
  if (!is.character(modes) || anyNA(modes) || anyDuplicated(modes) ||
      !all(modes %in% .ip_set_modes)) {
    .ip_abort(sprintf("modes must be distinct values of %s",
                      paste0("\"", .ip_set_modes, "\"", collapse = ", ")))
  }
  .ip_check_flag(png, "png")
  .ip_check_number(dpi, "dpi", min = 0)
  if (dpi <= 0) .ip_abort("dpi must be a positive number")

  size <- figure_size(preset)
  formats <- if (png) c("pdf", "png") else "pdf"
  rows <- lapply(modes, function(folder) {
    args <- .ip_set_mode(folder)
    plot <- build(mode = args$mode, transparent = args$transparent)
    if (!inherits(plot, "ggplot")) {
      .ip_abort(sprintf("build must return a ggplot; for mode \"%s\" it returned class %s",
                        folder, paste(class(plot), collapse = "/")))
    }
    path <- file.path(dir, folder)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    files <- file.path(path, paste0(name, ".", formats))
    for (f in files) {
      save_figure(plot, f, preset = preset, dpi = dpi, transparent = args$transparent)
    }
    data.frame(mode = folder, file = files, format = formats, width_mm = size$width,
               height_mm = size$height, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(.ip_set_empty()), rows))
  rownames(out) <- NULL
  out
}
