# save_figure_set(): one figure in the four mode folders of a Bleisatz deck
# (SPEC amendment 01, D14).

.ir_set_modes <- c("light", "dark", "light-transparent", "dark-transparent")
# Every folder save_figure_set() can write: the four modes and the palette folders.
.ir_all_modes <- c(.ir_set_modes, "light-grey", "dark-grey", "light-1bit", "dark-1bit")
# The six folders of the Tufte set (Bleisatz figures/tufte/<folder>/).
.ir_tufte_modes <- c("light", "dark", "light-grey", "dark-grey", "light-1bit", "dark-1bit")

# A folder name of .ir_set_modes as the arguments of a build function. Plain
# and transparent folders take the palette of the set; "-grey" and "-1bit"
# folders fix it to "grey" and "onebit".
.ir_set_mode <- function(folder, palette = "onebit") {
  pal <- if (endsWith(folder, "-grey")) {
    "grey"
  } else if (endsWith(folder, "-1bit")) {
    "onebit"
  } else {
    palette
  }
  list(mode = sub("-(transparent|grey|1bit)$", "", folder),
       transparent = endsWith(folder, "-transparent"), palette = pal)
}

# Calls build with the palette only when it accepts one, so build functions
# written for 0.2.0, function(mode, transparent), keep working.
.ir_set_build <- function(build, args) {
  f <- names(formals(build))
  if ("palette" %in% f || "..." %in% f) {
    return(build(mode = args$mode, transparent = args$transparent, palette = args$palette))
  }
  build(mode = args$mode, transparent = args$transparent)
}

.ir_check_set_modes <- function(modes, allowed = .ir_all_modes) {
  if (!is.character(modes) || anyNA(modes) || anyDuplicated(modes) ||
      !all(modes %in% allowed)) {
    .ir_abort(sprintf("modes must be distinct values of %s",
                      paste0("\"", allowed, "\"", collapse = ", ")))
  }
  invisible(modes)
}

.ir_set_empty <- function() {
  data.frame(mode = character(), file = character(), format = character(),
             width_mm = double(), height_mm = double(), stringsAsFactors = FALSE)
}

# A base name that stays inside its folder: no directory part, no "." or "..",
# no home-directory tilde and no file extension of its own.
.ir_check_base_name <- function(name, arg = "name") {
  .ir_check_string(name, arg)
  if (grepl("[/\\\\]", name) || name %in% c(".", "..") || startsWith(name, "~")) {
    .ir_abort(sprintf("%s must be a file name without a directory part", arg))
  }
  if (grepl("[.](pdf|png|svg|tiff)$", name, ignore.case = TRUE)) {
    .ir_abort(sprintf("%s must be given without a file extension", arg))
  }
  invisible(name)
}

#' Save a figure in light, dark and transparent modes
#'
#' Writes one figure in several modes, by default four, each into its own folder below
#' `dir`, with the same file name in every folder; for `dir = "figures"`:
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
#' **Palette folders.** Beside the four folders above, `modes` may name the
#' palette folders of the Tufte set of Bleisatz (`figures/tufte/<folder>/`):
#' `"light-grey"` and `"dark-grey"` (ink, paper and one grey) and
#' `"light-1bit"` and `"dark-1bit"` (two values). The plain and transparent
#' folders are drawn in `palette`, so the Tufte set uses `palette = "tufte"`
#' with `modes = c("light", "dark", "light-grey", "dark-grey", "light-1bit",
#' "dark-1bit")`: there plain `light` and `dark` mean Tufte's palette. A
#' Bleisatz document or deck with `figures=tufte` finds them through
#' `\figurefolder{figures}`.
#'
#' `build` is called once per mode, as `build(mode = , transparent = )` with
#' `mode` `"light"` or `"dark"` and `transparent` `TRUE` or `FALSE`, and must
#' return a ggplot. If `build` has a `palette` argument (or `...`), it is
#' called with `palette = ` as well (`"onebit"`, `"grey"` or `"tufte"`). Pass
#' them on to [theme_figure()], [report_figure()], the layers and builders of
#' the package, or [char_figure()]. Each figure is
#' written with [save_figure()] at the size of `preset`: a PDF (Cairo, fonts
#' embedded) and, with `png = TRUE`, a PNG ('ragg' when installed) at `dpi`.
#' The light and dark modes are saved on the mode's paper (white or black),
#' so space a figure leaves free, as with `coord_fixed()`, has the colour of
#' its background. The transparent modes are saved with `transparent = TRUE`, so their PNG
#' files keep an alpha channel and their PDF files have no page background.
#'
#' Nothing is written outside `dir`, which has no default, so nothing is
#' written to the working directory unless it is asked for: `name` must be a
#' plain file name, and the folders are created as needed. Every argument is checked before `build` is
#' first called.
#'
#' @param build A function with the arguments `mode` and `transparent`, and
#'   optionally `palette`, that returns a ggplot.
#' @param name File name without directory and extension, used in every mode
#'   folder.
#' @param preset Size preset, see [figure_size()]. Required.
#' @param dir Directory that holds the mode folders, created if needed.
#'   Required; use `"figures"` for the folder layout of a Bleisatz deck.
#' @param modes Mode folders to write, any of `"light"`, `"dark"`,
#'   `"light-transparent"` and `"dark-transparent"` (these four by default),
#'   `"light-grey"`, `"dark-grey"`, `"light-1bit"` and `"dark-1bit"`.
#'   `character()` writes nothing.
#' @param png If `TRUE` (default), write a PNG beside each PDF.
#' @param dpi Resolution of the PNG files in dots per inch.
#' @param palette Palette of the plain and transparent folders: `"onebit"`
#'   (default), `"grey"` or `"tufte"`.
#' @return A data.frame with one row per written file and the columns `mode`
#'   (the folder name), `file` (the path), `format` (`"pdf"` or `"png"`),
#'   `width_mm` and `height_mm`. With `modes = character()` it has 0 rows and
#'   the same columns.
#' @seealso [save_figure()] for a single file, [theme_figure()] for the modes,
#'   [figure_size()] for the presets.
#' @examples
#' library(ggplot2)
#' build <- function(mode, transparent) {
#'   ink <- if (mode == "dark") "white" else "black"
#'   ggplot(PlantGrowth, aes(group, weight)) +
#'     geom_point(shape = 1, colour = ink) +
#'     theme_figure("slide", mode = mode, transparent = transparent) +
#'     labs(x = "Group", y = "Dried weight (g)")
#' }
#' dir <- file.path(tempdir(), "figures")
#' if (capabilities("cairo")) {
#'   files <- save_figure_set(build, "plant-growth", preset = "slide_half", dir = dir,
#'                            modes = "light", png = FALSE)
#'   files
#'   \donttest{
#'   # Light and dark, each in its own folder under the same file name:
#'   save_figure_set(build, "plant-growth", preset = "slide_half", dir = dir,
#'                   modes = c("light", "dark"), png = FALSE)
#'   list.files(dir, recursive = TRUE)
#'   }
#' }
#' unlink(dir, recursive = TRUE)
#' @export
save_figure_set <- function(build, name, preset, dir,
                            modes = c("light", "dark", "light-transparent", "dark-transparent"),
                            png = TRUE, dpi = 300, palette = "onebit") {
  if (!is.function(build)) {
    .ir_abort("build must be a function(mode, transparent) that returns a ggplot")
  }
  if (missing(name)) .ir_abort("name must be given")
  .ir_check_base_name(name)
  if (missing(preset)) .ir_abort("preset must be given")
  preset <- .ir_check_choice(preset, .ir_presets$preset, "preset")
  if (missing(dir)) .ir_abort("dir must be given")
  .ir_check_string(dir, "dir")
  .ir_check_set_modes(modes)
  .ir_check_flag(png, "png")
  .ir_check_number(dpi, "dpi", min = 0)
  if (dpi <= 0) .ir_abort("dpi must be a positive number")
  palette <- .ir_check_choice(palette, .ir_palettes, "palette")

  size <- figure_size(preset)
  formats <- if (png) c("pdf", "png") else "pdf"
  rows <- lapply(modes, function(folder) {
    args <- .ir_set_mode(folder, palette)
    plot <- .ir_set_build(build, args)
    if (!inherits(plot, "ggplot")) {
      .ir_abort(sprintf("build must return a ggplot; for mode \"%s\" it returned class %s",
                        folder, paste(class(plot), collapse = "/")))
    }
    path <- file.path(dir, folder)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    files <- file.path(path, paste0(name, ".", formats))
    for (f in files) {
      # The device background is the mode's paper, so space a figure leaves
      # free (coord_fixed) matches its plot background in dark mode too.
      if (args$transparent) {
        save_figure(plot, f, preset = preset, dpi = dpi, transparent = TRUE)
      } else {
        save_figure(plot, f, preset = preset, dpi = dpi, bg = .ir_mode(args$mode)$paper)
      }
    }
    data.frame(mode = folder, file = files, format = formats, width_mm = size$width,
               height_mm = size$height, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(.ir_set_empty()), rows))
  rownames(out) <- NULL
  out
}
