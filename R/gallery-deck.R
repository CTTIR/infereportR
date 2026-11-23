# The gallery decks: the twelve Bleisatz figures as the deck of gallery.tex,
# and the Tufte set as a deck and handout in the structure of the
# example-tufte-* decks and doc-tufte-* handouts of Bleisatz 1.3. Both are
# built with the public deck and document API.

.ir_gallery_source <- paste("Simulated data (seed 20261117); figures:",
                            "infereportR::figure_gallery(). Statistics: infercalcR;",
                            "displayed numbers: infereportR.")

# Frame titles, rules and sections of gallery.tex, in figure order 01-12.
.ir_gallery_frames <- data.frame(
  name = c("01-timecourse-direct-labels", "05-ecdf-distribution", "07-small-multiples",
           "02-raw-data-mean-ci", "03-stacked-hatch", "09-paired-slopegraph",
           "04-scatter-correlation", "08-correlation-matrix", "06-forest-subgroups",
           "10-character-mode", "11-character-mode-c64", "12-character-mode-c64-bw"),
  section = c("Change over time", "", "", "Groups", "", "", "Relationships", "", "Effects",
              "Character mode", "", ""),
  title = c("C5a peaks later and higher after severe injury",
            "In severely injured patients the C5a peak arrives about two hours later",
            "Complement and cytokine time courses follow the same pattern",
            "IL-6 at admission rises with injury severity",
            "Organ failure becomes more frequent with injury severity",
            "Lactate falls within 24 hours in most patients",
            "IL-6 at admission correlates with injury severity",
            "Complement factors correlate more with each other than with lactate",
            "A late peak means higher IL-6 in every subgroup",
            "Any line figure can be redrawn in text characters",
            "The same figure, as a C64 would have shown it",
            "The C64 screen also works in two values"),
  label = c("Rule", "Rule", "", "Rule", "Rule", "Rule", "Rule", "Rule", "", "Use", "Rule",
            "Rule"),
  rule = c("Label lines directly.", "Show the distribution, not only its center.", "",
           "Show the data, then the summary.", "Pattern, not color.",
           paste("Show the pairing itself. Two bars of means would hide that one patient",
                 "got worse."),
           "Report the estimate with its interval, not p alone.",
           "Encode with size and fill, not a color ramp.", "",
           "Openers, section breaks, a light moment.",
           paste("The only figure allowed to break the two-value rule, and only for effect.",
                 "The black-and-white version on the next slide keeps the deck 1-bit."),
           paste("Recreate a color effect with inversion and pattern. The deck stays at",
                 "#000 and #FFF.")),
  stringsAsFactors = FALSE
)

# The slides of the Bleisatz gallery deck (figures are functions of the mode).
.ir_gallery_slides <- function(data) {
  caps <- .ir_gallery_captions
  slides <- list()
  for (k in seq_len(nrow(.ir_gallery_frames))) {
    fr <- .ir_gallery_frames[k, ]
    i <- match(fr$name, caps$name)
    if (nzchar(fr$section)) slides <- c(slides, list(section_slide(fr$section)))
    builder <- .ir_gallery_builders[[i]]
    plot <- local({
      b <- builder
      function(mode, transparent) b(data, mode = mode, transparent = transparent)
    })
    slot <- caps$slot[[i]]
    text <- if (nzchar(fr$rule)) stats::setNames(fr$rule, fr$label)
    source <- if (slot == "full") "Simulated data (seed 20261117)." else .ir_gallery_source
    slides <- c(slides, list(figure_slide(plot, fr$title, layout = slot,
                                          caption = caps$caption[[i]], text = text,
                                          source = source, name = fr$name)))
  }
  # Labels of the four mode folders (set by name, not as argument tags).
  modes <- stats::setNames(
    c("Black ink on white. The default set.",
      "White ink on black, for inverse frames and dark decks.",
      "Black ink, no background, no borders: information only, for any light ground.",
      "White ink, no background, no borders: information only, for any dark ground."),
    .ir_set_modes)
  c(slides, list(
    section_slide("Modes"),
    figure_slide(title = "Every figure exists in four modes", layout = "text", text = modes,
                 source = paste("Folders under figures/, the same file names in every folder:",
                                "switching is one \\graphicspath.")),
    figure_slide(title = "Inverse frames take the opposite set", layout = "side",
                 caption = "Figure 1 again, from the opposite mode folder, on an inverse frame.",
                 text = c(How = paste("Inside the frame, point \\graphicspath at the opposite",
                                      "folder.")),
                 source = .ir_gallery_source, name = "01-timecourse-direct-labels",
                 inverse = TRUE),
    figure_slide(title = "Regenerating and reusing the figures", layout = "text",
                 text = c(paste("figure_gallery(\"figures\") writes the four mode folders;",
                                "figure_gallery_deck() writes this deck."),
                          paste("Slots 134, 86 or 64 mm wide, all 44 mm high: the frame body",
                                "below a two-line title, minus one source line."),
                          paste("Include at 100 % (width=\\linewidth in \\sidefigure or a",
                                "0.48 column); figure text stays at 8 to 9 pt.")))
  ))
}

# Median of one marker over the 48 hours, by stratum, from the simulation.
.ir_gallery_medians <- function(data, marker) {
  med <- data$med
  lapply(split(med, med$severity), function(x) x[order(x$hours), marker])
}

.ir_tufte_figure <- function(data, i) {
  function(mode, transparent, palette) {
    .ir_tufte_build(i, data, mode = mode, transparent = transparent, palette = palette)
  }
}

.ir_tufte_sparkline_values <- function(data) {
  rows <- list()
  labels <- character()
  for (mk in c("C5a", "C3a", "IL6")) {
    v <- .ir_gallery_medians(data, mk)
    rows <- c(rows, unname(v))
    unit <- if (mk == "IL6") "pg/ml" else "ng/ml"
    name <- if (mk == "IL6") "IL-6" else mk
    labels <- c(labels, paste(name, tex_muted(unit), .ir_tufte_show(names(v))))
  }
  list(values = rows, labels = labels)
}

# The Tufte deck: the frames of example-tufte-*.tex.
.ir_tufte_slides <- function(data) {
  hours <- c(0, 2, 4, 6, 8, 12, 24, 48)
  c5a <- .ir_gallery_medians(data, "C5a")[[1L]]
  fmt <- function(v) .ir_format_fixed(v, 1L)
  spark <- sparkline_tex(c5a, at = hours, marks = "both")
  intro <- paste("Deaths from early haemorrhage have fallen; late organ failure has not.",
                 "C5a, the anaphylatoxin of the complement cascade, rose from", fmt(c5a[[1L]]),
                 "to a peak of", tex_green(fmt(max(c5a))), "ng/ml at", hours[which.max(c5a)],
                 "hours", spark, "and was back to", tex_red(fmt(c5a[[length(c5a)]])),
                 "by 48 hours. A sample taken at admission sees the start of this curve, not",
                 "its peak.")
  sv <- .ir_tufte_sparkline_values(data)
  fig <- function(i) .ir_tufte_figure(data, i)
  list(
    section_slide("Question"),
    figure_slide(title = "Late organ failure follows the immune response, not the injury",
                 layout = "text", text = intro,
                 source = "Illustrative content; simulated data (seed 20261117)."),
    figure_slide(fig(9), "Every patient for context, the medians in ink",
                 caption = .ir_tufte_captions$caption[[9]], name = "19-context-grey",
                 source = "Simulated data. Figure: infereportR::figure_gallery(set = \"tufte\")."),
    slide_sparkline("Sparklines put the data where the words are", sv$values, sv$labels,
                    at = hours, header = c("Marker", "0-48 h", "Peak", "48 h"),
                    source = paste("Green marks the peak, red the last value, in the line and",
                                   "in the table. Simulated data.")),
    figure_slide(fig(6), "Twenty patients, one design, the same scales", layout = "full",
                 caption = .ir_tufte_captions$caption[[6]], name = "18-small-multiples"),
    section_slide("Evidence"),
    figure_slide(fig(5), "A late peak raises IL-6 in every subgroup",
                 caption = .ir_tufte_captions$caption[[5]], name = "17-slopegraph",
                 source = "Mean IL-6 at the early and the late peak. Simulated data."),
    figure_slide(fig(3), "The box plot without the box", layout = "half",
                 caption = .ir_tufte_captions$caption[[3]], name = "15-quartile-plot"),
    figure_slide(fig(2), "The axes made of data", layout = "half",
                 caption = .ir_tufte_captions$caption[[2]], name = "14-range-frame-dot-dash"),
    figure_slide(fig(4), "Organ failure rises with injury severity",
                 caption = .ir_tufte_captions$caption[[4]], name = "16-bars-white-grid",
                 source = "Simulated data."),
    .ir_new_slide("statement", title = "", inverse = TRUE,
                  text = paste("A single admission sample misses the C5a peak in two of three",
                               "severely injured patients.")),
    keyfigure_slide("Sample twice: at admission and six hours later", "8 h",
                    "median time from admission to the C5a peak, ISS 25 or more",
                    "IQR 6 to 12 h, n = 57, illustrative data"),
    figure_slide(title = "Hand out the evidence, project the argument", layout = "text",
                 text = paste("Tufte's advice for talks is to give the audience a paper handout",
                              "with the full evidence and to project only what everyone must",
                              "see at the same time. The handout written beside this deck has",
                              "the same figures at the same size, with sidenotes, sources and",
                              "the tables in full."))
  )
}

# The Tufte handout: the blocks of doc-tufte-*.tex.
.ir_tufte_blocks <- function(data) {
  hours <- c(0, 2, 4, 6, 8, 12, 24, 48)
  c5a <- .ir_gallery_medians(data, "C5a")[[1L]]
  fmt <- function(v) .ir_format_fixed(v, 1L)
  sv <- .ir_tufte_sparkline_values(data)
  fig <- function(i) .ir_tufte_figure(data, i)
  list(
    document_epigraph("Above all else show the data.", source = "Edward R. Tufte"),
    document_section("Introduction"),
    document_newthought("Late organ failure",
                        paste("has replaced early haemorrhage as the main cause of death after",
                              "severe injury.")),
    document_note("Illustrative content: the data are simulated (seed 20261117)."),
    document_text(paste("Complement activation rises within minutes of injury and peaks hours",
                        "later: C5a went from", fmt(c5a[[1L]]), "to",
                        tex_green(fmt(max(c5a))), "ng/ml",
                        sparkline_tex(c5a, at = hours, marks = "both"), "and back to",
                        tex_red(fmt(c5a[[length(c5a)]])), "by 48 hours.")),
    document_figure(fig(8), layout = "margin", caption = .ir_tufte_captions$caption[[8]],
                    name = "15-quartile-plot-margin"),
    document_section("Methods",
                     paste("The simulated cohort has 128 patients with C5a, C3a and IL-6",
                           "measured at admission and at 2, 4, 6, 8, 12, 24 and 48 hours.")),
    document_note("The same figure files fill the slots of the slides.", numbered = FALSE),
    document_section("Results"),
    document_figure(fig(9), caption = .ir_tufte_captions$caption[[9]], label = "fig:context",
                    name = "19-context-grey", source = "Simulated data."),
    document_text("C5a peaked later and higher with increasing severity (Figure \\ref{fig:context})."),
    document_sparkline(sv$values, sv$labels, at = hours,
                       caption = paste("Median of three markers over 48 hours by stratum.",
                                       "Green is the peak, red the last value."),
                       header = c("Marker", "0-48 h", "Peak", "48 h")),
    document_figure(fig(6), layout = "full", caption = .ir_tufte_captions$caption[[6]],
                    name = "18-small-multiples", source = "Simulated data."),
    document_figure(fig(5), caption = .ir_tufte_captions$caption[[5]], name = "17-slopegraph",
                    source = "Simulated data."),
    document_section("Discussion"),
    document_fullwidth(paste("The data are simulated; strata, sample sizes and effects were",
                             "chosen to exercise the layout. Nothing here should inform a",
                             "sampling protocol."))
  )
}

#' The Bleisatz gallery as a deck
#'
#' `figure_gallery_deck()` writes the example figures of [figure_gallery()]
#' as a complete Bleisatz deck, built with [figure_slide()] and
#' [figure_deck()] and written with [save_deck()]:
#'
#' * `set = "bleisatz"` (default): the twelve figures in the frames of the
#'   reference deck `gallery.tex` of the theme (the same sections, titles,
#'   slots, captions and rules, an inverse frame with the opposite mode
#'   folder), written as `gallery.tex`;
#' * `set = "tufte"`: the Tufte set in the structure of the theme's four
#'   `example-tufte-*` decks, written as `tufte-deck.tex`, and a handout in
#'   the structure of its four `doc-tufte-*` documents (epigraph, new
#'   thought, sidenote and margin note, margin figure, sparklines in text
#'   and table, side and full figures), written with [save_document()] as
#'   `tufte-handout.tex`. The deck options default to `tufte = TRUE`; the
#'   four reference variants are the defaults, `mode = "dark", palette =
#'   "grey"`, `font = "plex", palette = "onebit"` and `mode = "dark", font =
#'   "c64"`.
#'
#' The figures are drawn from the simulated gallery data in the deck's
#' palette, so the decks are the same on every run. A Tufte deck with
#' `font = "c64"` draws them as the C64 Tufte set of
#' `figure_gallery(font = "c64")` into `figures/c64/tufte/`. With `render = TRUE`
#' they are compiled with [render_deck()] and [render_document()], which
#' need LaTeX.
#'
#' @param dir Directory to write into. Required.
#' @param set `"bleisatz"` (default) or `"tufte"`.
#' @param mode `"light"` (default) or `"dark"`.
#' @param progress Progress indicator, see [figure_deck()]; `NULL` for the
#'   theme default.
#' @param render If `TRUE`, also compile the deck (and the handout).
#' @param overwrite If `TRUE`, replace existing files.
#' @param png If `TRUE`, also write PNG files of the figures.
#' @param ... Further options of [figure_deck()] (and [figure_document()]
#'   for the handout), such as `palette`, `font`, `tufte` or
#'   `figure_modes`.
#' @return Invisibly: without `render`, the data.frame of written files of
#'   [save_deck()] (and [save_document()]); with `render = TRUE`, the paths
#'   of the PDF files.
#' @seealso [figure_gallery()], [figure_deck()], [save_deck()]
#' @examples
#' \donttest{
#' if (capabilities("cairo")) {
#'   dir <- file.path(tempdir(), "gallery-deck")
#'   # The Tufte deck and handout with their figures in the light folder. The
#'   # twelve-figure deck (set = "bleisatz") has inverse slides, which take the
#'   # opposite mode: give it figure_modes = c("light", "dark").
#'   files <- figure_gallery_deck(dir, set = "tufte", figure_modes = "light")
#'   table(files$type)
#'   unlink(dir, recursive = TRUE)
#' }
#' }
#' @export
figure_gallery_deck <- function(dir, set = c("bleisatz", "tufte"), mode = c("light", "dark"),
                                progress = NULL, render = FALSE, overwrite = FALSE,
                                png = FALSE, ...) {
  if (missing(dir)) .ir_abort("dir must be given")
  .ir_check_string(dir, "dir")
  set <- .ir_check_choice(set, c("bleisatz", "tufte"), "set")
  mode <- .ir_check_choice(mode, .ir_modes, "mode")
  .ir_check_flag(render, "render")
  .ir_check_flag(overwrite, "overwrite")
  dots <- list(...)
  data <- .ir_gallery_data()
  if (set == "bleisatz") {
    deck <- do.call(figure_deck, c(list(
      .ir_gallery_slides(data), title = "Twelve figures, one rule each",
      subtitle = paste("Example figures for the Bleisatz theme: black and white,",
                       "APA-sized text, slot-exact"),
      author = "R. Heller", institute = "CTTIR", venue = c("Figure gallery", "Bleisatz 1.3"),
      mode = mode, progress = progress), dots))
    files <- save_deck(deck, dir, file = "gallery.tex", overwrite = overwrite, png = png)
    if (render) return(invisible(render_deck(dir, "gallery.tex")))
    return(invisible(files))
  }
  if (is.null(dots$tufte)) dots$tufte <- TRUE
  title <- "Complement activation is a clock, not a switch"
  deck <- do.call(figure_deck, c(list(
    .ir_tufte_slides(data), title = title,
    subtitle = "Sample deck of the Tufte figure set", author = "R. Heller",
    institute = "CTTIR", venue = c("Bleisatz 1.3", "Tufte"), mode = mode,
    progress = progress), dots))
  doc_dots <- dots[setdiff(names(dots), c("sectionpages"))]
  doc <- do.call(figure_document, c(list(
    .ir_tufte_blocks(data), title = title, subtitle = "Sample handout of the Tufte figure set",
    shorttitle = "Complement activation is a clock", author = "R. Heller",
    institute = "CTTIR", venue = c("Bleisatz 1.3", "Tufte"),
    keywords = "complement, C5a, polytrauma, sampling time, simulated data",
    abstract = paste("Illustrative content for the sample handout. In a simulated cohort of",
                     "severely injured patients the complement split product C5a peaked later",
                     "and higher with increasing injury severity."),
    mode = mode, progress = progress), doc_dots))
  handout <- file.path(dir, "tufte-handout.tex")
  .ir_check_targets(handout, overwrite)
  # A C64 deck finds its figures in figures/c64/tufte/: the C64 set.
  font <- if (identical(dots$font, "c64")) "c64" else "plex"
  files <- .ir_with_type(font, save_deck(deck, dir, file = "tufte-deck.tex",
                                         overwrite = overwrite, png = png))
  # The handout shares the figures written for the deck.
  doc_files <- .ir_with_type(font, save_document(doc, dir, file = "tufte-handout.tex",
                                                 overwrite = TRUE, png = png))
  if (render) {
    return(invisible(c(render_deck(dir, "tufte-deck.tex"),
                       render_document(dir, "tufte-handout.tex"))))
  }
  out <- rbind(files, doc_files[!doc_files$file %in% files$file, , drop = FALSE])
  rownames(out) <- NULL
  invisible(out)
}
