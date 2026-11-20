# Helpers of the Tufte tests (test-tufte.R, test-figure-tufte.R).

pts_tufte <- data.frame(x = c(2, 5, 3, 9, 7), y = c(10, 4, 8, 6, 12))

# check_figure()'s palette rule passes for `palette`.
palette_ok_any <- function(p, palette) {
  chk <- check_figure(p, "slide", palette = palette)
  chk$status[chk$check == "two_values"] == "pass"
}

tufte_near <- function(object, expected, tol) {
  expect_lte(max(abs(unname(object) - expected)), tol)
}

# The page size of a PDF in mm, from its MediaBox (as in test-gallery.R).
tufte_mediabox_mm <- function(file) {
  bytes <- readBin(file, "raw", file.size(file))
  starts <- grepRaw("stream\r?\n", bytes, all = TRUE, value = FALSE)
  ends <- grepRaw("endstream", bytes, all = TRUE)
  texts <- rawToChar(bytes[!bytes %in% as.raw(0)], multiple = FALSE)
  texts <- c(texts, unlist(lapply(starts, function(s) {
    e <- ends[ends > s][1]
    if (is.na(e)) return(character())
    body <- bytes[(s + 7L):(e - 1L)]
    if (body[1] == as.raw(10)) body <- body[-1]
    out <- tryCatch(memDecompress(body, type = "gzip"), error = function(err) raw())
    if (!length(out)) return(character())
    rawToChar(out[out != as.raw(0)])
  })))
  hit <- regmatches(texts, regexpr("/MediaBox *\\[[^]]*\\]", texts, useBytes = TRUE))
  v <- as.numeric(strsplit(trimws(gsub("[^0-9. ]", " ", hit[[1]])), " +")[[1]])
  (v[3:4] - v[1:2]) * 25.4 / 72
}
