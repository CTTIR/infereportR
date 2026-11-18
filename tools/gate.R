#!/usr/bin/env Rscript
# Quality gate for the package in the current working directory.
#   Rscript tools/gate.R quick     docs in sync, source scan, attribution scan, tests
#   Rscript tools/gate.R full      quick + R CMD check --as-cran (no incoming checks) = 0/0/0
#   Rscript tools/gate.R release   full + coverage >= 90 % + CRAN incoming check (known items only)
# Exit status 0 only if every step passes. Copy to tools/gate.R (tools/ is build-ignored).

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[[1L]] else "quick"
if (!mode %in% c("quick", "full", "release")) stop("mode must be quick, full or release")
failed <- character()
step <- function(name, ok, detail = character()) {
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", name))
  if (length(detail)) cat(paste0("       ", detail), sep = "\n")
  if (!ok) failed <<- c(failed, name)
  invisible(ok)
}
hash_paths <- function(paths) {
  files <- unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE) else p[file.exists(p)]
  }))
  if (!length(files)) return(character())
  stats::setNames(unname(tools::md5sum(files)), files)
}

# 1. Documentation is in sync with roxygen comments --------------------------
before <- hash_paths(c("man", "NAMESPACE"))
suppressMessages(roxygen2::roxygenise())
after <- hash_paths(c("man", "NAMESPACE"))
changed <- union(setdiff(names(after), names(before)),
                 names(after)[names(after) %in% names(before) & after[names(after)] != before[names(after)]])
step("docs-in-sync (roxygenise changed nothing)", !length(changed) && length(before) == length(after),
     if (length(changed)) paste("regenerated:", changed))

# 2. Source scan of R/ -------------------------------------------------------
r_files <- list.files("R", pattern = "[.][Rr]$", full.names = TRUE)
problems <- character()
for (f in r_files) {
  pd <- utils::getParseData(parse(f, keep.source = TRUE))
  calls <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL"]
  if (any(calls == "sapply")) problems <- c(problems, paste(f, ": sapply() - use vapply()"))
  if (any(calls %in% c("library", "require")))
    problems <- c(problems, paste(f, ": library()/require() in package code"))
  if (any(pd$token == "SYMBOL" & pd$text %in% c("T", "F")))
    problems <- c(problems, paste(f, ": T/F - use TRUE/FALSE"))
  if (basename(f) != "print-methods.R" && any(calls %in% c("cat", "print", "message")))
    problems <- c(problems, paste(f, ": cat()/print()/message() outside R/print-methods.R"))
  strings <- pd$text[pd$token == "STR_CONST"]
  if (basename(f) != "tokens.R" && any(grepl("#[0-9A-Fa-f]{6}", strings)))
    problems <- c(problems, paste(f, ": colour literal outside R/tokens.R"))
  # tools::showNonASCIIfile() writes to stderr on R >= 4.6, so capture.output()
  # missed it; test the bytes directly.
  if (any(grepl("[^\\x01-\\x7f]", readLines(f, warn = FALSE), perl = TRUE, useBytes = TRUE)))
    problems <- c(problems, paste(f, ": non-ASCII characters - use \\uXXXX escapes"))
}
step("source-scan (R/)", !length(problems), problems)

# 3. No AI attribution anywhere (tracked files and commit messages) ------------
pattern <- "(?i)(claude|anthropic|generated with|ai[- ]assisted|chatgpt|copilot|openai)"
tracked <- system2("git", c("ls-files"), stdout = TRUE)
tracked <- tracked[file.exists(tracked) & tracked != "tools/gate.R" &
                   !grepl("[.](png|ico|pdf|svg|tar[.]gz)$", tracked)]
hits <- character()
for (f in tracked) {
  lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
  idx <- grep(pattern, lines, perl = TRUE)
  if (length(idx)) hits <- c(hits, paste0(f, ":", idx))
}
msgs <- suppressWarnings(system2("git", c("log", shQuote("--format=%H %B")), stdout = TRUE))
if (!is.null(attr(msgs, "status"))) stop("git log failed; run the gate inside the repository")
bad_msgs <- grep(pattern, msgs, perl = TRUE, value = TRUE)
step("attribution-scan (files + commit messages)", !length(hits) && !length(bad_msgs),
     c(hits, bad_msgs))

# 4. Tests -------------------------------------------------------------------
res <- as.data.frame(testthat::test_local(".", reporter = "silent", stop_on_failure = FALSE))
n_fail <- sum(res$failed); n_err <- sum(res$error); n_warn <- sum(res$warning)
step(sprintf("tests (%d expectations, %d skipped)", sum(res$nb), sum(res$skipped)),
     n_fail == 0 && n_err == 0 && n_warn == 0,
     sprintf("failed %d, errors %d, warnings %d", n_fail, n_err, n_warn))

# 5. R CMD check --as-cran ---------------------------------------------------
if (mode %in% c("full", "release")) {
  incoming <- if (mode == "release") "true" else "false"
  chk <- rcmdcheck::rcmdcheck(".", args = c("--as-cran", "--no-manual"), build_args = "--no-manual",
                              error_on = "never", quiet = TRUE,
                              env = c("_R_CHECK_CRAN_INCOMING_" = incoming,
                                      "_R_CHECK_CRAN_INCOMING_REMOTE_" = incoming,
                                      "_R_CHECK_SYSTEM_CLOCK_" = "FALSE"))
  issues <- c(chk$errors, chk$warnings, chk$notes)
  if (mode == "release") {
    # The only accepted items: CRAN incoming feasibility for a new package whose
    # suite dependencies are not yet on CRAN.
    known <- grepl("New submission|not in mainstream repositories|Strong dependencies not in",
                   issues)
    issues <- issues[!known]
  }
  step(sprintf("R CMD check --as-cran: %d errors, %d warnings, %d notes",
               length(chk$errors), length(chk$warnings), length(chk$notes)),
       !length(issues), issues)
}

# 6. Coverage ----------------------------------------------------------------
if (mode == "release") {
  cov <- covr::percent_coverage(covr::package_coverage("."))
  step(sprintf("coverage %.1f %% (>= 90 %%)", cov), cov >= 90)
}

if (length(failed)) {
  cat("\nGATE FAILED:", paste(failed, collapse = ", "), "\n")
  quit(status = 1L)
}
cat("\nGATE PASSED (", mode, ")\n", sep = "")
