library(infereportR)

# Exercise actual calculation and imported htest results across native R policies.
note <- "native-reported confidence level; exact attainment may differ by R version"
x <- c(1, 3, 5); y <- c(1.5, 3.5, 5.5, 7)
results <- list()
for (alternative in c("two.sided", "less", "greater")) {
  rank <- infercalcR::run_inference(
    infercalcR::method_spec("rank_sum_mann_whitney", alternative=alternative),
    x=x, y=y, location_shift=TRUE)
  one <- infercalcR::run_inference(
    infercalcR::method_spec("signed_rank_wilcoxon_one_sample", alternative=alternative), x=x, mu=0)
  paired <- infercalcR::run_inference(
    infercalcR::method_spec("signed_rank_wilcoxon", alternative=alternative),
    data=data.frame(id=letters[1:3], x=x+10, y=rep(10, 3)))
  native_rank <- suppressWarnings(stats::wilcox.test(x, y, alternative=alternative, conf.int=TRUE))
  native_one <- suppressWarnings(stats::wilcox.test(x, alternative=alternative, conf.int=TRUE))
  imported_rank <- infercalcR::as_inference(native_rank,
    population="Synthetic rank comparison", groups=c("A", "B"))
  imported_one <- infercalcR::as_inference(native_one,
    population="Synthetic one-sample comparison", method_id="signed_rank_wilcoxon_one_sample")
  results <- c(results, list(rank, one, paired, imported_rank, imported_one))
}

# Rendering must retain the warning and finalized values without rerunning a test.
local({
  suppressMessages(trace("wilcox.test", tracer=quote(stop("unexpected test during rendering")),
    where=asNamespace("stats"), print=FALSE))
  on.exit(suppressMessages(untrace("wilcox.test", where=asNamespace("stats"))), add=TRUE)
  for (result in results) {
    before <- serialize(result, NULL)
    source <- infercalcR::result_data(result)
    table <- report_table(result)
    text <- report_text(result)
    intervals <- !is.na(source$conf.low) & !is.na(source$conf.high)
    stopifnot(any(intervals), all(grepl(note, text[intervals], fixed=TRUE)),
      identical(table$conf.level, source$conf.level),
      identical(table$conf.low, source$conf.low), identical(table$conf.high, source$conf.high),
      identical(table$p.value, source$p.value),
      identical(report_metadata(text), infercalcR::result_metadata(result)),
      note %in% report_metadata(text)$warnings,
      identical(serialize(result, NULL), before))
  }
})
cat("PASS native rank confidence reporting: 15 calculated/imported results, preserved values and no refit\n")
