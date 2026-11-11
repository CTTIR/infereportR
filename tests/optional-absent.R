library(infereportR)
full <- identical(Sys.getenv("INFER_SUITE_FULL"),"true")
for (package in "ggplot2") {
  available <- requireNamespace(package,quietly=TRUE)
  if (full && !available) stop(paste("required full integration backend absent:",package))
  if (!available) {
    tools::assertError(stat_inference(NULL,NULL),classes="infereportR_missing_dependency")
    cat("PASS explicit unavailable-feature error:",package,"\n")
  }
}
