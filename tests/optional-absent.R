library(infereportR)
full <- identical(Sys.getenv("INFER_SUITE_FULL"),"true")
for (package in c("ggplot2","DescrTab2")) {
  available <- requireNamespace(package,quietly=TRUE)
  if (full && !available) stop(paste("required full integration backend absent:",package))
  if (!available) {
    if (package=="ggplot2") tools::assertError(stat_inference(NULL,NULL),classes="infereportR_missing_dependency")
    else tools::assertError(import_descrtab2(NULL),classes="infereportR_missing_dependency")
    cat("PASS explicit unavailable-feature error:",package,"\n")
  }
}
