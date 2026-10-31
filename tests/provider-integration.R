library(infercalcR)
library(infereportR)
# Real provider results are finalized before any reporting call.
x <- c(2,4,5,7,9,12); y <- c(1,3,4,4,6,8,11)
r <- run_inference(method_spec("t_means_two_independent_welch"),x=x,y=y,result_id="welch-real")
t <- report_table(r); before <- serialize(r,NULL)
stopifnot(nrow(t)>0L,identical(t$p.value,result_data(r)$p.value),any(grepl("mean",report_text(r),fixed=TRUE)))
h <- as_inference(stats::t.test(x,y),population="Complete synthetic independent groups",groups=c("A","B"),n=c(length(x),length(y)))
stopifnot(identical(report_table(h)$p.value,result_data(h)$p.value))
if (identical(Sys.getenv("INFER_SUITE_FULL"),"true")) {
  b <- as_inference(boschloo_test(1,5,4,7,variant="less"))
  bt <- report_table(b)
  stopifnot(all(is.na(bt$conf.low)),all(is.na(bt$conf.high)),any(grepl("boschloo",report_text(b),ignore.case=TRUE)))
  cl <- as_inference(evaluate_cutoff(classification_spec(counts=c(tp=40,fn=10,fp=20,tn=130))))
  ct <- report_table(cl)
  stopifnot(nrow(ct)>0L,identical(ct$estimate,result_data(cl)$estimate),identical(report_metadata(ct)$kind,"classification"))
} else {
  cat("SKIP legacy Boschloo/classifier provider views: exercised in the full lane with provider hash/JSON dependencies\n")
}
stopifnot(identical(serialize(r,NULL),before))
cat("PASS executed provider integration checks\n")
