fixture <- function() {
  infercalcR::new_inference_result(data.frame(row_id=c("a-b","a-c","b-c"),variable_id="outcome",
    component="contrast",estimand="difference in means",contrast=c("A minus B","A minus C","B minus C"),
    estimate=c(-.71234567890123,-1.25,-.53765432109877),conf.low=c(-1.2,-2,-1),conf.high=c(-.2,-.5,.2),
    conf.level=.95,interval.method="Welch t",interval.scale="difference",interval.scope="pointwise",
    p.value=c(.004,.00000001,.21),statistic=c(-3.1,-5.2,-1.2),df=c(17.3,18.2,19.1),n=c(24,26,22),
    n.excluded=c(1,0,2),status="ok",stringsAsFactors=FALSE),
    list(method_id="synthetic_contrasts",variant_id="welch",result_id="experiment-1",population="synthetic complete observations"))
}
invalid <- function(expr) tools::assertError(force(expr),classes="infereportR_invalid_input")
