library(infereportR)
if (!requireNamespace("DescrTab2",quietly=TRUE)) {
  if (identical(Sys.getenv("INFER_SUITE_FULL"),"true")) stop("DescrTab2 required in full lane")
  cat("SKIP DescrTab2 integration: optional backend absent in minimal lane\n")
} else if (!identical(dt2_version <- as.character(utils::packageVersion("DescrTab2")),"2.1.26")) {
  # The adapter is pinned to 2.1.26; CRAN's archive only has older releases.
  if (identical(Sys.getenv("INFER_SUITE_FULL"),"true")) stop("DescrTab2 2.1.26 required in full lane, found ",dt2_version)
  cat("SKIP DescrTab2 integration: installed version",dt2_version,"is not the pinned 2.1.26\n")
} else local({
  d <- data.frame(value=c(1,2,4,5,3,5,6,9),group=factor(rep(c("control","treated"),each=4)))
  # Explicit computation happens once before importing/rendering.
  native <- DescrTab2::descr(d,group="group",format_options=list(print_CI=TRUE))
  # The numeric print route must not evaluate stored presentation callbacks.
  blocked_callback <- function(...) stop("presentation callback must not execute")
  environment(blocked_callback) <- baseenv()
  native$format$p <- blocked_callback
  native$format$summary_stats <- lapply(native$format$summary_stats,function(z) blocked_callback)
  native$format$reshape_rows <- list(custom=list(args="mean",fun=blocked_callback))
  original <- serialize(native,NULL)
  imported <- import_descrtab2(native)
  stopifnot(identical(serialize(native,NULL),original),length(imported$native$group$var)==0L,
    identical(levels(imported$native$group$var),levels(native$group$var)))
  stopifnot(identical(report_metadata(imported)$tests$value,native$variables$value$test_list[c("p","test_name","CI_name","CI")]),is.null(imported$native$format$p))
  functions <- list(DescrTab2=c("descr","sig_test"),stats=c("t.test","wilcox.test","prop.test","fisher.test","chisq.test","lm","glm","confint","p.adjust"))
  for (pkg in names(functions)) for (fn in functions[[pkg]]) suppressMessages(trace(fn,tracer=quote(stop("forbidden computation while rendering native results")),where=asNamespace(pkg),print=FALSE))
  on.exit(for (pkg in names(functions)) for (fn in functions[[pkg]]) suppressMessages(untrace(fn,where=asNamespace(pkg))),add=TRUE)
  table <- render_descrtab2(imported,"table")
  text <- render_descrtab2(imported,"text")
  html <- render_descrtab2(imported,"html")
  stopifnot(is.data.frame(table),nrow(table)>0L,identical(table$p[!is.na(table$p)],native$variables$value$test_list$p))
  stopifnot(is.character(text),length(text)>0L,is.character(html),any(grepl("<table",html,fixed=TRUE)))
  stopifnot(!any(c("CI_upper","CI_lower") %in% names(table)),all(c("native_CI_first","native_CI_second") %in% names(table)),
    identical(table$native_CI_first[1],native$variables$value$test_list$CI[1]),
    identical(table$native_CI_second[1],native$variables$value$test_list$CI[2]))
  stopifnot(identical(serialize(native,NULL),original))
  tools::assertError(import_descrtab2(native,source_version="2.1.16"),classes="infereportR_invalid_input")
  tools::assertError(import_descrtab2(list()),classes="infereportR_invalid_input")
  tools::assertError(render_descrtab2(native),classes="infereportR_invalid_input")
  out <- Sys.getenv("INFER_SUITE_REPORT_DIR",unset=tempfile("infereport-native-"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
  writeLines(text,file.path(out,"descrtab2-console.txt"));writeLines(html,file.path(out,"descrtab2-table.html"))
  utils::write.csv(table,file.path(out,"descrtab2-native-table.csv"),row.names=FALSE)
  cat("PASS pinned DescrTab2 native import/table/text/HTML rendering without recomputation\n")
})
