library(infereportR)
source("helpers/fixtures.R")
if (!requireNamespace("ggplot2",quietly=TRUE)) {
  if (identical(Sys.getenv("INFER_SUITE_FULL"),"true")) stop("ggplot2 required in full lane")
  cat("SKIP ggplot integration: optional backend absent in minimal lane\n")
} else local({
  library(ggplot2)
  x <- infercalcR::adjust_inference(fixture(),"holm",family_id="all-three")
  original <- serialize(x,NULL)
  d <- data.frame(group=factor(rep(c("A","B","C"),each=4),levels=c("A","B","C")),value=c(1,2,3,4,2,3,5,6,3,4,6,7),fill=rep(c("u","v"),6))
  base <- ggplot(d,aes(group,value,fill=fill,colour=fill,group=fill))+geom_point(shape=21)+theme_minimal()
  pos <- data.frame(row_id=x$data$row_id,y_position=c(8,9,10),group1=c("A","A","B"),group2=c("B","C","C"))
  layers <- stat_inference(x,pos,groups=levels(d$group),p="adjusted")
  p1 <- base+layers
  b1 <- ggplot_build(p1); b2 <- ggplot_build(p1)
  stopifnot(nrow(b1$data[[2]])==9L,nrow(b1$data[[3]])==3L,identical(b1$data[[3]]$label,b2$data[[3]]$label))
  stopifnot(identical(b1$data[[3]]$label,report_table(x,p="adjusted")$p_display))
  subpos <- pos[1,,drop=FALSE]
  one <- stat_inference(x,subpos,rows="a-b",groups=levels(d$group),p="adjusted")
  stopifnot(identical(attr(one,"inference_metadata")$adjustment[[1]]$size,3L),identical(attr(one,"annotation_data")$.ir_label,report_table(x,rows="a-b",p="adjusted")$p_display))
  # Original y=100 is transformed once to 2, without changing inference.
  lp <- subpos; lp$y_position <- 100
  logplot <- base+stat_inference(x,lp,rows="a-b",groups=levels(d$group))+scale_y_log10()
  lb <- ggplot_build(logplot)
  stopifnot(identical(lb$data[[3]]$y,2),identical(serialize(x,NULL),original))
  # Explicit group-axis order decouples contrast orientation from visual endpoints.
  reorder <- d[rev(seq_len(nrow(d))),]; reorder$group <- factor(reorder$group,levels=c("C","B","A"))
  reordered <- ggplot(reorder,aes(group,value))+geom_point()+stat_inference(x,subpos,rows="a-b",groups=levels(reorder$group))
  rb <- ggplot_build(reordered)
  stopifnot(as.numeric(rb$data[[2]]$x[1])==3,as.numeric(rb$data[[2]]$xend[1])==2,x$data$contrast[1]=="A minus B")
  dodge <- data.frame(row_id="a-b",y_position=8,xmin=.8,xmax=1.2)
  db <- ggplot_build(base+stat_inference(x,dodge,rows="a-b"))
  stopifnot(as.numeric(db$data[[2]]$x[1])==.8,as.numeric(db$data[[2]]$xend[1])==1.2)
  hpos <- pos; hpos$y_position <- c(8,10.5,13)
  horiz <- base+stat_inference(x,hpos,groups=levels(d$group),p="adjusted",text_hjust=-.1,text_vjust=.5)+scale_y_continuous(expand=expansion(mult=c(.05,.2)))+coord_flip(); hb <- ggplot_build(horiz)
  stopifnot(inherits(hb$layout$coord,"CoordFlip"),identical(hb$data[[3]]$label,b1$data[[3]]$label))
  fac <- x; fac$data$site <- c("east","west","east"); fac$metadata$facet_vars <- "site"
  fp <- pos; fp$site <- fac$data$site
  fd <- rbind(transform(d,site="east"),transform(d,site="west"))
  facplot <- ggplot(fd,aes(group,value,fill=fill))+geom_point()+facet_wrap(~site)+stat_inference(fac,fp,groups=levels(d$group),facet_vars="site")
  fb <- ggplot_build(facplot)
  panel_site <- as.character(fb$layout$layout$site[match(fb$data[[3]]$PANEL,fb$layout$layout$PANEL)])
  stopifnot(identical(panel_site,fac$data$site))
  bad <- fp; bad$site[1] <- "west"; invalid(stat_inference(fac,bad,groups=levels(d$group),facet_vars="site"))
  invalid(stat_inference(fac,pos,groups=levels(d$group),facet_vars="site"))
  invalid(stat_inference(x,rbind(pos,pos[1,]),groups=levels(d$group)))
  invalid(stat_inference(x,pos,groups=c("A","B")))
  invalid(annotate_inference(base+coord_polar(),x,pos,groups=levels(d$group)))
  # Omnibus results are explicitly text-only.
  omni <- fixture(); omni$data$component <- "omnibus"
  invalid(stat_inference(omni,pos,groups=levels(d$group)))
  ot <- data.frame(row_id="a-b",y_position=8,x=2)
  ob <- ggplot_build(base+stat_inference(omni,ot,rows="a-b"))
  stopifnot(length(ob$data)==2L)
  missing <- fixture(); missing$data$p.value[1] <- missing$data$p.reported[1] <- NA_real_; missing$data$status[1] <- "undefined"
  unavailableplot <- base+stat_inference(missing,subpos,rows="a-b",groups=levels(d$group),label="stars")
  ub <- ggplot_build(unavailableplot)
  stopifnot(grepl("unavailable",ub$data[[3]]$label),!identical(ub$data[[3]]$label,"ns"))
  invalid(stat_inference(missing,subpos,rows="a-b",groups=levels(d$group),unavailable="error"))
  # Guard model/test/adjustment entry points while building and drawing twice.
  functions <- c("t.test","wilcox.test","lm","glm","confint","p.adjust")
  for (fn in functions) suppressMessages(trace(fn,tracer=quote(stop("forbidden inference during plot rendering")),where=asNamespace("stats"),print=FALSE))
  on.exit(for (fn in functions) suppressMessages(untrace(fn,where=asNamespace("stats"))),add=TRUE)
  out <- Sys.getenv("INFER_SUITE_PLOT_DIR",unset=tempfile("infereport-plots-"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
  plots <- list(two_groups=base+one,several=p1,facets=facplot,horizontal=horiz,log_scale=logplot,unavailable=unavailableplot)
  for (name in names(plots)) {
    grDevices::pdf(file.path(out,paste0(name,".pdf")),width=7,height=4.5)
    print(plots[[name]]);print(plots[[name]]);grDevices::dev.off()
    grDevices::png(file.path(out,paste0(name,".png")),width=1120,height=720,res=160)
    print(plots[[name]]);grDevices::dev.off()
  }
  stopifnot(identical(serialize(x,NULL),original))
  cat("PASS ggplot integration: 12 required behavior groups and six rendered plot examples\n")
})
