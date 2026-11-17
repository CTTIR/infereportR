pg <- PlantGrowth
pg_values <- function(level) pg$weight[pg$group == level]
# Two panels: the first and second half of each group.
pg_facets <- within(PlantGrowth, panel <- rep(rep(c("first", "second"), each = 5), 3))
mt <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg)
welch <- "t_means_two_independent_welch"

# The family of the infereportR vignette: comparisons against ctrl, holm.
manual_family <- function(pairs = list(c("ctrl", "trt1"), c("ctrl", "trt2")), adjust = "holm",
                          id = "inferplotR:compare:t_means_two_independent_welch:panel1") {
  spec <- infercalcR::method_spec(welch)
  rows <- do.call(rbind, lapply(pairs, function(p) {
    infercalcR::result_data(infercalcR::run_inference(
      spec, x = pg_values(p[[2]]), y = pg_values(p[[1]]), result_id = id))
  }))
  rows$row_id <- vapply(pairs, function(p) paste0(p[[2]], "-", p[[1]]), "")
  rows$variable_id <- "weight"
  rows$contrast <- vapply(pairs, function(p) paste(p[[2]], "minus", p[[1]]), "")
  r <- infercalcR::new_inference_result(rows, list(method_id = welch, result_id = id))
  if (adjust != "none") r <- infercalcR::adjust_inference(r, method = adjust, family_id = id)
  r
}

layer_data_of <- function(p, i = 1L) ggplot2::ggplot_build(p)$data[[i]]

draws_silently <- function(p) {
  f <- withr::local_tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(f) else grDevices::png(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  ggplot2::ggplotGrob(p)
}
