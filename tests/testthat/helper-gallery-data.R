# Simulation blocks of reference/gallery/make-figures.R (CTTIR's own code),
# copied verbatim inside functions; P14 replaces this helper with
# .ip_gallery_data(). Each function restores the caller's random seed.

gallery_cohort <- function() {
  withr::with_preserve_seed({
  set.seed(20261117)
  hours <- c(0, 2, 4, 6, 8, 12, 24, 48)
  curve <- function(h, peak, height, base) base + height * (h / peak) * exp(1 - h / peak)
  cohort <- rbind(
    data.frame(id = 1:71, severity = "ISS 16-24", peak = rlnorm(71, log(6), 0.25)),
    data.frame(id = 72:128, severity = "ISS \u2265 25", peak = rlnorm(57, log(8.5), 0.25))
  )
  cohort$severity <- factor(cohort$severity, levels = c("ISS 16-24", "ISS \u2265 25"))
  long <- do.call(rbind, lapply(seq_len(nrow(cohort)), function(i) {
    k <- cohort[i, ]
    height <- if (k$severity == "ISS 16-24") 12 else 22
    data.frame(id = k$id, severity = k$severity, hours = hours,
               C5a = curve(hours, k$peak, height, 9) * rlnorm(length(hours), 0, 0.12),
               C3a = curve(hours, k$peak * 0.8, height * 25, 300) * rlnorm(length(hours), 0, 0.15),
               IL6 = curve(hours, k$peak * 0.6, height * 9, 15) * rlnorm(length(hours), 0, 0.2))
  }))
  cohort
  })
}

gallery_forest_data <- function() {
  withr::with_preserve_seed({
  set.seed(6)
  fp <- data.frame(id = 1:240,
                   age = sample(c("< 50 years", "\u2265 50 years"), 240, TRUE),
                   sex = sample(c("Male", "Female"), 240, TRUE, prob = c(0.72, 0.28)),
                   iss = sample(c("ISS 16-24", "ISS \u2265 25"), 240, TRUE),
                   late = rbinom(240, 1, 0.45))
  fp$il6 <- 60 + 22 * fp$late + 8 * (fp$iss == "ISS \u2265 25") + rnorm(240, 0, 38)
  subsets <- list(
    "All patients" = rep(TRUE, 240),
    "< 50 years" = fp$age == "< 50 years", "\u2265 50 years" = fp$age == "\u2265 50 years",
    "Male" = fp$sex == "Male", "Female" = fp$sex == "Female",
    "ISS 16-24" = fp$iss == "ISS 16-24", "ISS \u2265 25" = fp$iss == "ISS \u2265 25")
  list(fp = fp, subsets = subsets)
  })
}

gallery_markers <- function() {
  withr::with_preserve_seed({
  set.seed(8)
  n8 <- 90
  z <- matrix(rnorm(n8 * 5), n8)
  R <- matrix(c(1, .7, .5, .2, .3,  .7, 1, .6, .3, .4,  .5, .6, 1, .55, .5,
                .2, .3, .55, 1, -.35, .3, .4, .5, -.35, 1), 5)
  mk <- z %*% chol(R)
  colnames(mk) <- c("C3a", "C5a", "IL-6", "IL-10", "Lactate")
  pairs8 <- t(utils::combn(5, 2))
  mk
  })
}

gallery_lactate <- function() {
  withr::with_preserve_seed({
  set.seed(9)
  pl <- data.frame(id = 1:14, admission = round(rnorm(14, 4.4, 1.3), 1))
  pl$h24 <- round(pmax(pl$admission - rnorm(14, 1.6, 0.8), 0.6), 1)
  pl
  })
}
