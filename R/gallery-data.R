# Simulated data of the Bleisatz example figures (amendment D15, A6).
#
# The simulation sections below are ported verbatim from
# reference/gallery/make-figures.R, CTTIR's own code (2026-11-17): same seeds,
# same order of random-number calls. The only edits are namespace prefixes
# (stats::, utils::) required in package code, \uXXXX escapes for non-ASCII
# characters, and the seed handling, which .ip_with_seed() keeps local.

# Evaluate `expr` with the random seed `seed` (default generators) and leave the
# caller's random state as it was: the old .Random.seed is restored, or removed
# again when there was none. withr is only suggested, so this is written out.
.ip_with_seed <- function(seed, expr) {
  env <- globalenv()
  had <- exists(".Random.seed", envir = env, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = env, inherits = FALSE) else NULL
  on.exit({
    if (had) {
      assign(".Random.seed", old, envir = env)
    } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
      rm(".Random.seed", envir = env)
    }
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  expr
}

# The data of all twelve figures as a named list: cohort, long, med (figures 01,
# 05, 07, 10-12), il6 (02), counts (03), sc (04), fp (06), mk (08), pl (09).
.ip_gallery_data <- function() {
  # ---- Simulated cohort (seeded): polytrauma patients, serial sampling -------
  cohort_data <- .ip_with_seed(20261117, {
    hours <- c(0, 2, 4, 6, 8, 12, 24, 48)
    curve <- function(h, peak, height, base) base + height * (h / peak) * exp(1 - h / peak)
    cohort <- rbind(
      data.frame(id = 1:71, severity = "ISS 16-24", peak = stats::rlnorm(71, log(6), 0.25)),
      data.frame(id = 72:128, severity = "ISS \u2265 25", peak = stats::rlnorm(57, log(8.5), 0.25))
    )
    cohort$severity <- factor(cohort$severity, levels = c("ISS 16-24", "ISS \u2265 25"))
    long <- do.call(rbind, lapply(seq_len(nrow(cohort)), function(i) {
      k <- cohort[i, ]
      height <- if (k$severity == "ISS 16-24") 12 else 22
      data.frame(id = k$id, severity = k$severity, hours = hours,
                 C5a = curve(hours, k$peak, height, 9) * stats::rlnorm(length(hours), 0, 0.12),
                 C3a = curve(hours, k$peak * 0.8, height * 25, 300) *
                   stats::rlnorm(length(hours), 0, 0.15),
                 IL6 = curve(hours, k$peak * 0.6, height * 9, 15) *
                   stats::rlnorm(length(hours), 0, 0.2))
    }))
    med <- stats::aggregate(cbind(C5a, C3a, IL6) ~ severity + hours, data = long,
                            FUN = stats::median)
    list(cohort = cohort, long = long, med = med)
  })

  # ---- 02: IL-6 at admission by severity -------------------------------------
  il6 <- .ip_with_seed(2, {
    il6 <- data.frame(
      group = factor(rep(c("ISS < 16", "ISS 16-24", "ISS \u2265 25"), each = 24),
                     levels = c("ISS < 16", "ISS 16-24", "ISS \u2265 25")),
      value = c(stats::rnorm(24, 42, 14), stats::rnorm(24, 68, 20), stats::rnorm(24, 104, 30)))
    il6$value <- pmax(il6$value, 4)
    il6
  })
  groups <- levels(il6$group)

  # ---- 03: organ failure by severity (no random numbers) ---------------------
  counts <- data.frame(
    group = factor(rep(groups, each = 3), levels = groups),
    outcome = factor(rep(c("No organ failure", "Single organ", "Multiple organs"), 3),
                     levels = c("No organ failure", "Single organ", "Multiple organs")),
    n = c(52, 9, 3, 47, 15, 9, 24, 17, 16))
  counts$pct <- 100 * counts$n / stats::ave(counts$n, counts$group, FUN = sum)

  # ---- 04: IL-6 and ISS -------------------------------------------------------
  sc <- .ip_with_seed(4, {
    sc <- data.frame(id = 1:80, iss = round(stats::runif(80, 9, 50)))
    sc$il6 <- exp(log(25) + 0.035 * sc$iss + stats::rnorm(80, 0, 0.45))
    sc
  })

  # ---- 06: subgroups ----------------------------------------------------------
  fp <- .ip_with_seed(6, {
    fp <- data.frame(id = 1:240,
                     age = sample(c("< 50 years", "\u2265 50 years"), 240, TRUE),
                     sex = sample(c("Male", "Female"), 240, TRUE, prob = c(0.72, 0.28)),
                     iss = sample(c("ISS 16-24", "ISS \u2265 25"), 240, TRUE),
                     late = stats::rbinom(240, 1, 0.45))
    fp$il6 <- 60 + 22 * fp$late + 8 * (fp$iss == "ISS \u2265 25") + stats::rnorm(240, 0, 38)
    fp
  })

  # ---- 08: five markers -------------------------------------------------------
  mk <- .ip_with_seed(8, {
    n8 <- 90
    z <- matrix(stats::rnorm(n8 * 5), n8)
    R <- matrix(c(1, .7, .5, .2, .3,  .7, 1, .6, .3, .4,  .5, .6, 1, .55, .5,
                  .2, .3, .55, 1, -.35, .3, .4, .5, -.35, 1), 5)
    mk <- z %*% chol(R)
    colnames(mk) <- c("C3a", "C5a", "IL-6", "IL-10", "Lactate")
    mk
  })

  # ---- 09: lactate, paired ----------------------------------------------------
  pl <- .ip_with_seed(9, {
    pl <- data.frame(id = 1:14, admission = round(stats::rnorm(14, 4.4, 1.3), 1))
    pl$h24 <- round(pmax(pl$admission - stats::rnorm(14, 1.6, 0.8), 0.6), 1)
    pl
  })

  list(cohort = cohort_data$cohort, long = cohort_data$long, med = cohort_data$med,
       il6 = il6, counts = counts, sc = sc, fp = fp, mk = mk, pl = pl)
}

# The seven subgroups of figure 06 as logical row selections of `fp`
# (make-figures.R, verbatim).
.ip_gallery_subsets <- function(fp) {
  list(
    "All patients" = rep(TRUE, 240),
    "< 50 years" = fp$age == "< 50 years", "\u2265 50 years" = fp$age == "\u2265 50 years",
    "Male" = fp$sex == "Male", "Female" = fp$sex == "Female",
    "ISS 16-24" = fp$iss == "ISS 16-24", "ISS \u2265 25" = fp$iss == "ISS \u2265 25")
}
