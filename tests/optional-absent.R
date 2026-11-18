library(infereportR)
# Since infereportR 0.2.0 the figure layer is part of the package and ggplot2
# is imported: loading infereportR loads ggplot2, so no reporting feature is
# optional and stat_inference() never stops for a missing backend.
description <- utils::packageDescription("infereportR")
stopifnot(isNamespaceLoaded("ggplot2"), grepl("ggplot2", description$Imports, fixed = TRUE),
  !grepl("ggplot2", if (is.null(description$Suggests)) "" else description$Suggests, fixed = TRUE))
cat("PASS hard dependency: ggplot2 is imported and loaded with infereportR\n")
