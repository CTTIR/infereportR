bad <- "inferplotR_invalid_input"

test_that(".ip_check_string validates single non-empty strings", {
  expect_invisible(.ip_check_string("a", "x"))
  for (v in list(1, character(), c("a", "b"), NA_character_, "", NULL)) {
    expect_error(.ip_check_string(v, "x"), class = bad)
  }
})

test_that(".ip_check_number validates finite numbers and bounds", {
  expect_invisible(.ip_check_number(2, "x"))
  expect_invisible(.ip_check_number(2L, "x", min = 0, max = 2))
  for (v in list("1", numeric(), c(1, 2), NA_real_, Inf, NaN, NULL)) {
    expect_error(.ip_check_number(v, "x"), class = bad)
  }
  expect_error(.ip_check_number(3, "x", max = 2), class = bad)
  expect_error(.ip_check_number(-1, "x", min = 0), class = bad)
})

test_that(".ip_check_count returns integers and rejects fractions", {
  expect_identical(.ip_check_count(24, "w", min = 24), 24L)
  for (v in list("1", numeric(), c(1, 2), NA_real_, Inf, 1.5, NULL)) {
    expect_error(.ip_check_count(v, "w"), class = bad)
  }
  expect_error(.ip_check_count(10, "w", min = 24), class = bad)
})

test_that(".ip_check_flag accepts TRUE and FALSE only", {
  expect_invisible(.ip_check_flag(TRUE, "f"))
  expect_invisible(.ip_check_flag(FALSE, "f"))
  for (v in list(1, logical(), c(TRUE, FALSE), NA, "TRUE", NULL)) {
    expect_error(.ip_check_flag(v, "f"), class = bad)
  }
})

test_that(".ip_check_choice works like match.arg with classed errors", {
  ch <- c("paper", "slide")
  expect_identical(.ip_check_choice(ch, ch, "target"), "paper")
  expect_identical(.ip_check_choice("slide", ch, "target"), "slide")
  for (v in list("poster", 1, character(), c("paper", "slide", "x"), NA_character_, "",
                 c("paper", "paper"))) {
    expect_error(.ip_check_choice(v, ch, "target"), class = bad)
  }
})

test_that(".ip_check_plot accepts ggplot objects only", {
  p <- ggplot2::ggplot()
  expect_invisible(.ip_check_plot(p))
  for (v in list(1, list(), NULL, "plot", NA)) {
    expect_error(.ip_check_plot(v), class = bad)
  }
})
