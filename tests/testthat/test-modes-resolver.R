test_that(".ir_mode resolves the four modes", {
  expect_identical(.ir_mode()[1:3], list(ink = "#000000", paper = "#FFFFFF", knock = "#FFFFFF"))
  expect_identical(.ir_mode("dark")[1:3], list(ink = "#FFFFFF", paper = "#000000", knock = "#000000"))
  expect_identical(.ir_mode("light", TRUE)[1:3],
                   list(ink = "#000000", paper = "#FFFFFF", knock = NA_character_))
  expect_identical(.ir_mode("dark", TRUE)[1:3],
                   list(ink = "#FFFFFF", paper = "#000000", knock = NA_character_))
})

test_that(".ir_mode validates its arguments", {
  expect_error(.ir_mode("grey"), class = "infereportR_invalid_input")
  expect_error(.ir_mode("light", NA), class = "infereportR_invalid_input")
})

test_that(".ir_mode resolves the palettes", {
  expect_named(.ir_mode(), c("ink", "paper", "knock", "grey", "red", "green", "palette"))
  one <- .ir_mode()
  expect_identical(c(one$grey, one$red, one$green), rep("#000000", 3))
  grey <- .ir_mode("dark", palette = "grey")
  expect_identical(c(grey$grey, grey$red, grey$green), c("#999999", "#FFFFFF", "#FFFFFF"))
  light <- .ir_mode(palette = "tufte")
  expect_identical(c(light$grey, light$red, light$green), c("#999999", "#B3202A", "#2E7D32"))
  dark <- .ir_mode("dark", TRUE, palette = "tufte")
  expect_identical(c(dark$red, dark$green, dark$knock), c("#E5484D", "#5DB760", NA))
  c64 <- .ir_mode("dark", palette = "tufte", c64 = TRUE)
  expect_identical(c(c64$grey, c64$red, c64$green), c("#959595", "#CB7E75", "#9AE29B"))
  expect_identical(.ir_mode(palette = "tufte", c64 = TRUE)$red, "#9F4E44")
  expect_identical(.ir_mode(palette = "tufte", c64 = TRUE)$green, "#5CAB5E")
  expect_error(.ir_mode(palette = "colour"), class = "infereportR_invalid_input")
  expect_identical(.ir_palette_colours(), c("#000000", "#FFFFFF"))
  expect_setequal(.ir_palette_colours("tufte"),
                  c("#000000", "#FFFFFF", "#999999", "#B3202A", "#2E7D32", "#E5484D", "#5DB760"))
})
