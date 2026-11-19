test_that(".ir_mode resolves the four modes", {
  expect_identical(.ir_mode(), list(ink = "#000000", paper = "#FFFFFF", knock = "#FFFFFF"))
  expect_identical(.ir_mode("dark"), list(ink = "#FFFFFF", paper = "#000000", knock = "#000000"))
  expect_identical(.ir_mode("light", TRUE),
                   list(ink = "#000000", paper = "#FFFFFF", knock = NA_character_))
  expect_identical(.ir_mode("dark", TRUE),
                   list(ink = "#FFFFFF", paper = "#000000", knock = NA_character_))
})

test_that(".ir_mode validates its arguments", {
  expect_error(.ir_mode("grey"), class = "infereportR_invalid_input")
  expect_error(.ir_mode("light", NA), class = "infereportR_invalid_input")
})
