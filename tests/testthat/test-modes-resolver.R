test_that(".ip_mode resolves the four modes", {
  expect_identical(.ip_mode(), list(ink = "#000000", paper = "#FFFFFF", knock = "#FFFFFF"))
  expect_identical(.ip_mode("dark"), list(ink = "#FFFFFF", paper = "#000000", knock = "#000000"))
  expect_identical(.ip_mode("light", TRUE),
                   list(ink = "#000000", paper = "#FFFFFF", knock = NA_character_))
  expect_identical(.ip_mode("dark", TRUE),
                   list(ink = "#FFFFFF", paper = "#000000", knock = NA_character_))
})

test_that(".ip_mode validates its arguments", {
  expect_error(.ip_mode("grey"), class = "inferplotR_invalid_input")
  expect_error(.ip_mode("light", NA), class = "inferplotR_invalid_input")
})
