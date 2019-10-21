test_that("test if error checking works", {
  expect_error(whichState(1234567890, usa = FALSE))
})
