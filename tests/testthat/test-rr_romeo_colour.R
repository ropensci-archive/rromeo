context("rr_romeo_colour")

test_that("rr_romeo_colour() works", {
  expect_error(rr_romeo_colour("purple"),
               regexp = paste0("'arg' should be one of ", dQuote("green"), ", ",
                              dQuote("blue"), ", ", dQuote("yellow"), ", ",
                              dQuote("white")))
})
