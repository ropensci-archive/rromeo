context("Tests utility functions for rromeo")

test_that("Can validate ISSN properly", {

  valid_issn   = c("0395-2037", "1050-124X")
  invalid_issn = c("1234-5678")

  expect_null(validate_issn(valid_issn[1]))
  expect_null(validate_issn(valid_issn[2]))

  expect_error(validate_issn(invalid_issn),
               regexp = "ISSN is invalid, please check the format")
})
