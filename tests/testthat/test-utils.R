context("Tests utility functions for rromeo")

test_that("Can validate ISSN properly", {

  valid_issn   = c("0395-2037", "1050-124X")
  invalid_issn = "1234-5678"
  not_issn     = "12345-678"

  expect_null(validate_issn(valid_issn[1]))
  expect_null(validate_issn(valid_issn[2]))

  expect_error(validate_issn(invalid_issn),
               regexp = "ISSN is invalid, please check the format",
               fixed = TRUE)
  expect_error(validate_issn(not_issn),
               regexp = "ISSN is invalid, please check the format",
               fixed = TRUE)
})

test_that("Can retrieve the API key", {

  expect_silent(check_key(NULL))
  expect_silent(check_key("abcd"))

  expect_match(check_key("abcd"), "abcd")

  actual_key = ifelse(Sys.getenv('SHERPAROMEO_KEY') != "",
                      Sys.getenv('SHERPAROMEO_KEY'), "")

  if (actual_key == "") {
    actual_key = NULL
  }

  expect_equal(check_key(NULL), actual_key)
})

test_that("Parse answer fails with invalid API", {
  use_cassette("invalid_api_key_journal", {
    expect_error(rr_journal_name("Journal of Geology", key = "azertyuiop"),
                 paste0("The provided API key is invalid. ",
                        "You can register for a free API at ",
                        "http://www.sherpa.ac.uk/romeo/apiregistry.php"))
  })

  use_cassette("invalid_api_key_publisher", {
    expect_error(rr_publisher(55, key = "azertyuiop"),
                 paste0("The provided API key is invalid. ",
                        "You can register for a free API at ",
                        "http://www.sherpa.ac.uk/romeo/apiregistry.php"))
  })
})