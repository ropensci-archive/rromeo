context("Tests utility functions for rromeo")

test_that("Can validate ISSN properly", {

  valid_issn   <- c("0395-2037", "1050-124X")
  invalid_issn <- "1234-5678"
  not_issn     <- "12345-678"

  expect_true(validate_issn(valid_issn[1]))
  expect_true(validate_issn(valid_issn[2]))

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

  actual_key <- ifelse(Sys.getenv('SHERPAROMEO_KEY') != "",
                       Sys.getenv('SHERPAROMEO_KEY'), "")

  if (actual_key == "") {
    actual_key <- NULL
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
    expect_error(rr_publisher_id(55, key = "azertyuiop"),
                 paste0("The provided API key is invalid. ",
                        "You can register for a free API at ",
                        "http://www.sherpa.ac.uk/romeo/apiregistry.php"))
  })
})

test_that("Can validate country two-letters ISO codes", {
  expect_true(validate_country_code("__"))
  expect_true(validate_country_code("AA"))
  expect_true(validate_country_code("ZZ"))
  expect_true(validate_country_code("IR"))
  expect_error(validate_country_code("Albania"),
               paste0("Albania is an invalid country code. The country code ",
                      "should be two letter long or '__' for undefined."))

  if (requireNamespace("ISOcodes", quietly = TRUE)) {
    expect_error(validate_country_code("WD"))
  }

  # Replace function temporarily
  mockery::stub(validate_country_code, "requireNamespace", FALSE)
  expect_true(validate_country_code("__"))
  expect_true(validate_country_code("AA"))
  expect_true(validate_country_code("ZZ"))
  expect_true(validate_country_code("IR"))
  expect_true(validate_country_code("WD"))
  expect_error(validate_country_code("Albania"),
               paste0("Albania is an invalid country code. The country code ",
                      "should be two letter long or '__' for undefined."))
})


test_that("Can parse embargo on problematic journal", {
  use_cassette("pb_embargo", {
    a = rromeo::rr_journal_issn("0027-8424")

    a$title = "Proceedings of the National Academy of Sciences"
    a$post = "after media"
    a$pdf  = "after media"
  })
})
