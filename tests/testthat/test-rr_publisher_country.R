context("rr_publisher_country")

test_that("rr_publisher_country() works", {
  expect_error(rr_publisher_country("Albania"),
               regexp = paste0("Albania is an invalid country code. ",
                               "The country code should be two letter long or ",
                               "'__' for undefined."))

  # Regular Query
  use_cassette("rr_publisher_country", {
    res = rr_publisher_country("IR")

    expect_is(res, "data.frame")

    expect_named(res, c("romeoid", "publisher", "alias", "romeocolour",
                        "preprint", "postprint", "pdf"))

    expect_is(res$romeoid,     "numeric")
    expect_is(res$publisher,   "character")
    expect_is(res$alias,       "character")
    expect_is(res$romeocolour, "character")
    expect_is(res$preprint,    "character")
    expect_is(res$postprint,   "character")
    expect_is(res$pdf,         "character")

    expect_equal(dim(res), c(18, 7))

    expect_equal(res$romeoid[[1]], 1936)
  })

  # When Publisher is not found
  use_cassette("rr_publisher_country_notfound", {
    expect_error(rr_publisher_country("CM"),
                 "No publisher matches the provided id. Please try another id.")
  })
})
