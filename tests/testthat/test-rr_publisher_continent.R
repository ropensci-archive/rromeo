context("rr_publisher_continent")

test_that("rr_publisher_continent() works", {
  expect_error(rr_publisher_continent("a"),
               regexp = paste0("Some continents are not valid, see ",
                               "?rr_publisher_continent to get the list of ",
                               "valid continents"),
               fixed = TRUE)

  # When Publisher is not found
  use_cassette("rr_publisher_continent_notfound", {
    expect_error(rr_publisher_continent("Antarctica"),
                 "No publisher was found. Maybe try another query? ;)",
                 fixed = TRUE)
  })

  skip("Not possible because of encoding problems waiting for a fix in vcr")
  # Regular Query
  use_cassette("rr_publisher_continent", {
    res <- rr_publisher_continent("Caribbean")

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

    expect_equal(dim(res), c(11, 7))
    expect_equal(res$romeoid[[1]], 2192)
  }, preserve_exact_body_bytes = TRUE)

  # Multiple continents
  use_cassette("rr_publisher_continent_multiple", {
    res <- rr_publisher_continent(c("Caribbean", "Central America"))

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

    expect_equal(dim(res), c(16, 7))
  })
})
