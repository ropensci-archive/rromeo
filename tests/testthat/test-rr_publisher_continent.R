context("rr_publisher_continent")

test_that("rr_publisher_continent() works", {
  expect_error(rr_publisher_continent("a"),
               regexp = paste0("'arg' should be one of \"Africa\", ",
                               "\"Antarctica\", \"Asia\", \"Australasia\", ",
                               "\"Caribbean\", \"Central America\", ",
                               "\"Europe\", \"North America\", \"Oceania\", ",
                               "\"South America\""))



  # Regular Query
  use_cassette("rr_publisher_continent", {
    res = rr_publisher_continent("Caribbean")

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

  # When Publisher is not found
  use_cassette("rr_publisher_continent_notfound", {
    expect_error(rr_publisher_continent("Antarctica"),
                 "No publisher matches the provided id. Please try another id.")
  })
})
