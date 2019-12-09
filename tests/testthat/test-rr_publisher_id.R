context("rr_publisher_id")

test_that("rr_publisher_id() works", {
  skip_on_cran()
  expect_error(rr_publisher_id("a"),
               regexp = "All provided IDs should be integers")

  # Regular Query
  use_cassette("rr_publisher_id", {
    res <- rr_publisher_id(55)

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

    expect_equal(res$alias, "OUP")
    expect_equal(res$pdf,   "unclear")
  })

  # Multiple publishers
  use_cassette("rr_publisher_id_multiple", {
    res <- rr_publisher_id(c(55, 735))

    expect_is(res, "data.frame")

    expect_named(res, c("romeoid", "publisher", "alias", "romeocolour",
                        "preprint", "postprint", "pdf"))

    expect_equal(dim(res), c(2, 7))
    expect_is(res$romeoid,     "numeric")
    expect_is(res$publisher,   "character")
    expect_is(res$alias,       "character")
    expect_is(res$romeocolour, "character")
    expect_is(res$preprint,    "character")
    expect_is(res$postprint,   "character")
    expect_is(res$pdf,         "character")
    expect_equal(res$romeoid, c(55, 735))
  })

  # When Publisher is not found
  use_cassette("rr_publisher_id_notfound", {
    expect_error(rr_publisher_id(1500000),
                 "No publisher was found. Maybe try another query? ;)",
                 fixed = TRUE)
  })

  # Invalid ID
  expect_error(rr_publisher_id("azerty"), "All provided IDs should be integers")
})
