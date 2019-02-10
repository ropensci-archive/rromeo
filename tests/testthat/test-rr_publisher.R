context("rr_publisher")

test_that("rr_publisher() works", {
  expect_error(rr_publisher("a"), regexp = "id needs to be an integer")

  # Regular Query
  use_cassette("rr_publisher", {
    res = rr_publisher(55)

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
  use_cassette("rr_publisher_multiple", {
    res = rr_publisher(c(55, 735))

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
  use_cassette("rr_publisher_notfound", {
    expect_error(rr_publisher(150000000, key = ""),
                 "No publisher matches the provided id. Please try another id.")
  })

  # Invalid ID
  expect_error(rr_publisher("azerty"), "id needs to be an integer")
})
