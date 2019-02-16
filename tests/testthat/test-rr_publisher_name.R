context("rr_publisher_name")

test_that("rr_publisher_name() works", {
  expect_error(rr_publisher_name("a", qtype = "bla"),
               regexp = paste0("'arg' should be one of ", dQuote("all"), ", ",
                               dQuote("any"), ", ", dQuote("exact")))

  # Regular Query
  use_cassette("rr_publisher_name", {
    res = rr_publisher_name("Swiss Chemistry", qtype = "exact")

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

    expect_equal(res$alias, "Swiss Chemistry Society")
    expect_equal(res$pdf,   "restricted")
  })

  # Multiple publishers
  use_cassette("rr_publisher_name_multiple", {
    res = rr_publisher_name(c("Swiss Chemistry", "Nordic Ecological Society"),
                            qtype = "exact")

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
    expect_equal(res$romeoid, c(411, 88))
  })

  # When Publisher is not found
  use_cassette("rr_publisher_name_notfound", {
    expect_error(rr_publisher_name("huhuhqsdhqdjh"),
                 "No publisher was found. Maybe try another query? ;)",
                 fixed = TRUE)
  })
})
