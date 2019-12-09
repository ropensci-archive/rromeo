context("rr_romeo_colour")

test_that("rr_romeo_colour() works", {
  skip_on_cran()
  expect_error(rr_romeo_colour("purple"),
               regexp = paste0("'arg' should be one of ", dQuote("green"), ", ",
                              dQuote("blue"), ", ", dQuote("yellow"), ", ",
                              dQuote("white")))



  use_cassette("rr_romeo_colour", {
    res <- rr_romeo_colour("green")

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
  })
})
