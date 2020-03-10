context("rr_journal_issn")

test_that("rr_journal_issn() works", {
  skip_on_cran()

  use_cassette("rr_journal_issn", {
    res <- rr_journal_issn("1947-6264")

    expect_is(res, "data.frame")

    expect_named(res, c("title", "provided_issn", "issn", "romeocolour",
                        "preprint", "postprint", "pdf", "pre_embargo",
                        "post_embargo", "pdf_embargo"))

    expect_is(res$title,         "character")
    expect_is(res$provided_issn, "character")
    expect_is(res$issn,          "character")
    expect_is(res$romeocolour,   "character")
    expect_is(res$preprint,      "character")
    expect_is(res$postprint,     "character")
    expect_is(res$pdf,           "character")
    expect_is(res$pre_embargo,   "character")
    expect_is(res$post_embargo,  "character")
    expect_is(res$pdf_embargo,   "character")

    res_expect <- structure(
      list(title = "A Critical Introduction to Media and Communication Theory",
           provided_issn = "1947-6264", issn = "1947-6264",
           romeocolour = "yellow", preprint = "can", postprint = "restricted",
           pdf = "restricted", pre_embargo = NA_character_,
           post_embargo = "12 months", pdf_embargo = "12 months"),
      class = "data.frame", row.names = c(NA, -1L))

    expect_equal(res, res_expect)
  })

  use_cassette("rr_journal_issn_multiple", {
    res <- rr_journal_issn(c("1947-6264", "0030-1299"))

    expect_is(res, "data.frame")

    expect_named(res, c("title", "provided_issn", "issn", "romeocolour",
                        "preprint", "postprint", "pdf", "pre_embargo",
                        "post_embargo", "pdf_embargo"))

    expect_equal(dim(res), c(2, 10))
    expect_is(res$title,        "character")
    expect_is(res$issn,         "character")
    expect_is(res$romeocolour,  "character")
    expect_is(res$preprint,     "character")
    expect_is(res$postprint,    "character")
    expect_is(res$pdf,          "character")
    expect_is(res$pre_embargo,  "character")
    expect_is(res$post_embargo, "character")
    expect_is(res$pdf_embargo,  "character")

    expect_equal(res$provided_issn[[1]], "1947-6264")
    expect_equal(res$issn[[1]], "1947-6264")
    expect_equal(res$provided_issn[[2]], "0030-1299")
    expect_equal(res$issn[[2]], "0030-1299")
  })
})

test_that("rr_journal_issn() returns provided ISSN", {
  skip_on_cran()

  use_cassette("rr_journal_issn_provided_issn", {
    res <- rr_journal_issn("1463-9084")

    expect_is(res, "data.frame")

    expect_named(res, c("title", "provided_issn", "issn", "romeocolour",
                        "preprint", "postprint", "pdf", "pre_embargo",
                        "post_embargo", "pdf_embargo"))

    expect_is(res$title,         "character")
    expect_is(res$provided_issn, "character")
    expect_is(res$issn,          "character")
    expect_is(res$romeocolour,   "character")
    expect_is(res$preprint,      "character")
    expect_is(res$postprint,     "character")
    expect_is(res$pdf,           "character")
    expect_is(res$pre_embargo,   "character")
    expect_is(res$post_embargo,  "character")
    expect_is(res$pdf_embargo,   "character")
  })
})
