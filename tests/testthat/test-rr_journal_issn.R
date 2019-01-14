context("rr_journal_issn")

test_that("rr_journal_issn() works", {
  skip_on_cran()

  use_cassette("rr_journal_issn", {
    res = rr_journal_issn("1947-6264")

    expect_is(res, "data.frame")

    expect_named(res, c("title", "issn", "romeocolour", "preprint", "postprint",
                        "pdf", "pre_embargo", "post_embargo", "pdf_embargo"))

    expect_is(res$title,        "character")
    expect_is(res$issn,         "character")
    expect_is(res$romeocolour,  "character")
    expect_is(res$preprint,     "character")
    expect_is(res$postprint,    "character")
    expect_is(res$pdf,          "character")
    expect_is(res$pre_embargo,  "character")
    expect_is(res$post_embargo, "character")
    expect_is(res$pdf_embargo,  "character")

    res_expect = structure(
      list(title = "A Critical Introduction to Media and Communication Theory",
           issn = "1947-6264", romeocolour = "yellow", preprint = "can",
           postprint = "restricted", pdf = "restricted",
           pre_embargo = NA_character_, post_embargo = "12 months",
           pdf_embargo = "12 months"), class = "data.frame",
      row.names = c(NA, -1L))

    expect_equal(res, res_expect)
  })
})
