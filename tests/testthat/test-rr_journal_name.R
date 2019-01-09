context("rr_journal_name")

test_that("rr_journal_name() works", {
  skip_on_cran()

  use_cassette("rr_journal_name", {
    res = rr_journal_name("Journal of Geology")

    expect_is(res, "data.frame")

    expect_named(res, c("title", "issn", "romeocolour", "preprint", "postprint",
                        "pdf", "pre_embargo", "post_embargo", "pdf_embargo"))
    expect_equal(res$issn, "0022-1376")
    expect_is(res$romeocolour, "character")
    expect_is(res$preprint, "character")
  })
})
