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

  use_cassette("rr_journal_name_multiple", {
    res = rr_journal_name("Biogeography", qtype = "contains", key = NULL)

    expect_is(res, "data.frame")

    expect_equal(dim(res), c(5, 2))
    expect_named(res, c("title", "issn"))
    expect_is(res$issn, "character")
    expect_is(res$title, "character")

    expect_equal(res$issn[[1]], "1345-0662")
    expect_equal(res$title[[1]], "Biogeography")
  })
})
