context("rr_journal_name")

test_that("rr_journal_name() works", {
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
    false_multiple = capture_warnings(
      res <- rr_journal_name("Biogeography", qtype = "contains", key = NULL)
    )

    expect_match(false_multiple[1], "5 journals match your query terms")
    expect_match(
      false_multiple[2],
      "Select one journal from the provided list or enable multiple = TRUE")

    expect_is(res, "data.frame")

    expect_equal(dim(res), c(5, 2))
    expect_named(res, c("title", "issn"))
    expect_is(res$issn, "character")
    expect_is(res$title, "character")

    expect_equal(res$issn[[1]], "1345-0662")
    expect_equal(res$title[[1]], "Biogeography")
  })

  use_cassette("rr_journal_name_notfound", {
    expect_error(
      rr_journal_name("Journal of Blabla", qtype = "contains", key = ""),
      "No journal matches your query terms. Please try another query.")
  })

  use_cassette("rr_journal_name_excess", {
    expect_warning(
      rr_journal_name("Ecology", qtype = "contains"),
      "Your request exceeded SHERPA/RoMEO API's cap of 50 results.")
  })

  use_cassette("api_unreachable", {
    expect_error(rr_journal_name("Journal", qtype = "contains"),
                 paste0("The API endpoint could not be reached. Please try",
                        " again later."))
  })
})
