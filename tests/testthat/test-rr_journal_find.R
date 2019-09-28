context("test-rr_journal_find")

test_that("rr_journal_find() works", {
  use_cassette("rr_journal_find", {
    res <- rr_journal_find("Geology")

    expect_is(res, "data.frame")

    expect_named(res, c("title", "provided_issn", "issn"))
    expect_equal(res$issn, "0091-7613")
  })

  use_cassette("rr_journal_find_multiple", {

    given_messages <- capture_messages(
      suppressWarnings({
        res <- rr_journal_find("Biogeography", qtype = "contains")
      })
    )

    expect_match(given_messages[1], "5 journals match your query terms",
                 fixed = TRUE)
    expect_match(given_messages[2],
                 paste0("Only titles and ISSNs of journals returned. ",
                        "Get more information using `rr_journal_name()`"),
                 fixed = TRUE)

    expect_is(res, "data.frame")

    expect_named(res, c("title", "provided_issn", "issn"))
    expect_equal(dim(res), c(5, 3))
    expect_equal(res$title[1], "Biogeography")
    expect_equal(res$issn[1], "1345-0662")
  })

  use_cassette("rr_journal_find_multiple_exact", {

    res <- rr_journal_find(c("Journal of Biogeography", "PLOS one"),
                           qtype = "exact", key = NULL)

    expect_is(res, "data.frame")

    expect_equal(dim(res), c(2, 3))
    expect_named(res, c("title", "provided_issn", "issn"))

    expect_is(res$title, "character")
    expect_is(res$provided_issn, "character")
    expect_is(res$issn, "character")
    expect_equal(res$issn[[1]], "0305-0270")
    expect_equal(res$issn[[2]], "1932-6203")
  }, record = "new_episodes")

  use_cassette("rr_journal_find_notfound", {
    expect_error(
      rr_journal_find("Journal of Blabla", qtype = "contains"),
      "No journal matches your query terms. Please try another query.")
  })

  use_cassette("rr_journal_find_excess", {
    expect_warning(
      rr_journal_find("Ecology", qtype = "contains"),
      "Your request exceeded SHERPA/RoMEO API's cap of 50 results.")
  })

  use_cassette("api_unreachable", {
    expect_error(rr_journal_find("Journal", qtype = "contains"),
                 paste0("The API endpoint could not be reached. Please try",
                        " again later."))
  })
})
