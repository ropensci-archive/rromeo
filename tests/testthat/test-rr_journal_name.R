context("rr_journal_name")

test_that("rr_journal_name() works", {
  use_cassette("rr_journal_name", {
    res <- rr_journal_name("Journal of Biogeography")

    expect_is(res, "data.frame")

    expect_named(res, c("title", "provided_issn", "issn", "romeocolour",
                        "preprint", "postprint",
                        "pdf", "pre_embargo", "post_embargo", "pdf_embargo"))
    expect_equal(res$issn, "0305-0270")
    expect_is(res$romeocolour, "character")
    expect_is(res$preprint, "character")
  }, preserve_exact_body_bytes = TRUE)

  use_cassette("rr_journal_name_multiple", {

    given_messages <- capture_messages(
      suppressWarnings({
        res <- rr_journal_name("Biogeography", qtype = "contains", key = NULL)
      })
    )

    expect_match(given_messages[1], "5 journals match your query terms")
    expect_match(given_messages[2],
                 paste0("Recursively fetching data from each journal. ",
                        "This may take some time..."))

    expect_is(res, "data.frame")

    expect_equal(dim(res), c(5, 10))
    expect_named(res, c("title", "provided_issn", "issn", "romeocolour",
                        "preprint", "postprint", "pdf", "pre_embargo",
                        "post_embargo", "pdf_embargo"))
    expect_is(res$issn, "character")
    expect_is(res$title, "character")

    expect_equal(res$issn[[1]], "1345-0662")
    expect_equal(res$title[[1]], "Biogeography")
  })

  use_cassette("rr_journal_name_multiple_exact", {

    res <- rr_journal_name(c("Journal of Biogeography", "PLOS one"),
                           qtype = "exact", key = NULL)

    expect_is(res, "data.frame")

    expect_equal(dim(res), c(2, 10))
    expect_named(res, c("title", "provided_issn", "issn", "romeocolour",
                        "preprint", "postprint", "pdf", "pre_embargo",
                        "post_embargo", "pdf_embargo"))

    expect_is(res$title, "character")
    expect_is(res$provided_issn, "character")
    expect_is(res$issn, "character")
    expect_is(res$romeocolour, "character")
    expect_is(res$preprint, "character")
    expect_is(res$postprint, "character")
    expect_is(res$pdf, "character")
    expect_is(res$pre_embargo, "character")
    expect_is(res$post_embargo, "character")
    expect_is(res$pdf_embargo, "character")

    expect_equal(res$issn[[1]], "0305-0270")
    expect_equal(res$issn[[2]], "1932-6203")
  }, record = "new_episodes")

  use_cassette("rr_journal_name_notfound", {
    expect_error(
      rr_journal_name("Journal of Blabla", qtype = "contains"),
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

  use_cassette("missing_issn", {
    res <- suppressWarnings({
      suppressMessages({
        rr_journal_name("real-Time", qtype = "contains")
      })
    })

    expect_is(res, "data.frame")

    expect_equal(dim(res), c(6, 10))
    expect_named(res, c("title", "provided_issn", "issn", "romeocolour",
                        "preprint", "postprint", "pdf", "pre_embargo",
                        "post_embargo", "pdf_embargo"))

    expect_is(res$title, "character")
    expect_is(res$provided_issn, "character")
    expect_is(res$issn, "character")
    expect_is(res$romeocolour, "character")
    expect_is(res$preprint, "character")
    expect_is(res$postprint, "character")
    expect_is(res$pdf, "character")
    expect_is(res$pre_embargo, "character")
    expect_is(res$post_embargo, "character")
    expect_is(res$pdf_embargo, "character")
  })
})
