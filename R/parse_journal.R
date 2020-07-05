#' Parse API answer
#'
#' Returns data.frame from parsed xml API answer.
#'
#' @inheritParams parse_publisher
#' @param type \[`character(1)` in `c("find", "name")`\]\cr{}
#'             If `type = "find"` returns only `title` and `issn` columns if
#'             `type = "name"` returns full data.frame as specified in Returns
#'             sections.
#' @inheritParams check_key
#'
#' @return Returns a data.frame with the following columns:
#' * `title`         \[`character(1)`\]\cr{}
#'                   the name of the journal
#' * `provided_issn` \[`character(1)`\]\cr{}
#'                   the ISSN you provided in your query (might differ from the
#'                   ISSN returned by the API)
#' * `issn`          \[`character(1)`\]\cr{}
#'                   the ISSN of the journal
#' * `romeocolour`   \[`character(1)`\]\cr{}
#'                   the SHERPA/RoMEO colour of the journal
#' * `preprint`      \[`character(1)`\]\cr{}
#'                   is the preprint (not reviewed) archivable?
#' * `postprint`     \[`character(1)`\]\cr{}
#'                   is the postprint (reviewed but not formatted) archivable?
#' * `pdf`           \[`character(1)`\]\cr{}
#'                   is the publisher's version (reviewed and formatted)
#' * `pre_embargo`   \[`character(1)`\]\cr{}
#'                   if applicable the embargo period before the author(s) can
#'                   archive the preprint
#' * `post_embargo`  \[`character(1)`\]\cr{}
#'                   if applicable the embargo period before the author(s) can
#'                   archive the postprint, if value is `"after media"`, it
#'                   means that the post-print can be archived after media
#'                   embargo has passed
#' * `pdf_embargo`   \[`character(1)`\]\cr{}
#'                   if applicable the embargo period before the author(s) can
#'                   archive the publisher's version, if value is `"after media"`,
#'                   it means that the publisher's version can be archived after
#'                   media embargo has passed
#'
#' @keywords internal
#'
#' @importFrom httr content
#' @importFrom xml2 xml_text xml_find_all xml_find_first
parse_journal <- function(xml_source, outcome, hits, type = c("find", "name"),
                          key = NULL) {

  if (outcome %in% c("singleJournal", "uniqueZetoc")) {
    # Some journals have multiple policies because they are owned by multiple
    # publishers or because of historic data. They return hits == 2 but it's
    # still a single journal. They are identified by a specific outcome
    # (uniqueZetoc) so we use it to treat them in the same way as singleJournal.

    # Here, we use xml_find_first instead of xml_find_all because we know there
    # won't be more than one result. xml_find_first also returns NA_character_
    # instead of character(0) if there is no match. This is required to
    # concatenate results in a data.frame that we return to the user.
    # Because RoMEO API returns 'gray' or 'unknown' when the policies of journal
    # are unknown, we convert them to NA

    # TODO: check whether xml_find_first returns the policy with the highest
    # priority.

    title <- xml_text(xml_find_first(xml_source, "//jtitle"))
    issn <- xml_text(xml_find_first(xml_source, "//issn"))
    # Find provided ISSN
    provided_issn <- xml_text(xml_find_first(
      xml_source, "//parameter[parametername='issn']/parametervalue"))

    if (type == "find") {
      return(data.frame(title, provided_issn, issn,
                        stringsAsFactors = FALSE))
    } else {
      romeocolour <- xml_text(xml_find_first(xml_source, "//romeocolour"))
      romeocolour[romeocolour == "gray"] <- NA_character_

      preprint <- xml_text(xml_find_first(xml_source, "//prearchiving"))
      preprint[preprint == "unknown"] <- NA_character_

      postprint <- xml_text(xml_find_first(xml_source, "//postarchiving"))
      postprint[postprint == "unknown"] <- NA_character_

      pdf <- xml_text(xml_find_first(xml_source, "//pdfarchiving"))
      pdf[pdf == "unknown"] <- NA_character_

      pre_embargo <- parse_embargo(xml_source, "pre")
      post_embargo <- parse_embargo(xml_source, "post")
      pdf_embargo <- parse_embargo(xml_source, "pdf")

      return(data.frame(title, provided_issn, issn, romeocolour,
                        preprint,    postprint,    pdf,
                        pre_embargo, post_embargo, pdf_embargo,
                        stringsAsFactors = FALSE))
    }


  } else if (outcome %in% c("manyJournals", "excessJournals") |
             (outcome %in% c("singleJournal", "uniqueZetoc"))) {

    message(hits, " journals match your query terms.")

    if (outcome == "excessJournals") {
      warning("Your request exceeded SHERPA/RoMEO API's cap of 50 results. ",
              "You should try to split your request into smaller chunks.",
              call. = FALSE)
    }

    journals <- xml_text(xml_find_all(xml_source, "//jtitle"))
    issns <- xml_text(xml_find_all(xml_source, "//issn"))
    provided_issns <- xml_text(xml_find_first(
      xml_source, "//parameter[parametername='issn']/parametervalue"))
    if (is.na(provided_issns)) {
      provided_issns <- rep_len(NA_character_, length(issns))
    }

    journal_df <- data.frame(title = journals,
                             provided_issn = provided_issns,
                             issn  = issns,
                             stringsAsFactors = FALSE)
    journal_df[journal_df == ""] <- NA_character_

    # Keep only journal with unique titles or unique ISSNs
    unique_titles <- gsub("&", "and", tolower(journal_df$title), fixed = TRUE)
    journal_df <- journal_df[!duplicated(unique_titles) |
                               !is.na(journal_df$issn),]

    if (type == "find") {
      message("Only titles and ISSNs of journals returned. Get more ",
              "information using `rr_journal_name()`")

      return(journal_df)
    } else {
      message("Recursively fetching data from each journal. ",
              "This may take some time...")

      # Retrieve RoMEO data for all matched journals
      # Use ISSN if available, use title otherwise to retrieve info
      result_df <- apply(journal_df, 1, function(x) {
        if (!is.na(x["issn"])) {
          journal_policy <- rr_journal_issn(x["issn"], key)
        } else {
          journal_policy <- tryCatch({
            rr_journal_name(x["title"], key, qtype = "exact")
          },
          error = function(err) {
            return(data.frame(title         = x["title"],
                              provided_issn = x["provided_issn"],
                              issn          = x["issn"],
                              romeocolour   = NA_character_,
                              preprint      = NA_character_,
                              postprint     = NA_character_,
                              pdf           = NA_character_,
                              pre_embargo   = NA_character_,
                              post_embargo  = NA_character_,
                              pdf_embargo   = NA_character_,
                              stringsAsFactors = FALSE))
          })
        }})

      return(do.call(rbind.data.frame, c(result_df, make.row.names = FALSE)))
    }
  }
}
