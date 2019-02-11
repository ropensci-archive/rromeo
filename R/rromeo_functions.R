#' Get Publisher Information from Publisher ID
#'
#' Use SHERPA/RoMEO API to retrieve a specific publisher policies on manuscript
#' archivals
#'
#' @param given_id  `[integer(1+)]` one or a vector of SHERPA/RoMEO publisher's
#'                                  ID
#' @inheritParams check_key
#'
#' @return Returns a data frame with the following columns:
#' * `romeoid`     `[integer(1)]` the internal index of the publisher in
#'                 the SHERPA/RoMEO database
#' * `publisher`   `[character(1)]` the name of the publisher
#' * `alias`       `[character(1)]` if applicable an alternative name of the
#'                 publisher or the name of the specific publishing branch
#' * `romeocolour` `[character(1)]` a colour assigned by the database that
#'                 reflects the default policies of the publisher
#' * `preprint`    `[character(1)]` is the preprint (not reviewed) archivable?
#' * `postprint`   `[character(1)]` is the postprint (reviewed but not
#'                 typesetted) archivable?
#' * `pdf`         `[character(1)]` is the publisher's version (reviewed and
#'                 typesetted) archivable?
#'
#' @importFrom httr GET
#' @export
#' @examples
#' rr_publisher(55)
#' rr_publisher(c(55, 735))
rr_publisher = function(given_id, key = NULL) {

  given_id = tryCatch({
    as.integer(given_id)
  },
  warning = function(cond) {
    stop("id needs to be an integer", call. = FALSE)
  })

  api_key = check_key(key)

  answer_list = lapply(given_id, function(publisher_id,
                                          given_api_key = api_key) {
    api_answer = GET(rr_base_api(), query = list(id = publisher_id,
                                                 ak = given_api_key))

    parse_publisher(api_answer)
  })

  publishers_df = do.call(rbind.data.frame,
                          c(answer_list, stringsAsFactors = FALSE))

  return(publishers_df)
}

#' Retrieve journal policy using ISSN
#'
#' Retrieve policy information from the SHERPA/RoMEO API using the ISSN of
#' the journal
#'
#' @param issn `[character(1+)]` one or a vector of journal(s) ISSN(s)
#' @inheritParams check_key
#'
#' @importFrom httr GET
#' @export
#'
#' @examples
#' rr_journal_issn("1947-6264")
#' rr_journal_issn(c("1947-6264", "0030-1299"))
rr_journal_issn = function(issn, key = NULL) {

  vapply(issn, validate_issn, c(TRUE))

  api_key = check_key(key)

  answer_list = lapply(issn, function(journal_issn,
                                      given_api_key = api_key) {

    api_answer = GET(rr_base_api(), query = list(issn = journal_issn,
                                                 ak   = given_api_key))

    parse_answer(api_answer, multiple = FALSE, key = api_key)
  })

  journals_df = do.call(rbind.data.frame,
                        c(answer_list, stringsAsFactors = FALSE))

  return(journals_df)
}

#' Retrieve journal(s) policy(ies) by matching title
#
#'
#' @param name `[character(1+)]` one or several strings to match the titles of
#'             the journals
#' @param qtype `[character(1)]` in `c("exact", "contains", "starts with")` to
#'              set match type for the `name` search string
#' @inheritParams parse_answer
#' @inheritParams check_key
#'
#' @return Returns a data frame if multiple journals are found and
#'         `multiple = FALSE`:
#' * `title`        `[character(1)]` the name of the journal
#' * `issn`         `[character(1)]` the ISSN of the journal
#' if a single journal is found or if `multiple = TRUE` returns a larger
#' data frame:
#' * `title`        `[character(1)]` the name of the journal
#' * `issn`         `[character(1)]` the ISSN of the journal
#' * `romeocolour`  `[character(1)]` the SHERPA/RoMEO colour of the journal
#' * `preprint`     `[character(1)]` is the preprint (not reviewed) archivable?
#' * `postprint`    `[character(1)]` is the postprint (reviewed but not
#'                   typesetted) archivable?
#' * `pdf`          `[character(1)]` is the publisher's version
#'                  (reviewed and typesetted)
#' * `pre_embargo`  `[character(1)]` if applicable the embargo period before
#'                  the author(s) can archive the pre-print
#' * `post_embargo` `[character(1)]` if applicable the embargo period before
#'                  the author(s) can archive the post-print
#' * `pdf_embargo`  `[character(1)]` if applicable the embargo period before
#'                  the author(s) can archive the publisher's version
#'
#' @importFrom httr GET
#' @export
#'
#' @examples
#' rr_journal_name("Journal of Geology")
#' rr_journal_name("Biogeography", multiple = FALSE, qtype = "contains")
#' \dontrun{
#' rr_journal_name("Biogeography", multiple = TRUE, qtype = "contains")
#' # You can also query multiple journals with exact titles in a single call
#' rr_journal_name(c("Journal of Biogeography", "PLoS ONE"), qtype = "exact")
#' }
rr_journal_name = function(name, multiple = FALSE,
                           qtype = c("exact", "contains", "starts with"),
                           key = NULL) {

  qtype = match.arg(qtype)

  api_key = check_key(key)

  answer_list = lapply(name, function(journal_name, given_multiple = multiple,
                                      given_qtype = qtype,
                                      given_api_key = api_key) {

    api_answer = GET(rr_base_api(), query = list(jtitle = journal_name,
                                                 qtype = given_qtype,
                                                 ak = given_api_key))

    parse_answer(api_answer, multiple = given_multiple, key = given_api_key)
  })

  journals_df = do.call(rbind.data.frame,
                        c(answer_list, stringsAsFactors = FALSE))

  return(journals_df)
}

#' Query publisher by RoMEO colour
#'
#' SHERPA/RoMEO classifies publisher in different colours depending on their
#' archiving policies.
#' - **green** publishers let authors archive pre-print and post-print or
#'   publisher's version/PDF,
#' - **blue** publishers let authors archive post-print or publisher's
#'   version/PDF,
#' - **yellow** publishers let authors archive pre-print,
#' - **white** publishers do not formally support archival.
#'
#' For more details about the definitions of RoMEO colours check the
#' [FAQ section](http://sherpa.ac.uk/romeo/definitions.php#colours) of
#' SHERPA/RoMEO
#'
#' Note that when using `rr_romeo_colour()` the API returns **all** the
#' publishers in the selected category, so the results are generally bigger in
#' size than specific functions like [`rr_journal_name()`] or [`rr_publisher()`]
#'
#' @param romeo_colour `[character(1)]` in
#'                      `c("green", "blue", "yellow", "white")`
#'                      the SHERPA/RoMEO colour to retrieve
#' @inheritParams check_key
#'
#' @return Returns a data frame with the following columns:
#' * `romeoid`     `[integer(1)]` the internal index of the publisher in
#'                 the SHERPA/RoMEO database
#' * `publisher`   `[character(1)]` the name of the publisher
#' * `alias`       `[character(1)]` if applicable an alternative name of the
#'                 publisher or the name of the specific publishing branch
#' * `romeocolour` `[character(1)]` a colour assigned by the database that
#'                 reflects the default policies of the publisher
#' * `preprint`    `[character(1)]` is the preprint (not reviewed) archivable?
#' * `postprint`   `[character(1)]` is the postprint (reviewed but not
#'                 typesetted) archivable?
#' * `pdf`         `[character(1)]` is the publisher's version (reviewed and
#'                 typesetted) archivable?
#'
#' @importFrom httr GET
#' @export
#'
#' @examples
#' \dontrun{
#' rr_romeo_colour("green")
#' }
rr_romeo_colour = function(romeo_colour = c("green", "blue", "yellow", "white"),
                           key = NULL) {

  romeo_colour = match.arg(romeo_colour)

  api_answer = GET(rr_base_api(), query = list(colour = romeo_colour,
                                               ak = check_key(key)))

  parse_publisher(api_answer)
}
