#' Parse API answer
#'
#' Returns data.frame from parsed xml API answer.
#'
#' @inheritParams parse_publisher
#' @param multiple If multiple results match your query, should the function
#' recursively fetch data for each of one of them (`multiple = TRUE`) or
#' return a data.frame containing only titles and ISSN of all matches
#' (`multiple = FALSE`)
#' @inheritParams check_key
#'
#' @return Returns a data.frame with the following columns:
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
#' @keywords internal
#'
#' @importFrom httr content
#' @importFrom xml2 xml_text xml_find_all xml_find_first
parse_answer = function(api_answer, multiple = FALSE, key = NULL) {

  if (http_error(api_answer)) {
    stop("The API endpoint could not be reached. Please try again later.")
  }

  xml_source = content(api_answer, encoding = "ISO-8859-1")

  apicontrol = xml_text(xml_find_all(xml_source, "//apicontrol"))

  if (apicontrol == "invalidapikey") {
    stop("The provided API key is invalid. ",
         "You can register for a free API at ",
         "http://www.sherpa.ac.uk/romeo/apiregistry.php")
  }

  hits = xml_text(xml_find_all(xml_source, "//numhits"))
  outcome = xml_text(xml_find_all(xml_source, "//outcome"))

  if (outcome == "notFound") {
    stop("No journal matches your query terms. Please try another query.")
  } else if (outcome %in% c("singleJournal", "uniqueZetoc")) {
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

    title = xml_text(xml_find_first(xml_source, "//jtitle"))
    issn = xml_text(xml_find_first(xml_source, "//issn"))

    romeocolour = xml_text(xml_find_first(xml_source, "//romeocolour"))
    romeocolour = ifelse(romeocolour == "gray", NA_character_, romeocolour)

    preprint = xml_text(xml_find_first(xml_source, "//prearchiving"))
    preprint = ifelse(preprint == "unknown", NA_character_, preprint)

    postprint = xml_text(xml_find_first(xml_source, "//postarchiving"))
    postprint = ifelse(postprint == "unknown", NA_character_, postprint)

    pdf = xml_text(xml_find_first(xml_source, "//pdfarchiving"))
    pdf = ifelse(pdf == "unknown", NA_character_, pdf)

    pre_embargo = parse_embargo(xml_source, "pre")
    post_embargo = parse_embargo(xml_source, "post")
    pdf_embargo = parse_embargo(xml_source, "pdf")

    return(data.frame(title, issn, romeocolour,
                      preprint,    postprint,    pdf,
                      pre_embargo, post_embargo, pdf_embargo,
                      stringsAsFactors = FALSE))

  } else if (outcome %in% c("manyJournals", "excessJournals")) {

    warning(hits, " journals match your query terms.\n")

    if (outcome == "excessJournals") {
      warning("Your request exceeded SHERPA/RoMEO API's cap of 50 results. ",
              "You should try to split your request into smaller chunks.")
    }

    journals = xml_text(xml_find_all(xml_source, "//jtitle"))
    issns = xml_text(xml_find_all(xml_source, "//issn"))

    journal_df = data.frame(title = journals,
                            issn  = issns,
                            stringsAsFactors = FALSE)
    journal_df[journal_df == ""] = NA

    if (!multiple) {
      warning("Select one journal from the provided list or enable multiple = ",
              "TRUE")

      return(journal_df)
    } else {

      message("Recursively fetching data from each journal. ",
              "This may take some time...")

      # Retrieve RoMEO data for all matched journals
      # Use ISSN if available, use title otherwise to retrieve info
      result_df = apply(journal_df, 1, function(x) {
        if (!is.na(x["issn"])) {
          journal_policy = rr_journal_issn(x["issn"], key)
        } else {
          journal_policy = tryCatch({
            rr_journal_name(x["title"], key, qtype = "exact")
          },
          error = function(err) {
            return(data.frame(title        = x["title"],
                              issn         = x["issn"],
                              romeocolour  = NA,
                              preprint     = NA,
                              postprint    = NA,
                              pdf          = NA,
                              pre_embargo  = NA,
                              post_embargo = NA,
                              pdf_embargo  = NA,
                              stringsAsFactors = FALSE))
          })
        }})

      return(do.call(rbind.data.frame, c(result_df, make.row.names = FALSE)))
    }
  }
}

#' Parse publisher list
#'
#' When the returned content by the SHERPA/RoMEO API is a list of publishers
#' this function parses the list and returns a structured data.frame with
#' the default policies of the different publisher.
#'
#' @param api_answer xml API answer
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
#' @keywords internal
#'
#' @importFrom httr content http_error
#' @importFrom xml2 xml_text xml_find_all xml_find_first xml_attr
parse_publisher = function(api_answer) {
  if (http_error(api_answer)) {
    stop("The API endpoint could not be reached. Please try again later.")
  }

  xml_source = content(api_answer, encoding = "ISO-8859-1")

  apicontrol = xml_text(xml_find_all(xml_source, "//apicontrol"))
  outcome = xml_text(xml_find_all(xml_source, "//outcome"))

  if (apicontrol == "invalidapikey") {
    stop("The provided API key is invalid. ",
         "You can register for a free API at ",
         "http://www.sherpa.ac.uk/romeo/apiregistry.php")
  }

  if (outcome == "notFound") {
    stop("No publisher matches the provided id. Please try another id.")
  }

  romeoid     = xml_attr(xml_find_all(xml_source, "//publisher"), "id")

  publisher   = xml_text(xml_find_all(xml_source, "//name"))

  alias       = xml_text(xml_find_all(xml_source, "//alias"))
  alias       = ifelse(alias == "", NA_character_, alias)

  romeocolour = xml_text(xml_find_all(xml_source, "//romeocolour"))

  preprint    = xml_text(xml_find_all(xml_source, "//prearchiving"))
  preprint    = ifelse(preprint == "unknown", NA_character_, preprint)

  postprint   = xml_text(xml_find_all(xml_source, "//postarchiving"))
  postprint   = ifelse(postprint == "unknown", NA_character_, postprint)

  pdf         = xml_text(xml_find_all(xml_source, "//pdfarchiving"))
  pdf         = ifelse(pdf == "unknown", NA_character_, pdf)

  data.frame(romeoid = as.numeric(romeoid),
             publisher,
             alias,
             romeocolour,
             preprint,
             postprint,
             pdf,
             stringsAsFactors = FALSE)
}


#' Checks validity of the ISSN
#'
#' International Standard Serial Numbers (ISSNs) have a specific structure and
#' this function checks that the provided ISSN is valid. For more information
#' please go to
#' <https://en.wikipedia.org/wiki/International_Standard_Serial_Number>
#'
#' @param issn The ISSN of a journal
#' @return `NULL` if the ISSN is valid, errors otherwise
#'
#' @keywords internal
validate_issn = function(issn) {

  # How to validate ISSN?
  # https://en.wikipedia.org/wiki/International_Standard_Serial_Number

  # Pre-check: does it look like a valid ISSN?
  if (!grepl("^\\d{4}-\\d{3}[\\dxX]$", issn, perl = TRUE)) {
    stop("ISSN is invalid, please check the format")
  }

  # Weighted sum check
  issn_digits = strsplit(gsub("-", "", issn), "")[[1]]

  non_control_digits = as.numeric(issn_digits[1:7])
  control_digit = toupper(issn_digits[8])

  weighted_sum = sum(seq(8, 2, by = -1) * non_control_digits)

  weighted_sum = ifelse(control_digit == "X",
                        weighted_sum + 10,
                        weighted_sum + as.numeric(control_digit))

  if (weighted_sum %% 11 != 0) {
    stop("ISSN is invalid, please check the format")
  }

  return(NULL)
}

#' Check SHERPA/RoMEO API key
#'
#' The key can be either specified in various ways see the Details section.
#'
#' @param key a character string containing the API key or `NULL`
#'
#' @details To provide your API key to `rromeo` you can 1) provide it as a
#'          character string as the `key` arguments of `rromeo` functions
#'          as `rr_*(..., key = "my_key_as_a_string")` (**NOT RECOMMENDED**)
#'          2) you can define the variable `SHERPAROMEO_KEY` in an `.Renviron`
#'          file in your working directory, the file should contain the
#'          following line `SHERPAROMEO_KEY=my_key_without_quotes`
#'          3) you can also define the variable `SHERPAROMEO_KEY` in an
#'          `.Rprofile` file in your working directory, the file should contain
#'          the following line `SHERPAROMEO_KEY="my_key_with_quotes"`.
#'
#' @return if found the character string of the key, `NULL` otherwise
#' @export
check_key = function(key) {
  tmp = ifelse(is.null(key), Sys.getenv("SHERPAROMEO_KEY"), key)

  if (tmp == "") {
    tmp = NULL
  }

  return(tmp)
}

#' Parse embargo period from API return
#'
#' This function provides an easy way to return the embargo period in the
#' different categories
#' @param xml_source a parsed xml document
#' @param type       name of the embargo type must be in `"pre"`, `"post"`, and
#'                   `"pdf"`
#'
#' @return the embargo period as a string like `"12 months"`
#'
#' @keywords internal
#'
#' @importFrom xml2 xml_text xml_find_first read_html
parse_embargo = function(xml_source, type) {

  tag = paste0("//", type, "restriction")
  embargo_field = xml_text(xml_find_first(xml_source, tag))

  if (is.na(embargo_field)) {
    return(NA_character_)
  }
  else {
    embargo_field = read_html(embargo_field)
    time = xml_text(xml_find_first(embargo_field, "//num"))
    unit = xml_text(xml_find_first(embargo_field, "//period"))
    return(paste(time, unit))
  }
}
