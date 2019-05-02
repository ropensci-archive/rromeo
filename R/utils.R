#' Generic parsing function
#'
#' @param api_answer \[`httr::response()`\]\cr{}
#'                   The API answer
#'
#' @param ... Other options passed to parsing functions
#'
#' @return either results from [`parse_journal()`] or [`parse_publisher()`]
parse_generic <- function(api_answer, ...) {

  if (http_error(api_answer)) {
    stop("The API endpoint could not be reached. Please try again later.",
         call. = FALSE)
  }

  xml_source <- content(api_answer, encoding = "ISO-8859-1")

  # API Control Block
  apicontrol <- xml_text(xml_find_all(xml_source, "//apicontrol"))

  if (apicontrol == "invalidapikey") {
    stop("The provided API key is invalid. ",
         "You can register for a free API at ",
         "http://www.sherpa.ac.uk/romeo/apiregistry.php", call. = FALSE)
  } else if (apicontrol == "pastfreelimit") {
    stop("You have exceeded the free use limit of 500 requests per day. ",
         "To go beyond this limit you should register for a free API key ",
         "available at http://www.sherpa.ac.uk/romeo/apiregistry.php",
         call. = FALSE)
  } else if (apicontrol == "invalid") {
    stop("The query is invalid. Please check the query", call. = FALSE)
  }

  hits <- xml_text(xml_find_all(xml_source, "//numhits"))
  outcome <- xml_text(xml_find_all(xml_source, "//outcome"))

  # Outcome Block
  if (apicontrol == "journal" & (outcome == "notFound" | hits == 0)) {
    stop("No journal matches your query terms. Please try another query.",
         call. = FALSE)
  } else if (apicontrol %in% c("", "publisher", "identifier") &
             (outcome == "notFound" | hits == 0)) {
    stop("No publisher was found. Maybe try another query? ;)", call. = FALSE)
  }

  # Parsing branches
  if (apicontrol == "journal") {
    parsed <- parse_journal(xml_source, outcome = outcome, hits = hits, ...)
  } else if (apicontrol %in% c("publisher", "identifier", "colour") |
             (apicontrol %in% c("all", "") & outcome == "publisherFound")) {
    parsed <- parse_publisher(xml_source)
  }

  return(parsed)
}

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
#' * `title`        \[`character(1)`\]\cr{}
#'                  the name of the journal
#' * `issn`         \[`character(1)`\]\cr{}
#'                  the ISSN of the journal
#' * `romeocolour`  \[`character(1)`\]\cr{}
#'                  the SHERPA/RoMEO colour of the journal
#' * `preprint`     \[`character(1)`\]\cr{}
#'                  is the preprint (not reviewed) archivable?
#' * `postprint`    \[`character(1)`\]\cr{}
#'                  is the postprint (reviewed but not formatted) archivable?
#' * `pdf`          \[`character(1)`\]\cr{}
#'                  is the publisher's version (reviewed and formatted)
#' * `pre_embargo`  \[`character(1)`\]\cr{}
#'                  if applicable the embargo period before the author(s) can
#'                  archive the preprint
#' * `post_embargo` \[`character(1)`\]\cr{}
#'                  if applicable the embargo period before the author(s) can
#'                  archive the postprint
#' * `pdf_embargo`  \[`character(1)`\]\cr{}
#'                  if applicable the embargo period before the author(s) can
#'                  archive the publisher's version
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

    if (type == "find") {
      return(data.frame(title, issn,
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

      return(data.frame(title, issn, romeocolour,
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

    journal_df <- data.frame(title = journals,
                             issn  = issns,
                             stringsAsFactors = FALSE)
    journal_df[journal_df == ""] <- NA

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
#' * `romeoid`     \[`integer(1)`\]\cr{}
#'                 the internal index of the publisher in the SHERPA/RoMEO
#'                 database
#' * `publisher`   \[`character(1)`\]\cr{}
#'                 the name of the publisher
#' * `alias`       \[`character(1)`\]\cr{}
#'                 if applicable an alternative name of the publisher or the
#'                 name of the specific publishing branch
#' * `romeocolour` \[`character(1)`\]\cr{}
#'                 a colour assigned by the database that reflects the default
#'                 policies of the publisher
#' * `preprint`    \[`character(1)`\]\cr{}
#'                 is the preprint (not reviewed) archivable?
#' * `postprint`   \[`character(1)`\]\cr{}
#'                 is the postprint (reviewed but not formatted) archivable?
#' * `pdf`         \[`character(1)`\]\cr{}
#'                 is the publisher's version (reviewed and formatted)
#'                 archivable?
#'
#' @keywords internal
#'
#' @importFrom httr content http_error
#' @importFrom xml2 xml_text xml_find_all xml_find_first xml_attr
parse_publisher <- function(xml_source, outcome, hits) {

  romeoid     <- xml_attr(xml_find_all(xml_source, "//publisher"), "id")

  publisher   <- xml_text(xml_find_all(xml_source, "//name"))

  alias       <- xml_text(xml_find_all(xml_source, "//alias"))
  alias[alias == ""] <- NA_character_

  romeocolour <- xml_text(xml_find_all(xml_source, "//romeocolour"))

  preprint    <- xml_text(xml_find_all(xml_source, "//prearchiving"))
  preprint[preprint == "unknown"] <- NA_character_

  postprint   <- xml_text(xml_find_all(xml_source, "//postarchiving"))
  postprint[postprint == "unknown"] <- NA_character_

  pdf         <- xml_text(xml_find_all(xml_source, "//pdfarchiving"))
  pdf[pdf == "unknown"] <- NA_character_

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
#' @param issn \[`character(1)`\]\cr{}
#'             The ISSN of a journal
#' @return `NULL` if the ISSN is valid, errors otherwise
#'
#' @keywords internal
validate_issn <- function(issn) {

  # How to validate ISSN?
  # https://en.wikipedia.org/wiki/International_Standard_Serial_Number

  # Pre-check: does it look like a valid ISSN?
  if (!grepl("^\\d{4}-\\d{3}[\\dxX]$", issn, perl = TRUE)) {
    stop("ISSN is invalid, please check the format", call. = FALSE)
  }

  # Weighted sum check
  issn_digits <- strsplit(gsub("-", "", issn), "")[[1]]

  non_control_digits <- as.numeric(issn_digits[1:7])
  control_digit <- toupper(issn_digits[8])

  weighted_sum <- sum(seq(8, 2, by = -1) * non_control_digits)

  weighted_sum <- ifelse(control_digit == "X",
                         weighted_sum + 10,
                         weighted_sum + as.numeric(control_digit))

  if (weighted_sum %% 11 != 0) {
    stop("ISSN is invalid, please check the format", call. = FALSE)
  }

  return(TRUE)
}

#' Check SHERPA/RoMEO API key
#'
#' The key can be either specified in various ways see the Details section.
#'
#' @param key \[`character(1)`\]\cr{}
#'            a character string containing the API key or `NULL`
#'            (see Details section on how to specify it)
#'
#' @details There are several ways to provide your API key.
#' The best way to know about them is to refer to the vignette about
#' "Setting Up Your API key" accessible with the following command:
#' `vignette("setting_up_api_key", package = "rromeo")`.
#' You can also use [`rr_auth`] that will use the provided key to store it as
#' an environmental variable.
#'
#' @return if found the character string of the key, `NULL` otherwise
#' @export
check_key <- function(key = NULL) {
  tmp <- ifelse(is.null(key), Sys.getenv("SHERPAROMEO_KEY"), key)

  if (tmp == "") {
    tmp <- NULL
  }

  return(tmp)
}

#' Parse embargo period from API return
#'
#' This function provides an easy way to return the embargo period in the
#' different categories
#' @param xml_source \[`xml_document`\]\cr{}
#'                   a parsed xml document
#' @param type       \[`character(1)`\]
#'                   name of the embargo type must be in `"pre"`, `"post"`, and
#'                   `"pdf"`
#'
#' @return the embargo period as a string like `"12 months"`
#'
#' @keywords internal
#'
#' @importFrom xml2 xml_text xml_find_first read_html
parse_embargo <- function(xml_source, type = c("pre", "post", "pdf")) {

  type <- match.arg(type)

  tag <- paste0("//", type, "restriction[contains(text(), 'embargo')]")
  embargo_field <- xml_text(xml_find_first(xml_source, tag))

  if (is.na(embargo_field)) {
    return(NA_character_)
  }
  else {
    embargo_field <- read_html(embargo_field)
    time <- xml_text(xml_find_first(embargo_field, "//num"))
    unit <- xml_text(xml_find_first(embargo_field, "//period"))
    return(paste(time, unit))
  }
}

#' Validate ISO two-letters country code
#'
#' If available uses [`ISOcodes::ISO_3166_1`] to validate country code.
#' Otherwise assume that the code is valid as long as it is a two-letter code or
#' `__`. See [`rr_publisher_country()`] for use of country codes.
#'
#' @param country \[`character(1)`\]\cr{}
#'                a two-letter country code or `AA`, `ZZ` or
#'                `__` (special country codes for SHERPA/RoMEO)
#'
#' @return `TRUE` if the country code is valid, errors otherwise
#'
#' @keywords internal
validate_country_code <- function(country) {
  if (requireNamespace("ISOcodes", quietly = TRUE)) {
    if (!(country %in% c(ISOcodes::ISO_3166_1$Alpha_2, "AA", "ZZ", "__"))) {
      stop(country, " is an invalid country code. ",
           "The country code should be two letter long or '__' for undefined.",
           call. = FALSE)
    }
  } else {
    if (!grepl("^[A-Za-z|\\_]{2}$", country, perl = FALSE)) {
      stop(country, " is an invalid country code. ",
           "The country code should be two letter long or '__' for undefined.",
           call. = FALSE)
    }
  }

  return(TRUE)
}


#' rromeo User Agent
rr_ua <- function() {
  paste0("http://github.com/ropensci/rromeo R package rromeo/v.",
         utils::packageVersion("rromeo"))
}


#' rromeo internal GET function
#'
#' @param ... additional parameter to [`httr::GET`]
#'
#' @importFrom httr GET add_headers
rr_GET <- function(...) {
  GET(rr_base_api(), add_headers("user-agent" = rr_ua()), ...)
}
