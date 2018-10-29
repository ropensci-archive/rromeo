rr_base_url = function() {
  "http://www.sherpa.ac.uk/"
}

rr_base_rest_api = function() {
  "romeo/api29/"
}

rr_base_api = function() {
  "http://www.sherpa.ac.uk/romeo/api29.php"
}

#' Get SHERPA/RoMEO API version
#'
#' @import httr
#' @export
#'
#' @examples
#' rr_api_version()
rr_api_version = function() {
  rr_query = content(GET(rr_base_api()))

  xml2::xml_attr(rr_query, "version")
}

#' Get record from Publisher ID
#'
#' Use SHERPA/RoMEO RESTful API to retrieve a specific publisher status
#' @param id publisher ID
rr_rest_publisher = function(id) {

  id = tryCatch({
    as.integer(id)
  },
  error = function() {
    stop("id needs to be an integer")
    return(NA_integer_)
  })

  pub_path = paste0(rr_base_rest_api(), "pub/", id, "/")

  GET(rr_base_url(), path = pub_path)
}

#' Get record from Publication ID
#'
#' Use SHERPA/RoMEO regular API to retrieve a specific publication number status
#' @param given_id publication number
#'
#' @export
#' @examples
#' rr_publisher(55)
rr_publisher = function(given_id) {

  given_id = tryCatch({
    as.integer(given_id)
  },
  error = function() {
    stop("id needs to be an integer")
    return(NA_integer_)
  })

  GET(rr_base_api(), query = list(id = given_id))
}

#' Journal data by ISSN
#'
#' @param issn A single journal ISSN
#'
#' @export
#'
#' @examples
#'
#' rr_journal_issn("1947-6264")
rr_journal_issn = function(issn) {

  validate_issn(issn)

  api_answer = GET(rr_base_api(), query = list(issn = issn))

  parse_answer(api_answer, multiple = FALSE)
}

#' Journal data by title
#'
#' @param name A character string, containing the (possibly) partial name of the
#' journal
#' @param qtype A character string saying whether you are lookin for `exact`,
#' `contains` or `starts with` matches
#' @inheritParams parse_answer
#'
#' @export
#'
#' @examples
#' rr_journal_name("Journal of Geology")
#' rr_journal_name("Biogeography", multiple = FALSE, qtype = "contains")
#' rr_journal_name("Biogeography", multiple = TRUE, qtype = "contains")
rr_journal_name = function(name, multiple = FALSE,
                           qtype = c("exact", "contains", "starts with")) {

  qtype = match.arg(qtype)

  api_answer = GET(rr_base_api(), query = list(jtitle = name, qtype = qtype))

  parse_answer(api_answer, multiple = multiple)
}

#' Parse API answer.
#'
#' @param multiple If multiple results match your query, should the function
#' recursively fetch data for each of one of them (`multiple = TRUE`) or
#' return a data.frame containing only titles and ISSN of all matches
#' (`multiple = FALSE`)
#'
#' @keywords internal
#'
#' @import xml2
parse_answer = function(api_answer, multiple = FALSE) {

  if (http_error(api_answer)) {
    stop("The API endpoint could not be reached. Please try again later.")
  }

  xml_source = content(api_answer, encoding = "ISO-8859-1")

  hits = xml_text(xml_find_all(xml_source, "//numhits"))

  if (hits == 0) {
    stop("No journal matches your query terms. Please try another query.")
  }
  else if (hits == 1) {

    title = xml_text_or_na(xml_source, "//jtitle")
    issn = xml_text_or_na(xml_source, "//issn")

    romeocolour = xml_text_or_na(xml_source, "//romeocolour")
    preprint = xml_text_or_na(xml_source, "//prearchiving")
    postprint = xml_text_or_na(xml_source, "//postarchiving")
    pdf = xml_text_or_na(xml_source, "//pdfarchiving")

    return(data.frame(title, issn, preprint, postprint, pdf, romeocolour))
  } else {
    warning(hits, " journals match your query terms.\n")

    if (xml_text_or_na(xml_source, "//outcome") == "excessJournals") {
      warning("Your request exceeded SHERPA/RoMEO API's cap of 50 results. ",
              "You should try to split your request into smaller chunks.")
    }

    journals = xml_text_or_na(xml_source, "//jtitle")
    issns = xml_text_or_na(xml_source, "//issn")

    if (!multiple) {
      warning("Select one journal from the provided list or enable multiple = ",
              "TRUE")

      return(data.frame("title" = journals,
                        "issn" = issns))
    } else {

      message("Recursively fetching data from each journal. This may take some",
              " time...")

      return(do.call(rbind.data.frame, lapply(issns, rr_journal_issn)))
    }
  }
}

#' Checks validity of the ISSN
#'
#' Silent if the ISSN is valid, errors otherwise.
#' @param issn The ISSN of a journal
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

xml_text_or_na = function(parsed_xml, xml_tag) {
  given_text = xml_text(xml_find_all(parsed_xml, xml_tag))

  ifelse(length(given_text) == 0, NA_character_, given_text)
}
