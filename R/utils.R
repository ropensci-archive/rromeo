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
#' @return `NULL` if the ISSN is valid, errors otherwise.
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


#' Search xml tag and get character value
#'
#' This function extract the character value from a given parsed xml document
#' using [xml2::xml_find_all()] and [xml2::xml_text()].
#'
#' @param parsed_xml parsed xml document
#' @param xml_tag    a string containing an xpath expression
#'
#' @return `NA` if the tag isn't found, the string of the tag content otherwise
xml_text_or_na = function(parsed_xml, xml_tag) {
  given_text = xml_text(xml_find_all(parsed_xml, xml_tag))

  ifelse(length(given_text) == 0, NA_character_, given_text)
}
