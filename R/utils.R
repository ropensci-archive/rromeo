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

  apicontrol = xml_text(xml_find_all(xml_source, "//apicontrol"))

  if (apicontrol == "invalid") {
    stop("The query is invalid")
  } else if (apicontrol == "invalidkey") {
    stop("The provided API key is invalid. ",
         "You can register for a free API at ",
         "http://www.sherpa.ac.uk/romeo/apiregistry.php")
  }

  hits = xml_text(xml_find_all(xml_source, "//numhits"))

  if (hits == 0) {
    stop("No journal matches your query terms. Please try another query.")
  }
  else if (hits == 1) {

    # Here, we use xml_find_first instead of xml_find_all because we know there
    # won't be more than one result. xml_find_first also returns NA_character_
    # instead of character(0) if there is no match. This is required to
    # concatenate results in a data.frame that we return to the user.

    title = xml_text(xml_find_first(xml_source, "//jtitle"))
    issn = xml_text(xml_find_first(xml_source, "//issn"))

    romeocolour = xml_text(xml_find_first(xml_source, "//romeocolour"))
    preprint = xml_text(xml_find_first(xml_source, "//prearchiving"))
    postprint = xml_text(xml_find_first(xml_source, "//postarchiving"))
    pdf = xml_text(xml_find_first(xml_source, "//pdfarchiving"))

    return(data.frame(title, issn, preprint, postprint, pdf, romeocolour))

  } else {

    warning(hits, " journals match your query terms.\n")

    if (xml_text(xml_find_all(xml_source, "//outcome")) == "excessJournals") {
      warning("Your request exceeded SHERPA/RoMEO API's cap of 50 results. ",
              "You should try to split your request into smaller chunks.")
    }

    journals = xml_text(xml_find_all(xml_source, "//jtitle"))
    issns = xml_text(xml_find_all(xml_source, "//issn"))

    if (!multiple) {
      warning("Select one journal from the provided list or enable multiple = ",
              "TRUE")

      return(data.frame("title" = journals,
                        "issn" = issns))
    } else {

      message("Recursively fetching data from each journal. ",
              "This may take some time...")

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

#' Check SHERPA/RoMEO API key
#'
#' The key can be either specified in an `.Renviron` file using
#' `SHERPAROMEO_KEY=my_key` or by passing it when calling the function.
#'
#' @param key a character string or `NULL`
#'
#' @export
check_key = function(key) {
  tmp = ifelse(is.null(key), Sys.getenv("SHERPAROMEO_KEY"), key)

  tmp = ifelse(tmp == "", NULL, tmp)
}