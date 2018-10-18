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
#' @return
#' @import httr
#' @export
#'
#' @examples
#' rr_api_version()
rr_api_version = function() {
  rr_query = content(GET(rr_base_api()))

  xml2::xml_attr(rr_query, "version")
}

#' Get record from Publichser ID
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
#' @export
#'
#' @examples
#'
#' rr_journal("1947-6264")
rr_journal = function(issn) {

  validate_issn(issn)

  GET(rr_base_api(), query = list(issn = issn))
}

#' @param name A character string, containing the (possibly) partial name of the
#' journal
#' @param qtype A character string saying whether you are lookin for `exact`,
#' `contains` or `starts with` matches
#' @examples
#' rr_journal_name("Journal of Geology")
rr_journal_name = function(name,
                           qtype = c("exact", "contains", "starts with")) {

  qtype = match.arg(qtype)

  api_answer = GET(rr_base_api(), query = list(jtitle = name, qtype = qtype))

  return(api_answer)
}

#' Parse API.
parse_answer = function(api_answer) {

  if (http_error(api_answer)) {
    stop("The API endpoint could not be reached. Please try again later.")
  }

  xml_source = content(api_answer, encoding = "ISO-8859-1")

  hits = xml_text(xml_find_all(xml_source, "//numhits"))

  if (hits == 0) {
    stop("No journal matches your query terms. Please try another query.")
  }
  else if (hits == 1) {
    romeocolour = xml_text(xml_find_all(xml_source, "//romeocolour"))

    # TODO: which characteristics should we return?
    return(romeocolour)
  }
  else {
    # TODO: if multiple journals are found:
    # - return a df with their names and issn and ask to user to try again with
    #   a more precise query
    # - automatically perform an API request for each one of them and return the
    #   results in a df
    # This could also depend on a switch 'multiple = TRUE' to determine which
    # one of these two alternatives the user wants to follow.
    # At the moment, we just return an error.
    stop(hits, " journals match your query terms. Please modify your query to
         only match one journal.")
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

