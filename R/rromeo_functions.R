rr_base_rest_api = function() {
  api("http://www.sherpa.ac.uk/romeo/api29/")
}

rr_base_api = function() {
  api("http://www.sherpa.ac.uk/romeo/api29.php")
}

#' Checks SHERPA/RoMEO API
#'
#' If reaching the RoMEO API this function returns an error
#' @return Nothing if can reach, error otherwise
#' @import request
#' @export
#'
#' @examples
#' rr_check_api()
rr_check_api = function() {
  rr_base_api() %>%
    http()
}

#' Get SHERPA/RoMEO API version
#'
#' @return
#' @import request
#' @export
#'
#' @examples
#' rr_get_version()
rr_get_version = function() {
  rr_base_api() %>%
    http() %>%
    xml2::read_xml() %>%
    xml2::xml_attr("version")
}

#' Get record from Publication ID
#'
#' Use SHERPA/RoMEO RESTful API to retrieve a specific publication number status
#' @param id publication number
#'
#' @return
#' @export
#'
#' @examples
rr_get_rest_pub = function(id) {

  id = tryCatch({
    as.integer(id)
  },
  error = function() {
    stop("id needs to be an integer")
    return(NA_integer_)
  })

  rr_base_rest_api() %>%
    api_template(template = "pub/{{pub_id}}/", data = list(pub_id = id)) %>%
    http()
}

#' Get record from Publication ID
#'
#' Use SHERPA/RoMEO regular API to retrieve a specific publication number status
#' @param given_id publication number
#'
#' @return
#' @export
#'
#' @examples
rr_get_pub = function(given_id) {

  given_id = tryCatch({
    as.integer(given_id)
  },
  error = function() {
    stop("id needs to be an integer")
    return(NA_integer_)
  })

  rr_base_api() %>%
    api_query_(.dots = list(id = as.character(given_id))) %>%
    http() %>%
    xml2::read_xml()
}

#' Retrieve RoMEO data by ISSN
#'
#' @param issn A single ISSN
#'
#' @return
#' @export
#'
#' @examples
rr_get_issn = function(issn) {

  validate_issn(issn)

  rr_base_api() %>%
    api_query_(.dots = list(issn = issn))
}


#' Checks validity of the ISSN
#'
#' Silent if the ISSN is valid, errors otherwise.
#' @param issn The ISSN of a journal
#'
validate_issn = function(issn) {

  # How to validate ISSN?
  # https://en.wikipedia.org/wiki/International_Standard_Serial_Number

  # Pre-check: does it look like a valid ISSN?
  if (!grepl("^\\d{4}-\\d{3}[\\dxX]$", issn, perl = TRUE)) {
    stop("ISSN is invalid, please check the format")
  }

  # Weighted sum check
  to_sum = gsub("-", "", issn)

  non_control_digits = as.numeric(strsplit(substr(to_sum, 1, 7), "")[[1]])
  control_digit = substr(to_sum, 8, 8)

  if (control_digit %in% c("X", "x")) {
    control_digit = 10
  }
  control_digit = as.numeric(control_digit)

  weighted_sum = sum(seq(8, 2, by = -1) * non_control_digits)

  if (11 - (weighted_sum %% 11) != control_digit) {
    stop("ISSN is invalid, please check the format")
  }

  return(NULL)
}

