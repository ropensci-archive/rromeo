rr_base_rest_api = function() {
  api("http://www.sherpa.ac.uk/romeo/api29/")
}

rr_base_api = function() {
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

  is_char      = is.character(issn)
  n_char       = nchar(issn) == 9
  contains_sep = grepl("-", issn, fixed = TRUE)
  issn_groups = strsplit(issn, "-", fixed = TRUE)[[1]]
  group_len = nchar(issn_groups[1]) == 4 & nchar(issn_groups[2]) == 4

  group_num = tryCatch({
    as.numeric(issn_groups[1])
    as.numeric(issn_groups[2])

    TRUE
  },
  warning = function(w) {
    return(FALSE)
  })

  if (!all(is_char, n_char, contains_sep, group_len, group_num)) {
    stop("ISSN is invalid, please check the format")
  }
}

