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
    xml2::as_list() %>%
    .[["romeoapi"]] %>%
    attributes() %>%
    .[["version"]]
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
rr_get_pub = function(id) {

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
