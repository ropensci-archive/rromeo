# rromeo base functions

rr_base_url = function() {
  "http://www.sherpa.ac.uk/"
}

rr_base_rest_api = function() {
  "romeo/api29/"
}

rr_base_api = function() {
  paste0(rr_base_url(), "romeo/api29.php")
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
