# rromeo base functions

rr_base_url <- function() {
  "http://www.sherpa.ac.uk/"
}

rr_base_api <- function() {
  paste0(rr_base_url(), "romeo/api29.php")
}

#' Return SHERPA/RoMEO API version
#'
#' This function queries SHERPA/RoMEO and returns the version of the API.
#' @importFrom httr content
#' @importFrom xml2 xml_attr
#' @export
#'
#' @examples
#' rr_api_version()
rr_api_version <- function() {
  rr_query <- content(rr_GET(), encoding = "ISO-8859-1")

  xml_attr(rr_query, "version")
}
