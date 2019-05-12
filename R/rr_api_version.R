#' Return SHERPA/RoMEO API version
#'
#' This function queries SHERPA/RoMEO and returns the version of the API.
#' @importFrom httr content
#' @importFrom xml2 xml_attr
#' @export
#'
#' @examples \dontrun{
#' rr_api_version()
#' }
rr_api_version <- function() {
  rr_query <- content(rr_GET(), encoding = "ISO-8859-1")

  xml_attr(rr_query, "version")
}
