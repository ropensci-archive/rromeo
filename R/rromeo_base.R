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
#' @examples \dontrun{
#' rr_api_version()
#' }
rr_api_version <- function() {
  rr_query <- content(rr_GET(), encoding = "ISO-8859-1")

  xml_attr(rr_query, "version")
}

#' Store provided API key into Environment Variable
#'
#' This function stores the provided API key as argument in to an environment
#' variable `SHERPAROMEO_KEY` for further use by other `rromeo` functions.
#'
#' For more information regarding API keys, please refer to dedicated vignette
#' with the following command
#' `vignette("setting_up_api_key", package = "rromeo")`
#'
#' @param key \[`character(1)`\]\cr{}
#'            A string giving the API key to save into the environment
#'
#' @export
#' @examples \dontrun{
#' rr_auth("Iq83AIL5bss")
#' }
rr_auth <- function(key) {
  Sys.setenv("SHERPAROMEO_KEY" = key)
}
