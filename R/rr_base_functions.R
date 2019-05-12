# rromeo base functions
rr_base_url <- function() {
  "http://www.sherpa.ac.uk/"
}

rr_base_api <- function() {
  paste0(rr_base_url(), "romeo/api29.php")
}

#' rromeo User Agent
rr_ua <- function() {
  paste0("http://github.com/ropensci/rromeo R package rromeo/v.",
         utils::packageVersion("rromeo"))
}


#' rromeo internal GET function
#'
#' @param ... additional parameter to [`httr::GET`]
#'
#' @importFrom httr GET add_headers
rr_GET <- function(...) {
  GET(rr_base_api(), add_headers("user-agent" = rr_ua()), ...)
}
