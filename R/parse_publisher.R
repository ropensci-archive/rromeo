#' Parse publisher list
#'
#' When the returned content by the SHERPA/RoMEO API is a list of publishers
#' this function parses the list and returns a structured data.frame with
#' the default policies of the different publisher.
#'
#' @param api_answer xml API answer
#'
#' @return Returns a data frame with the following columns:
#' * `romeoid`     \[`integer(1)`\]\cr{}
#'                 the internal index of the publisher in the SHERPA/RoMEO
#'                 database
#' * `publisher`   \[`character(1)`\]\cr{}
#'                 the name of the publisher
#' * `alias`       \[`character(1)`\]\cr{}
#'                 if applicable an alternative name of the publisher or the
#'                 name of the specific publishing branch
#' * `romeocolour` \[`character(1)`\]\cr{}
#'                 a colour assigned by the database that reflects the default
#'                 policies of the publisher
#' * `preprint`    \[`character(1)`\]\cr{}
#'                 is the preprint (not reviewed) archivable?
#' * `postprint`   \[`character(1)`\]\cr{}
#'                 is the postprint (reviewed but not formatted) archivable?
#' * `pdf`         \[`character(1)`\]\cr{}
#'                 is the publisher's version (reviewed and formatted)
#'                 archivable?
#'
#' @keywords internal
#'
#' @importFrom httr content http_error
#' @importFrom xml2 xml_text xml_find_all xml_find_first xml_attr
parse_publisher <- function(xml_source, outcome, hits) {

  romeoid     <- xml_attr(xml_find_all(xml_source, "//publisher"), "id")

  publisher   <- xml_text(xml_find_all(xml_source, "//name"))

  alias       <- xml_text(xml_find_all(xml_source, "//alias"))
  alias[alias == ""] <- NA_character_

  romeocolour <- xml_text(xml_find_all(xml_source, "//romeocolour"))

  preprint    <- xml_text(xml_find_all(xml_source, "//prearchiving"))
  preprint[preprint == "unknown"] <- NA_character_

  postprint   <- xml_text(xml_find_all(xml_source, "//postarchiving"))
  postprint[postprint == "unknown"] <- NA_character_

  pdf         <- xml_text(xml_find_all(xml_source, "//pdfarchiving"))
  pdf[pdf == "unknown"] <- NA_character_

  data.frame(romeoid = as.numeric(romeoid),
             publisher,
             alias,
             romeocolour,
             preprint,
             postprint,
             pdf,
             stringsAsFactors = FALSE)
}
