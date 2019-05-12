#' Generic parsing function
#'
#' @param api_answer \[`httr::response()`\]\cr{}
#'                   The API answer
#'
#' @param ... Other options passed to parsing functions
#'
#' @return either results from [`parse_journal()`] or [`parse_publisher()`]
parse_generic <- function(api_answer, ...) {

  if (http_error(api_answer)) {
    stop("The API endpoint could not be reached. Please try again later.",
         call. = FALSE)
  }

  xml_source <- content(api_answer, encoding = "ISO-8859-1")

  # API Control Block
  apicontrol <- xml_text(xml_find_all(xml_source, "//apicontrol"))

  if (apicontrol == "invalidapikey") {
    stop("The provided API key is invalid. ",
         "You can register for a free API at ",
         "http://www.sherpa.ac.uk/romeo/apiregistry.php", call. = FALSE)
  } else if (apicontrol == "pastfreelimit") {
    stop("You have exceeded the free use limit of 500 requests per day. ",
         "To go beyond this limit you should register for a free API key ",
         "available at http://www.sherpa.ac.uk/romeo/apiregistry.php",
         call. = FALSE)
  } else if (apicontrol == "invalid") {
    stop("The query is invalid. Please check the query", call. = FALSE)
  }

  hits <- xml_text(xml_find_all(xml_source, "//numhits"))
  outcome <- xml_text(xml_find_all(xml_source, "//outcome"))

  # Outcome Block
  if (apicontrol == "journal" & (outcome == "notFound" | hits == 0)) {
    stop("No journal matches your query terms. Please try another query.",
         call. = FALSE)
  } else if (apicontrol %in% c("", "publisher", "identifier") &
             (outcome == "notFound" | hits == 0)) {
    stop("No publisher was found. Maybe try another query? ;)", call. = FALSE)
  }

  # Parsing branches
  if (apicontrol == "journal") {
    parsed <- parse_journal(xml_source, outcome = outcome, hits = hits, ...)
  } else if (apicontrol %in% c("publisher", "identifier", "colour") |
             (apicontrol %in% c("all", "") & outcome == "publisherFound")) {
    parsed <- parse_publisher(xml_source)
  }

  return(parsed)
}
