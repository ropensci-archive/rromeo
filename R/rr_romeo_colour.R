#' Query publisher by RoMEO colour
#'
#' SHERPA/RoMEO classifies publisher in different colours depending on their
#' archiving policies.
#' - **green** publishers let authors archive preprint and postprint or
#'   publisher's version/PDF,
#' - **blue** publishers let authors archive postprint or publisher's
#'   version/PDF,
#' - **yellow** publishers let authors archive preprint,
#' - **white** publishers do not formally support archival.
#'
#' For more details about the definitions of RoMEO colours check the
#' [FAQ section](http://sherpa.ac.uk/romeo/definitions.php#colours) of
#' SHERPA/RoMEO
#'
#' Note that when using `rr_romeo_colour()` the API returns **all** the
#' publishers in the selected category, so the results are generally bigger in
#' size than specific functions like \[`rr_journal_name()`\] or
#' \[`rr_publisher_id()`\]
#'
#' @param romeo_colour \[`character(1)`\]\cr{}
#'                     in `c("green", "blue", "yellow", "white")`
#'                     the SHERPA/RoMEO colour to retrieve
#' @inheritParams check_key
#'
#' @inherit parse_publisher return
#'
#' @inherit check_key details
#'
#' @aliases rr_romeo_color
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rr_romeo_colour(romeo_colour = "green")
#' }
rr_romeo_colour <- rr_romeo_color <- function(
  romeo_colour = c("green", "blue","yellow", "white"), key = NULL) {

  romeo_colour <- match.arg(romeo_colour)

  api_answer <- rr_GET(query = list(colour = romeo_colour,
                                    ak     = check_key(key)))

  parse_generic(api_answer)
}
