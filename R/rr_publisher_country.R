#' Get Publisher Policy by Publisher's Country
#'
#' Retrieve publisher's policy based on publisher's country. The code should be
#' the ISO_3166-1_alpha-2 code of the country
#' <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2>.
#'
#' @param country \[`character(1+)`\]\cr{}
#'                one or a vector of ISO two-letter country code or `AA` for
#'                international publisher, `ZZ` for publisher of unknown
#'                countries and `__` for publishers without specified country
#'                (case insensitive).
#' @inheritParams check_key
#'
#' @inherit parse_publisher return
#'
#' @inherit check_key details
#'
#' @export
#'
#' @examples \donttest{
#' # Taiwan
#' rr_publisher_country("TW")
#' # Egypt
#' rr_publisher_country("EG")
#' rr_publisher_country(c("TW", "EG"))
#' }
rr_publisher_country <- function(country, key = NULL) {

  vapply(country, validate_country_code, logical(1))

  api_key <- check_key(key)

  answer_list <- lapply(country, function(given_country) {
    api_answer <- rr_GET(query = list(country = given_country,
                                      ak      = api_key))

    parse_generic(api_answer)
  })

  publishers_df <- do.call(rbind.data.frame,
                           c(answer_list, stringsAsFactors = FALSE))

  return(publishers_df)
}
