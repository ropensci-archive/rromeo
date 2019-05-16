#' Get Publisher Policy by Publisher's Continent
#'
#' Retrieve publisher's policy based on publisher's continent. This function
#' does not work for unclassified or international publishers.
#'
#' @param continent \[`character(1+)`\]\cr{}
#'                  one or a vector of strings in ```
#'                  c("Africa", "Antarctica",  "Asia",  "Australasia",
#'                  "Carribean",  "Central America",  "Europe",
#'                  "North America",  "Oceania",  "South America")
#'                  ```\cr{}
#'                  the continent name to retrieve
#' @inheritParams check_key
#'
#' @inherit parse_publisher return
#'
#' @inherit check_key details
#'
#' @export
#'
#' @examples \donttest{
#' rr_publisher_continent(continent = "Caribbean")
#' rr_publisher_continent(continent = "Central America")
#' rr_publisher_continent(continent = c("Caribbean", "Central America"))
#' }
rr_publisher_continent <- function(continent = c("Africa",
                                                 "Antarctica",
                                                 "Asia",
                                                 "Australasia",
                                                 "Caribbean",
                                                 "Central America",
                                                 "Europe",
                                                 "North America",
                                                 "Oceania",
                                                 "South America"),
                                   key = NULL) {

  valid_continents <- continent %in% c("Africa", "Antarctica", "Asia",
                                       "Australasia",
                                       "Caribbean", "Central America", "Europe",
                                       "North America", "Oceania", "South America")

  if (any(!valid_continents)) {
    stop("Some continents are not valid, see ?rr_publisher_continent to get ",
         "the list of valid continents", call. = FALSE)
  }

  api_key <- check_key(key)

  answer_list <- lapply(continent, function(single_continent) {
    api_answer <- rr_GET(query = list(country = single_continent,
                                      ak      = api_key))

    parse_generic(api_answer)
  })

  publishers_df <- do.call(rbind.data.frame,
                           c(answer_list, stringsAsFactors = FALSE))

  return(publishers_df)
}
