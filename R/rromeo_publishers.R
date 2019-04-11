#' Get Publisher Policy from Publisher ID
#'
#' Use SHERPA/RoMEO API to retrieve a specific publisher policies on manuscript
#' archival
#'
#' @param id \[`integer(1+)`\]\cr{}
#'           one or a vector of SHERPA/RoMEO publisher's ID
#' @inheritParams check_key
#'
#' @inherit parse_publisher return
#'
#' @inherit check_key details
#'
#' @export
#' @examples \dontrun{
#' rr_publisher_id(id = 55)
#' rr_publisher_id(id = c(55, 735))
#' }
rr_publisher_id <- function(id, key = NULL) {

  if (any(!grepl("^[[:digit:]]+$", id))) {
    stop("All provided IDs should be integers", call. = FALSE)
  }

  api_key <- check_key(key)

  answer_list <- lapply(id, function(publisher_id) {
    api_answer <- rr_GET(query = list(id = publisher_id,
                                      ak = api_key))

    parse_generic(api_answer)
  })

  publishers_df <- do.call(rbind.data.frame,
                           c(answer_list, stringsAsFactors = FALSE))

  return(publishers_df)
}

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


#' Get Publisher Policy by Publisher Name
#'
#' Use SHERPA/RoMEO API to retrieve a specific publisher policies on manuscript
#' archival based on matching the name of the publishers.
#'
#' @param name  \[`character(1+)`\]\cr{}
#'              One or a vector of query string(s) to search publisher name
#' @param qtype \[`character(1)`\]\cr{}
#'              in `c("all", "any", "exact")` define the type of matching:
#' * `all` means that all strings in `name` must appear in any
#'   order or location
#' * `any` means that at least one of the strings in `name` must
#'   appear
#' * `exact` means that the `name` string must appear in the
#'   publisher's name or its alias.
#'
#' @inheritParams check_key
#'
#' @inherit parse_publisher return
#'
#' @inherit check_key details
#'
#' @export
#'
#' @examples \dontrun{
#' rr_publisher_name(name = "Optical Society", qtype = "all")
#' rr_publisher_name(name = "Swiss Chemistry", qtype = "any")
#' rr_publisher_name(name = "Swiss Chemistry", qtype = "exact")
#' }
rr_publisher_name <- function(name, qtype = c("all", "any", "exact"),
                              key = NULL) {

  qtype <- match.arg(qtype)

  api_key <- check_key(key)

  answer_list <- lapply(name, function(publisher_name) {
    api_answer <- rr_GET(query = list(pub   = publisher_name,
                                      qtype = qtype,
                                      ak    = api_key))

    parse_generic(api_answer)
  })

  publishers_df <- do.call(rbind.data.frame,
                           c(answer_list, stringsAsFactors = FALSE))

  return(publishers_df)
}

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
#' @examples \dontrun{
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
#' @examples \dontrun{
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

#' Get all Publisher Policies
#'
#' Retrieve all data on publishers policies from SHERPA/RoMEO.
#'
#' @inheritParams check_key
#'
#' @inherit parse_publisher return
#'
#' @inherit check_key details
#'
#' @export
rr_publisher_all <- function(key = NULL) {

  message("This function can take a long time to run, please be patient.")

  api_answer <- rr_GET(query = list(all = "yes",
                                    ak  = check_key(key)))

  parse_generic(api_answer)
}
