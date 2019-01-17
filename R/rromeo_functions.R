#' Get record from Publisher ID
#'
#' Use SHERPA/RoMEO RESTful API to retrieve a specific publisher status
#' @param id publisher ID
rr_rest_publisher = function(id) {

  id = tryCatch({
    as.integer(id)
  },
  error = function() {
    stop("id needs to be an integer")
    return(NA_integer_)
  })

  pub_path = paste0(rr_base_rest_api(), "pub/", id, "/")

  GET(rr_base_url(), path = pub_path)
}

#' Get record from Publication ID
#'
#' Use SHERPA/RoMEO regular API to retrieve a specific publication number status
#' @param given_id publication number
#' @inheritParams check_key
#'
#' @export
#' @examples
#' rr_publisher(55)
rr_publisher = function(given_id, key = NULL) {

  given_id = tryCatch({
    as.integer(given_id)
  },
  error = function(cond) {
    stop("id needs to be an integer")
  },
  warning = function(cond) {
    stop("id needs to be an integer")
  })

  api_answer = GET(rr_base_api(), query = list(id = given_id,
                                               ak = check_key(key)))

  parse_publisher(api_answer)
}

#' Journal data by ISSN
#'
#' @param issn A single journal ISSN
#' @inheritParams check_key
#'
#' @export
#'
#' @examples
#'
#' rr_journal_issn("1947-6264")
rr_journal_issn = function(issn, key = NULL) {

  validate_issn(issn)

  api_key = check_key(key)


  api_answer = GET(rr_base_api(), query = list(issn = issn,
                                               ak   = api_key))

  parse_answer(api_answer, multiple = FALSE, key = api_key)
}

#' Journal data by title
#'
#' @param name A character string, containing the (possibly) partial name of the
#' journal
#' @param qtype A character string saying whether you are looking for `exact`,
#' `contains` or `starts with` matches
#' @inheritParams parse_answer
#' @inheritParams check_key
#'
#' @export
#'
#' @examples
#' rr_journal_name("Journal of Geology")
#' rr_journal_name("Biogeography", multiple = FALSE, qtype = "contains")
#' rr_journal_name("Biogeography", multiple = TRUE, qtype = "contains")
rr_journal_name = function(name, multiple = FALSE,
                           qtype = c("exact", "contains", "starts with"),
                           key = NULL) {

  qtype = match.arg(qtype)

  api_key = check_key(key)

  api_answer = GET(rr_base_api(), query = list(jtitle = name, qtype = qtype,
                                               ak = api_key))

  parse_answer(api_answer, multiple = multiple, key = api_key)
}

#' Query publisher by RoMEO colour
#'
#' @param romeo_colour indicates the SHERPA/RoMEO classification of a publisher
#'     see <http://www.sherpa.ac.uk/romeo/definitions.php?la=en&fIDnum=|&mode=simple&version= for definitions of colour>
#' @inheritParams check_key
#'
#' @return a data frame containing publisher name and the different statuses
#'         of preprint, postprint and publisher's pdf archival
#'
#' @export
rr_romeo_colour = function(romeo_colour = c("green", "blue", "yellow", "white"),
                           key = NULL) {

  romeo_colour = match.arg(romeo_colour)

  api_answer = GET(rr_base_api(), query = list(colour = romeo_colour,
                                               ak = check_key(key)))

  parse_publisher(api_answer)
}
