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
#'
#' @export
#' @examples
#' rr_publisher(55)
rr_publisher = function(given_id) {

  given_id = tryCatch({
    as.integer(given_id)
  },
  error = function() {
    stop("id needs to be an integer")
    return(NA_integer_)
  })

  GET(rr_base_api(), query = list(id = given_id))
}

#' Journal data by ISSN
#'
#' @param issn A single journal ISSN
#'
#' @export
#'
#' @examples
#'
#' rr_journal_issn("1947-6264")
rr_journal_issn = function(issn) {

  validate_issn(issn)

  api_answer = GET(rr_base_api(), query = list(issn = issn))

  parse_answer(api_answer, multiple = FALSE)
}

#' Journal data by title
#'
#' @param name A character string, containing the (possibly) partial name of the
#' journal
#' @param qtype A character string saying whether you are lookin for `exact`,
#' `contains` or `starts with` matches
#' @inheritParams parse_answer
#'
#' @export
#'
#' @examples
#' rr_journal_name("Journal of Geology")
#' rr_journal_name("Biogeography", multiple = FALSE, qtype = "contains")
#' rr_journal_name("Biogeography", multiple = TRUE, qtype = "contains")
rr_journal_name = function(name, multiple = FALSE,
                           qtype = c("exact", "contains", "starts with")) {

  qtype = match.arg(qtype)

  api_answer = GET(rr_base_api(), query = list(jtitle = name, qtype = qtype))

  parse_answer(api_answer, multiple = multiple)
}
