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
#' @examples \donttest{
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
