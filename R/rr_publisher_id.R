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
