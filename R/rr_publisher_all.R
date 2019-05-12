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
