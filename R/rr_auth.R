#' Store provided API key into Environment Variable
#'
#' This function stores the provided API key as argument in to an environment
#' variable `SHERPAROMEO_KEY` for further use by other `rromeo` functions.
#'
#' For more information regarding API keys, please refer to dedicated vignette
#' with the following command
#' `vignette("setting_up_api_key", package = "rromeo")`
#'
#' @param key \[`character(1)`\]\cr{}
#'            A string giving the API key to save into the environment
#'
#' @export
#' @examples \dontrun{
#' rr_auth("Iq83AIL5bss")
#' }
rr_auth <- function(key) {
  Sys.setenv("SHERPAROMEO_KEY" = key)
}
