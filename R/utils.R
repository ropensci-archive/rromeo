#' Checks validity of the ISSN
#'
#' International Standard Serial Numbers (ISSNs) have a specific structure and
#' this function checks that the provided ISSN is valid. For more information
#' please go to
#' <https://en.wikipedia.org/wiki/International_Standard_Serial_Number>
#'
#' @param issn \[`character(1)`\]\cr{}
#'             The ISSN of a journal
#' @return `NULL` if the ISSN is valid, errors otherwise
#'
#' @keywords internal
validate_issn <- function(issn) {

  # How to validate ISSN?
  # https://en.wikipedia.org/wiki/International_Standard_Serial_Number

  # Pre-check: does it look like a valid ISSN?
  if (!grepl("^\\d{4}-\\d{3}[\\dxX]$", issn, perl = TRUE)) {
    stop("ISSN is invalid, please check the format", call. = FALSE)
  }

  # Weighted sum check
  issn_digits <- strsplit(gsub("-", "", issn), "")[[1]]

  non_control_digits <- as.numeric(issn_digits[1:7])
  control_digit <- toupper(issn_digits[8])

  weighted_sum <- sum(seq(8, 2, by = -1) * non_control_digits)

  weighted_sum <- ifelse(control_digit == "X",
                         weighted_sum + 10,
                         weighted_sum + as.numeric(control_digit))

  if (weighted_sum %% 11 != 0) {
    stop("ISSN is invalid, please check the format", call. = FALSE)
  }

  return(TRUE)
}

#' Check SHERPA/RoMEO API key
#'
#' The key can be either specified in various ways see the Details section.
#'
#' @param key \[`character(1)`\]\cr{}
#'            a character string containing the API key or `NULL`
#'            (see Details section on how to specify it)
#'
#' @details There are several ways to provide your API key.
#' The best way to know about them is to refer to the vignette about
#' "Setting Up Your API key" accessible with the following command:
#' `vignette("setting_up_api_key", package = "rromeo")`.
#' You can also use [`rr_auth`] that will use the provided key to store it as
#' an environmental variable.
#'
#' @return if found the character string of the key, `NULL` otherwise
#' @export
check_key <- function(key = NULL) {
  tmp <- ifelse(is.null(key), Sys.getenv("SHERPAROMEO_KEY"), key)

  if (tmp == "") {
    tmp <- NULL
  }

  return(tmp)
}

#' Parse embargo period from API return
#'
#' This function provides an easy way to return the embargo period in the
#' different categories
#' @param xml_source \[`xml_document`\]\cr{}
#'                   a parsed xml document
#' @param type       \[`character(1)`\]
#'                   name of the embargo type must be in `"pre"`, `"post"`, and
#'                   `"pdf"`
#'
#' @return the embargo period as a string like `"12 months"`
#'
#' @keywords internal
#'
#' @importFrom xml2 xml_text xml_find_first read_html
parse_embargo <- function(xml_source, type = c("pre", "post", "pdf")) {

  type <- match.arg(type)

  tag <- paste0("//", type, "restriction[contains(text(), 'embargo')]")
  embargo_field <- xml_text(xml_find_first(xml_source, tag))

  if (is.na(embargo_field)) {
    return(NA_character_)
  }
  else {
    embargo_field <- read_html(embargo_field)
    time <- xml_text(xml_find_first(embargo_field, "//num"))
    unit <- xml_text(xml_find_first(embargo_field, "//period"))
    return(paste(time, unit))
  }
}

#' Validate ISO two-letters country code
#'
#' If available uses [`ISOcodes::ISO_3166`] to validate country code.
#' Otherwise assume that the code is valid as long as it is a two-letter code or
#' `__`. See [`rr_publisher_country()`] for use of country codes.
#'
#' @param country \[`character(1)`\]\cr{}
#'                a two-letter country code or `AA`, `ZZ` or
#'                `__` (special country codes for SHERPA/RoMEO)
#'
#' @return `TRUE` if the country code is valid, errors otherwise
#'
#' @keywords internal
validate_country_code <- function(country) {
  if (requireNamespace("ISOcodes", quietly = TRUE)) {
    if (!(country %in% c(ISOcodes::ISO_3166_1$Alpha_2, "AA", "ZZ", "__"))) {
      stop(country, " is an invalid country code. ",
           "The country code should be two letter long or '__' for undefined.",
           call. = FALSE)
    }
  } else {
    if (!grepl("^[A-Za-z|\\_]{2}$", country, perl = FALSE)) {
      stop(country, " is an invalid country code. ",
           "The country code should be two letter long or '__' for undefined.",
           call. = FALSE)
    }
  }

  return(TRUE)
}
