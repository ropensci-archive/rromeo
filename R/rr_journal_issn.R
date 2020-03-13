#' Retrieve journal policy using ISSN
#'
#' Retrieve policy information from the SHERPA/RoMEO API using the ISSN
#' from the paper edition of the journal or the ISSN of the electronic version
#' (e-ISSN or ESSN)
#'
#' @param issn \[`character(1+)`\]\cr{}
#'             one or a vector of journal(s) ISSN(s) or ESSN(s)
#' @inheritParams check_key
#'
#' @inherit rr_journal_name return
#'
#' @inherit check_key details
#'
#' @export
#'
#' @examples \donttest{
#' # Query single ISSN
#' rr_journal_issn(issn = "1947-6264")
#'
#' # Query multiple ISSN
#' rr_journal_issn(issn = c("1947-6264", "0030-1299"))
#'
#' # Query by ESSN
#' rr_journal_issn("1463-9084")
#' }
rr_journal_issn <- function(issn, key = NULL) {

  vapply(issn, validate_issn, logical(1))

  api_key <- check_key(key)

  answer_list <- lapply(issn, function(journal_issn) {

    api_answer <- rr_GET(query = list(issn = journal_issn,
                                      ak   = api_key))

    journal_df <- parse_generic(api_answer, type = "name", key = api_key)
  })

  journals_df <- do.call(rbind.data.frame,
                         c(answer_list, stringsAsFactors = FALSE))

  return(journals_df)
}
