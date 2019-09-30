#' Retrieve journals policies by matching title
#'
#' Note that SHERPARoMEO will not return more than 50 journals in a single
#' query. The function will warn you if you are in this case.
#'
#' @param name \[`character(1+)`\]\cr{}
#'             one or several strings to match the titles of the journals
#' @param qtype \[`character(1)`\]\cr{}
#'              in:
#'              * `"exact"` full title must be exactly to provided `name`,
#'              * `"contains"` the provided `name` must appear anywhere in the
#'                title of the journal,
#'              * `"starts"` the provided `name` must appear at the start of
#'                title of the journal.
#' @inheritParams parse_journal
#' @inheritParams check_key
#'
#' @inherit parse_journal return
#'
#' @inherit check_key details
#'
#' @export
#'
#' @examples \donttest{
#' rr_journal_name(name = "Journal of Geology")
#' rr_journal_name(name = "Biogeography", qtype = "contains")
#' # You can also query multiple journals with exact titles in a single call
#' rr_journal_name(name = c("Journal of Biogeography", "PLoS ONE"),
#'                 qtype = "exact")
#' }
rr_journal_name <- function(name,
                            qtype = c("exact", "contains", "starts"),
                            key = NULL) {

  qtype <- match.arg(qtype)

  api_key <- check_key(key)

  answer_list <- lapply(name, function(journal_name) {

    api_answer <- rr_GET(query = list(jtitle = journal_name,
                                      qtype  = qtype,
                                      ak     = api_key))

    parse_generic(api_answer, type = "name", key = api_key)
  })

  journals_df <- do.call(rbind.data.frame,
                         c(answer_list, stringsAsFactors = FALSE))

  return(journals_df)
}
