#' Find if journals are available in SHERPA/RoMEO
#'
#' @inheritParams rr_journal_name
#'
#' @return
#' Returns a data frame:
#' * `title`         \[`character(1)`\]\cr{}
#'                   the name of the journal
#' * `provided_issn` \[`character(1)`\]\cr{}
#'                   the ISSN you provided in your query (might differ from the
#'                   ISSN returned by the API)
#' * `issn`          \[`character(1)`\]\cr{}
#'                   the ISSN of the journal
#'
#' @inherit check_key details
#'
#' @examples \donttest{
#' rr_journal_find(name = "Biostatistics", qtype = "contains")
#' }
#'
#' @export
rr_journal_find  <- function(name,
                             qtype = c("exact", "contains", "starts"),
                             key = NULL) {

  qtype <- match.arg(qtype)

  api_key <- check_key(key)

  answer_list <- lapply(name, function(journal_name) {

    api_answer <- rr_GET(query = list(jtitle = journal_name,
                                   qtype  = qtype,
                                   ak     = api_key))

    parse_generic(api_answer, type = "find", key = api_key)
  })

  journals_df <- do.call(rbind.data.frame,
                         c(answer_list, stringsAsFactors = FALSE))

  return(journals_df)
}
