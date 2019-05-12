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
#' @return
#' Returns a data.frame with complete information from journal:
#' * `title`        \[`character(1)`\]\cr{}
#'                  the name of the journal
#' * `issn`         \[`character(1)`\]\cr{}
#'                  the ISSN of the journal
#' * `romeocolour`  \[`character(1)`\]\cr{}
#'                  the SHERPA/RoMEO colour of the journal
#' * `preprint`     \[`character(1)`\]\cr{}
#'                  is the preprint (not reviewed) archivable?
#' * `postprint`    \[`character(1)`\]\cr{}
#'                  is the postprint (reviewed but not formatted)?
#' * `pdf`          \[`character(1)`\]\cr{}
#'                  is the publisher's version (reviewed and formatted)
#' * `pre_embargo`  \[`character(1)`\]\cr{}
#'                  if applicable the embargo period before the author(s) can
#'                  archive the preprint
#' * `post_embargo` \[`character(1)`\]\cr{}
#'                  if applicable the embargo period before the author(s) can
#'                  archive the postprint
#' * `pdf_embargo`  \[`character(1)`\]\cr{}
#'                  if applicable the embargo period before the author(s) can
#'                  archive the publisher's version
#'
#' @inherit check_key details
#'
#' @export
#'
#' @examples \dontrun{
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
