# Need to register an API key --------------------------------------------------
api_key = Sys.getenv("V2_KEY")

# Publisher Data ---------------------------------------------------------------
pub_data = GET("https://v2.sherpa.ac.uk/cgi/retrieve",
        add_headers("user-agent" = rr_ua()),
    query = list("item-type" = "publisher",
                 format      = "Json",
                 limit       = 10,
                 "api-key"   = api_key)) %>%
  content()

# Publisher Policy
str(pub_data$items[[1]]$policies)

# An example to extract permitted oa location
pub_data$items[[1]]$policies[[1]]$permitted_oa[[3]]

# Get publisher name
pub_data$items[[1]]$name

# Date modified
pub_data$items[[1]]$system_metadata

# Journal associated with publisher
str(pub_data$items[[1]]$publications[[1]])

# Publisher Policies -----------------------------------------------------------

pol_data = GET("https://v2.sherpa.ac.uk/cgi/retrieve",
               add_headers("user-agent" = rr_ua()),
               query = list("item-type" = "publisher_policy",
                            format      = "Json",
                            limit       = 10,
                            "api-key"   = api_key)) %>%
  content()

str(pol_data$items[[1]])

# Same information as in publishers but directly indexed

# Publication Data -------------------------------------------------------------

journal_data = GET("https://v2.sherpa.ac.uk/cgi/retrieve",
               add_headers("user-agent" = rr_ua()),
               query = list("item-type" = "publication",
                            format      = "Json",
                            limit       = 10,
                            "api-key"   = api_key)) %>%
  content()

# Single journal info
str(journal_data$items[[1]], max.level = 1)

# Publishers' info in journal data
journal_data$items[[1]]$publishers

# Last modified date and system data
journal_data$items[[1]]$system_metadata


parse_journal_data = function(journal_dat) {
  journal_df_list = lapply(journal_dat$items, parse_journal_single_item)

  do.call(rbind, journal_df_list)
}

parse_journal_single_item = function(single_journal_item) {

  journal_id    = single_journal_item$id
  journal_title = single_journal_item$title[[1]]$title
  journal_issn  = single_journal_item$issns[[1]]$issn

  journal_policies = single_journal_item$publisher_policy[[1]]

  journal_authorized_oa = lapply(journal_policies$permitted_oa, function(x) {

    given_embargo = x$embargo

    embargo_time = paste(given_embargo$amount, given_embargo$units)
    embargo_time = ifelse(length(embargo_time) == 0, NA, embargo_time)

    data.frame(version = x$article_version[[1]],
               presence = "yes",
               embargo = embargo_time)
  })

  journal_authorized_oa_df = do.call(rbind, journal_authorized_oa)

  ms_version = data.frame(
    version = c("submitted", "accepted", "published"),
    presence = "no",
    embargo = "NA"
  )

  journal_authorized_oa_df = merge(journal_authorized_oa, ms_version,
                                by = c("version", "presence"))

  browser()

  journal_versions = lapply(journal_authorized_oa, function(y) y$version)
  journal_embargo  = lapply(journal_authorized_oa, function(y) y$embargo)

  journal_preprint = ifelse("submitted" %in% journal_versions, "yes", "no")
  journal_postprint = ifelse("accepted" %in% journal_versions, "yes", "no")
  journal_published = ifelse("published" %in% journal_versions, "yes", "no")

  data.frame(
    id        = journal_id,
    title     = journal_title,
    issn      = journal_issn,
    preprint  = journal_preprint,
    postprint = journal_postprint,
    published = journal_published
  )
}


# Funder data ------------------------------------------------------------------
funder_data = GET("https://v2.sherpa.ac.uk/cgi/retrieve",
                   add_headers("user-agent" = rr_ua()),
                   query = list("item-type" = "funder",
                                format      = "Json",
                                limit       = 10,
                                "api-key"   = api_key)) %>%
  content()
str(funder_data$items[[1]], max.level = 1)


# Repository data --------------------------------------------------------------
# SHERPA/RoMEO now indexes repository information about which repo can be used
# to deposit manuscript
repo_data = GET("https://v2.sherpa.ac.uk/cgi/retrieve",
               add_headers("user-agent" = rr_ua()),
               query = list("item-type" = "repository",
                            format      = "Json",
                            limit       = 10,
                            "api-key"   = api_key)) %>%
  content()

str(repo_data$items[[1]])
