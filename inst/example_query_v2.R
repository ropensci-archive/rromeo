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

# Publisher Poilicy
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
               query = list("item-type" = "publisher",
                            format      = "Json",
                            limit       = 10,
                            "api-key"   = api_key)) %>%
  content()

str(pol_data$items[[1]])

# Same information as in publishers but directly indexed

# Publication Data -------------------------------------------------------------



# Single journal info
str(journal_data$items[[1]], max.level = 1)

# Publishers' info in journal data
journal_data$items[[1]]$publishers

# Last modified date and system data
journal_data$items[[1]]$system_metadata


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
