library("vcr")
vcr_configure(dir = "../fixtures/vcr_cassettes",
              filter_sensitive_data = list("<<rromeo_key>>" = Sys.getenv("SHERPAROMEO_KEY")))
