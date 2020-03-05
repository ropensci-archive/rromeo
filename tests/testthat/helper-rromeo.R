library("vcr")
vcr_configure(
  dir = "../fixtures/vcr_cassettes",
  filter_sensitive_data = list("<<<api_key>>>" = Sys.getenv('SHERPAROMEO_KEY'))
)
