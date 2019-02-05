
<!-- README.md is generated from README.Rmd. Please edit that file -->
`rromeo` – an R interface for SHERPA/RoMEO API
==============================================

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Travis build status](https://travis-ci.org/Rekyt/rromeo.svg?branch=master)](https://travis-ci.org/Rekyt/rromeo) [![codecov](https://codecov.io/gh/Rekyt/rromeo/branch/master/graph/badge.svg)](https://codecov.io/gh/Rekyt/rromeo)

`rromeo` is an R client for the [SHERPA/RoMEO API](http://www.sherpa.ac.uk/romeo/index.php?la=en&fIDnum=&mode=simple). SHERPA/RoMEO is a database that gives information on editorial policies of scientific journals regarding the archival of pre-print, post-print and typesetted manuscripts.

Installation
------------

`rromeo` is not yet on CRAN but you can install the development version of `rromeo` with:

``` r
# install.packages("remotes")
remotes::install_github("Rekyt/rromeo")
```

Usage
-----

`rromeo` contains functions to retrieve data from the SHERPA/RoMEO API (for a complete overview please refer to the [vignette](https://rekyt.github.io/rromeo/articles/overview.html)). Usable functions are prefixed with `rr_` such as `rr_journal_name()` that lets you retrieve a journal policy information using the title of a journal:

``` r
rromeo::rr_journal_name("Journal of Biogeography", qtype = "exact")
#>                     title      issn romeocolour preprint  postprint    pdf
#> 1 Journal of Biogeography 0305-0270      yellow      can restricted cannot
#>   pre_embargo post_embargo pdf_embargo
#> 1        <NA>    12 months        <NA>
```

the `qtype` argument indicates the type of query to make (`exact` for exact matching of the title, `contains` for partial matching and `starts with` to match only the beginning of the title).

You can also retrieve a journal information using its ISSN:

``` r
rromeo::rr_journal_issn("0305-0270")
#>                     title      issn romeocolour preprint  postprint    pdf
#> 1 Journal of Biogeography 0305-0270      yellow      can restricted cannot
#>   pre_embargo post_embargo pdf_embargo
#> 1        <NA>    12 months        <NA>
```

`rromeo` also provides a function to retrieve information based on publisher ID `rr_publisher()`.

SHERPA/RoMEO provides a synthetic "colour" for each journal, the colour summarizes the editorial policy of a journal:

| RoMEO colour | Archiving policy                                          |
|:-------------|:----------------------------------------------------------|
| `green`      | can archive pre-print, post-print and publisher's version |
| `blue`       | can archive post-print **or** publisher's version         |
| `yellow`     | can archive pre-print                                     |
| `white`      | archiving not formally supported                          |

(Table taken from <http://www.sherpa.ac.uk/romeo/definitions.php#colours>)

`rromeo` lets you retrieve the policies of all journals of a given colour using the function `rr_romeo_colour()` (**NOTE:** this function can be slow as there many journals to retrieve):

``` r
green_journals = rromeo::rr_romeo_colour("green")
green_journals[8:12,]
#>    romeoid                      publisher                alias romeocolour
#> 8     1937          University of Arizona          Radiocarbon       green
#> 9     2951  Geological Society of America            GSA Today       green
#> 10    2521 University of California Press             Collabra       green
#> 11    2306     Optical Society of America  No Paid Open Access       green
#> 12    2305     Optical Society of America Open Access Journals       green
#>    preprint postprint        pdf
#> 8       can       can restricted
#> 9       can       can        can
#> 10      can       can        can
#> 11      can       can     cannot
#> 12      can       can        can
```

API Key
-------

Note that SHERPA/RoMEO lets you run 500 requests per day per IP address, by [registering for a free API key](http://www.sherpa.ac.uk/romeo/apiregistry.php) you can bypass this limit.

`rromeo` can use your registered SHERPA/RoMEO API key; you can either pass it as a string when querying the data with the argument `key`:

``` r
rr_journal_name("Journal of Geology", key = "Iq83AIL5bss")
```

or you can specify the environment variable `SHERPAROMEO_KEY` in an `.Rprofile` or in an `.Renviron` file and `rromeo` will automatically retrieve the API key.
