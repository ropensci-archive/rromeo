
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rromeo` – an R interface for SHERPA/RoMEO API

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis build
status](https://travis-ci.org/ropensci/rromeo.svg?branch=master)](https://travis-ci.org/ropensci/rromeo)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ropensci/rromeo?branch=master&svg=true)](https://ci.appveyor.com/project/ropensci/rromeo)
[![codecov](https://codecov.io/gh/ropensci/rromeo/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/rromeo)
[![cran
checks](https://cranchecks.info/badges/summary/rromeo)](https://cran.r-project.org/web/checks/check_results_rromeo.html)
[![CRAN-version](https://www.r-pkg.org/badges/version/rromeo)](https://cran.r-project.org/package=rromeo)
[![](https://badges.ropensci.org/285_status.svg)](https://github.com/ropensci/onboarding/issues/285)

`rromeo` is an R client for the [SHERPA/RoMEO
API](http://www.sherpa.ac.uk/romeo/index.php?la=en&fIDnum=&mode=simple).
SHERPA/RoMEO is a database that gives information on editorial policies
of scientific journals regarding the archival of preprint, postprint and
publishers’ manuscripts. `rromeo` is aimed at scientists interested in
archival practices of scientific journals, such as professionals of
[scientometrics](https://en.wikipedia.org/wiki/Scientometrics) but also
at scientist of specific fields interested in the practices of their
fields.

## Install

The latest stable release of `rromeo` is available on CRAN and can be
installed with:

``` r
install.packages("rromeo")
```

You can also install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/rromeo")
```

## API Key

Note that SHERPA/RoMEO lets you run 500 requests per day per IP address,
by [registering for a free API
key](http://www.sherpa.ac.uk/romeo/apiregistry.php) you can bypass this
limit.

`rromeo` can use your registered SHERPA/RoMEO API key; you can either
pass it as a string when querying the data with the argument `key`:

``` r
rr_journal_name("Journal of Geology", key = "Iq83AIL5bss")
```

or you can specify the environment variable `SHERPAROMEO_KEY` in an
`.Rprofile` or in an `.Renviron` file and `rromeo` will automatically
retrieve the API key. See the [specific
vignette](https://docs.ropensci.org/rromeo/articles/setting_up_api_key.html)
to know how to apply and use the API key with `rromeo`.

## Usage

`rromeo` contains functions to retrieve data from the SHERPA/RoMEO API
(for a complete overview please refer to the
[vignette](https://docs.ropensci.org/rromeo/articles/rromeo.html)). The
data is released under the [Creative Commons
Attribution-NonCommercial-ShareAlike 2.5 (CC BY-NC-SA 2.5)
license](https://creativecommons.org/licenses/by-nc-sa/2.5/). A
suggestion of citation is included in `rromeo` via `citation("rromeo")`.

`rromeo` functions are prefixed with `rr_` such as `rr_journal_name()`
that lets you retrieve a journal policy information using the title of a
journal:

``` r
rromeo::rr_journal_name("Journal of Biogeography", qtype = "exact")
#>                     title provided_issn      issn romeocolour preprint
#> 1 Journal of Biogeography          <NA> 0305-0270      yellow      can
#>    postprint    pdf pre_embargo post_embargo pdf_embargo
#> 1 restricted cannot        <NA>    12 months        <NA>
```

the `qtype` argument indicates the type of query to make (`exact` for
exact matching of the title, `contains` for partial matching and `starts
with` to match only the beginning of the title).

You can also retrieve a journal information using its ISSN:

``` r
rromeo::rr_journal_issn("0305-0270")
#>                     title provided_issn      issn romeocolour preprint
#> 1 Journal of Biogeography     0305-0270 0305-0270      yellow      can
#>    postprint    pdf pre_embargo post_embargo pdf_embargo
#> 1 restricted cannot        <NA>    12 months        <NA>
```

`rromeo` also provides a function to retrieve information based on
publisher ID `rr_publisher()`.

SHERPA/RoMEO provides a synthetic “colour” for each journal, the colour
summarizes the editorial policy of a journal:

| RoMEO colour | Archiving policy                                        |
| :----------- | :------------------------------------------------------ |
| `green`      | can archive preprint, postprint and publisher’s version |
| `blue`       | can archive postprint **or** publisher’s version        |
| `yellow`     | can archive preprint                                    |
| `white`      | archiving not formally supported                        |

(Table taken from
<http://www.sherpa.ac.uk/romeo/definitions.php#colours>)

`rromeo` lets you retrieve the policies of all journals of a given
colour using the function `rr_romeo_colour()` (**NOTE:** this function
can be slow as there are many journals to retrieve):

``` r
green_journals = rromeo::rr_romeo_colour("green")
green_journals[8:12,]
#>    romeoid                                                   publisher
#> 8     1128 Association for Information Science and Technology (ASIS&T)
#> 9     1937                                       University of Arizona
#> 10    2951                               Geological Society of America
#> 11    2521                              University of California Press
#> 12    2306                                  Optical Society of America
#>                  alias romeocolour preprint postprint        pdf
#> 8              JASIS&T       green      can       can     cannot
#> 9          Radiocarbon       green      can       can restricted
#> 10           GSA Today       green      can       can        can
#> 11            Collabra       green      can       can        can
#> 12 No Paid Open Access       green      can       can     cannot
```

## Dependency network (Imports only)

<img src="man/figures/README-dependency_network_imports-1.svg" width="100%" />

## Dependency network (Imports and Suggests)

<img src="man/figures/README-dependency_network_full-1.svg" width="100%" />

## Contributing to `rromeo`

We welcome contribution to `rromeo`\! Please read the [contribution
guidelines](https://docs.ropensci.org/rromeo/CONTRIBUTING.html) if you
want to contribute, as well as the below-mentioned Code of Conduct.

## Code of Conduct

Please note that the `rromeo` project is released with a [Contributor
Code of Conduct](https://ropensci.org/code-of-conduct/).
By contributing to this project, you agree to abide by its terms.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
