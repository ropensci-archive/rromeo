
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rromeo` – an R interface for SHERPA/RoMEO data

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Travis build
status](https://travis-ci.org/Rekyt/rromeo.svg?branch=master)](https://travis-ci.org/Rekyt/rromeo)
[![codecov](https://codecov.io/gh/Rekyt/rromeo/branch/master/graph/badge.svg)](https://codecov.io/gh/Rekyt/rromeo)

The goal of `rromeo` is to provide an R client for the [SHERPA/RoMEO
API](http://www.sherpa.ac.uk/romeo/index.php?la=en&fIDnum=&mode=simple).
The API documentation can be found
[here](http://www.sherpa.ac.uk/romeo/apimanual.php?la=en&fIDnum=%7C&mode=simple).

## Installation

You can install the development version of rromeo with:

``` r
remotes::install_github("Rekyt/rromeo")
```

## API Key

Note that SHERPA/RoMEO lets you run 500 requests per day per IP address,
by [registring for a free API
key](http://www.sherpa.ac.uk/romeo/apiregistry.php) you can bypass this
limit.

`rromeo` can use your registered SHERPA/RoMEO API key:

You can either pass it as a string when querying the data with the
argument `key`

``` r
rr_journal_name("Journal of Geology", key = "Iq83AIL5bss")
```

Or you can specify the enviromental variable `SHERPAROMEO_KEY` in an
`.Rprofile` or in an `.Renviron` file. `rromeo` will automatically
retrieve the API key.
