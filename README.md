
<!-- README.md is generated from README.Rmd. Please edit that file -->

# News Time Series <img src="man/figures/newsTS_logo.png" width="160px" align="right"/>

![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

News Time Series is an R package designed to interact with the New York
Times API, facilitating the collection and processing of articles and
their images from 1851 to 2024.

## Installation

You can install the development version of News Time Series from
[GitHub](https://github.com/mateoservent/NewsTimeSeries) with:

``` r
# install.packages("devtools")
devtools::install_github("mateoservent/NewsTimeSeries")
```

## nyt_timeseries()

`nyt_timeseries()` is a function that retrieves articles from the [New
York Times API](https://developer.nytimes.com/) based on specified
search criteria. This function allows users to query articles over a
specified date range and search term, returning a tibble containing
details about each article.

``` r
library(NewsTimeSeries)

# Example usage of nyt_timeseries()
nyt_timeseries(api_key = "<your_api_key>", query = "search_term",
                             begin_date = "YYYY-MM-DD", end_date = "YYYY-MM-DD")
```

The `nyt_timeseries()` function is designed with API rate limits in
mind. However, for extensive search queries covering a wide date range,
the data collection process might need to be spread over multiple days.

To accommodate this, `nyt_timeseries()` includes the `continue_loading`
argument. Setting `continue_loading = TRUE` allows the function to pick
up where it left off, continuing to add articles to an existing
`articles` tibble stored in the global environment.

For example, if you have already collected some data and need to
continue from a specific point, you can use:

``` r
library(NewsTimeSeries)

# Continuing the article collection from a specific point
nyt_timeseries(api_key = "<your_api_key>", query = 'search_term',
               begin_date = '1851-01-01', end_date = '2024-01-01', 
               continue_loading = TRUE)
```

## About

- This experimental package was developed during a
  [SICSS](https://sicss.io/2023/howard-mathematica/schedule) in 2023 and
  in collaboration with [Joel Martinez](https://joeledmartinez.com/).
