#' Collect Articles from The New York Times API
#'
#' This function fetches articles from The New York Times API based on a specified query and date range.
#' It collects articles page by page and stores the results in a global tibble `articles`.
#' If the function is interrupted, the articles fetched up to that point are retained in `articles`.
#' The function can continue adding to an existing articles dataset or start from scratch.
#' It also allows specifying a starting page number for continued collection.
#'
#' @param api_key A character string of your New York Times API key.
#' @param query A character string specifying the search query for articles.
#' @param begin_date A character string specifying the start date for article search in 'YYYY-MM-DD' format. Default: '1851-01-01'.
#' @param end_date A character string specifying the end date for article search in 'YYYY-MM-DD' format. Default: '2024-01-01'. Try using Sys.Date() for the current date
#' @param continue_loading A logical value indicating whether to continue adding to an existing dataset (TRUE) or start anew (FALSE).
#' @param sort A string of characters to determine the order of collecting items. The default is "oldest", it can be changed to "newest".
#' 
#' @return A tibble containing articles fetched from the API. This tibble is also stored in the global environment.
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils tail
#' @importFrom dplyr bind_rows
#' @export
#'

nyt_timeseries <- function(api_key, query, begin_date = '1851-01-01', end_date = '2024-01-01', continue_loading = FALSE, start_page = 0, sort = "oldest") {
  url <- "https://api.nytimes.com/svc/search/v2/articlesearch.json"
  params <- list('api-key' = api_key, 'q' = query, 'begin_date' = begin_date, 'end_date' = end_date, 'sort' = sort)
  
  if (!continue_loading || !exists("articles", envir = .GlobalEnv)) {
    .GlobalEnv$articles <- tibble::tibble()
  }
  
  while (TRUE) {
    for (page in (start_page + 1):200) {
      message(paste0("Collecting from page: ", page - 1))
      params$page <- page - 1
      
      response <- httr::GET(url, query = params)
      
      if (response$status_code == 429) {
        stop("Reached the daily rate limit of the API. HTTP status code: 429")
      } else if (response$status_code == 400) {
        stop("Invalid request or maximum pages reached for the given time period. HTTP status code: 400")
      } else if (response$status_code != 200) {
        message("HTTP request failed with status code: ", response$status_code)
        next
      }
      
      data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      .GlobalEnv$articles <- dplyr::bind_rows(.GlobalEnv$articles, data$response$docs)
      
      Sys.sleep(16)  # API rate limit
      
      if (page == 199) {
        last_article <- tail(data$response$docs, 1)
        if (!is.null(last_article) && 'pub_date' %in% names(last_article)) {
          last_pub_date <- gsub("T.*", "", last_article$pub_date)
          params$begin_date <- substr(last_pub_date, 1, 10)
          message("Updated begin_date to ", params$begin_date, ". Starting from page 0 on next iteration.")
          start_page <- 0
          break
        }
      }
    }
    
    # Check if 'end_date' has been reached or if the page is not reset to 0
    if (params$begin_date >= end_date || start_page != 0) {
      message("Search completed up to the end_date: ", end_date)
      break
    }
  }
  
  return(.GlobalEnv$articles)
}