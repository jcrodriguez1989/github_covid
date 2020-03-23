library("httr")
library("magrittr")
library("purrr")
library("readr")
library("tibble")

github_pat <- Sys.getenv("GITHUB_PAT")
api_url <- "https://api.github.com/"

language <- "" # select if you want to query for a specific coding language
# language <- "R"
lng_query <- ifelse(language == "", "", paste0("+language:", language))
out_file <- paste0("Data/github_covid", language, ".csv")

# query until results are obtained, if error is returned (mostly timeout),
# then it sleeps 30 seconds and retries.
safe_query_counts <- function(url, headers = c()) {
  resp <- GET(url, authenticate(github_pat, ""), add_headers(headers))
  while (resp$status_code != 200) {
    message(content(resp)$message)
    Sys.sleep(30)
    resp <- GET(url, authenticate(github_pat, ""), add_headers(headers))
  }
  content(resp)$total_count
}

# range dates
start_date <- as.Date("2020-01-01")
(end_date <- Sys.Date() - 1) # [1] "2020-03-21"

# initialize date results data.frame
date_data <- tibble(
  date = as.Date(x = integer(), origin = "1970-01-01"),
  event = character(),
  count = numeric()
)

gh_data_lst <- map(seq.Date(start_date, end_date, by = "days"), function(date) {
  message(date)

  # Note: repositories do not differentiate forks
  for (event in c("repositories", "code", "users")) {
    count <-
      paste0(api_url, "search/", event, "?q=+", "created:", date, lng_query) %>%
      safe_query_counts()
    date_data <- add_row(date_data, date = date, event = event, count = count)
  }

  # pr and issues need to be differentiated with 'is:' tag
  for (event in c("pr", "issue")) {
    count <-
      paste0(
        api_url, "search/issues?q=+created:", date, "+is:", event, lng_query
      ) %>%
      safe_query_counts()
    date_data <- add_row(date_data, date = date, event = event, count = count)
  }

  # commits need specific header
  count <- paste0(api_url, "search/commits?q=+created:", date, lng_query) %>%
    safe_query_counts(c("Accept" = "application/vnd.github.cloak-preview"))
  date_data <-
    add_row(date_data, date = date, event = "commits", count = count)

  # topics need specific header
  count <- paste0(api_url, "search/topics?q=+created:", date, lng_query) %>%
    safe_query_counts(
      c("Accept" = "application/vnd.github.mercy-preview+json")
    )
  date_data <- add_row(date_data, date = date, event = "topics", count = count)

  print(tail(date_data, 7))
  date_data
})

# rbind all dates into one data.frame
gh_data <- map_df(gh_data_lst, rbind)

# save results
write_csv(gh_data, path = out_file)
