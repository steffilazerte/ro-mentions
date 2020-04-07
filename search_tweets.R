#' R Code to run and save this report:
#+ eval = FALSE
# rmarkdown::render(input = "search_tweets.R",
#                   output_file = paste0('search_tweets_', Sys.Date(), '.html'),
#                   envir = new.env())

#' # Setup
library(rtweet)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(DT)

#' Twitter users to ignore
ignore_users <- c("CRANberriesFeed", "ropensci", "tidyversetweets",
                  "roknowtifier")

#' rOpenSci packages to ignore
ignore_packages <- c("plotly")

#' Earliest date to look from (one week)
start_date <- Sys.Date() - days(7)

#' Get rOpenSci packages
pkgs <- "https://ropensci.github.io/roregistry/registry.json" %>%
  jsonlite::fromJSON(.) %>%
  .[["packages"]] %>%
  filter(status == "active",
         !name %in% ignore_packages) %>%
  mutate(maintainer_link = paste0(
    "<a href='https://ropensci.org/author/",
    str_replace(maintainer, " ", "-"), "'>",
    str_replace(maintainer, " ", "<br>"), "</a>"))

#' # Tweets
#' Get tweets (will authorize rtweet to your twitter account)
#'
#' - This may take a while to run
#' - Search for all #rstats and ropensci tweets, *then* filter by package
#'   Seem to run into rate limits if try to put package names directly in search
#' - Consider adding 'lang = "en"' to `search_tweets()` (but this isnt' perfect)

tweets_raw <- search_tweets(q = "#rstats OR ropensci",
                            n = 18000,
                            include_rts = FALSE)

#' The limit is 18000, do we have that many? If so, we may be missing some tweets
nrow(tweets_raw)

#' Narrow in on what we care about
tweets_relevant <- tweets_raw %>%
  select(created_at, screen_name, text, status_url,
         hashtags, urls_expanded_url) %>%
  mutate(name = str_extract_all(text,
                                paste0("\\b", paste0(pkgs$name,
                                                     collapse = "\\b|\\b"), "\\b")),
         name = map(name, unique),
         n = map_int(name, length)) %>%
  filter(n > 0)


#' Filter what we don't need
tweets_subset <- tweets_relevant %>%
  unnest(cols = c(name)) %>%
  left_join(select(pkgs, name, maintainer_link), by = "name") %>%
  filter(created_at >= start_date,       # Must be on or after start date
         !tolower(screen_name) %in% tolower(ignore_users)) # Must not contain ignored users

#' # Tweets to Consider
tweets_subset %>%
  mutate(status_url = paste0("<a href = '", status_url, "'>", status_url, "</a>")) %>%
  group_by(created_at, screen_name, text, status_url) %>%
  summarize(names = paste0(name, collapse = " and "),
            date = as_date(created_at[1]),
            twitter_user = paste0("<a href = 'https://twitter.com/",
                                  screen_name[1], "'>",
                                  screen_name[1], "</a>"),
            maintainer_link = paste0(maintainer_link, collapse = " and "),
            draft_tweet = paste0("[use case] Example of using ", names, ", by @", screen_name[1], "<br><br>",
                                urls_expanded_url[1], "<br><br>",
                                "with XXX and XXX<br><br>",
                                names, " by @GET_TWITTER_HANDLE(S)<br><br>",
                                "#rstats #", paste0(name, collapse = " #"), "<br><br>",
                                "GIF? MEDIA?")) %>%
  ungroup() %>%
  arrange(names, created_at) %>%
  select(names, date, twitter_user, text,
         original_tweet = status_url, ropensci_author = maintainer_link, draft_tweet) %>%

  datatable(escape = FALSE, options = list(pageLength = 50))



