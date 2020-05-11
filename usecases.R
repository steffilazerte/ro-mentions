#' # Setup
library(rtweet)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)
library(glue)

#' Need some dev packages:
# remotes::install_github("tidyverse/googlesheets4/")
# remotes::install_github("r-lib/gargle")
library(googlesheets4)

# Get sheets
ss <- gs4_get("https://docs.google.com/spreadsheets/d/1c0WgD9DcF_ib5g6V98jj984QH7r3jTWqwp5fwHLtw5g/")
sheets <- sheet_names(ss)

# Remove starting Sheet1
if(any(sheets == "Sheet1")) sheet_delete(ss, "Sheet1")

# Get dates/tweets from previous sheets or from 7 days ago
if(any(sheets != "Sheet1")) {
  prev_tweets <- read_sheet(ss, as.character(max(as_datetime(sheets)))) %>%
    mutate(date_time = as.POSIXct(round(date_time, units = "secs")))
  prev_date <- max(c(prev_tweets$date_time, as_datetime(sheets)), na.rm = TRUE)
} else {
  prev_tweets <- tibble()
  prev_date <- Sys.time() - days(7)
}



#' Twitter users to ignore
ignore_users <- c("CRANberriesFeed", "ropensci", "tidyversetweets",
                  "roknowtifier", "g_rpkg", "rweekly_live", "rweekly_org")

#' rOpenSci packages to ignore
ignore_packages <- c("plotly", "gender", "changes", "scientist")

#' Get rOpenSci packages
pkgs <- "https://ropensci.github.io/roregistry/registry.json" %>%
  jsonlite::fromJSON(.) %>%
  .[["packages"]] %>%
  filter(!name %in% ignore_packages)

#' # Tweets
#' Get tweets (will authorize rtweet to your twitter account)
#'
#' - This may take a while to run
#' - Search for all #rstats and ropensci tweets, *then* filter by package
#'   Seem to run into rate limits if try to put package names directly in search
#' - Consider adding 'lang = "en"' to `search_tweets()` (but this isnt' perfect)

check_time <- trunc(with_tz(Sys.time(), tzone = "UTC"), units = "hours")

tweets_raw <- search_tweets(q = "#rstats OR ropensci OR CRAN",
                            n = 18000,
                            include_rts = FALSE)

#' The limit is 18000, do we have that many? If so, we may be missing some tweets
nrow(tweets_raw)

#' Narrow in on what we care about
tweets_relevant <- tweets_raw %>%
  select(date_time = created_at,
         twitter_user = screen_name,
         text, status_url,
         urls_expanded_url) %>%
  mutate(pkgs = str_extract_all(text,
                                glue("\\b",
                                     glue_collapse(pkgs$name, "\\b|\\b"),
                                     "\\b")),
         pkgs = map(pkgs, unique),
         n = map_int(pkgs, length)) %>%
  filter(n > 0)


#' Get what we need
tweets_subset <- tweets_relevant %>%
  unnest(cols = c(pkgs)) %>%
  filter(date_time >= prev_date,       # Must be on or after start date/time
         !tolower(twitter_user) %in% tolower(ignore_users)) %>% # Must not contain ignored users
  left_join(select(pkgs, name, maintainer), by = c("pkgs" = "name"))

#' # Format tweets
tweets_final <- tweets_subset %>%
  mutate(ropensci_author = stringi::stri_trans_general(maintainer, "Latin-ASCII"),
         ropensci_author = str_replace_all(ropensci_author, c(" " = "-", "\\." = "")),
         ropensci_author = glue("https://ropensci.org/author/{ropensci_author}"),
         ropensci_author = glue("=HYPERLINK(\"{ropensci_author}\", \"",
                                "{maintainer}\")"),
         text = str_wrap(text, width = 40),
         original_tweet = glue("=HYPERLINK(\"{status_url}",
                               "\", \"Original tweet\")")) %>%
  group_by(date_time, text, original_tweet, status_url) %>%
  summarize(pkgs = as.character(glue_collapse(pkgs, "\n")),
            ropensci_author = if_else(length(unique(ropensci_author)) > 1,
                                      glue("=\"", glue_collapse(maintainer, " and "), "\""),
                                      ropensci_author[1]),
            draft_tweet = glue("[use case] Example of using {pkgs}, by @{twitter_user[1]}\n\n",
                               "FORUM_URL\n\n",
                               "with CODE and GIF EXAMPLES\n\n",
                               "{pkgs} by @GET_TWITTER_USER\n\n",
                               "#rstats #", glue_collapse(pkgs, " #"), "\n\n",
                               "[GIF? MEDIA? CHECK USER, CHECK HASHTAGS]"),
            twitter_user = glue("=HYPERLINK(\"https://twitter.com/",
                                "{twitter_user[1]}\", \"",
                                "{twitter_user[1]}\")")) %>%
  ungroup() %>%
  mutate_at(vars(ropensci_author, twitter_user, original_tweet),
           gs4_formula) %>%
  arrange(pkgs, date_time) %>%
  mutate(`use?` = "", media = "") %>%
  select(`use?`, pkgs, date_time, twitter_user, text, original_tweet,
         ropensci_author, draft_tweet, media)


# Remove dates already in previous sheets
if(nrow(prev_tweets) > 0) {
  tweets_final <- anti_join(tweets_final, prev_tweets,
                            by = c("pkgs", "date_time"))
  if(nrow(tweets_final) > 0) {
    sheet_rename(ss = ss, sheet = sheets, new_name = as.character(check_time))
    sheet_append(tweets_final, ss = ss, sheet = as.character(check_time))
  } else message("No new features")
} else {
  # If no previous features, start a new sheet
  sheet_write(tweets_final, ss = ss, sheet = as.character(check_time))
}

