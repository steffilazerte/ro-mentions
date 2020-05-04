#' # Setup
library(rtweet)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)

#' Need some dev packages:
# remotes::install_github("tidyverse/googlesheets4/")
# remotes::install_github("r-lib/gargle")
library(googlesheets4)

# Get sheets
ss <- gs4_get("https://docs.google.com/spreadsheets/d/1c0WgD9DcF_ib5g6V98jj984QH7r3jTWqwp5fwHLtw5g/")
sheets <- sheet_names(ss)

# Remove starting Sheet1
if(any(sheets == "Sheet1")) sheet_delete(ss, "Sheet1")

# Get dates from previous sheets or from 7 days ago
if(any(sheets != "Sheet1")) {
  prev_date <- read_sheet(ss, as.character(max(as_date(sheets))))
} else {
  prev_date <- Sys.Date() - days(7)
}

prev_date <- max(as_date(sheet_names(ss)))

#' Twitter users to ignore
ignore_users <- c("CRANberriesFeed", "ropensci", "tidyversetweets",
                  "roknowtifier", "g_rpkg")

#' rOpenSci packages to ignore
ignore_packages <- c("plotly", "gender", "changes")

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

tweets_raw <- search_tweets(q = "#rstats OR ropensci OR CRAN",
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


#' Get what we need
tweets_subset <- tweets_relevant %>%
  unnest(cols = c(name)) %>%
  filter(created_at >= prev_date,       # Must be on or after start date
         !tolower(screen_name) %in% tolower(ignore_users)) %>% # Must not contain ignored users
  left_join(select(pkgs, name, maintainer), by = "name")

#' # Format tweets
tweets_final <- tweets_subset %>%
  mutate(ropensci_author = stringi::stri_trans_general(maintainer, "Latin-ASCII"),
         ropensci_author = paste0("https://ropensci.org/author/",
                                  str_replace_all(ropensci_author,
                                                  c(" " = "-", "\\." = ""))),
         ropensci_author = paste0("=HYPERLINK(\"", ropensci_author, "\", \"",
                                  maintainer, "\")"),
         text = str_wrap(text, width = 40),
         original_tweet = paste0("=HYPERLINK(\"", status_url,
                                 "\", \"Original tweet\")")) %>%
  group_by(created_at, screen_name, text, original_tweet, status_url) %>%
  summarize(names = paste0(name, collapse = "\n"),
            date = as_date(created_at[1]),
            twitter_user = paste0("=HYPERLINK(\"https://twitter.com/",
                                  screen_name[1], "\", \"",
                                  screen_name[1], "\")"),
            ropensci_author = if_else(length(unique(ropensci_author)) > 1,
                                      paste0("=\"", paste0(maintainer, collapse = " and "), "\""),
                                      ropensci_author[1]),
            draft_tweet = paste0("[use case] Example of using ", names, ", by @", screen_name[1], "\n\n",
                                 "FORUM_URL\n\n",
                                 "with CODE and GIF EXAMPLES\n\n",
                                 names, " by @GET_TWITTER_USER\n\n",
                                 "#rstats #", paste0(name, collapse = " #"), "\n\n",
                                 "[GIF? MEDIA?]")) %>%
  ungroup() %>%
  mutate_at(vars(ropensci_author, twitter_user, original_tweet),
           gs4_formula) %>%
  arrange(names, created_at) %>%
  mutate(`use?` = "", media = "") %>%
  select(`use?`, names, date, twitter_user, text, original_tweet,
         ropensci_author, draft_tweet, media)


sheet_write(tweets_final, ss = ss, sheet = as.character(Sys.Date()))




