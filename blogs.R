library(tidyverse)
library(rvest)
library(purrr)
library(lubridate)

#' Need some dev packages:
# remotes::install_github("tidyverse/googlesheets4/")
# remotes::install_github("r-lib/gargle")
library(googlesheets4)

# Get and setup sheets
ss <- sheets_get("https://docs.google.com/spreadsheets/d/1WcSEM6yRTgN5R4HPv_jXKnCEHv-5Z5N0t9yCfDs7ZSk")
sheets <- sheets_sheet_names(ss)
prev_posts <- read_sheet(ss, 1) %>%
  mutate(post_date = as_date(post_date))

start_date <- Sys.Date() - days(60)

url <- "https://www.r-bloggers.com/"
dates <- format(seq(start_date, Sys.Date(), by = "1 month"), "%Y/%m")

ro_text <- regex("ropensci", ignore_case = TRUE)

posts <- tibble(archives = paste0(url, dates)) %>%
  mutate(last_page = map_dbl(archives, ~{
    read_html(.) %>%
      html_nodes(css = ".last") %>%
      html_text() %>%
      as.numeric()
  })) %>%
  mutate(archives_page = map2(archives, last_page,
                              ~paste0(.x, "/page/", seq_len(.y)))) %>%
  unnest(archives_page) %>%
  mutate(archive_text = map(archives_page, read_html),
         post_link = map(archive_text, ~{
           html_nodes(., css = ".post") %>%
             html_nodes(css = ".more-link") %>%
             html_attr(name = "href")
         }),
         post_date = map(archive_text, ~{
           html_nodes(., css = ".meta .date") %>%
             html_text()
         }),
         post_title = map(archive_text, ~{
           html_nodes(., css = ".post") %>%
             html_nodes(css = "h2 a") %>%
             html_text()
           }),
         post_author = map(archive_text, ~{
           html_nodes(., css = ".meta a") %>%
             html_text()
         })) %>%
  unnest(cols = c(post_link, post_date, post_title, post_author)) %>%
  mutate(post_date = mdy(post_date))

# Get posts not authored by rOpenSci
posts_text <- posts %>%
  filter(post_date >= start_date,
         !str_detect(post_author, pattern = ro_text)) %>%
  mutate(post_text = map(post_link, ~html_nodes(read_html(.), css = ".entry")),
         post_orig_link = map_chr(post_text, ~{
           html_node(., css = "strong a") %>%
             html_attr("href")
         }),
         post_text = map_chr(post_text, html_text),
         ropensci_text = str_detect(post_text, pattern = ro_text),
         ropensci_title = str_detect(post_title, pattern = ro_text))


posts_relevant <- posts_text %>%
  filter(ropensci_title | ropensci_text) %>%
  select(ropensci_title,
         post_date, post_link, post_orig_link, post_title, post_author) %>%
  mutate(`Use?` = "",
         post_link = paste0("=HYPERLINK(\"", post_link, "\", \"R-bloggers\")"),
         post_orig_link = paste0("=HYPERLINK(\"", post_orig_link, "\", \"Original Post\")"),
         tweet_draft = "",
         post_title = str_wrap(post_title, width = 80)) %>%
  mutate_at(vars(post_link, post_orig_link), sheets_formula) %>%
  select(`Use?`, everything()) %>%
  arrange(post_date, post_title)

# Remove posts already added to sheet
if(nrow(prev_posts) > 0) {
  posts_relevant <- anti_join(posts_relevant,
                              prev_posts,
                              by = c("post_date", "post_title"))
  if(nrow(posts_relevant) > 0) sheet_append(posts_relevant, ss = ss, sheet = 1)
} else {
  # If no previous posts, start a new sheet
  sheet_write(posts_relevant, ss = ss, sheet = 1)
}

