library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(rvest)
library(stringr)
library(lubridate)

#' Need some dev packages:
# remotes::install_github("tidyverse/googlesheets4/")
# remotes::install_github("r-lib/gargle")
library(googlesheets4)

#' rOpenSci packages to ignore
ignore_packages <- c("plotly", "gender", "changes")

# Get sheets
ss <- gs4_get("https://docs.google.com/spreadsheets/d/1gVt7SwbP1tSxcPtiYkFnUX_2yA_Zmy0TW5rT4_qcz5w/")
prev_sites <- read_sheet(ss, 1) %>%
  mutate(date = as_date(date))


#' Get rOpenSci packages
pkgs <- "https://ropensci.github.io/roregistry/registry.json" %>%
  jsonlite::fromJSON(.) %>%
  .[["packages"]] %>%
  filter(!name %in% ignore_packages)

pkgs_search <- paste0("\\b", paste0(pkgs$name,   collapse = "\\b|\\b"), "\\b")


# Extract R Views Top 40
rviews <- tibble(type = "R Views Top 40",
                 site = "https://rviews.rstudio.com/categories/opinion/",
                 base_path = "https://rviews.rstudio.com",
                 class = ".archive-article-header",
                 key = "top\\-40\\-",
                 date_str = "[a-zA-Z]+ [0-9]{4}") %>%
  mutate(pages = map2(site, class,
                      ~read_html(.x) %>%
                        html_nodes(css = .y)),
         date = map2(pages, date_str,
                     ~html_text(.x) %>%
                       str_extract(pattern = .y) %>%
                       myd(truncated = 1)),
         pages = map2(pages, base_path,
                      ~html_nodes(.x, css = "a") %>%
                        html_attr(name = "href") %>%
                        paste0(.y, .) %>%
                        unique())) %>%
  unnest(c(pages, date)) %>%
  filter(str_detect(pages, key),
         date >= as_date("2019-12-31")) %>%
  mutate(mentions = map(pages, ~html_session(.) %>%
                          html_nodes(css = "a") %>%
                          html_text())) %>%
  select(-site, -base_path, -class, -key, -date_str)

# Extract R Views Top 40
rweekly <- tibble(type = "R Weekly",
                 site = "https://rweekly.org/archive",
                 base_path = "https://rweekly.org",
                 class = "li>p",
                 key = "/[0-9]{4}-[0-9]{2}",
                 date_str = "^[0-9]{2} [a-zA-Z]+ [0-9]{4}") %>%
  mutate(pages = map2(site, class,
                      ~read_html(.x) %>%
                        html_nodes(css = .y)),
         date = map2(pages, date_str,
                     ~html_text(.x) %>%
                       str_extract(pattern = .y) %>%
                       dmy()),
         pages = map2(pages, base_path,
                      ~html_nodes(.x, css = "a") %>%
                        html_attr(name = "href") %>%
                        paste0(.y, .) %>%
                        unique())) %>%
  unnest(c(pages, date)) %>%
  filter(str_detect(pages, key),
         date >= as_date("2020-01-01")) %>%
  mutate(highlights = map(pages,
                          ~html_session(.) %>%
                            html_nodes(css = "#highlight+ul") %>%
                            html_nodes(css = "a") %>%
                            html_text()),
         mentions = map(pages, ~html_session(.) %>%
                          html_nodes(css = "a") %>%
                          html_text())) %>%
  select(-site, -base_path, -class, -key, -date_str)

featured <- bind_rows(rviews, rweekly) %>%
  mutate(mentions_pkgs = map(mentions, ~str_extract(., pkgs_search)),
         mentions_pkgs = map(mentions_pkgs, ~unique(.[!is.na(.)])),
         mentions_n = map_int(mentions_pkgs, ~length(.[. != ""])),
         highlights_pkgs = map(highlights, ~str_extract(., pkgs_search)),
         highlights_pkgs = map(highlights_pkgs, ~unique(.[!is.na(.)])),
         highlights_n = map_int(highlights_pkgs, ~length(.[. != ""]))) %>%
  filter(mentions_n > 0) %>%
  unnest(mentions_pkgs) %>%
  mutate(highlighted = map2_lgl(mentions_pkgs, highlights_pkgs, ~.x %in% .y)) %>%
  select(-mentions, -highlights)

# Format for site
featured_formatted <- featured %>%
  left_join(select(pkgs, name, maintainer), by = c("mentions_pkgs" = "name")) %>%
  mutate(docs_site = paste0("https://docs.ropensci.org/", mentions_pkgs)) %>%
  group_by(type, pages, date) %>%
  mutate(draft_tweet_head = case_when(
    type == "R Views Top 40" ~
      paste0(if_else(mentions_n[1] == 1, "", paste0(mentions_n[1], " ")),
             "rOpenSci peer reviewed package", if_else(mentions_n[1] > 1, "s", ""), " in ",
             month(date[1], label = TRUE, abbr = FALSE), " ", year(date[1]), " Top 40 New Packages by @RStudioJoe\n\n"),
    type == "R Weekly" & highlighted ~
      paste0(if_else(highlights_n[1] == 1, "", paste0(highlights_n[1], " ")),
             "rOpenSci peer reviewed package", if_else(highlights_n[1] > 1, "s", ""),
             " in @rweekly_org highlights!\n\n"),
    TRUE ~ ""),
    draft_tweet_footer = case_when(
      type == "R Views Top 40" ~ paste0("RViews: ", pages[1], "\n\n#rstats"),
      type == "R Weekly" & highlighted ~ paste0(pages[1], "\n\n#rstats"),
      TRUE ~ ""),
    draft_tweet_body = case_when(
      type == "R Views Top 40" ~
        paste0(paste0("#", mentions_pkgs, " do THIS COOL THING\n",
                      "by @THISMAINTAINER \n",
                       docs_site[1], "\n\n"),
               collapse = ""),
      type == "R Weekly" & highlighted ~
        paste0(paste0("#", unique(highlights_pkgs), " do THIS COOL THING\n",
                      "by @THISMAINTAINER \n",
                      docs_site[1], "\n\n"),
               collapse = ""),
      TRUE ~ "")) %>%
  ungroup() %>%
  mutate(draft_tweet = paste0(draft_tweet_head,
                              draft_tweet_body,
                              draft_tweet_footer),
         pages = paste0("=HYPERLINK(\"", pages, "\", \"Original Post\")"),
         docs_site = paste0("=HYPERLINK(\"", docs_site, "\", \"Docs Link Work?\")"),
         ropensci_author = stringi::stri_trans_general(maintainer, "Latin-ASCII"),
         ropensci_author = paste0("https://ropensci.org/author/",
                                  str_replace_all(ropensci_author,
                                                  c(" " = "-", "\\." = ""))),
         ropensci_author = paste0("=HYPERLINK(\"", ropensci_author, "\", \"",
                                  maintainer, "\")")) %>%
  mutate_at(vars(ropensci_author, pages, docs_site), gs4_formula) %>%
  mutate(`Use?` = "",
         media = "") %>%
  select(`Use?`, type, pages, date, mentions_pkgs, highlighted, docs_site,
         ropensci_author, draft_tweet, media) %>%
  arrange(date, type, mentions_pkgs)

# Remove dates already in previous sheets
if(nrow(prev_sites) > 0) {
  featured_relevant <- anti_join(featured_formatted, prev_sites,
                                 by = c("type", "date", "mentions_pkgs"))
  if(nrow(featured_relevant) > 1) {
    sheets_append(featured_relevant, ss = ss, sheet = 1)
  } else message("No new features")
} else {
  # If no previous features, start a new sheet
  sheets_write(featured_relevant, ss = ss, sheet = 1)
}
