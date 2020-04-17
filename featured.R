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
ignore_packages <- c("plotly")


# Get sheets
ss <- sheets_get("https://docs.google.com/spreadsheets/d/1gVt7SwbP1tSxcPtiYkFnUX_2yA_Zmy0TW5rT4_qcz5w/")
sheets <- sheets_sheet_names(ss)
if(any(sheets == "Sheet1")) sheets_sheet_delete(ss, "Sheet1")
if(any(sheets != "Sheet1")) {
  prev_sites <- read_sheet(ss, as.character(max(as_date(sheets))))
} else {
  prev_sites <- NULL
}


#' Get rOpenSci packages
pkgs <- "https://ropensci.github.io/roregistry/registry.json" %>%
  jsonlite::fromJSON(.) %>%
  .[["packages"]] %>%
  filter(status == "active")

pkgs_search <- paste0("\\b", paste0(pkgs$name,   collapse = "\\b|\\b"), "\\b")

sources <- tribble(~type, ~site,  ~base_path,  ~class, ~key,
                   "R Views Top 40",
                   "https://rviews.rstudio.com/categories/opinion/",
                   "https://rviews.rstudio.com/",
                   ".archive-article-title",
                   "top\\-40\\-",

                   "R Weekly",
                   "https://www.r-craft.org/author/r-weekly/",
                   "",
                   "a",
                   "r\\-weekly\\-")

featured <- sources %>%
  mutate(pages = pmap(list(site, class, key, base_path),
                      ~read_html(..1) %>%
                        html_nodes(css = ..2) %>%
                        html_attr(name = "href") %>%
                        str_subset(..3) %>%
                        paste0(..4, .) %>%
                        unique())) %>%
  unnest(c(pages)) %>%
  mutate(mentions = map(pages, ~html_session(.) %>%
                          html_nodes(css = "a") %>%
                          html_text()),
         pkgs = map(mentions, ~str_extract(., pkgs_search)),
         pkgs = map(pkgs, ~.[!is.na(.)])) %>%
  unnest(pkgs) %>%
  select(-site, -base_path, -class, -key, -mentions) %>%
  distinct()

# Format for site
featured_formatted <- featured %>%
  filter(!pkgs %in% ignore_packages) %>%
  left_join(select(pkgs, name, maintainer), by = c("pkgs" = "name")) %>%
  mutate(draft_tweet = case_when(
    type == "R Views Top 40" ~
      paste0("X rOpenSci peer reviewed packages in MONTH YEAR Top 40 New Packages by @RStudioJoe\n\n",
             "#pkg to THIS COOL THING\n",
             "by @THISMAINTAINER \n",
             "http:\\link-blog-post OR github page\n\n",
             "#pkg by @THISMAINTAINER\n",
             "github.com/ropensci/etc.\n\n",
             "RViews: ", pages, "\n\n#rstats"),
    type == "R Weekly" ~
      paste0("X rOpenSci peer reviewed packages in @rweekly_live highlights!\n\n",
             "#pkg to THIS COOL THING\n",
             "by @THISMAINTAINER \n",
             "http:\\link-blog-post OR github page\n\n",
             "#pkg by @THISMAINTAINER\n",
             "github.com/ropensci/etc.\n\n", pages, "\n\n#rstats")),
    pages = paste0("=HYPERLINK(\"", pages, "\")"),
    ropensci_author = stringi::stri_trans_general(maintainer, "Latin-ASCII"),
    ropensci_author = paste0("https://ropensci.org/author/",
                             str_replace_all(ropensci_author,
                                             c(" " = "-", "\\." = ""))),
    ropensci_author = paste0("=HYPERLINK(\"", ropensci_author, "\", \"",
                             maintainer, "\")")) %>%
  mutate_at(vars(ropensci_author, pages), sheets_formula) %>%
  select(-maintainer) %>%
  mutate(`Tweeted?` = "",
         media = "")

# Remove sites already in previous sheets
if(!is.null(prev_sites)) {
  featured_relevant <- anti_join(featured_formatted, prev_sites, by = c("pages", "pkgs"))
} else {
  featured_relevant <- featured_formatted
}

if(nrow(featured_relevant) > 0) {
  sheets_write(featured_relevant, ss = ss, sheet = as.character(Sys.Date()))
}
