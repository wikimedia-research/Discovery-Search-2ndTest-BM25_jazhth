library(tidyverse)
library(binom)
#library(textcat)
source("utils.R")

# Import events fetched from MySQL
load(path("data/ab-test_bm25.RData"))
events <- events[!duplicated(events$event_id),]
events$test_group <- factor(
  events$test_group,
  levels = c("bm25:control", "bm25:inclinks_pv"),
  labels = c("Control Group (tf–idf)", "Using per-field query builder with incoming links and pageviews as QIFs"))
cirrus <- readr::read_tsv(path("data/ab-test_bm25_cirrus-results.tsv.gz"), col_types = "cccc")
cirrus <- cirrus[!duplicated(cirrus),]
events <- left_join(events, cirrus, by = c("event_id", "page_id", "cirrus_id"))
rm(cirrus)
nrow(events) # number of events 104587
length(unique(events$search_id)) # number of unique sessions 36355

# Summary Table
events_summary <- events %>%
  group_by(wiki, `Test group` = test_group) %>%
  summarize(`Search sessions` = length(unique(search_id)), `Events recorded` = n()) %>% ungroup %>%
  {
    rbind(., tibble(
      `wiki` = "Total",
      `Test group` = "Total",
      `Search sessions` = sum(.$`Search sessions`),
      `Events recorded` = sum(.$`Events recorded`)
    ))
  } %>%
  mutate(`Search sessions` = prettyNum(`Search sessions`, big.mark = ","),
         `Events recorded` = prettyNum(`Events recorded`, big.mark = ","))
knitr::kable(events_summary, format = "markdown", align = c("l", "l", "r", "r"))

# SERP De-duplication
temp <- events %>%
  filter(action == "SERP") %>%
  group_by(session_id, search_id, query) %>%
  mutate(new_page_id = min(page_id)) %>%
  ungroup %>%
  select(c(page_id, new_page_id)) %>%
  distinct
events <- left_join(events, temp, by = "page_id"); rm(temp)
temp <- events %>%
  filter(action == "SERP") %>%
  arrange(new_page_id, ts) %>%
  mutate(dupe = duplicated(new_page_id, fromLast = FALSE)) %>%
  select(c(event_id, dupe))
events <- left_join(events, temp, by = "event_id"); rm(temp)
events$dupe[events$action == "click"] <- FALSE
events <- events[!events$dupe & !is.na(events$new_page_id), ] %>%
  select(-c(page_id, dupe)) %>%
  rename(page_id = new_page_id) %>%
  arrange(date, session_id, search_id, page_id, desc(action), ts)
# Summarize on a page-by-page basis:
searches <- events %>%
  group_by(wiki, `test group` = test_group, session_id, search_id, page_id) %>%
  filter("SERP" %in% action) %>% # filter out searches where we have clicks but not SERP events
  summarize(ts = ts[1], query = query[1],
            results = ifelse(n_results_returned[1] > 0, "some", "zero"),
            clickthrough = "click" %in% action,
            `first clicked result's position` = ifelse(clickthrough, position_clicked[2], NA),
            `result page IDs` = result_pids[1],
            `Query score (F=0.1)` = query_score(position_clicked, 0.1),
            `Query score (F=0.5)` = query_score(position_clicked, 0.5),
            `Query score (F=0.9)` = query_score(position_clicked, 0.9)) %>%
  arrange(ts)
# 87517 events, 64527 searches

# Summary Table
events_summary2 <- events %>%
  group_by(wiki, `Test group` = test_group) %>%
  summarize(`Search sessions` = length(unique(search_id)), `Events recorded` = n()) %>% ungroup %>%
  {
    rbind(., tibble(
      `wiki` = "Total",
      `Test group` = "Total",
      `Search sessions` = sum(.$`Search sessions`),
      `Events recorded` = sum(.$`Events recorded`)
    ))
  } %>%
  mutate(`Search sessions` = prettyNum(`Search sessions`, big.mark = ","),
         `Events recorded` = prettyNum(`Events recorded`, big.mark = ","))
searches_summary <- searches %>%
  group_by(wiki, `Test group` = `test group`) %>%
  summarize(`Search sessions` = length(unique(search_id)), `Searches recorded` = n()) %>% ungroup %>%
  {
    rbind(., tibble(
      `wiki` = "Total",
      `Test group` = "Total",
      `Search sessions` = sum(.$`Search sessions`),
      `Searches recorded` = sum(.$`Searches recorded`)
    ))
  } %>%
  mutate(`Search sessions` = prettyNum(`Search sessions`, big.mark = ","),
         `Searches recorded` = prettyNum(`Searches recorded`, big.mark = ","))
knitr::kable(inner_join(searches_summary, events_summary2, by=c("wiki", "Test group", "Search sessions")), 
             format = "markdown", align = c("l", "l", "r", "r", "r"))


# my.profiles <- TC_byte_profiles[names(TC_byte_profiles) %in% c("english", "chinese-big5", "chinese-gb2312", "japanese-euc_jp", "japanese-shift_jis", "thai")]
# temp <- textcat(events$query, p = my.profiles)
