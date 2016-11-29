library(igraph)

# searches per search_id
searches %>% group_by(search_id) %>% summarise(n_search=n()) %>% 
  mutate(n_search=ifelse(n_search>=10,"10+",n_search)) %>%
  group_by(n_search) %>%
  summarise(n_session=n()) %>%
  mutate(prop=n_session/sum(n_session)) %>%
  ggplot(aes(x = n_search, y = prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Proportion of search sessions", labels = scales::percent_format()) +
  scale_x_discrete("Number of Searches", limits = c(1:9, "10+")) +
  geom_text(aes(label = sprintf("%.1f%%", 100*prop)), nudge_y = 0.025) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of search sessions making N searches")
  
# No NA in query, but many NA in result_pids, even in searches with some results
sum(is.na(searches$`result page IDs`[searches$results=="some"])) #32041

all_tokens <- c(tokens_ja, tokens_zh, tokens_th)

overlapping_results <- function(x) {
  if (all(is.na(x))) {
    return(diag(length(x)))
  }
  input <- strsplit(stringr::str_replace_all(x, "[\\[\\]]", ""), ",")
  output <- vapply(input, function(y) {
    temp <- vapply(input, function(z) { length(intersect(z, y)) }, 0L)
    temp[is.na(x)] <- 0L
    return(temp)
  }, rep(0L, length(input)))
  diag(output) <- 1L
  return(output)
}

reformulation <- function(result_pids, page_ids, all_tokens) {
  if(length(page_ids)==1){
    return(data.frame(n_search=1, n_reformulate="0"))
  }
  # result overlap
  overlaps_res <- overlapping_results(result_pids)
  # tokens overlap
  this_tokens <- all_tokens[page_ids]
  overlaps_token <- vapply(this_tokens, function(y) {
    temp <- vapply(this_tokens, function(z) { length(intersect(z, y)) }, 0L)
    temp[is.na(this_tokens)] <- 0L
    return(temp)
  }, rep(0L, length(this_tokens)))
  diag(overlaps_token) <- 1L
  
  overlaps_all <- (overlaps_res + overlaps_token) > 0
  diag(overlaps_all) <- 0L
  graph_object <- graph_from_adjacency_matrix(overlaps_all, mode = c("undirected"), diag = F)
  csize <- clusters(graph_object)$csize
  return(data.frame(n_search=length(csize), n_reformulate=ifelse(length(csize)>1,paste(csize-1, collapse=',' ), as.character(csize-1))))
}

reform_times <- searches %>% 
  group_by(wiki, `test group`, session_id, search_id) %>% 
  do(reformulation(.$`result page IDs`, .$page_id, all_tokens))

###########################

# After grouping, searches per search_id
reform_times %>% ungroup %>%
  mutate(n_search=ifelse(n_search>=10,"10+",n_search)) %>%
  group_by(n_search) %>%
  summarise(n_session=n()) %>%
  mutate(prop=n_session/sum(n_session)) %>%
  ggplot(aes(x = n_search, y = prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Proportion of search sessions", labels = scales::percent_format()) +
  scale_x_discrete("Number of Search Groups", limits = c(1:9, "10+")) +
  geom_text(aes(label = sprintf("%.1f%%", 100*prop)), nudge_y = 0.025) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of search sessions making N search groups")

# Calculate proportion of searches where user reformulated their query
reformulation_counts <- reform_times %>%
  group_by(`test group`) %>%
  summarize(`searches with query reformulations` = sum(as.numeric(unlist(lapply(n_reformulate, function(x) strsplit(x, ",")))) > 0),
            searches = sum(n_search),
            proportion = `searches with query reformulations`/searches) %>%
  ungroup
reformulation_counts <- cbind(
  reformulation_counts,
  as.data.frame(
    binom:::binom.bayes(
      reformulation_counts$`searches with query reformulations`,
      n = reformulation_counts$searches,
      tol = .Machine$double.eps^0.1)[, c("mean", "lower", "upper")]
  )
)
reformulation_counts %>%
  ggplot(aes(x = `test group`, y = `mean`, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits = rev(levels(events$test_group))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer("Test Group", palette = "Set1", guide = FALSE) +
  labs(x = NULL, y = "% of searches with query reformulations",
       title = "Searches with reformulated queries by test group",
       subtitle = "Queries were grouped when they shared common results or common key words") +
  geom_text(aes(label = sprintf("%.2f%%", 100 * proportion),
                vjust = "bottom", hjust = "center"), nudge_x = 0.1)

reformulation_counts <- reform_times %>%
  group_by(wiki, `test group`) %>%
  summarize(`searches with query reformulations` = sum(as.numeric(unlist(lapply(n_reformulate, function(x) strsplit(x, ",")))) > 0),
            searches = sum(n_search),
            proportion = `searches with query reformulations`/searches) %>%
  ungroup
reformulation_counts <- cbind(
  reformulation_counts,
  as.data.frame(
    binom:::binom.bayes(
      reformulation_counts$`searches with query reformulations`,
      n = reformulation_counts$searches)[, c("mean", "lower", "upper")]
  )
)
reformulation_counts %>%
  ggplot(aes(x = 1, y = `mean`, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  labs(x = NULL, y = "% of searches with query reformulations",
       title = "Searches with reformulated queries by test group and wiki",
       subtitle = "Queries were grouped when they shared common results or common key words") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * proportion), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  theme(legend.position = "bottom")+
  facet_wrap(~ wiki, ncol = 3)

# number of query reformulation vs proportion of searches
count_reformulation <- function(n_reformulate){
  temp <- as.numeric(unlist(lapply(n_reformulate, function(x) strsplit(x, ","))))
  temp <- ifelse(temp>=3, "3+", temp)
  output <- as.data.frame(table(temp))
  output$proportion <- output$Freq/sum(output$Freq)
  colnames(output)[1] <- "query reformulations"
  return(output)
}
reform_times %>%
  group_by(`test group`) %>%
  do(count_reformulation(.$n_reformulate)) %>%
  ggplot(aes(x = `query reformulations`, y = proportion, fill = `test group`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * proportion), vjust = "bottom"),
            position = position_dodge(width = 1)) +
  labs(y = "Proportion of searches", x = "Approximate number of query reformulations per grouped search",
       title = "Number of query reformulations by test group",
       subtitle = "Queries were grouped when they shared common results or common key words") +
  theme(legend.position = "bottom")

reform_times %>%
  group_by(wiki, `test group`) %>%
  do(count_reformulation(.$n_reformulate)) %>%
  ggplot(aes(x = `query reformulations`, y = proportion, fill = `test group`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * proportion), vjust = "bottom"),
            position = position_dodge(width = 1)) +
  labs(y = "Proportion of searches", x = "Approximate number of query reformulations per grouped search",
       title = "Number of query reformulations by test group and wiki",
       subtitle = "Queries were grouped when they shared common results or common key words") +
  theme(legend.position = "bottom")+
  facet_wrap(~ wiki, ncol = 3)
