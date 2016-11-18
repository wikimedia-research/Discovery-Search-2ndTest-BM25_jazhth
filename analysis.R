library(tidyverse)
library(binom)
source("utils.R")

# Zero Results Rate
zrr_pages <- searches %>%
  group_by(`test group`, results) %>%
  tally %>%
  spread(results, n) %>%
  mutate(`zero results rate` = zero/(some + zero)) %>%
  ungroup
zrr_pages <- cbind(zrr_pages, as.data.frame(binom:::binom.bayes(zrr_pages$zero, n = zrr_pages$some + zrr_pages$zero)[, c("mean", "lower", "upper")]))
zrr_pages %>%
  ggplot(aes(x = `test group`, y = `mean`, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits = rev(levels(events$test_group))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer("Test Group", palette = "Set1", guide = FALSE) +
  labs(x = NULL, y = "Zero Results Rate",
       title = "Proportion of searches that did not yield any results, by test group",
       subtitle = "With 95% credible intervals.") +
  geom_text(aes(label = sprintf("%.2f%%", 100 * `zero results rate`),
                vjust = "bottom", hjust = "center"), nudge_x = 0.1) #+
  #theme_minimal(base_family = "Lato")

zrr_pages <- searches %>%
  group_by(wiki, `test group`, results) %>%
  tally %>%
  spread(results, n) %>%
  mutate(`zero results rate` = zero/(some + zero)) %>%
  ungroup
zrr_pages <- cbind(zrr_pages, as.data.frame(binom:::binom.bayes(zrr_pages$zero, n = zrr_pages$some + zrr_pages$zero)[, c("mean", "lower", "upper")]))
zrr_pages %>%
  ggplot(aes(x = `test group`, y = `mean`, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits = rev(levels(events$test_group))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer("Test Group", palette = "Set1", guide = FALSE) +
  labs(x = NULL, y = "Zero Results Rate",
       title = "Proportion of searches that did not yield any results, by test group and wiki",
       subtitle = "With 95% credible intervals.") +
  geom_text(aes(label = sprintf("%.2f%%", 100 * `zero results rate`),
                vjust = "bottom", hjust = "center"), nudge_x = 0.1) +
  facet_wrap(~ wiki, ncol = 3) #+
#theme_minimal(base_family = "Lato")

# PaulScore
set.seed(777)
paulscores <- searches %>%
  ungroup %>%
  filter(clickthrough==TRUE) %>% 
  select(c(`test group`, `Query score (F=0.1)`, `Query score (F=0.5)`, `Query score (F=0.9)`)) %>%
  gather(`F value`, `Query score`, -`test group`) %>%
  mutate(`F value` = sub("^Query score \\(F=(0\\.[159])\\)$", "F = \\1", `F value`)) %>%
  group_by(`test group`, `F value`) %>%
  summarize(
    PaulScore = mean(`Query score`),
    Interval = paste0(quantile(bootstrap_mean(`Query score`, 1000), c(0.025, 0.975)), collapse = ",")
  ) %>%
  extract(Interval, into = c("Lower", "Upper"), regex = "(.*),(.*)", convert = TRUE)
paulscores %>%
  ggplot(aes(x = `F value`, y = PaulScore, color = `test group`)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.7)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  #scale_y_continuous(limits = c(0.2, 0.35)) +
  labs(x = NULL, y = "PaulScore(F)",
       title = "PaulScore(F) by test group and value of F",
       subtitle = "With bootstrapped 95% confidence intervals.") +
  geom_text(aes(label = sprintf("%.3f", PaulScore), y = Upper + 0.01, vjust = "bottom"),
            position = position_dodge(width = 0.7)) +
  #theme_minimal(base_family = "Lato") +
  theme(legend.position = "bottom") 

set.seed(777)
paulscores <- searches %>%
  ungroup %>%
  filter(clickthrough==TRUE) %>% 
  select(c(wiki, `test group`, `Query score (F=0.1)`, `Query score (F=0.5)`, `Query score (F=0.9)`)) %>%
  gather(`F value`, `Query score`, -c(`test group`, wiki)) %>%
  mutate(`F value` = sub("^Query score \\(F=(0\\.[159])\\)$", "F = \\1", `F value`)) %>%
  group_by(wiki, `test group`, `F value`) %>%
  summarize(
    PaulScore = mean(`Query score`),
    Interval = paste0(quantile(bootstrap_mean(`Query score`, 1000), c(0.025, 0.975)), collapse = ",")
  ) %>%
  extract(Interval, into = c("Lower", "Upper"), regex = "(.*),(.*)", convert = TRUE)
paulscores %>%
  ggplot(aes(x = `F value`, y = PaulScore, color = `test group`)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.7)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  #scale_y_continuous(limits = c(0.2, 0.35)) +
  labs(x = NULL, y = "PaulScore(F)",
       title = "PaulScore(F) by wiki, test group and value of F",
       subtitle = "With bootstrapped 95% confidence intervals.") +
  geom_text(aes(label = sprintf("%.3f", PaulScore), y = Upper + 0.01, vjust = "bottom"),
            position = position_dodge(width = 0.7)) +
  facet_wrap(~ wiki, ncol = 3) +
  theme(legend.position = "bottom") 

# Engagement
engagement_overall <- searches %>%
  filter(results=="some") %>% 
  group_by(`test group`) %>%
  summarize(clickthroughs = sum(clickthrough > 0),
            searches = n(), ctr = clickthroughs/searches) %>%
  ungroup
engagement_overall <- cbind(
  engagement_overall,
  as.data.frame(
    binom:::binom.bayes(
      engagement_overall$clickthroughs,
      n = engagement_overall$searches)[, c("mean", "lower", "upper")]
  )
)
engagement_overall %>%
  ggplot(aes(x = 1, y = mean, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Clickthrough rate",
       title = "Engagement with search results by test group") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * ctr), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  #theme_minimal(base_family = "Lato") +
  theme(legend.position = "bottom")

engagement_overall <- searches %>%
  filter(results=="some") %>% 
  group_by(wiki, `test group`) %>%
  summarize(clickthroughs = sum(clickthrough > 0),
            searches = n(), ctr = clickthroughs/searches) %>%
  ungroup
engagement_overall <- cbind(
  engagement_overall,
  as.data.frame(
    binom:::binom.bayes(
      engagement_overall$clickthroughs,
      n = engagement_overall$searches)[, c("mean", "lower", "upper")]
  )
)
engagement_overall %>%
  ggplot(aes(x = 1, y = mean, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Clickthrough rate",
       title = "Engagement with search results by test group and wiki") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * ctr), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  facet_wrap(~ wiki, ncol = 3) +
  theme(legend.position = "bottom")


# First Clicked Result’s Position
safe_ordinals <- function(x) {
  return(vapply(x, toOrdinal::toOrdinal, ""))
}
first_clicked <- searches %>%
  filter(results == "some" & clickthrough & !is.na(`first clicked result's position`)) %>%
  mutate(`first clicked result's position` = ifelse(`first clicked result's position` < 4, safe_ordinals(`first clicked result's position` + 1), "5th or higher")) %>%
  group_by(`test group`, `first clicked result's position`) %>%
  tally %>%
  mutate(total = sum(n), prop = n/total) %>%
  ungroup
set.seed(0)
temp <- as.data.frame(binom:::binom.bayes(first_clicked$n, n = first_clicked$total, tol = .Machine$double.eps^0.1)[, c("mean", "lower", "upper")])
first_clicked <- cbind(first_clicked, temp); rm(temp)
first_clicked %>%
  ggplot(aes(x = 1, y = mean, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  geom_text(aes(label = sprintf("%.1f", 100 * prop), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0.005), breaks = seq(0, 1, 0.01)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  facet_wrap(~ `first clicked result's position`, scale = "free_y", nrow = 1) +
  labs(x = NULL, y = "Proportion of searches",
       title = "Position of the first clicked result",
       subtitle = "With 95% credible intervals") +
  #theme_minimal(base_family = "Lato") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_rect(fill = "gray90"),
        panel.border = element_rect(color = "gray30", fill = NA))

first_clicked <- searches %>%
  filter(results == "some" & clickthrough & !is.na(`first clicked result's position`)) %>%
  mutate(`first clicked result's position` = ifelse(`first clicked result's position` < 4, safe_ordinals(`first clicked result's position` + 1), "5th or higher")) %>%
  group_by(wiki, `test group`, `first clicked result's position`) %>%
  tally %>%
  mutate(total = sum(n), prop = n/total) %>%
  ungroup
set.seed(0)
temp <- as.data.frame(binom:::binom.bayes(first_clicked$n, n = first_clicked$total, tol = .Machine$double.eps^0.1)[, c("mean", "lower", "upper")])
first_clicked <- cbind(first_clicked, temp); rm(temp)
first_clicked %>%
  ggplot(aes(x = 1, y = mean, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  geom_text(aes(label = sprintf("%.1f", 100 * prop), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0.005), breaks = seq(0, 1, 0.01)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  facet_wrap(~ wiki+`first clicked result's position`, scale = "free_y", ncol=5, nrow = 3) +
  labs(x = NULL, y = "Proportion of searches",
       title = "Position of the first clicked result",
       subtitle = "With 95% credible intervals") +
  #theme_minimal(base_family = "Lato") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_rect(fill = "gray90"),
        panel.border = element_rect(color = "gray30", fill = NA))
