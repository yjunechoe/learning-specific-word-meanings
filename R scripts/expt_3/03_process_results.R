library(tidyverse)
library(penngradlings)

results_encoded <- read_rds("R scripts/expt_2/02_results_encoded.rds")

prop_table <- results_encoded %>% 
  mutate(
    basic_prop = basic_n/3,
    contrast_prop = contrast_n/2,
    sub_prop = sub_n/2,
    sup_prop = sup_n/5,
    other_prop = other_n/8
  ) %>% 
  select(participant, group, number, target, item, ends_with("_prop")) %>% 
  mutate(sub_only = if_all(ends_with("_prop") & !matches("sub_prop"), ~ is.na(.x) | .x == 0))

sub_only_df <- prop_table %>% 
  # filter(sub_prop != 0) %>% # "incorrect" responses
  group_by(number, target, item) %>% 
  summarize(sub_only = mean(sub_only), .groups = 'drop') %>% 
  unite(condition, number, target, sep = "-") %>% 
  mutate(condition = factor(condition))

sub_only_df %>% 
  ggplot(aes(item, sub_only, fill = condition)) +
  geom_col(position = position_dodge(), color = "white") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_pgl_discrete() +
  labs(
    title = "Proportion of subordinate-only responses",
    x = NULL, y = NULL,
    fill = "Condition"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")

sub_only_tbl <- sub_only_df %>% 
  pivot_wider(names_from = "item", values_from = "sub_only")


## Aggregate

prop_table %>% 
  filter(sub_prop != 0) %>% 
  unite(condition, number, target, sep = "-") %>% 
  group_by(condition) %>% 
  summarize(across(contains("_prop"), mean))

# 
# binary_table %>% 
#   {print(.); cat("\n"); .} %>% 
#   reduce(
#     names(binary_table)[-1],
#     ~ {
#       row_loc <- which(binary_table[[.y]] == max(binary_table[[.y]]))
#       eval(rlang::expr(hl(.x, "forestgreen", rows = !!row_loc, cols = !!.y)))
#     },
#     .init = .
#   )
# 
# binary_table %>%
#   select(-1) %>%
#   as.matrix() %>%
#   `rownames<-`(c("contrast", "single")) %>% 
#   hl_mat(colorspace::scale_color_continuous_diverging("Green-Orange", mid = 0.5, rev = TRUE))
