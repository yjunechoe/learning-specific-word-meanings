library(tidyverse)
library(penngradlings)

results_encoded <- read_rds("R scripts/expt_2/02_results_encoded.rds")

prop_table <- results_encoded %>% 
  mutate(
    target = c("label1" = "first", "label2" = "second")[target],
    basic_prop = basic_n/3,
    contrast_prop = contrast_n/2,
    sub_prop = sub_n/2,
    sup_prop = sup_n/5,
    other_prop = other_n/8
  ) %>% 
  select(participant, group, number, target, item, ends_with("_prop")) %>% 
  mutate(
    uncodeable = sub_prop < 0 | other_prop > 1,
    sub_only = sub_prop == 1 & if_all(ends_with("_prop") & !matches("sub_prop"), ~ is.na(.x) | .x == 0),
    basic_gen = sub_prop == 1 & other_prop == 0 & if_any(c(contrast_prop, basic_prop), ~ !is.na(.x) & .x > 0)
  ) %>% 
  unite(condition, number, target, sep = "-", remove = FALSE) %>% 
  mutate(condition = fct_inorder(condition))

# Subordinate responses ----

sub_only_df <- prop_table %>% 
  filter(!uncodeable) %>% 
  group_by(condition, target, number, item) %>% 
  summarize(sub_only = mean(sub_only), n = n(), .groups = 'drop')

sub_only_df %>% 
  ggplot(aes(item, sub_only, fill = condition)) +
  geom_col(position = position_dodge(), color = "white") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_pgl_discrete() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_grid(target ~ number) +
  labs(
    title = "Proportion of subordinate-only responses",
    x = NULL, y = NULL,
    fill = "Condition"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")

sub_only_tbl <- sub_only_df %>% 
  pivot_wider(-n, names_from = "item", values_from = "sub_only")

sub_only_tbl

# Basic responses ----

basic_df <- prop_table %>% 
  filter(!uncodeable) %>% 
  group_by(condition, target, number, item) %>% 
  summarize(basic_gen = mean(basic_gen), n = n(), .groups = 'drop')

basic_df %>% 
  ggplot(aes(item, basic_gen, fill = condition)) +
  geom_col(position = position_dodge(), color = "white") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_pgl_discrete() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_grid(target ~ number) +
  labs(
    title = "Proportion of generalizations to the basic-level",
    x = NULL, y = NULL,
    fill = "Condition"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")

basic_tbl <- basic_df %>% 
  pivot_wider(-n, names_from = "item", values_from = "basic_gen")

basic_tbl

# Mutually exclusive vs. basic

prop_table %>% 
  mutate(coding = case_when(
    sub_prop == 1 & contrast_prop == 0 & basic_prop == 0 & sup_prop == 0 & other_prop == 0 ~ "Subordinate",
    sub_prop == 1 & contrast_prop == 0 & basic_prop > 0 & sup_prop == 0 & other_prop == 0 ~ "ME",
    sub_prop == 1 & contrast_prop == 1 & basic_prop == 0 & sup_prop == 0 & other_prop == 0 ~ "Basic_narrow",
    sub_prop == 1 & contrast_prop == 1 & basic_prop > 0 & sup_prop == 0 & other_prop == 0 ~ "Basic_broad",
    TRUE ~ NA_character_
  )) %>% 
  mutate(coding = factor(coding, rev(c("Subordinate", "ME", "Basic_narrow", "Basic_broad")))) %>% 
  group_by(condition, number, target) %>% 
  count(coding) %>% 
  ungroup() %>% 
  ggplot(aes(x = coding, n, fill = coding)) +
  geom_col() +
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), color = "white", family = "Inter-Black") +
  facet_grid(number ~ target) +
  labs(
    title = "Proportion of Mutually Exclusive meanings in\n1target-1contrast order",
    y = NULL, x = NULL
  ) +
  discrete_scale("fill", "pgl_continuous", pgl_pals(), na.value = "grey") +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")

