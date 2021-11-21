library(tidyverse)
library(penngradlings)
library(emphatic)

results_encoded <- read_rds("R scripts/expt_1/02_results_encoded.rds")

prop_table <- results_encoded %>% 
  mutate(
    basic_prop = ifelse(condition == "contrast", basic_n/3, basic_n/5),
    contrast_prop = ifelse(condition == "contrast", contrast_n/2, NA),
    sub_prop = sub_n/2,
    sup_prop = sup_n/5,
    other_prop = other_n/8
  ) %>% 
  select(participant, group, condition, item, ends_with("_prop")) %>% 
  mutate(
    uncodeable = sub_prop < 0 | other_prop > 1,
    sub_only = sub_prop == 1 & if_all(ends_with("_prop") & !matches("sub_prop"), ~ is.na(.x) | .x == 0),
    basic_gen = sub_prop == 1 & other_prop == 0 & ifelse(is.na(contrast_prop), basic_prop > 0, contrast_prop == 1),
    strict_basic_gen = sub_prop == 1 & other_prop == 0 & basic_prop == 1 & ifelse(is.na(contrast_prop), TRUE, contrast_prop == 1),
    item = fct_drop(item)
  )

# Subordinate responses ----

sub_only_df <- prop_table %>% 
  filter(!uncodeable) %>% 
  group_by(condition, item) %>% 
  summarize(sub_only = mean(sub_only), n = n(), .groups = 'drop')

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
  pivot_wider(-n, names_from = "item", values_from = "sub_only")

sub_only_tbl

# Basic responses ----

basic_df <- prop_table %>% 
  filter(!uncodeable) %>% 
  group_by(condition, item) %>% 
  summarize(basic_gen = mean(basic_gen), n = n(), .groups = 'drop')

basic_df %>% 
  ggplot(aes(item, basic_gen, fill = condition)) +
  geom_col(position = position_dodge(), color = "white") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_pgl_discrete() +
  labs(
    title = "Proportion of generalizations to the basic-level",
    subtitle = "basic-level = target & contrast inclusive",
    x = NULL, y = NULL,
    fill = "Condition"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")

prop_table %>% 
  filter(!uncodeable) %>% 
  group_by(condition, item) %>% 
  summarize(strict_basic_gen = mean(strict_basic_gen), n = n(), .groups = 'drop') %>% 
  ggplot(aes(item, strict_basic_gen, fill = condition)) +
  geom_col(position = position_dodge(), color = "white") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_pgl_discrete() +
  labs(
    title = "Proportion of generalizations to the basic-level (strict)",
    subtitle = "basic-level = all target & contrast & basic",
    x = NULL, y = NULL,
    fill = "Condition"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")

basic_tbl <- basic_df %>% 
  pivot_wider(-n, names_from = "item", values_from = "basic_gen")

basic_tbl


# Basic responses by group ----

library(ggh4x)
trial_template <- read_csv(here("pcibex", "Experiment 1", "01_trial_templates.csv"))
condition_weight_scale <- trial_template %>% 
  filter(str_detect(domain, "^Filler", negate = TRUE)) %>% 
  group_split(group) %>% 
  set_names(unique(trial_template$group)) %>% 
  map(~ ifelse(pull(.x, condition) == "single", "Inter-Regular", "Inter-Black"))

group_x_scales <- map(LETTERS[1:4], ~ {
  as.formula(expr(
    group == !!.x ~ scale_x_discrete(
      # gets reversed because beginning of weight vector is aligned to bottom of y-axis
      guide = guide_axis_manual(
        label_family = condition_weight_scale[[!!.x]],
        n.dodge = 2
      ),
      drop = FALSE
    )
  ))
})

facet_labels <- prop_table %>%
  distinct(participant, group) %>%
  count(group) %>%
  transmute(group, label = paste0("Group ", group, " (n=", n, ")")) %>% 
  pull(label, group)

prop_table %>% 
  filter(!uncodeable) %>% 
  group_by(group, condition, item) %>%
  summarize(basic_gen = mean(basic_gen), n = n(), .groups = 'drop') %>%
  ggplot(aes(item, basic_gen)) +
  geom_col(
    position = position_dodge(),
    color = "white",
    fill = pgl_pals()(1)
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(
    title = "Proportion of generalizations to the basic-level",
    x = NULL, y = NULL,
    fill = "Condition"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
  facet_wrap(
    ~ group,
    scales = "free_x",
    labeller = labeller(group = facet_labels)
  ) +
  facetted_pos_scales(x = group_x_scales)

# Within contrast condition

within_contrast <- prop_table %>% 
  filter(condition == "contrast") %>% 
  mutate(coding = case_when(
    sub_prop == 1 & contrast_prop == 0 & basic_prop == 0 & sup_prop == 0 & other_prop == 0 ~ "Subordinate",
    sub_prop == 1 & contrast_prop == 0 & basic_prop > 0 & sup_prop == 0 & other_prop == 0 ~ "ME",
    sub_prop == 1 & contrast_prop == 1 & basic_prop == 0 & sup_prop == 0 & other_prop == 0 ~ "Basic_narrow",
    sub_prop == 1 & contrast_prop == 1 & basic_prop > 0 & sup_prop == 0 & other_prop == 0 ~ "Basic_broad",
    TRUE ~ NA_character_
  )) %>% 
  mutate(
    coding = factor(coding, rev(c("Subordinate", "ME", "Basic_narrow", "Basic_broad"))),
    item = fct_drop(item)
  )

within_contrast %>% 
  count(item, coding) %>% 
  ggplot(aes(item, n, fill = coding)) +
  geom_col(position = position_fill()) +
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), color = "white", family = "Inter-Black") +
  labs(
    title = "Proportion of Mutually Exclusive meanings in\n1target-1contrast order",
    y = NULL, x = NULL
  ) +
  discrete_scale("fill", "pgl_continuous", pgl_pals(), na.value = "grey") +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")


within_contrast %>% 
  group_by(group) %>% 
  count(item, coding) %>% 
  ggplot(aes(item, n, fill = coding)) +
  geom_col(position = position_fill()) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), color = "white", family = "Inter-Black") +
  discrete_scale("fill", "pgl_continuous", pgl_pals(), na.value = "grey") +
  labs(y = NULL, x = NULL) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
  facet_wrap(
    ~ group,
    scales = "free_x",
    labeller = labeller(group = facet_labels)
  ) +
  facetted_pos_scales(x = group_x_scales) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )
