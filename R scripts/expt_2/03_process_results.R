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
  mutate(coding = case_when(
    sub_prop == 1 & if_all(ends_with("_prop") & !matches("sub_prop"), ~ is.na(.x) | .x == 0) ~ "narrow subordinate",
    sub_prop == 1 & other_prop == 0 & ifelse(is.na(contrast_prop), FALSE, contrast_prop == 0 & basic_prop > 0) ~ "broad subordinate",
    sub_prop == 1 & other_prop == 0 & ifelse(is.na(contrast_prop), basic_prop > 0, contrast_prop == 1 & basic_prop > 0) ~ "basic",
    sub_prop == 1 & ifelse(is.na(contrast_prop), basic_prop == 1, contrast_prop == 1 & basic_prop == 1) & sup_prop > 0 ~ "superordinate",
    TRUE ~ "Uncodeable"
  )) %>% 
  mutate(
    coding = factor(coding, levels = rev(c("basic", "narrow subordinate", "broad subordinate", "superordinate", "Uncodeable"))),
    fill = set_names(fct_inorder(rev(c("#44889CFF", "#E1AF00FF", "#CC4D06FF", "#3343CFFF", "grey"))), levels(coding))[coding],
  ) %>% 
  unite(condition, number, target, sep = "-", remove = FALSE) %>% 
  mutate(condition = fct_inorder(condition))

write_csv(prop_table, "R scripts/expt_2/03_prop_table.csv")

# ----

prop_table %>% 
  mutate(number = ifelse(number == "one", "One-One", "Three-Three")) %>% 
  ggplot(aes(number, fill = fill)) +
  geom_bar(
    position = position_fill(),
    color = "white", size = .2, width = .7
  ) +
  geom_label(
    aes(label = paste0(round(after_stat(count/sum(count)) * 2 * 100, 1), "%"),
        color = colorspace::darken(fill, .5), group = fill),
    show.legend = FALSE, family = "Inter-Bold",
    stat = StatCount, fill = "white", label.size = 0.5,
    position = position_fill(vjust = 0.5)
  ) +
  scale_color_identity() +
  scale_fill_identity(
    labels = rev(c("Basic", "Narrow Subordinate", "Broad Subordinate", "Superordinate", "Uncodeable")),
    guide = guide_legend(title = NULL, nrow = 1, reverse = TRUE), drop = FALSE
  ) +
  scale_x_discrete(labels = c("single" = "No contrast", "contrast" = "Contrast")) +
  scale_y_continuous(
    limits = c(-.03, 1),
    breaks = (0:4)/4,
    expand = expansion(c(0, 0)),
    labels = scales::label_percent(1)
  ) +
  labs(
    title = "Distribution of responses at test",
    x = NULL,
    y = NULL
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, family = "Inter-SemiBold", margin = margin(t = .1, unit = "in")),
    legend.text = element_text(size = 9, family = "Inter-SemiBold"),
    plot.tag = element_text(margin = margin(t = 0, b = .2, l = .2, unit = "in")),
    legend.position = "top"
  )

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

