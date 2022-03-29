library(tidyverse)
library(penngradlings)

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
    uncodeable = sub_prop < 0 | other_prop > 0,
    sub_only = sub_prop == 1 & if_all(ends_with("_prop") & !matches("sub_prop"), ~ is.na(.x) | .x == 0),
    basic_gen = sub_prop == 1 & other_prop == 0 & ifelse(is.na(contrast_prop), basic_prop > 0, contrast_prop == 1),
    strict_basic_gen = sub_prop == 1 & other_prop == 0 & basic_prop == 1 & ifelse(is.na(contrast_prop), TRUE, contrast_prop == 1),
    item = fct_drop(item)
  )

write_csv(prop_table, "R scripts/expt_1/03_prop_table.csv")


# many categories
prop_table_5_categories <- prop_table %>% 
  mutate(coding = case_when(
    sub_prop == 1 & if_all(ends_with("_prop") & !matches("sub_prop"), ~ is.na(.x) | .x == 0) ~ "narrow subordinate",
    sub_prop == 1 & other_prop == 0 & ifelse(is.na(contrast_prop), FALSE, contrast_prop == 0 & basic_prop > 0) ~ "broad subordinate",
    sub_prop == 1 & other_prop == 0 & ifelse(is.na(contrast_prop), basic_prop > 0, contrast_prop == 1 & basic_prop > 0) ~ "basic",
    sub_prop == 1 & ifelse(is.na(contrast_prop), basic_prop == 1, contrast_prop == 1 & basic_prop == 1) & sup_prop > 0 ~ "superordinate",
    TRUE ~ "other"
  )) %>% 
  mutate(
    coding = factor(coding, levels = rev(c("basic", "narrow subordinate", "broad subordinate", "superordinate", "other"))),
    condition = c("single" = "No Contrast", "contrast" = "Contrast")[condition],
    condition = factor(condition, levels = c("No Contrast", "Contrast")),
    fill = set_names(fct_inorder(rev(c("#44889CFF", "#E1AF00FF", "#CC4D06FF", "#3343CFFF", "grey"))), levels(coding))[coding]
  )

prop_table_5_categories %>% 
  ggplot(aes(condition, fill = fill)) +
  geom_bar(
    position = position_fill(),
    color = "white", size = .2, width = .7
  ) +
  geom_label(
    aes(label = paste0(after_stat(count/sum(count)) * 2 * 100, "%"),
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

# Stacked plot (HSP)

prop_table1_HSP <- prop_table %>% 
  transmute(
    condition,
    coding = case_when(
      sub_only ~ "Subordinate",
      basic_gen | strict_basic_gen ~ "Basic",
      uncodeable ~ "Other",
      TRUE ~ "Other"
    )
  ) %>% 
  mutate(
    fill = case_when(
      coding == "Other" ~ "grey85",
      TRUE ~ scales::col_factor(as.character(penngradlings::pgl_pals()(2)), coding)(coding)
    ),
    coding = factor(coding, levels = c("Other", "Subordinate", "Basic")),
    fill = penngradlings::fct_derive(fill, coding),
    condition = fct_rev(condition)
  )

prop_plot1_HSP <- prop_table1_HSP %>% 
  ggplot(aes(condition, fill = fill)) +
  geom_bar(
    position = position_fill(),
    color = "white", size = .2, width = .7
  ) +
  ggfx::with_outer_glow(
    geom_errorbar(
      aes(ymin = lower, ymax = upper, fill = NULL),
      color = "#3A6B7A", width = .2,
      data = . %>% 
        transmute(condition, basic = as.integer(coding == "Basic")) %>%
        group_by(condition) %>%
        summarize(confint = binom::binom.confint(sum(basic), n(), methods = "logit"), .groups = 'drop') %>%
        unnest(confint)
    ) ,
    colour = "white", sigma = 0.01, expand = 2
  ) +
  scale_fill_identity(
    labels = unique(prop_table1_HSP$coding),
    guide = guide_legend(title = NULL, nrow = 1, reverse = TRUE)
  ) +
  scale_x_discrete(labels = c("single" = "No contrast", "contrast" = "Contrast")) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = (0:4)/4,
    expand = expansion(c(0, 0.03)),
    labels = scales::label_percent(1)
  ) +
  labs(
    title = "Experiment 1",
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
    plot.tag = element_text(margin = margin(t = 0, b = .2, l = .2, unit = "in"))
  )

# Aggregate ----
prop_table %>%
  filter(!uncodeable) %>%
  group_by(condition) %>% 
  summarize(
    sum = sum(basic_gen),
    mean = sum/n(),
    confint = map2(sum, n(), binom::binom.confint, methods = "logit"),
    .groups = 'drop'
  ) %>%
  hoist(confint, "lower", "upper") %>% 
  ggplot(aes(condition, mean, fill = condition)) +
  geom_col(color = "white", width = .7) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = .2
  ) +
  scale_x_discrete(
    labels = c("Contrast", "Single"),
    expand = expansion(.5)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = (0:4)/4,
    expand = expansion(c(0, 0.05)),
    labels = scales::label_percent(1)
  ) +
  penngradlings::scale_fill_pgl_discrete(what = "blueberry_matcha_boba") +
  guides(fill = guide_none()) +
  labs(
    title = "Expt.1) Basic-level generalizations",
    x = NULL, y = NULL,
    fill = "Condition",
    tag = "Fig.2"
  ) + 
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
  theme(
    plot.title = element_text(size = 12),
    # axis.text.x = element_text(size = 10, family = "Inter-SemiBold", margin = margin(t = .1, unit = "in")),
    axis.text.y = element_text(size = 9),
    # plot.tag = element_text(margin = margin(t = 0, b = .2, unit = "in"))
  )
ggsave_auto(width = 4, height = 3.5)




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
  scale_y_continuous(limits = 0:1, expand = expansion(c(0, 0.05))) +
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
    sub_prop == 1 & contrast_prop == 1 & basic_prop == 1 & sup_prop > 0 & other_prop == 0 ~ "Superordinate",
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

