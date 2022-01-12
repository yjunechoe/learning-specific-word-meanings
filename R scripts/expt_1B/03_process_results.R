library(tidyverse)
library(penngradlings)
library(ggh4x)

results_encoded <- read_rds("R scripts/expt_1B/02_results_encoded.rds")

prop_table <- results_encoded %>% 
  mutate(
    basic_prop = basic_n/3,
    contrast_prop = contrast_n/2,
    sub_prop = sub_n/2,
    sup_prop = sup_n/5,
    other_prop = other_n/8
  ) %>% 
  mutate(coding = case_when(
    sub_prop == 1 & contrast_prop == 0 & basic_prop == 0 & sup_prop == 0 & other_prop == 0 ~ "Subordinate",
    sub_prop == 1 & contrast_prop == 0 & basic_prop > 0 & sup_prop == 0 & other_prop == 0 ~ "ME",
    sub_prop == 1 & contrast_prop == 1 & basic_prop == 0 & sup_prop == 0 & other_prop == 0 ~ "Basic_narrow",
    sub_prop == 1 & contrast_prop == 1 & basic_prop > 0 & sup_prop == 0 & other_prop == 0 ~ "Basic_broad",
    sub_prop == 1 & contrast_prop == 1 & basic_prop == 1 & sup_prop > 0 & other_prop == 0 ~ "Superordinate",
    TRUE ~ NA_character_
  )) %>% 
  mutate(
    coding = factor(coding, rev(c("Subordinate", "ME", "Basic_narrow", "Basic_broad", "Superordinate"))),
    item = fct_drop(item)
  )

# Smaller set (CogSci)

prop_table_recoded_CogSci <- prop_table %>%
  select(-selections, -clicks) %>% 
  mutate(
    coding = as.character(coding),
    coding = case_when(
      str_detect(coding, "Basic") ~ "Basic",
      coding == "ME" ~ "ME",
      coding == "Superordinate" ~ "Other",
      is.na(coding) ~ "Other",
      TRUE ~ coding
    )
  )

# write_csv(prop_table_recoded_CogSci, "R scripts/expt_1B/03_prop_table_CogSci.csv")

# Stacked plot (HSP)

prop_table_recoded_HSP <- prop_table %>%
  select(-selections, -clicks) %>% 
  mutate(
    coding = as.character(coding),
    coding = case_when(
      str_detect(coding, "Basic") ~ "Basic",
      coding == "ME" ~ "Subordinate",
      coding == "Superordinate" ~ "Other",
      is.na(coding) ~ "Other",
      TRUE ~ coding
    )
  )

# write_csv(prop_table_recoded_HSP, "R scripts/expt_1B/03_prop_table_HSP.csv")

prop_table2_HSP <- prop_table_recoded_HSP %>% 
  mutate(
    fill = case_when(
      coding == "Other" ~ "grey85",
      TRUE ~ scales::col_factor(as.character(pgl_pals()(2)), coding)(coding)
    ),
    coding = factor(coding, levels = c("Other", "Subordinate", "Basic")),
    fill = penngradlings::fct_match(fill, coding)
  )

prop_plot2_HSP <- prop_table_HSP %>% 
  ggplot(aes(labelled, fill = fill)) +
  geom_bar(
    position = position_fill(),
    color = "white", size = .2, width = .7
  ) +
  ggfx::with_outer_glow(
    geom_errorbar(
      aes(ymin = lower, ymax = upper, fill = NULL),
      color = "#3A6B7A", width = .2,
      data = . %>% 
        transmute(target, labelled, basic = as.integer(coding == "Basic")) %>%
        group_by(labelled) %>%
        summarize(confint = binom::binom.confint(sum(basic), n(), methods = "logit"), .groups = 'drop') %>%
        unnest(confint)
    ) ,
    colour = "white", sigma = 0.01, expand = 2
  ) +
  scale_fill_identity(
    labels = unique(prop_table_HSP$coding),
    guide = guide_legend(title = NULL, nrow = 1, reverse = TRUE)
  ) +
  scale_x_discrete(labels = c("TRUE" = "Labelled", "FALSE" = "Unlabelled")) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = (0:4)/4,
    expand = expansion(c(0, 0.03)),
    labels = scales::label_percent(1)
  ) +
  labs(
    title = "Experiment 2",
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

layout <- c(
  area(t = 2, l = 1, b = 5, r = 4),
  area(t = 1, l = 3, b = 3, r = 5)
)

legend <- cowplot::get_legend(prop_plot2_HSP + theme(legend.margin = margin(0, 0, 0, .1, "in")))

wrap_elements(panel = legend) +
  (prop_plot1_HSP + labs(title = "(A) Experiment 1")) +
  (prop_plot2_HSP + labs(title = "(B) Experiment 2")) +
  plot_layout(
    design = c(
      area(t = 1, l = 1, b = 1, r = 2),
      area(t = 2, l = 1, b = 6, r = 4),
      area(t = 2, l = 5, b = 6, r = 8)
    )
  ) &
  theme(legend.position = 0) 
  
# Stacked plot

prop_table_plot_df <- prop_table %>% 
  mutate(
    coding = as.character(coding),
    coding = case_when(
      str_detect(coding, "_") ~ str_to_title(str_replace(coding, "_", " ")),
      coding == "ME" ~ "Subordinate Exclusionary",
      coding == "Subordinate" ~ "Subordinate Narrow",
      is.na(coding) ~ "(uncodeable)",
      TRUE ~ coding
    ),
    coding = factor(coding, levels = rev(c("Basic Broad", "Basic Narrow", "Subordinate Exclusionary", "Subordinate Narrow", "Superordinate", "(uncodeable)")))
  ) %>% 
  mutate(coding2 = fct_recode(coding, "Basic" = "Basic Broad", "Basic" = "Basic Narrow")) %>% 
  mutate(fill = case_when(
    coding2 == "(uncodeable)" ~ "grey85",
    TRUE ~ scales::col_factor(as.character(pgl_pals()(6)), coding2)(coding2)
  )) %>% 
  mutate(
    fill = fct_match(fill, coding2),
    labelled = fct_inorder(ifelse(labelled, "Alternative Labelled", "Alternative Un-labelled"))
  ) 

prop_table_plot_df %>% 
  ggplot(aes(target, fill = fill)) +
  geom_bar(
    position = position_fill(),
    color = "white", size = .2, width = .7
  ) +
  ggfx::with_outer_glow(
    geom_errorbar(
      aes(ymin = lower, ymax = upper, fill = NULL),
      color = "#903402", width = .2,
      data = . %>% 
        transmute(target, labelled, basic = as.integer(coding2 == "Basic")) %>%
        group_by(target, labelled) %>%
        summarize(confint = binom::binom.confint(sum(basic), n(), methods = "logit"), .groups = 'drop') %>%
        unnest(confint)
    ) ,
    colour = "white", sigma = 0.01, expand = 2
  ) +
  scale_fill_identity(
    labels = levels(prop_table_plot_df$coding2),
    guide = guide_legend(title = NULL, nrow = 2, reverse = TRUE)
  ) +
  facet_wrap(~ labelled) +
  scale_x_discrete(labels = c("label1" = "Target First", "label2" = "Target Second")) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = (0:4)/4,
    expand = expansion(c(0, 0.1)),
    labels = scales::label_percent(1)
  ) +
  labs(
    title = "Coded responses to the meaning of the target label",
    x = NULL,
    y = NULL
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, family = "Inter-SemiBold", margin = margin(t = .1, unit = "in")),
    panel.spacing.x = unit(.2, "in"),
    strip.text = element_text(size = 10),
    legend.position = "top",
    legend.text = element_text(size = 9, family = "Inter-SemiBold"),
    plot.tag = element_text(margin = margin(t = 0, b = .2, l = .2, unit = "in"))
  )
ggsave_auto(width = 6, height = 4)


# Aggregate plots

plot_agg <- function(code = "^Basic", title = "basic-level", legend_pos = c(0.25, 0.88)) {
  prop_table %>% 
    filter(!is.na(coding)) %>% 
    group_by(labelled, target) %>% 
    summarize(
      sum = sum(as.integer(str_detect(coding, code))),
      mean = sum/n(),
      confint = map2(sum, n(), ~ binom::binom.confint(.x, .y, methods = "logit")),
      .groups = 'drop'
    ) %>% 
    hoist(confint, "lower", "upper") %>% 
    mutate(target = fct_inorder(c("label1" = "Preceding", "label2" = "Following"))[target]) %>% 
    ggplot(aes(x = fct_rev(factor(labelled)), y = mean, fill = target)) +
    geom_col(
      width = .7, position = position_dodge(width = .75),
    ) +
    geom_errorbar(
      aes(y = mean, ymin = lower, ymax = upper),
      width = .2, position = position_dodge(width = .75)
    ) +
    scale_fill_pgl_discrete() +
    scale_x_discrete(labels = c("TRUE" = "Labelled contrast", "FALSE" = "Unlabelled contrast")) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = (0:4)/4,
      expand = expansion(c(0, 0.1)),
      labels = scales::label_percent(1)
    ) +
    guides(
      fill = guide_legend(
        title = "Target position",
        title.position = "top",
        direction = "horizontal"
      )
    ) +
    labs(
      title = paste("Percentage of", title, "responses"),
      x = NULL,
      y = NULL
    ) +
    theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
    facet_wrap(~labelled, scales = "free_x") +
    theme(
      plot.title = element_text(size = 14),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 11, family = "Inter-SemiBold", margin = margin(t = .1, unit = "in")),
      strip.text = element_blank(),
      legend.position = legend_pos,
      legend.background = element_rect(),
      legend.text = element_text(size = 10, family = "Inter-SemiBold"),
      plot.tag = element_text(margin = margin(t = 0, b = .2, l = .2, unit = "in"))
    )
}

p_basic <- plot_agg() +
  labs(subtitle = "Main effect of contrast label")
p_me <- plot_agg("ME", "mutually-exclusive") +
  labs(subtitle = "Interaction effect between contrast label and target position")
p_sub <- plot_agg("Sub", "subordinate-only") +
  labs(subtitle = "Interaction effect between contrast label and target position")

library(patchwork)

p_basic + p_sub + p_me + plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A", tag_prefix = "Fig.3")
ggsave_auto(width = 7, height = 11)

# Quick models ----

prop_table <- prop_table %>%
  mutate(target = factor(target)) %>% 
  mutate(
    labelled_coded = ifelse(labelled == "TRUE", 1, -1),
    target_coded = ifelse(target == "label2", 1, -1)
  )

p_basic_mod_df <- prop_table %>% 
  filter(!is.na(coding)) %>% 
  select(participant, item, starts_with("labelled"), starts_with("target"), coding) %>% 
  mutate(basic = as.integer(str_detect(coding, "^Basic")))

p_me_mod_df <- prop_table %>% 
  filter(!is.na(coding)) %>% 
  select(participant, item, starts_with("labelled"), starts_with("target"), coding) %>% 
  mutate(me = as.integer(str_detect(coding, "ME")))

p_sub_mod_df <- prop_table %>% 
  filter(!is.na(coding)) %>% 
  select(participant, item, starts_with("labelled"), starts_with("target"), coding) %>% 
  mutate(sub = as.integer(str_detect(coding, "Sub")))

summary(glm(basic ~ labelled * target, p_basic_mod_df, family = binomial()))
summary(glm(me ~ labelled * target, p_me_mod_df, family = binomial()))
summary(glm(sub ~ labelled * target, p_sub_mod_df, family = binomial()))

library(lme4)

summary(glmer(basic ~ labelled_coded * target_coded +
                (1 + labelled_coded || item) + (1 + labelled_coded || participant),
              p_basic_mod_df, family = binomial()))
summary(glmer(me ~ labelled_coded * target_coded +
                (1 + labelled_coded || item) + (1 + labelled_coded || participant),
              p_me_mod_df, family = binomial()))


browserer <- function(...) {
  params <- list(...)
  parent_frame <- parent.frame()
  browser()
}

x <- function(f) {
  a <- 1
  f()
}

# 
# 
# # coding types by 2x2 conditions ----
# 
# prop_table %>%
#   group_by(labelled, target) %>% 
#   count(item, coding) %>% 
#   ggplot(aes(item, n, fill = coding)) +
#   geom_col(position = position_fill()) +
#   geom_text(aes(label = n), position = position_fill(vjust = 0.5), color = "white", family = "Inter-Black") +
#   labs(y = NULL, x = NULL) +
#   discrete_scale("fill", "pgl_continuous", pgl_pals(), na.value = "grey") +
#   theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
#   facet_grid(labelled ~ target)
# 
# # coding types by group ----
# 
# condition_style_scale <- trial_template %>% 
#   filter(str_detect(domain, "^Filler", negate = TRUE)) %>% 
#   group_split(group) %>% 
#   set_names(unique(trial_template$group)) %>% 
#   map(~ case_when(
#     .x$labelled & .x$target == "label1" ~ "Inter-Bold",
#     .x$labelled & .x$target == "label2" ~ "Inter-BoldItalic",
#     !.x$labelled & .x$target == "label1" ~ "Inter-Regular",
#     !.x$labelled & .x$target == "label2" ~ "Inter-Italic"
#   ))
# 
# cond_x_scales <- map(LETTERS[1:4], ~ {
#   as.formula(expr(
#     group == !!.x ~ scale_x_discrete(
#       # gets reversed because beginning of weight vector is aligned to bottom of y-axis
#       guide = guide_axis_manual(
#         label_family = condition_style_scale[[!!.x]],
#         n.dodge = 2
#       ),
#       drop = FALSE
#     )
#   ))
# })
# 
# facet_labels <- prop_table %>%
#   distinct(participant, group) %>%
#   count(group) %>%
#   transmute(group, label = paste0("Group ", group, " (n=", n, ")")) %>% 
#   pull(label, group)
# 
# prop_table %>%
#   group_by(group, labelled, target) %>% 
#   count(item, coding) %>%
#   ungroup() %>% 
#   ggplot(aes(item, n, fill = coding)) +
#   geom_col(position = position_fill()) +
#   geom_text(aes(label = n), position = position_fill(vjust = 0.5), color = "white", family = "Inter-Black") +
#   labs(y = NULL, x = NULL) +
#   discrete_scale("fill", "pgl_continuous", pgl_pals(), na.value = "grey") +
#   facet_wrap(~ group, labeller = labeller(group = facet_labels), scales = "free_x") +
#   ggh4x::facetted_pos_scales(x = cond_x_scales) +
#   theme_pgl_minimal(axis_lines = "x", grid_lines = "y")
# 
# # sub-only plot ----
# 
# sub_only_df <- prop_table %>% 
#   filter(!uncodeable) %>% 
#   group_by(labelled, target, item) %>% 
#   summarize(sub_only = mean(sub_only), n = n(), .groups = 'drop')
# 
# sub_only_df %>% 
#   ggplot(aes(item, sub_only)) +
#   geom_col(position = position_dodge(), color = "white") +
#   scale_y_continuous(expand = expansion(c(0, 0.05))) +
#   scale_fill_pgl_discrete() +
#   labs(
#     title = "Proportion of subordinate-only responses",
#     x = NULL, y = NULL,
#     fill = "Condition"
#   ) +
#   theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
#   facet_grid(labelled ~ target)
# 
# sub_only_tbl <- sub_only_df %>% 
#   pivot_wider(-n, names_from = "item", values_from = "sub_only")
# 
# sub_only_tbl
# 
# # Basic responses plot ----
# 
# basic_df <- prop_table %>% 
#   filter(!uncodeable) %>% 
#   group_by(labelled, target, item) %>% 
#   summarize(basic_gen = mean(basic_gen), n = n(), .groups = 'drop')
# 
# basic_df %>% 
#   ggplot(aes(item, basic_gen)) +
#   geom_col(position = position_dodge(), color = "white") +
#   scale_y_continuous(expand = expansion(c(0, 0.05))) +
#   scale_fill_pgl_discrete() +
#   labs(
#     title = "Proportion of generalizations to the basic-level",
#     subtitle = "basic-level = target & contrast inclusive",
#     x = NULL, y = NULL,
#     fill = "Condition"
#   ) +
#   theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
#   facet_grid(labelled ~ target)
# 
# 
