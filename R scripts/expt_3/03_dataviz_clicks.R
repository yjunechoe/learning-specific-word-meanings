library(tidyverse)
library(here)
library(ggtext)
library(ggh4x)
library(penngradlings)
library(patchwork)

## Read ----

trial_template <- read_csv(here("pcibex", "Experiment 3", "01_trial_templates.csv"))
keys <- read_csv(here("R Scripts", "keys.csv"))
results_encoded <- read_rds(here("R scripts", "expt_3", "02_results_encoded.rds"))
results_clicks <- read_csv(here("R scripts", "expt_3", "02_results_clicks.csv"))

## Helpers ----

condition_weight_scale <- trial_template %>% 
  unite(condition, order, type, sep = "_") %>% 
  filter(str_detect(domain, "^Filler", negate = TRUE)) %>% 
  group_split(group) %>% 
  set_names(unique(trial_template$group)) %>% 
  map(~ {
    c(
      "1-3_3sub" = "Inter-Regular",
      "1-3_3basic" = "Inter-Black",
      "3-1_3sub" = "Inter-Italic",
      "3-1_3basic" = "Inter-BlackItalic"
    )[pull(.x, condition)]
  })

condition_y_scales <- map(LETTERS[1:4], ~ {
  as.formula(expr(
    group == !!.x ~ scale_y_discrete(
      # gets reversed because beginning of weight vector is aligned to bottom of y-axis
      guide = guide_axis_manual(label_family = rev(condition_weight_scale[[!!.x]]))
    )
  ))
})

clicks_plot_df <- results_clicks %>% 
  mutate(
    category = factor(category, levels = c("sub", "contrast", "basic", "sup", "other")),
    time = time / 1000,
    item = fct_rev(as.factor(item))
  )


# Plot ====

plot_individual <- function(x) {
  if (is.na(x)) { return(patchwork::guide_area()) }
  df <- clicks_plot_df %>% 
    filter(participant == x)
  df %>% 
    ggplot(aes(time, item)) +
    geom_point(
      aes(shape = category, color = selected),
      size = 2
    ) +
    scale_shape_manual(values = c(16, 2, 15, 3, 4), drop = FALSE) +
    scale_color_manual(values = c("TRUE" = "#011F5b", "FALSE" = "#990000"), drop = FALSE) +
    scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(c(0, 0.05))
    ) +
    scale_y_discrete(
      guide = guide_axis_manual(label_family = rev(condition_weight_scale[[unique(df$group)]]))
    ) +
    labs(
      title = paste("Group ", unique(df$group)),
      subtitle = unique(df$participant),
      y = NULL
    ) +
    theme_pgl_minimal(
      axis_lines = "x",
      grid_lines = "y"
    ) +
    theme(
      axis.line.x.top = element_blank(),
      axis.text.x.top = element_blank(),
      axis.ticks.x.bottom = element_line(),
      axis.ticks.length.x.bottom = unit(0.05, "in"),
      ggh4x.axis.ticks.length.minor = rel(0.7),
      ggh4x.axis.ticks.length.mini = rel(0.3)
    )
}

plot_individual(clicks_plot_df$participant[1])

fill_grid <- clicks_plot_df %>%
  distinct(group, participant) %>%
  group_by(group) %>% 
  mutate(idx = row_number(), .before = 1) %>%
  ungroup() %>%
  complete(idx, group) %>%
  arrange(group, idx)
wrap_plots(lapply(fill_grid$participant, plot_individual), ncol = 4, byrow = FALSE) +
  plot_layout(guides = 'collect')

ggsave_auto(width = 10, height = 9, scale = 1.5)


# Experimental

clicks_plot_df %>% 
  filter(participant == participant[1]) %>% 
  ggplot(aes(time, item)) +
  geom_point(
    color = "forestgreen", shape = "|", size = 5,
    data = . %>% 
      filter(type == "sub", selected)
  ) +
  geom_point(
    aes(shape = type, color = selected, alpha = type),
    size = 2
  ) +
  scale_shape_manual(values = c(16, 2, 0, 3, 4)) +
  scale_alpha_manual(values = c(1, rep(0.7 ,4))) +
  scale_color_manual(values = c("#990000", "#011F5b")) +
  scale_y_continuous()

clicks_plot_df %>% 
  arrange(desc(type)) %>% 
  ggplot(aes(time, fct_rev(item))) +
  geom_point(
    color = "forestgreen",
    shape = "|", size = 5,
    data = . %>% 
      filter(type == "sub", selected) %>%
      group_by(participant, group, item, img) %>%
      slice_head(n = 1)
  ) +
  geom_point(
    aes(shape = type, color = selected, alpha = type),
    size = 2
  ) +
  scale_shape_manual(values = c(16, 2, 0, 3, 4)) +
  scale_alpha_manual(values = c(1, rep(0.7 ,4))) +
  scale_color_manual(values = c("#990000", "#011F5b")) +
  facet_wrap(
    ~ group,
    scales = "free_y",
    labeller = label_both
  ) +
  facetted_pos_scales(
    x = scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05)),
      sec.axis = sec_axis("identity")
    ),
    # x = scale_x_log10(
    #   limits = c(100, NA),
    #   breaks = c(1e2, 1e3, 1e4),
    #   minor_breaks = scales::breaks_log(12),
    #   guide = guide_axis_logticks()
    # ),
    y = condition_y_scales
  ) +
  labs(y = NULL, x = "Time (seconds)") +
  theme_pgl_minimal(
    axis_lines = "x",
    grid_lines = "y"
  ) +
  theme(
    axis.line.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    axis.ticks.x.bottom = element_line(),
    axis.ticks.length.x.bottom = unit(0.05, "in"),
    ggh4x.axis.ticks.length.minor = rel(0.7),
    ggh4x.axis.ticks.length.mini = rel(0.3)
  )
# ggsave_auto(height = 6, width = 8)

## ggtext solution with patchwork
# results_clicks %>% 
#   ggplot(aes(time, fct_rev(item))) +
#   geom_point(
#     aes(shape = type, color = selected),
#     alpha = 0.7
#   ) +
#   theme(
#     axis.text.y = element_markdown(
#       fill = c("red", "blue"),
#       align_heights = TRUE,
#       align_widths = TRUE,
#       padding = unit(0.5, "line"),
#       hjust = 0.5
#     ))
