library(tidyverse)
library(here)
library(ggtext)
library(ggh4x)
library(penngradlings)
library(patchwork)

## Read ----

trial_template <- read_csv(here("R Scripts", "01_trial_templates.csv"))
keys <- read_csv(here("R Scripts", "01_keys.csv"))
results_encoded <- read_rds(here("R scripts", "02_results_encoded.rds"))
results_clicks <- read_csv(here("R scripts", "02_results_clicks.csv"))

## Helpers ----

condition_weight_scale <- trial_template %>% 
  group_split(group) %>% 
  set_names(unique(trial_template$group)) %>% 
  map(~ ifelse(pull(.x, condition) == "single", "Inter-Regular", "Inter-Black"))

condition_y_scales <- map(LETTERS[1:4], ~ {
  as.formula(expr(
    group == !!.x ~ scale_y_discrete(
      guide = guide_axis_manual(label_family = condition_weight_scale[[!!.x]])
    )
  ))
})

clicks_plot_df <- results_clicks %>% 
  mutate(
    item = toupper(item),
    type = factor(type, levels = c("sub", "contrast", "basic", "sup", "other")),
    time = time / 1000
  )


# Plot ====

clicks_plot_df %>% 
  ggplot(aes(time, fct_rev(item))) +
  geom_linerange(
    aes(x = time, ymin = stage(item, after_stat = y - 0.3), ymax = stage(item, after_stat = y + 0.3)),
    alpha = 0.8, color = "forestgreen",
    data = . %>% 
      filter(type == "sub", selected) %>%
      group_by(participant, item, img) %>%
      slice_head(n = 1)
  ) +
  geom_point(
    aes(shape = type, color = selected),
    size = 2, alpha = 0.7
  ) +
  scale_shape_manual(values = c(16, 2, 4, 3, 8)) +
  scale_color_manual(values = c("#990000", "#011F5b")) +
  facet_wrap(~ group, scales = "free_y") +
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
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y") +
  theme(
    axis.line.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    axis.ticks.x.bottom = element_line(),
    axis.ticks.length.x.bottom = unit(0.05, "in"),
    ggh4x.axis.ticks.length.minor = rel(0.7),
    ggh4x.axis.ticks.length.mini = rel(0.3)
  )


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
