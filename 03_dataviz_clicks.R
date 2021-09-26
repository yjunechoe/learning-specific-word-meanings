library(tidyverse)
library(ggtext)
library(ggh4x)
library(penngradlings)
library(patchwork)

trial_template <- read_csv(here("R Scripts", "01_trial_templates.csv"))
keys <- read_csv(here("R Scripts", "01_keys.csv"))


condition_weight_scale <- trial_template %>% 
  group_split(group) %>% 
  set_names(unique(trial_template$group)) %>% 
  map(~ ifelse(pull(.x, condition) == "single", "Inter-Regular", "Inter-Black"))

results_clicks %>% 
  mutate(item = toupper(item)) %>% 
  ggplot(aes(time, fct_rev(item))) +
  geom_point(
    aes(shape = type, color = selected),
    alpha = 0.7
  ) +
  scale_shape_manual(values = 1:4) +
  scale_color_manual(values = c("#990000", "#011F5b")) +
  facet_wrap(~ group, scales = "free_y") +
  facetted_pos_scales(y = list(
    group == "C" ~ scale_y_discrete(guide = guide_axis_manual(label_family = condition_weight_scale[["C"]])),
    group == "D" ~ scale_y_discrete(guide = guide_axis_manual(label_family = condition_weight_scale[["D"]]))
  )) +
  theme_pgl_minimal()

z <- map(LETTERS[3:4], ~ expr(group == !!.x ~ scale_y_discrete(guide = guide_axis_manual(label_family = condition_weight_scale[[!!.x]]))))


log_scaling <- list(
  scale_x_log10(
    limits = c(100, 10000),
    breaks = scales::breaks_log(5),
    expand = expansion(mult = c(0.02, 0.05)),
    guide = guide_axis_logticks()
  ),
  theme(axis.ticks.length.x = unit(0.25, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.7),
        ggh4x.axis.ticks.length.mini = rel(0.4))
)


z <- results_clicks %>% 
  ggplot(aes(time, fct_rev(item))) +
  geom_point(
    aes(shape = type, color = selected),
    alpha = 0.7
  ) +
  theme(
    axis.text.y = element_markdown(
      fill = c("red", "blue"),
      align_heights = TRUE,
      align_widths = TRUE,
      padding = unit(0.5, "line"),
      hjust = 0.5
    ))

cowplot::get_legend(z)
