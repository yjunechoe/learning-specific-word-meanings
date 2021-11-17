library(tidyverse)
library(penngradlings)

results_encoded <- read_rds("R scripts/expt_3/02_results_encoded.rds")

# match_coding <- function(type, basic, contrast, sub, sup, other) {
#   if (type == "3basic" && basic < 3 | type == "3sub" && sub < 2 | other > 0) {
#     "uncodeable"
#   } else {
#     if (type == "3sub") {
#       if (sub == 2 & all(map_lgl(list(basic, contrast, sup, other), ~ .x == 0))) {
#         "only_sub"
#       } else if (sub == 2 & contrast == 2) {
#         "uncodeable"
#       }
#     } else if (type == "3basic") {
#       if (basic == 3 & all(map_lgl(list(sub, contrast, sup), ~ .x == 0))) {
#         "narrow_ME"
#       } else if (basic == 3 & contrast == 2 & all(map_lgl(list(sub, sup), ~ .x == 0))) {
#         "broad_ME"
#       } else if (basic == 3 & sub == 2 & all(map_lgl(list(contrast, sup), ~ .x == 0))) {
#         "narrow_basic"
#       } else if (basic == 3 & sub == 2 & contrast == 2 & sup == 0) {
#         "broad_basic"
#       } else if (basic == 3 & sub == 2 & contrast == 2 & sup > 0) {
#         "sup"
#       } else {
#         "uncodeable"
#       }
#     }
#   }
# }

prop_table <- results_encoded %>% 
  mutate(
    # Exclude if selection does not match all targets (failed retention) or selects another category
    uncodeable = ifelse(type == "3basic", basic_n < 3, sub_prop < 2) | other_prop > 0,
    match = case_when(
      type == "3sub" & sub_prop == 3 ~ "only_sub",
      type == "3basic" & basic_prop == 3 ~ "narrow_basic",
      type == "3basic" & basic_prop == 3 & contrast_prop > 0 ~ "broad_basic",
      type == "3basic" & basic_prop == 3 & contrast_prop > 0 & sub_prop == 0 ~ "ME"
    )
  ) %>% 
  unite(condition, type, order, sep = "_", remove = FALSE) %>% 
  mutate(condition = fct_inorder(condition))

# Basic responses ----

basic_df <- prop_table %>% 
  filter(!uncodeable) %>% 
  group_by(condition, type, order, item) %>% 
  summarize(basic_gen = mean(basic_gen), n = n(), .groups = 'drop')

basic_df %>% 
  ggplot(aes(item, basic_gen, fill = condition)) +
  geom_col(position = position_dodge(), color = "white") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_pgl_discrete() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_grid(type ~ order) +
  labs(
    title = "Proportion of generalizations to the basic-level",
    x = NULL, y = NULL,
    fill = "Condition"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")

basic_tbl <- basic_df %>% 
  pivot_wider(-n, names_from = "item", values_from = "basic_gen")

basic_tbl

