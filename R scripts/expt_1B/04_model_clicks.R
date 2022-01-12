library(tidyverse)
library(penngradlings)
library(ggh4x)

clicks <- read_csv("R scripts/expt_1B/02_results_clicks.csv")
prop_table <- read_csv("R scripts/expt_1B/03_prop_table_CogSci.csv") # HSP/CogSci

all_data <- clicks %>% 
  left_join(
    select(prop_table, participant, item, coding),
    by = c("participant", "item")
  ) %>% 
  select(-trial)

# RT analysis

## until last selection ---- 

total_RT <- all_data %>% 
  group_by(participant, item) %>% 
  slice(n()) %>% 
  group_by(participant) %>%
  mutate(
    time_log = log(time),
    time_z = scale(time_log)[,1]
  ) %>%
  ungroup() %>% 
  select(participant, item, starts_with("time"), coding, group, labelled, target)

total_RT_plot_df <- total_RT %>% 
  mutate(
    target = c("label1" = "First", "label2" = "Second")[target],
    labelled = ifelse(labelled, "Labelled", "Unlabelled"),
    labelled = fct_rev(labelled)
  )

total_RT_plot_df %>% 
  ggplot(aes(interaction(target, labelled), time_z)) +
  geom_hline(aes(yintercept = 0), color = "grey", linetype = 2) +
  geom_errorbar(
    width = .2,
    stat = StatSummary, fun.data = mean_se
  ) +
  geom_point(
    aes(shape = labelled, fill = labelled),
    size = 2,
    stat = StatSummary, fun.data = mean_se,
    show.legend = FALSE
  ) +
  geom_label(
    aes(label = n, y = mean),
    stat = "unique",
    position = position_nudge(x = .1),
    hjust = 0, label.size = 0,
    data = . %>%
      group_by(coding, interaction(target, labelled)) %>%
      mutate(n = n(), mean = mean(time_z))
  ) +
  labs(
    title = "Response times (total)",
    x = NULL,
    y = "z-scored log RT"
  ) +
  guides(x = guide_axis_nested()) +
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 24)) +
  facet_wrap(~coding) +
  theme_pgl_minimal(grid_lines = "y") +
  theme(
    axis.text.x = element_text(margin = margin(b = 1, unit = "mm")),
    ggh4x.axis.nestline.x = element_line(size = 0.6),
    ggh4x.axis.nesttext.x = element_text(size = 9, family = "Inter-Bold", margin = margin(2, unit = "mm"))
  )

## ---- up to selection of the two subs

up_to_subs_RT <- all_data %>%
  filter(coding != "Other") %>% 
  group_by(participant, item) %>% 
  slice(max(which(type == "sub"))) %>% 
  group_by(participant) %>%
  mutate(
    time_log = log(time),
    time_z = scale(time_log)[,1]
  ) %>%
  ungroup() %>% 
  select(participant, item, starts_with("time"), coding, group, labelled, target)

up_to_subs_RT %>% 
  ggplot(aes(interaction(target, labelled), time_z)) +
  geom_hline(aes(yintercept = 0), color = "grey", linetype = 2) +
  geom_errorbar(
    width = .2,
    stat = StatSummary, fun.data = mean_se
  ) +
  geom_point(
    aes(shape = labelled, fill = labelled),
    size = 2,
    stat = StatSummary, fun.data = mean_se,
    show.legend = FALSE
  ) +
  geom_label(
    aes(label = n, y = mean),
    stat = "unique",
    position = position_nudge(x = .1),
    hjust = 0, label.size = 0,
    data = . %>%
      group_by(coding, interaction(target, labelled)) %>%
      mutate(n = n(), mean = mean(time_z))
  ) +
  labs(
    title = "Response times (up to subordinate choices)",
    x = NULL,
    y = "z-scored log RT"
  ) +
  guides(x = guide_axis_nested()) +
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 24)) +
  facet_wrap(~coding) +
  theme_pgl_minimal(grid_lines = "y") +
  theme(
    axis.text.x = element_text(margin = margin(b = 1, unit = "mm")),
    ggh4x.axis.nestline.x = element_line(size = 0.6),
    ggh4x.axis.nesttext.x = element_text(size = 9, family = "Inter-Bold", margin = margin(2, unit = "mm"))
  )

# Certainty analysis

results_encoded <- read_rds("R scripts/expt_1B/02_results_encoded.rds")

setxor <- function(x, y) {
  if (y %in% x) {
    x[-which(x == y)]
  } else {
    c(x, y)
  }
}

reanalysis_df <- all_data %>% 
  filter(coding != "Other") %>% 
  group_by(participant, item) %>% 
  mutate(
    cum_selection = accumulate(img, setxor),
    first_hit = map_lgl(cum_selection, setequal, cum_selection[[n()]])
  ) %>% 
  ungroup()

# People who stumbled on the final answer, reanalyzed, then did it again

reanalysis_df %>% 
  group_by(participant, item) %>%
  filter(sum(first_hit) > 1) # this heuristic asumes subordinate is narrow subordinate

# People who answered subordinate but overshot first

reanalysis_df %>% 
  filter(coding == "Subordinate") %>% 
  count(participant, item) %>% 
  count(n)

reanalysis_df %>% 
  filter(coding == "Subordinate") %>%
  group_by()

