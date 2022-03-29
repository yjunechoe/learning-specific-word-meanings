library(tidyverse)
library(penngradlings)
library(ggh4x)

clicks <- read_csv("R scripts/expt_2/02_results_clicks.csv")
prop_table <- read_csv("R scripts/expt_2/03_prop_table.csv")

all_data <- clicks %>% 
  left_join(
    select(prop_table, participant, item, coding),
    by = c("participant", "item")
  ) %>% 
  select(-trial) %>% 
  mutate(
    number = c("one" = "One-One", "three" = "Three-Three")[number],
    target = c("label1" = "First", "label2" = "Second")[target]
  ) %>% 
  filter(coding %in% c("narrow subordinate", "basic")) %>% 
  mutate(coding = factor(ifelse(coding == "basic", "Basic", "Subordinate"), levels = c("Subordinate", "Basic")))

# RT analysis
sub_basic <- all_data %>% 
  filter(coding == "Subordinate")
## until last selection ---- 

RT_z <- sub_basic %>% 
  group_by(participant, item) %>% 
  slice(n()) %>% 
  group_by(participant, coding) %>% 
  summarize(across(time, name_self(list(mean, sd))), n = n(), .groups = "drop") %>% 
  complete(participant, coding)

total_RTs <- sub_basic %>% 
  group_by(participant, item) %>% 
  slice(n()) %>% 
  ungroup()

up_to_sub_RTs <- sub_basic %>% 
  group_by(participant, item) %>% 
  slice(max(which(type == "sub" & selected))) %>% 
  ungroup()

up_to_first_RTs <- sub_basic %>% 
  group_by(participant, item) %>% 
  slice(1) %>% 
  ungroup()

timemark_data <- bind_rows(
  mutate(total_RTs, up_to = "Final selection") %>% 
    left_join(select(up_to_sub_RTs, participant, item, time_subs = time), by = c("participant", "item")) %>% 
    mutate(time = time - time_subs),
  mutate(up_to_sub_RTs, up_to = "Both target subordinates") %>% 
    left_join(select(up_to_first_RTs, participant, item, time_subs = time), by = c("participant", "item")) %>% 
    mutate(time = time - time_subs),
  mutate(up_to_first_RTs, up_to = "First target subordinate")
) %>% 
  mutate(up_to = factor(up_to, c("First target subordinate", "Both target subordinates", "Final selection")))

options(pgl.ggsave_auto.width = 7)
options(pgl.ggsave_auto.height = 5)

waterfall_plot <- function(participant = unique(sub_basic$participant),
                           data = timemark_data) {
  .width <- n_distinct(data$up_to) * .8/3
  data %>% 
    filter(participant %in% .env$participant) %>% 
    ggplot(aes(number, time, group = interaction(number, target, up_to), fill = up_to)) +
    geom_bar(
      width = .width,
      stat = StatSummary, fun.data = mean_se,
      position = ggbg::position_waterfall()
    ) +
    stat_summary(
      width = .2, geom = GeomErrorbar,
      fun.data = mean_se, fun.args = list(mult = 2),
      position = ggbg::position_waterfall(width = .width)
    ) +
    geom_point(
      size = 2, shape = 15,
      stat = StatSummary, fun.data = mean_se,
      show.legend = FALSE,
      position = ggbg::position_waterfall(width = .width, vjust = 1)
    ) +
    geom_text(
      aes(x = Inf, y = 500, label = paste("n =", n)), inherit.aes = FALSE,
      hjust = "inward", family = "Piazzolla-SemiBold", vjust = "inward",
      data = . %>%
        distinct(participant, item, number, target) %>% 
        count(number, target)
    ) +
    labs(
      title = "RT timestamps for selections at test phase",
      subtitle = if (length(participant) == 1) participant,
      x = NULL, y = NULL,
      fill = "Time until"
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(
      values = setNames(pgl_pals("cat_coaster")(3)[seq_len(n_distinct(data$up_to))], unique(data$up_to)),
      drop = TRUE,
    ) +
    scale_x_discrete(labels = NULL) +
    scale_y_continuous(expand = expansion(c(0, 0.1)), breaks = scales::pretty_breaks()) +
    facet_grid2(number ~ target, scales = "free_x", drop = TRUE, independent = "x") +
    theme_pgl_minimal(grid_lines = "y", axis_lines = "x") +
    theme(
      panel.spacing.y = unit(0.3, "in"),
      legend.position = "top"
    )
}

waterfall_plot()



`waterfall_plot(
  data = mutate(total_RTs, up_to = "Last selection") %>% 
    mutate(up_to = factor(up_to, c("First selection", "Target subordinates", "Last selection")))
)
waterfall_plot(
  data = bind_rows(
    mutate(total_RTs, up_to = "Last selection") %>% 
      left_join(select(up_to_sub_RTs, participant, item, time_subs = time), by = c("participant", "item")) %>% 
      mutate(time = time - time_subs),
    mutate(up_to_sub_RTs, up_to = "Target subordinates")) %>% 
    mutate(up_to = factor(up_to, c("First selection", "Target subordinates", "Last selection")))
)
