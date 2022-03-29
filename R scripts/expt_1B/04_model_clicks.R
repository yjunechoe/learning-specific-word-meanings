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
  select(-trial) %>% 
  mutate(
    target = c("label1" = "First", "label2" = "Second")[target],
    labelled = ifelse(labelled, "Labelled", "Unlabelled"),
    labelled = fct_rev(labelled)
  )

# RT analysis
sub_basic <- all_data %>% 
  filter(coding %in% c("Subordinate", "Basic")) %>% 
  mutate(coding = factor(coding, c("Subordinate", "Basic"))) %>% 
  filter(!(participant == "d8bff87638510d9c6f20de43107cd8a0" & coding == "Basic"))  # time > 80s -- 1 trial
  
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
    ggplot(aes(labelled, time, group = interaction(labelled, up_to), fill = up_to)) +
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
          distinct(participant, item, labelled, coding) %>% 
          count(labelled, coding)
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
    facet_grid2(labelled ~ coding, scales = "free_x", drop = TRUE, independent = "x") +
    theme_pgl_minimal(grid_lines = "y", axis_lines = "x") +
    theme(
      panel.spacing.y = unit(0.3, "in"),
      legend.position = "top"
    )
}

waterfall_plot(
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
waterfall_plot()

waterfall_plot() +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = up_to, color = after_scale(fill)),
    inherit.aes = FALSE, alpha = .5,
    data = timemark_data %>%
      filter(labelled == "Labelled", coding == "Basic") %>% 
      group_by(up_to) %>% 
      summarize(mean = mean(time), se = sqrt(var(time)/n())) %>%
      ungroup() %>% 
      mutate(mean = cumsum(mean), ymin = mean - se * 2, ymax = mean + se * 2) %>% 
      mutate(xmin = -Inf, xmax = Inf) %>% 
      crossing(labelled = fct_inorder(c("Unlabelled", "Labelled")),
               coding = fct_inorder(c("Subordinate", "Basic"))) %>% 
      filter(!(labelled == "Labelled" & coding == "Basic")) %>% 
      filter(up_to != "Last selection")
  )

waterfall_plot("46d02a2f57441c7c97aaf2f6fa16550c")
waterfall_plot("360168676e9d90ee6e7a67175901d9aa")

balanced_participants <- RT_z %>% 
  drop_na(n) %>% 
  group_by(participant) %>% 
  filter(n() == 2) %>% # 86 -> 42
  filter(all(n > 1)) %>% # 42 -> 28
  pull(participant) %>% 
  unique()

ggsave(
  "9-samples.png", patchwork::wrap_plots(map(sample(balanced_participants, 9), waterfall_plot), ncol = 3),
  width = 21, height = 15
)
  
# until sub selection


# timemark modelling ----
timemark_data %>%
  distinct(participant, item, labelled, coding) %>% 
  count(labelled, coding)

timemark_data_mdl_df <- timemark_data %>% 
  mutate(condition = paste0(labelled, "-", coding)) %>% 
  filter(condition != "Labelled-Basic") %>% 
  mutate(
    condition = factor(condition, c("Labelled-Subordinate", "Unlabelled-Subordinate", "Unlabelled-Basic")),
    until = up_to
  )

cond_contrasts <- psycholing::contr.helmert.weighted(timemark_data_mdl_df$condition)
colnames(cond_contrasts) <- c("Within Subordinates", "Subordinates vs. Basic")
contrasts(timemark_data_mdl_df$condition) <- cond_contrasts
contrasts(timemark_data_mdl_df$condition)

contrasts(timemark_data_mdl_df$until)

library(lme4)
library(lmerTest)
timemark_mdl <- lmer(time ~ condition * until + (1 | participant) + (1 | item),
                     data = timemark_data_mdl_df)
summary(timemark_mdl)
broom.mixed::tidy(timemark_mdl)
gtsummary::tbl_regression(timemark_mdl, intercept = TRUE, conf.int = TRUE)

timemark_data_fitted <- timemark_data_mdl_df %>% 
  distinct(condition, until) %>% 
  broom::augment(newdata = ., timemark_mdl, interval = "confidence", re.fit = NA) %>% 
  arrange(condition, until) %>% 
  group_by(condition) %>% 
  mutate(cum_time = cumsum(.fitted)) %>% 
  ungroup()

timemark_data_fitted_wide <- timemark_data_fitted %>% 
  select(condition:.fitted, cum_time) %>% 
  pivot_wider(
    condition, names_from = "until",
    values_from = "cum_time",
    unused_fn = list
  )

## -- filtered data
total_RT_plot_df %>% 
  filter(coding %in% c("Subordinate", "Basic")) %>% 
  mutate(coding = factor(coding, levels = c("Subordinate", "Basic"))) %>% 
  ggplot(aes(x = labelled, time_z)) +
  geom_hline(aes(yintercept = 0), color = "grey", linetype = 2) +
  geom_errorbar(
    width = .2,
    stat = StatSummary, fun.data = mean_se
  ) +
  geom_point(
    aes(shape = labelled, fill = labelled),
    size = 3,
    stat = StatSummary, fun.data = mean_se,
    show.legend = FALSE
  ) +
  geom_label(
    aes(label = n, y = mean),
    stat = "unique",
    position = position_nudge(x = .1),
    size = 4.5,
    hjust = 0, label.size = 0,
    data = . %>%
      group_by(coding, labelled) %>%
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
  theme_pgl_minimal(base_size = 15, grid_lines = "y") +
  theme(
    axis.text.x = element_text(margin = margin(b = 1, unit = "mm")),
  )

## ---- up to selection of the two subs

up_to_subs_RT <- all_data %>%
  filter(coding %in% c("Subordinate", "Basic")) %>% 
  mutate(coding = factor(coding, levels = c("Subordinate", "Basic"))) %>% 
  group_by(participant, item) %>% 
  slice(n()) %>% 
  group_by(participant) %>%
  mutate(
    time_log = log(time),
    time_z = scale(time_log)[,1]
  )
  group_by(participant, item) %>% 
  slice(max(which(type == "sub"))) %>% 
  ungroup() %>% 
  select(participant, item, starts_with("time"), coding, group, labelled, target) %>% 
  mutate(
    target = c("label1" = "First", "label2" = "Second")[target],
    labelled = ifelse(labelled, "Labelled", "Unlabelled"),
    labelled = fct_rev(labelled)
  )

up_to_subs_RT %>% 
  ggplot(aes(labelled, time_z)) +
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
      group_by(coding, labelled) %>%
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
    axis.text.x = element_text(margin = margin(b = 1, unit = "mm"))
  )


## Combined
bind_rows(
  mutate(total_RT_plot_df, to = "After all selections"),
  mutate(up_to_subs_RT, to = "Up to target subordinates")
) %>% 
  ggplot(aes(interaction(labelled, to), time_z)) +
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
      group_by(coding, interaction(labelled, to)) %>%
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


# Transitional Probabilities (3/3) ----

sub_basic_idx <- sub_basic %>% 
  filter(coding == "Basic") %>% 
  select(participant, item, coding, img, type, selected) %>% 
  group_by(participant, item, coding, type) %>% 
  mutate(type_idx = paste0(type, as.integer(fct_inorder(img)))) %>% 
  ungroup() %>% 
  mutate(type_idx = factor(type_idx)) %>% 
  group_split(participant, item, coding, .keep = FALSE) %>% 
  map(~ pull(.x, selected, type_idx))

find_transitions <- function(x) {
  transitions <- map(sub_basic_idx, ~ {
    if (x == "START") {
      .x[1]
    } else {
      c(.x, "END" = TRUE)[which(names(.x) == x) + 1L]
    }
  })
  transitions_all <- unlist(transitions)
  as.data.frame(table(transitions_all, names(transitions_all))) %>% 
    mutate(from = x)
}

uq_nodes <- c("START", unique(names(unlist(sub_basic_idx))))

transitions <- setNames(map(uq_nodes, find_transitions), uq_nodes)
  
transitions_filled <- bind_rows(transitions) %>% 
  select(from, to = Var2, selected = transitions_all, count = Freq) %>% 
  complete(from, to, selected, fill = list(count = 0)) %>% 
  mutate(
    across(c(from, to), factor, levels =
                c("START", "sub1", "sub2", "contrast1", "contrast2",
                  "basic1", "basic2", "basic3",
                  "sup1", "END")),
    selected = as.logical(selected)
  ) %>% 
  arrange(from, to, selected)

transitions_filled_matrix <- transitions_filled %>% 
  filter(selected) %>% 
  pivot_wider(id_cols = from, names_from = to, values_from = count) %>% 
  column_to_rownames("from")

transitions_filled_df <- transitions_filled_matrix %>% 
  as.matrix() %>% 
  prop.table(1) %>% 
  as.data.frame() %>% 
  mutate(n = rowSums(transitions_filled_matrix))

library(reactable)
library(reactablefmtr)


reactable(
  transitions_filled_df[, -ncol(transitions_filled_df)],
  compact = TRUE,
  pagination = FALSE,
  showSortIcon = FALSE,
  defaultColDef = colDef(
    maxWidth = 80,
    cell = color_tiles(select(transitions_filled_df, -n),
                       number_fmt = scales::percent_format(accuracy = 1),
                       text_color = "black", bold_text = TRUE, span = TRUE,
                       colors = c("white", rev(colorspace::sequential_hcl(10, palette = "Red-Blue"))))
  ),
  # columns = list(n = colDef(name = "n", style = "font-style: italic;"))
)


library(fs)
dir_ls("C:/Users/jchoe/Downloads/YUFOs/YUFOs/F1/Lights", glob = "*.jpg") %>% 
  sample(24)
