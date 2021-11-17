library(tidyverse)
source("R scripts/read_pcibex.R")

# Read in data ====

results_raw <- read_pcibex("data/expt3_test_11-14-2021.csv")

# Check window sizes

results_raw %>%
  filter(!is.na(window_size)) %>%
  select(participant = contains("participant"), window_size) %>%
  mutate(at = rep(c("start", "end"), n()/2)) %>%
  separate(window_size, into = c("width", "height"), sep = "x", convert = TRUE)
  # group_by(participant) %>% 
  # summarize(across(width:height, sd))

# Processing

results_parsed <- results_raw %>% 
  rename(participant = contains("participant")) %>% 
  filter(!Value %in% c("Start", "End")) %>% 
  select(participant, Value, group, type, order, target, item) %>% 
  separate(Value, "\\|", into = c("selections", "clicks")) %>% 
  mutate(
    selections = str_split(selections, ";"),
    clicks = map(str_split(clicks, ":"), ~ {
      as_tibble(.x) %>% 
        separate(value, ";", into = c("img", "selected", "time")) %>% 
        mutate(
          selected = selected == "true",
          time = as.double(time)
        )
    }),
    trial = ifelse(str_detect(item, "^Filler"), "catch", "critical"),
    participant = as.factor(participant),
    item = as.factor(item)
  )

# Check on catch trials
pass_catch <- function(item, clicks) {
  switch(
    item,
    "Filler-Color-red" = {
      all(clicks$img == "red-rect.jpg")
    },
    "Filler-Shape-triangle" = {
      every(clicks$img, ~ str_detect(.x, "^triangle"))
    }
  )
}

# Filter participants
results_catch <- results_parsed %>%
  filter(trial == "catch") %>%
  mutate(pass = map2_lgl(as.character(item), clicks, pass_catch))
failed_catch <- results_catch %>% 
  filter(!pass) %>% 
  pull(participant) %>% 
  unique()
failed_catch

results_parsed <- results_parsed %>% 
  filter(!participant %in% failed_catch) %>% 
  mutate(participant = factor(participant))

# Validations ====
## check that selections are imgs where the click event was a selection ----
stopifnot(
  all.equal(
    map(results_parsed$selections, sort),
    results_parsed$clicks %>% 
      map(~ {
        .x %>% 
          group_by(img) %>% 
          slice_tail(n = 1) %>% 
          filter(selected) %>% 
          pull(img) %>% 
          sort()
      })
  )
)


# Keys ====
## Read / transform ----
library(here)

trial_template <- read_csv(here("pcibex", "Experiment 2", "01_trial_templates.csv"))
img_tbl <- read_csv(here("R Scripts", "image_table.csv"))
keys <- read_csv(here("R Scripts", "keys.csv"))
keys_nested <- nest(keys, referents = c(type, category, img))


# Encode selections as categories ====

categorize_responses <- function(item, selections, type) {
  domain_keys <- keys %>% 
    filter(domain == item) %>% 
    pull(type, img)
  
  category_counts <- tidyr::replace_na(domain_keys[selections], "other")
  
  category_list <- modifyList(
    list(basic = 0L, contrast = 0L, sub = 0L, sup = 0L, other = 0L),
    as.list(table(category_counts))
  )
  if (type == "3basic" && category_list$sub == 0L) { category_list$sub <- NA_integer_ }
  bind_cols(category_list)
}

results_encoded <- results_parsed %>%
  filter(trial == "critical") %>% 
  mutate(pmap_dfr(list(item, selections, type), categorize_responses)) %>% 
  rename_with(~ paste0(.x, "_n"), matches("(basic|contrast|sub|sup|other)"))

write_rds(results_encoded, here::here("R scripts", "expt_3", "02_results_encoded.rds"))

# Click data ====

categorize_clicks <- function(item, img, type) {
  
  domain_keys <- keys %>% 
    filter(domain == item) %>% 
    pull(type, img)
  
  domain_keys[img]
  
}

results_clicks <- results_encoded %>% 
  select(-ends_with("_n"), -selections) %>% 
  unnest(clicks) %>% 
  mutate(category = Vectorize(categorize_clicks)(item, img, type)) %>% 
  replace_na(list(category = "other"))

write_csv(results_clicks, here::here("R scripts", "expt_3", "02_results_clicks.csv"))
