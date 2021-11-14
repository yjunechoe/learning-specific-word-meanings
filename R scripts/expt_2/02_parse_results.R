library(tidyverse)
source("R scripts/read_pcibex.R")

# Read in data ====

results_raw <- read_pcibex("data/expt2_pilot_11-13-2021.csv")

# Check window sizes

results_raw %>%
  filter(!is.na(window_size)) %>%
  select(participant = contains("participant"), window_size) %>%
  mutate(at = rep(c("start", "end"), n()/2)) %>%
  separate(window_size, into = c("width", "height"), sep = "x", convert = TRUE) %>% 
  mutate(across(width:height, ~ .x / 96, .names = "{.col}_px"))
  # group_by(participant) %>% 
  # summarize(across(width:height, sd))

# Processing

results_parsed <- results_raw %>% 
  rename(participant = contains("participant")) %>% 
  filter(!Value %in% c("Start", "End")) %>% 
  select(participant, Value, group, number, target, item) %>% 
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

categorize_responses <- function(item, selections, target) {
  domain_keys <- keys %>% 
    filter(domain == item) %>% 
    pull(type, img)
  
  if (target == "label2") {
    domain_keys <- setNames(c("basic" = "basic", "sup" = "sup", "sub" = "contrast", "contrast" = "sub")[domain_keys], nm = names(domain_keys))
  }

  category_counts <- tidyr::replace_na(domain_keys[selections], "other")
  category_list <- modifyList(
    list(basic = 0, contrast = 0, sub = 0, sup = 0, other = 0),
    as.list(table(category_counts))
  )
  bind_cols(category_list)
}

results_encoded <- results_parsed %>%
  filter(trial == "critical") %>% 
  mutate(pmap_dfr(list(item, selections, target), categorize_responses)) %>% 
  rename_with(~ paste0(.x, "_n"), matches("(basic|contrast|sub|sup|other)"))

write_rds(results_encoded, here::here("R scripts", "expt_2", "02_results_encoded.rds"))


# Click data ====

categorize_clicks <- function(item, img, target) {
  
  domain_keys <- keys %>% 
    filter(domain == item) %>% 
    pull(type, img)
  
  if (target == "label2") {
    c("basic" = "basic", "sup" = "sup", "sub" = "contrast", "contrast" = "sub")[domain_keys[img]]
  } else {
    domain_keys[img]
  }
  
}

results_clicks <- results_encoded %>% 
  select(-ends_with("_n"), -selections) %>% 
  unnest(clicks) %>% 
  mutate(type = Vectorize(categorize_clicks)(item, img, target)) %>% 
  replace_na(list(type = "other"))

write_csv(results_clicks, here::here("R scripts", "expt_2", "02_results_clicks.csv"))
