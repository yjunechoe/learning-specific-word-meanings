library(tidyverse)
library(assertr)
source("R scripts/read_pcibex.R")

# Read in data ====

results <- read_pcibex("R scripts/02_results_practice.csv")

results_parsed <- results %>% 
  select(contains("participant"), Value:last_col()) %>% 
  filter(!Value %in% c("Start", "End")) %>% 
  separate(Value, "\\|", into = c("selections", "clicks")) %>% 
  mutate(
    selections = str_split(selections, ";"),
    clicks = map(str_split(clicks, ":"), ~ {
      as_tibble(.x) %>% 
        separate(value, ";", into = c("img", "selected", "time")) %>% 
        mutate(selected = selected == "true")
    })
  )

## Checks ----
## - check that selections are imgs where the click event was a selection
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
## Read ----
library(here)
trial_template <- read_csv(here("R Scripts", "01_trial_templates.csv"))
img_tbl <- read_csv(here("R Scripts", "01_image_table.csv"))
keys <- read_csv(here("R Scripts", "01_keys.csv"))
keys_nested <- nest(keys, referents = c(type, category, img))
