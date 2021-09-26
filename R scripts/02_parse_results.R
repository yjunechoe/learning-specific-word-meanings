library(tidyverse)
source("R scripts/read_pcibex.R")

# Read in data ====

results_raw <- read_pcibex("R scripts/results_practice.csv")

results_parsed <- results_raw %>% 
  select(participant = contains("participant"), value = Value, group, condition, item) %>% 
  filter(!value %in% c("Start", "End")) %>% 
  separate(value, "\\|", into = c("selections", "clicks")) %>% 
  mutate(
    selections = str_split(selections, ";"),
    clicks = map(str_split(clicks, ":"), ~ {
      as_tibble(.x) %>% 
        separate(value, ";", into = c("img", "selected", "time")) %>% 
        mutate(
          selected = selected == "true",
          time = as.double(time)
        )
    })
  )

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

trial_template <- read_csv(here("R Scripts", "01_trial_templates.csv"))
img_tbl <- read_csv(here("R Scripts", "01_image_table.csv"))
keys <- read_csv(here("R Scripts", "01_keys.csv"))
keys_nested <- nest(keys, referents = c(type, category, img))


# Encode selections as categories ====

categorize_responses <- function(item, selections, condition) {
  domain_keys <- keys %>% 
    filter(domain == item) %>% 
    pull(type, img)
  # in the single condition, seeing "contrast" subordinate exemplar is actually as if it's sampled from basic
  if (condition == "single") {
    domain_keys[domain_keys == "contrast"] <- "basic"
  }
  category_counts <- tidyr::replace_na(domain_keys[selections], "other")
  category_list <- modifyList(
    list(basic = 0, contrast = 0, sub = 0, sup = 0, other = 0),
    as.list(table(category_counts))
  )
  bind_cols(category_list)
}

results_encoded <- results_parsed %>% 
  mutate(pmap_dfr(list(item, selections, condition), categorize_responses)) %>% 
  rename_with(~ paste0(.x, "_n"), matches("(basic|contrast|sub|sup)"))

write_rds(results_encoded, here::here("R scripts", "02_results_encoded.rds"))


# Click data ====

results_clicks <- results_encoded %>% 
  select(-ends_with("_n"), -selections) %>% 
  unnest(clicks) %>% 
  left_join(
    keys %>% 
      select(item = domain, img, type),
    by = c("img", "item")
  ) %>% 
  replace_na(list(type = "other"))

write_csv(results_clicks, here::here("R scripts", "02_results_clicks.csv"))
