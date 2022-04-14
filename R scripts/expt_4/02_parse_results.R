library(tidyverse)

# Read in data ====

results_raw <- penngradlings::read_pcibex("C:/Users/jchoe/Downloads/results (33).csv")


# Check window sizes

results_raw %>%
  filter(!is.na(window_size)) %>%
  select(sonaID, window_size) %>%
  mutate(at = rep(c("start", "end"), n()/2)) %>%
  separate(window_size, into = c("width", "height"), sep = "x", convert = TRUE) %>% 
  mutate(across(width:height, ~ .x / 96, .names = "{.col}_px"))


# Exclusion criteria

# Check duplicates
sona_duplicates <- results_raw %>% 
  distinct(Results_reception_time, sonaID) %>% 
  count(sonaID, sort = TRUE) %>% 
  filter(n > 1) %>% 
  pull(sonaID)

sona_failed_catch <- results_raw %>% 
  filter(
    PennElementType == "SelectionArray",
    str_detect(item, "^Filler")
  ) %>% 
  select(sonaID, Value) %>% 
  mutate(Value = str_extract(Value, "^.*(?=\\|)")) %>% 
  filter(Value != "No;No;Yes;No;No;Yes;No;No;No;Yes") %>% 
  pull(sonaID)

sona_excludes <- union(sona_duplicates, sona_failed_catch)

# Processing
order_cond = list(
  # x-sub-x-sub-x-basic-x-basic/contrast-x-basic
  "sub" = c("other", "sub", "other", "sub", "other",
            "basic", "other", "contrast", "other", "basic"),
  # x-basic-x-basic/contrast-x-basic-x-sub-x-sub
  "basic" = c("other", "basic", "other", "contrast", "other",
              "basic", "other", "sub", "other", "sub")
)
order_cond_chr <- vapply(order_cond, paste, collapse = ";", character(1))

results_packed <- results_raw %>% 
  filter(
    Label == "experiment",
    PennElementType == "SelectionArray",
    !is.na(order)
  ) %>% 
  select(sonaID, group:item, Value) %>% 
  separate(Value, c("response", "response_time"), sep = "\\|") %>% 
  mutate(across(sonaID:item, as.factor))

results_parsed <- results_packed %>% 
  mutate(type = order_cond_chr[as.character(order)]) %>% 
  separate_rows(response, response_time, type, sep = ";") %>% 
  mutate(type = if_else(contrast == "single" & type == "contrast", "basic", type))

code_response <- function(contrast, data) {
  selection <- data$type[data$response == "Yes"]
  if ("other" %in% selection || sum(selection == "sub") != 2) {
    return("other")
  }
  if (! "basic" %in% selection) {
    if ("contrast" %in% selection) {
      return("ME")
    } else {
      return("sub")
    }
  }
  if ("basic" %in% selection) {
    return("basic")
  }
}

results_coded_nested <- results_parsed %>%
  nest_by(sonaID, group, contrast, target, order, item) %>% 
  mutate(
    coding = code_response(contrast, data),
    selections = list(table(data$type[data$response == "Yes"]))
  ) %>% 
  ungroup() %>% 
  arrange(sonaID, group, item)







# Keys ====
## Read / transform ----
library(here)

trial_template <- read_csv(here("pcibex", "Experiment 1", "01_trial_templates.csv"))
img_tbl <- read_csv(here("R Scripts", "image_table.csv"))
keys <- read_csv(here("R Scripts", "keys.csv"))
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
  filter(trial == "critical") %>% 
  mutate(pmap_dfr(list(item, selections, condition), categorize_responses)) %>% 
  rename_with(~ paste0(.x, "_n"), matches("(basic|contrast|sub|sup|other)"))

write_rds(results_encoded, here::here("R scripts", "expt_1", "02_results_encoded.rds"))


# Click data ====

results_clicks <- results_encoded %>% 
  select(-ends_with("_n"), -selections) %>% 
  unnest(clicks) %>% 
  left_join(
    keys %>% 
      select(item = domain, img, type),
    by = c("img", "item")
  ) %>% 
  replace_na(list(type = "other")) %>% 
  mutate(type = ifelse(condition == "single" & type == "contrast", "basic", type))

write_csv(results_clicks, here::here("R scripts", "expt_1", "02_results_clicks.csv"))
