library(fs)
library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(jsonlite)
library(assertr)

set.seed(2021)

img_dir <- here("image_stimuli")

img_paths <- fs::dir_ls(img_dir, regexp = "/[a-z]+$") %>% 
  dir_ls(recurse = TRUE, type = "file") %>% 
  path_rel(img_dir)

img_info_col <- c("number", "type", "category", "id")

img_tbl <- tibble(path = img_paths) %>%
  mutate(
    domain = str_extract(path_dir(img_paths), "^[a-z]+"),
    file = path_file(path)
  ) %>% 
  extract(
    col = file,
    into = img_info_col,
    regex = "^(\\d)?(sup|basic|contrast|sub)-([a-z_]+)-?(\\d)?",
    convert = TRUE
  ) %>% 
  relocate(path, .after = everything())

img_tbl %>% 
  verify(
    expr = has_all_names(!!!c(img_info_col), "domain", "path"),
    description = "Image info successfully parsed from relative paths."
  ) %>% 
  verify(
    expr = (sum(is.na(number)) + sum(is.na(id))) == nrow(.),
    description = "All images are used for either learning OR testing - always one never both."
  ) %>% 
  verify(
    expr = (n_distinct(domain) == 9) && (var(table(domain[domain != "planet"])) == 0),
    description = "There are 9 semantic domains, and all have the same # of images except the filler domain 'planet'."
  )

# design pre-randomized trial order and condition split (within-participant)

nonsense_labels <- c(
  "kapsin",
  "tantol",
  "garlet",
  "crittum",
  "tazzai",
  "pitten",
  "mipen",
  "kalmick",
  "gavai",
  "flippet",
  "belpid",
  "spadil",
  "tuffee",
  "blicket",
  "clazzo",
  "dalkeet"
)

group_designs <- tibble(
  domain = setdiff(unique(img_tbl$domain), "planet"),
  label1 = nonsense_labels[c(TRUE, FALSE)],
  label2 = nonsense_labels[c(FALSE, TRUE)],
  groupA = rep(c("S", "C"), 4),
  groupB = rep(c("C", "S"), 4),
  groupC = rep(c("S", "S", "C", "C"), 2),
  groupD = rep(c("C", "C", "S", "S"), 2)
) %>% 
  pivot_longer(
    cols = starts_with("group"),
    names_to = "group",
    names_prefix = "group",
    values_to = "condition"
  ) %>% 
  mutate(condition = c(S = "single", C = "contrast")[condition]) %>% 
  arrange(group)

group_designs %>% 
  verify(
    expr = all(table(group, condition) == 4),
    description = "There are 4 trials from each condition in all 4 design groups."
  )

## fill design with trial templates

category_dict <- img_tbl %>% 
  distinct(domain, type, category) %>% 
  filter(type %in% c("sub", "contrast")) %>% 
  arrange(domain, desc(type))

gen_learn_set <- function(d, condition) {
  learn_set <- category_dict %>% 
    filter(domain == d) %>% 
    left_join(
      filter(img_tbl, number == 1),
      by = c("domain", "type", "category")
    )
  if (condition == "contrast") {
    pull(learn_set, path)
  } else if (condition == "single") {
    learn_set %>% 
      filter(type == "sub") %>% 
      pull(path)
  }
}

gen_test_set <- function(d) {
  other_refs <- img_tbl %>% 
    filter(domain != d, type == "sup") %>% 
    group_by(domain) %>% 
    slice_sample(n = 1) %>% 
    pull(path)
  domain_refs <- img_tbl %>% 
    filter(domain == d, !is.na(id)) %>% 
    filter(id <= 3) %>%
    pull(path)
  sample(c(other_refs, domain_refs))
}

trial_template_tbl <- group_designs %>% 
  rowwise() %>% 
  mutate(
    learn_set = toJSON(gen_learn_set(domain, condition)),
    test_set = toJSON(gen_test_set(domain))
  ) %>% 
  ungroup()

write_csv(trial_template_tbl, here::here("R Scripts", "01_trial_templates.csv"))
