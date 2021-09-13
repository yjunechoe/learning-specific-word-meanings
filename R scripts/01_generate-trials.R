library(fs)
library(here)
library(tidyverse)
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

group_designs <- tibble(
  domain = setdiff(unique(img_tbl$domain), "planet"),
  group1 = rep(c("S", "C"), 4),
  group2 = rep(c("C", "S"), 4),
  group3 = rep(c("S", "S", "C", "C"), 2),
  group4 = rep(c("C", "C", "S", "S"), 2)
) %>% 
  pivot_longer(
    cols = starts_with("group"),
    names_to = "group",
    names_pattern = "group(\\d)",
    values_to = "condition"
  ) %>% 
  mutate(condition = c(S = "single", C = "contrast")[condition]) %>% 
  arrange(group)

gen_test_set <- function(domain) {
  
}

