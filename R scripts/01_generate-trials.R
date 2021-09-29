library(fs)
library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(jsonlite)
library(assertr)

`%$%` <- magrittr::`%$%`

set.seed(2021)

img_dir <- here("image_stimuli")

img_paths <- fs::dir_ls(img_dir, regexp = "/[a-z]+$") %>% 
  dir_ls(recurse = TRUE, type = "file") %>% 
  path_rel(img_dir)

img_info_col <- c("number", "type", "category", "id")

img_tbl <- tibble(path = img_paths) %>%
  mutate(
    domain = str_extract(path_dir(img_paths), "^[a-z]+"),
    kind = ifelse(domain %in% c("animal", "beast", "fruit", "vegetable"), "natural", "artificial"),
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

write_csv(img_tbl, here::here("R Scripts", "01_image_table.csv"))

# keys

keys <- img_tbl %>% 
  filter(!is.na(id) & domain != "planet") %>% 
  mutate(img = str_extract(path, "[^/]+\\.jpg$")) %>% 
  select(domain, kind, type, category, img) %>% 
  group_by(domain) %>% 
  mutate(target = unique(category[type == "sub"]), .after = "kind") %>% 
  ungroup()

# fillers

fillers <- tibble(
  domain = c("Filler-Color-red", "Filler-Shape-triangle"),
  label1 = c("gelder", "panzet"),
  label2 = ""
)

write_csv(keys, here::here("R Scripts", "01_keys.csv"))

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
  add_row(fillers[1,], .after = 3) %>% 
  add_row(fillers[2,], .after = 7) %>% 
  pivot_longer(
    cols = starts_with("group"),
    names_to = "group",
    names_prefix = "group",
    values_to = "condition"
  ) %>% 
  replace_na(list(condition = "S")) %>% 
  mutate(condition = c(S = "single", C = "contrast")[condition]) %>% 
  arrange(group)

group_designs %>%
  verify(
    expr = length(str_subset(domain, "^Filler")) == 2 * 4,
    description = "There are 4 trials from each condition in all 4 design groups."
  ) %>% 
  verify(
    expr = filter(., str_detect(domain, "^Filler", negate = TRUE)) %$% 
      all(table(group, condition) == 4),
    description = "There are 4 trials from each condition in all 4 design groups."
  )

## fill design with trial templates

category_dict <- img_tbl %>% 
  distinct(domain, type, category) %>% 
  filter(type %in% c("sub", "contrast")) %>% 
  arrange(domain, desc(type))

gen_learn_set <- function(d, condition) {
  if (str_detect(d, "^Filler")) {
    if (str_detect(d, "Shape")) {
      "Fillers/Shape/triangle.jpg"
    } else if (str_detect(d, "Color")) {
      "Fillers/Color/red-rect.jpg"  
    }
  } else {
    learn_set <- category_dict %>% 
      filter(domain == d) %>% 
      left_join(
        filter(img_tbl, number == 1), # single exemplar in learn phase
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
}

gen_test_set <- function(d) {
  if (str_detect(d, "^Filler")) {
    filler_dir <- paste0("image_stimuli/Fillers/", str_extract(d, "Color|Shape"), "/")
    if (str_detect(d, "Shape")) {
      c("Fillers/Shape/lines-11.jpg", "Fillers/Shape/lines-13.jpg", "Fillers/Shape/triangle-5.jpg",
        "Fillers/Shape/lines-5.jpg", "Fillers/Shape/lines-3.jpg", "Fillers/Shape/lines-2.jpg",
        "Fillers/Shape/lines-9.jpg", "Fillers/Shape/lines-8.jpg", "Fillers/Shape/triangle-3.jpg",
        "Fillers/Shape/triangle-2.jpg", "Fillers/Shape/triangle-4.jpg", "Fillers/Shape/lines-7.jpg",
        "Fillers/Shape/lines-10.jpg", "Fillers/Shape/lines-6.jpg", "Fillers/Shape/lines-4.jpg",
        "Fillers/Shape/lines-1.jpg", "Fillers/Shape/triangle-1.jpg", "Fillers/Shape/lines-12.jpg")
    } else if (str_detect(d, "Color")) {
      c("Fillers/Color/blue-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/blue-rect.jpg",
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/green-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/red-rect.jpg", "Fillers/Color/blue-rect.jpg", "Fillers/Color/blue-rect.jpg",
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/red-rect.jpg")
    }
  }
  else {
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
}

trial_template_tbl <- group_designs %>% 
  rowwise() %>% 
  mutate(
    learn_set = toJSON(gen_learn_set(domain, condition)),
    test_set = toJSON(gen_test_set(domain))
  ) %>% 
  ungroup()

write_csv(trial_template_tbl, here::here("R Scripts", "01_trial_templates.csv"))
