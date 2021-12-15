library(fs)
library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(jsonlite)
library(assertr)
library(magrittr)

set.seed(2021)

img_tbl <- read_csv(here::here("R Scripts", "image_table.csv"))
keys <- read_csv(here::here("R Scripts", "keys.csv"))

# design pre-randomized trial order and condition split (within-participant)

fillers <- tibble(
  domain = c("Filler-Color-red", "Filler-Shape-triangle"),
  label1 = c("gelder", "panzet"),
  target = "label1",
  labelled = FALSE
)

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

cond_labelled <- c(TRUE, FALSE)
cond_order <- c("label1", "label2")

group_A <- tibble(
  labelled = cond_labelled[c(1, 1, 2, 2)],
  target = cond_order[c(1, 2, 1, 2)],
  group = "A"
)
group_B <- tibble(
  labelled = cond_labelled[c(2, 2, 1, 1)],
  target = cond_order[c(2, 1, 2, 1)],
  group = "B"
)
group_C <- tibble(
  labelled = cond_labelled[c(1, 2, 1, 2)],
  target = cond_order[c(1, 1, 2, 2)],
  group = "C"
)
group_D <- tibble(
  labelled = cond_labelled[c(2, 1, 2, 1)],
  target = cond_order[c(2, 2, 1, 1)],
  group = "D"
)

cond_design <- bind_rows(group_A, group_B, group_C, group_D) %>% 
  group_by(group) %>% 
  slice(1:n(), 1:n()) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  mutate(labelled = !labelled) #< round 2 design

item_design <- tibble(
  domain = setdiff(unique(img_tbl$domain), "planet"),
  labels = asplit(matrix(nonsense_labels, nrow = 2), 2)
) %>%
  mutate(id = row_number())

group_designs <- cond_design %>% 
  left_join(item_design, by = "id") %>% 
  rowwise() %>% 
  mutate(
    labels = ifelse(target == "label1", list(labels), list(rev(labels))),
    labels = ifelse(labelled, list(labels),
                    list(replace(labels, c("label1" = 2, "label2" = 1)[target], ""))),
    label1 = labels[1],
    label2 = labels[2]
  ) %>% 
  ungroup() %>%
  select(-c(id, labels)) %>% 
  group_split(group) %>%
  map_dfr(
    ~ .x %>%
      add_row(fillers[1, ], .after = 3) %>%
      add_row(fillers[2, ], .after = 7) %>%
      fill(group)
  )

# group_designs %>%
#   filter(str_detect(domain, "^Filler", TRUE)) %>%
#   pivot_wider(-c(label1, label2), names_from = group, values_from = c(labelled, target)) %>%
#   select(1, !!!str_order(str_extract(colnames(.)[-1], "\\w$")) + 1)
#
# |domain      |labelled_A |target_A |labelled_B |target_B |labelled_C |target_C |labelled_D |target_D |
# |:-----------|:----------|:--------|:----------|:--------|:----------|:--------|:----------|:--------|
# |animal      |TRUE       |label1   |FALSE      |label2   |TRUE       |label1   |FALSE      |label2   |
# |beast       |TRUE       |label2   |FALSE      |label1   |FALSE      |label1   |TRUE       |label2   |
# |electronics |FALSE      |label1   |TRUE       |label2   |TRUE       |label2   |FALSE      |label1   |
# |fruit       |FALSE      |label2   |TRUE       |label1   |FALSE      |label2   |TRUE       |label1   |
# |furniture   |TRUE       |label1   |FALSE      |label2   |TRUE       |label1   |FALSE      |label2   |
# |light       |TRUE       |label2   |FALSE      |label1   |FALSE      |label1   |TRUE       |label2   |
# |vegetable   |FALSE      |label1   |TRUE       |label2   |TRUE       |label2   |FALSE      |label1   |
# |vehicle     |FALSE      |label2   |TRUE       |label1   |FALSE      |label2   |TRUE       |label1   |

# group_designs %>%
#   mutate(labelled = !labelled) %>% 
#   filter(str_detect(domain, "^Filler", TRUE)) %>%
#   pivot_wider(-c(label1, label2), names_from = group, values_from = c(labelled, target)) %>%
#   select(1, !!!str_order(str_extract(colnames(.)[-1], "\\w$")) + 1)
#
# |domain      |labelled_A |target_A |labelled_B |target_B |labelled_C |target_C |labelled_D |target_D |
# |:-----------|:----------|:--------|:----------|:--------|:----------|:--------|:----------|:--------|
# |animal      |FALSE      |label1   |TRUE       |label2   |FALSE      |label1   |TRUE       |label2   |
# |beast       |FALSE      |label2   |TRUE       |label1   |TRUE       |label1   |FALSE      |label2   |
# |electronics |TRUE       |label1   |FALSE      |label2   |FALSE      |label2   |TRUE       |label1   |
# |fruit       |TRUE       |label2   |FALSE      |label1   |TRUE       |label2   |FALSE      |label1   |
# |furniture   |FALSE      |label1   |TRUE       |label2   |FALSE      |label1   |TRUE       |label2   |
# |light       |FALSE      |label2   |TRUE       |label1   |TRUE       |label1   |FALSE      |label2   |
# |vegetable   |TRUE       |label1   |FALSE      |label2   |FALSE      |label2   |TRUE       |label1   |
# |vehicle     |TRUE       |label2   |FALSE      |label1   |TRUE       |label2   |FALSE      |label1   |
  

## fill design with trial templates

category_dict <- img_tbl %>% 
  distinct(domain, type, category) %>% 
  filter(type %in% c("sub", "contrast")) %>% 
  arrange(domain, desc(type))

gen_learn_set <- function(d, target) {
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
    if (str_detect(d, "^Filler")) {
      pull(learn_set, path)
    } else {
      if (target == "label1") {
        learn_set %>% 
          pull(path)
      } else {
        learn_set %>% 
          pull(path) %>% 
          rev()
      }
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
    learn_set = list(gen_learn_set(domain, target)),
    test_set = list(gen_test_set(domain))
  ) %>% 
  mutate(across(ends_with("_set"), toJSON)) %>% 
  ungroup()


write_csv(trial_template_tbl, here::here("R Scripts", "expt_1B", "01_trial_templates.csv"))
