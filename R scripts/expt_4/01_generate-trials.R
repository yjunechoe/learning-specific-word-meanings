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

set.seed(2022)

img_tbl <- read_csv(here("R scripts", "image_table.csv"))
# keys

keys <- read_csv(here("R scripts", "keys.csv"))

# fillers

fillers <- tibble(
  domain = c("Filler-Color-red", "Filler-Shape-triangle"),
  label1 = c("gelder", "panzet"),
  label2 = ""
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

nonsense_labels_df <- tibble(
  id = 1:8,
  domain = setdiff(unique(img_tbl$domain), "planet"),
  label1 = nonsense_labels[c(TRUE, FALSE)],
  label2 = nonsense_labels[c(FALSE, TRUE)]
)

cond_contrast <- c("single", "contrast")
rand_target <- c("label1", "label2")
rand_order <- c("sub", "basic")

counterbalancing_template <- crossing(
  domain = setdiff(unique(img_tbl$domain), "planet"),
  target = rand_target,
  order = rand_order,
  contrast = cond_contrast
) %>% 
  mutate(
    group = LETTERS[unlist(accumulate(seq_len(8-1), function(x, y) coalesce(lead(x), rev(x)), .init = 1:8))],
    target = ifelse(contrast == "single", "label1", target)
  )

group_designs <- counterbalancing_template %>% 
  left_join(nonsense_labels_df, by = "domain") %>% 
  arrange(group) %>% 
  mutate(label2 = ifelse(contrast == "single", "", label2)) %>% 
  select(-id) %>% 
  group_split(group) %>%
  map_dfr(
    ~ .x %>%
      # add_row(fillers[1, ], .after = 3) %>% # remove color trial
      add_row(fillers[2, ], .after = 3) %>%
      fill(group)
  ) %>% 
  replace_na(list(target = "label1", contrast = "single"))

## fill design with trial templates

category_dict <- img_tbl %>% 
  distinct(domain, type, category) %>% 
  filter(type %in% c("sub", "contrast")) %>% 
  arrange(domain, desc(type))

gen_learn_set <- function(d, condition, target) {
  if (str_detect(d, "^Filler")) {
    if (str_detect(d, "Shape")) {
      return("Fillers/Shape/triangle.jpg")
    } else if (str_detect(d, "Color")) {
      return("Fillers/Color/red-rect.jpg")
    }
  } else {
    learn_set <- category_dict %>% 
      filter(domain == d) %>% 
      left_join(
        filter(img_tbl, number == 1), # single exemplar in learn phase
        by = c("domain", "type", "category")
      )
    if (condition == "contrast") {
      learn_set <- pull(learn_set, path)
    } else if (condition == "single") {
      learn_set <- learn_set %>% 
        filter(type == "sub") %>% 
        pull(path)
    }
  }
  if (target == "label2") {
    learn_set <- rev(learn_set)
  }
  learn_set
}

gen_test_set <- function(d, order) {
  if (str_detect(d, "^Filler")) {
    filler_dir <- paste0("image_stimuli/Fillers/", str_extract(d, "Color|Shape"), "/")
    if (str_detect(d, "Shape")) {
      c("Fillers/Shape/lines-11.jpg", "Fillers/Shape/lines-13.jpg", "Fillers/Shape/triangle-5.jpg",
        "Fillers/Shape/lines-9.jpg", "Fillers/Shape/lines-8.jpg", "Fillers/Shape/triangle-3.jpg",
        "Fillers/Shape/lines-10.jpg", "Fillers/Shape/lines-6.jpg", "Fillers/Shape/lines-4.jpg",
        "Fillers/Shape/triangle-1.jpg")
    } else if (str_detect(d, "Color")) {
      c("Fillers/Color/blue-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/blue-rect.jpg",
        "Fillers/Color/blue-rect.jpg")
    }
  }
  else {
    other_refs <- img_tbl %>% 
      filter(
        kind != unique(filter(img_tbl, domain == d)$kind) | domain == "planet",
        type == "sup"
      ) %>% 
      group_by(domain) %>% 
      slice_sample(n = 1) %>% 
      pull(path)
    domain_refs <- img_tbl %>% 
      filter(domain == d, !is.na(id)) %>% 
      filter(id <= 3) %>%
      pull(path, type)
    domain_refs_sampled_sub <- domain_refs[names(domain_refs) == "sub"][1:2]
    domain_refs_sampled_contrast <- domain_refs[names(domain_refs) == "contrast"][1]
    domain_refs_sampled_basic <- domain_refs[names(domain_refs) == "basic"][1:2]
    
    template <- character(10)
    
    if (order == "sub") {
      # x-sub-x-sub-x-basic-x-basic/contrast-x-basic
      template[c(2, 4)] <- domain_refs_sampled_sub
      template[c(6, 10)] <- domain_refs_sampled_basic
      template[8] <- domain_refs_sampled_contrast
      template[template == ""] <- sample(other_refs)
    } else if (order == "basic") {
      # x-basic-x-basic/contrast-x-basic-x-sub-x-sub
      template[c(8, 10)] <- domain_refs_sampled_sub
      template[c(2, 6)] <- domain_refs_sampled_basic
      template[4] <- domain_refs_sampled_contrast
      template[template == ""] <- sample(other_refs)
    }
    
    template
  }
}

trial_template_tbl <- group_designs %>% 
  rowwise() %>% 
  mutate(
    learn_set_list = list(gen_learn_set(domain, contrast, target)),
    test_set_list = list(gen_test_set(domain, order)),
    learn_set = toJSON(learn_set_list),
    test_set = toJSON(test_set_list)
  ) %>% 
  ungroup()

trial_template_tbl <- select(trial_template_tbl, -ends_with("list"))

write_csv(trial_template_tbl, here::here("R Scripts", "expt_4", "01_trial_templates.csv"))
