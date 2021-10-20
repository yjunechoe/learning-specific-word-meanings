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

img_tbl <- read_csv(here::here("R Scripts", "image_table.csv"))
keys <- read_csv(here::here("R Scripts", "keys.csv"))

# design pre-randomized trial order and condition split (within-participant)

fillers <- tibble(
  domain = c("Filler-Color-red", "Filler-Shape-triangle"),
  label1 = c("gelder", "panzet"),
  label2 = ""
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

cond_design <- crossing(
  number = c("one", "three"),
  target = c("label1", "label2")
) %>% 
  slice(c(1:4, c(3, 4, 1, 2))) %>% 
  mutate(group = rep(c("A", "B"), each = 4)) %>% 
  group_by(group) %>% 
  slice(rep(1:n(), 2)) %>% 
  ungroup()

group_designs <- tibble(
  domain = setdiff(unique(img_tbl$domain), "planet"),
  label1 = nonsense_labels[c(TRUE, FALSE)],
  label2 = nonsense_labels[c(FALSE, TRUE)],
) %>% 
  slice(rep(1:8, 2)) %>% 
  bind_cols(cond_design) %>% 
  group_split(group) %>% 
  map_dfr(
    ~ .x %>%
      add_row(fillers[1,], .after = 3) %>% 
      add_row(fillers[2,], .after = 7) %>% 
      fill(target:group, .direction = "down") %>% 
      replace_na(list(number = ""))
  )

## fill design with trial templates

category_dict <- img_tbl %>% 
  distinct(domain, type, category) %>% 
  filter(type %in% c("sub", "contrast")) %>% 
  arrange(domain, desc(type))

gen_learn_set <- function(d, num_cond) {
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
        filter(img_tbl, c("one" = 1, "three" = 3)[num_cond] == .data$number), # single exemplar in learn phase
        by = c("domain", "type", "category")
      )
    pull(learn_set, path)
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
    learn_set = toJSON(gen_learn_set(domain, number)),
    test_set = toJSON(gen_test_set(domain))
  ) %>% 
  ungroup()

write_csv(trial_template_tbl, here::here("R Scripts", "expt_2", "01_trial_templates.csv"))
