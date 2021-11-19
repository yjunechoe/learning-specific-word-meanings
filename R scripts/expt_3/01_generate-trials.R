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
  label = c("gelder", "panzet"),
  order = "1",
  number = "one"
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
)[c(TRUE, FALSE)]

cond_order <- c("3-1", "1-3")

group_A <- tibble(
  order = cond_order[c(1, 1, 2, 2)],
  group = "A"
)
group_B <- tibble(
  order = cond_order[c(1, 2, 1, 2)],
  group = "B"
)
group_C <- tibble(
  order = cond_order[c(2, 1, 2, 1)],
  group = "C"
)
group_D <- tibble(
  order = cond_order[c(2, 2, 1, 1)],
  group = "D"
)

cond_design <- bind_rows(group_A, group_B, group_C, group_D) %>% 
  group_by(group) %>% 
  slice(c(1:n(), 1:n())) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  mutate(number = "three")

# cond_design %>%
#   pivot_wider(-number, names_from = group, values_from = order) %>%
#   select(1, !!!str_order(str_extract(colnames(.)[-1], "\\w$")) + 1)
# 
#   | id|A   |B   |C   |D   |
#   |--:|:---|:---|:---|:---|
#   |  1|3-1 |3-1 |1-3 |1-3 |
#   |  2|3-1 |1-3 |3-1 |1-3 |
#   |  3|1-3 |3-1 |1-3 |3-1 |
#   |  4|1-3 |1-3 |3-1 |3-1 |
#   |  5|3-1 |3-1 |1-3 |1-3 |
#   |  6|3-1 |1-3 |3-1 |1-3 |
#   |  7|1-3 |3-1 |1-3 |3-1 |
#   |  8|1-3 |1-3 |3-1 |3-1 |


item_design <- tibble(
  domain = setdiff(unique(img_tbl$domain), "planet"),
  label = nonsense_labels,
) %>%
  mutate(id = row_number())

group_designs <- cond_design %>% 
  left_join(item_design, by = "id") %>% 
  select(-id) %>% 
  group_split(group) %>%
  map_dfr(
    ~ .x %>%
      add_row(fillers[1, ], .after = 3) %>%
      add_row(fillers[2, ], .after = 7) %>%
      fill(group)
  )

# TODO this point on ---------

## fill design with trial templates

category_dict <- img_tbl %>%
  distinct(domain, type, category) %>%
  filter(type %in% c("sub", "contrast")) %>%
  arrange(domain, desc(type))

gen_learn_set <- function(d, order) {
  if (str_detect(d, "^Filler")) {
    if (str_detect(d, "Shape")) {
      "Fillers/Shape/triangle.jpg"
    } else if (str_detect(d, "Color")) {
      "Fillers/Color/red-rect.jpg"
    }
  } else {
    cond_num <- "3"
    cond_type <- "basic"
    cond_order <- str_split(order, "-")[[1]]
    
    one <- img_tbl %>% 
      filter(domain == d, number == 1, type == "contrast") %>% 
      pull(path)
    three <- img_tbl %>% 
      filter(domain == d, number == cond_num, type == cond_type) %>% 
      pull(path)
    
    unname(c("1" = one, "3" = three)[cond_order])
    
  }
}
# gen_learn_set("animal", "1-3")

gen_test_set <- function(d) {
  if (str_detect(d, "^Filler")) {
    filler_dir <- paste0("image_stimuli/Fillers/", str_extract(d, "Color|Shape"), "/")
    if (str_detect(d, "Shape")) {
      c(
        "Fillers/Shape/lines-11.jpg", "Fillers/Shape/lines-13.jpg", "Fillers/Shape/triangle-5.jpg",
        "Fillers/Shape/lines-5.jpg", "Fillers/Shape/lines-3.jpg", "Fillers/Shape/lines-2.jpg",
        "Fillers/Shape/lines-9.jpg", "Fillers/Shape/lines-8.jpg", "Fillers/Shape/triangle-3.jpg",
        "Fillers/Shape/triangle-2.jpg", "Fillers/Shape/triangle-4.jpg", "Fillers/Shape/triangle-1.jpg"
      )[1:10]
    } else if (str_detect(d, "Color")) {
      c(
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/blue-rect.jpg",
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/green-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/red-rect.jpg", "Fillers/Color/blue-rect.jpg", "Fillers/Color/blue-rect.jpg",
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/red-rect.jpg"
      )[1:10]
    }
  } else {
    d_kind <- img_tbl %>% 
      filter(domain == d) %>% 
      distinct(kind) %>% 
      pull(kind)
    other_refs <- img_tbl %>%
      filter(kind != d_kind | domain == "planet", type == "sup") %>%
      group_by(domain) %>%
      slice_sample(n = 1) %>%
      pull(path)
    domain_refs <- img_tbl %>%
      filter(domain == d, type != "sub", !is.na(id)) %>%
      group_split(type) %>%
      map_dfr(~ {
        type_g <- unique(.x$type)
        if (type_g %in% c("basic", "contrast")) {
          .x
        } else {
          slice(.x, 1:2)
        }
      }) %>% 
      pull(path)
    sample(c(other_refs, domain_refs))
  }
}

# gen_test_set("animal")

trial_template_tbl <- group_designs %>%
  rowwise() %>%
  mutate(
    learn_set = list(gen_learn_set(domain, order)),
    test_set = list(gen_test_set(domain))
  ) %>% 
  mutate(across(ends_with("_set"), toJSON)) %>% 
  ungroup()

write_csv(trial_template_tbl, here::here("pcibex", "Experiment 3", "01_trial_templates.csv"))

