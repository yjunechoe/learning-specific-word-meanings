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

cond_order <- c("3-1", "1-3")
cond_contrast <- c("3basic", "3sub")

group_A <- tibble(
  order = cond_order[c(1, 2, 1, 2)],
  target = cond_contrast[c(1, 2, 2, 1)],
  group = "A"
)
group_B <- tibble(
  order = cond_order[c(1, 2, 1, 2)],
  target = cond_contrast[c(2, 1, 1, 2)],
  group = "B"
)
group_C <- tibble(
  order = cond_order[c(2, 1, 2, 1)],
  target = cond_contrast[c(1, 2, 2, 1)],
  group = "C"
)
group_D <- tibble(
  order = cond_order[c(2, 1, 2, 1)],
  target = cond_contrast[c(2, 1, 1, 2)],
  group = "D"
)

cond_design <- bind_rows(group_A, group_B, group_C, group_D) %>% 
  group_by(group) %>% 
  slice(c(1:n(), 1:n())) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  mutate(number = "three")

# cond_design %>% 
#   pivot_wider(-number, names_from = group, values_from = c(order, target)) %>% 
#   select(1, !!!str_order(str_extract(colnames(.)[-1], "\\w$")) + 1)
# 
# | id|order_A |target_A |order_B |target_B |order_C |target_C |order_D |target_D |
# |--:|:-------|:--------|:-------|:--------|:-------|:--------|:-------|:--------|
# |  1|3-1     |3basic   |3-1     |3sub     |1-3     |3basic   |1-3     |3sub     |
# |  2|1-3     |3sub     |1-3     |3basic   |3-1     |3sub     |3-1     |3basic   |
# |  3|3-1     |3sub     |3-1     |3basic   |1-3     |3sub     |1-3     |3basic   |
# |  4|1-3     |3basic   |1-3     |3sub     |3-1     |3basic   |3-1     |3sub     |
# |  5|3-1     |3basic   |3-1     |3sub     |1-3     |3basic   |1-3     |3sub     |
# |  6|1-3     |3sub     |1-3     |3basic   |3-1     |3sub     |3-1     |3basic   |
# |  7|3-1     |3sub     |3-1     |3basic   |1-3     |3sub     |1-3     |3basic   |
# |  8|1-3     |3basic   |1-3     |3sub     |3-1     |3basic   |3-1     |3sub     |


item_design <- tibble(
  domain = setdiff(unique(img_tbl$domain), "planet"),
  label1 = nonsense_labels[c(TRUE, FALSE)],
  label2 = nonsense_labels[c(FALSE, TRUE)],
) %>%
  mutate(id = row_number())

group_designs <- cond_design %>% 
  left_join(item_design, by = "id") %>% 
  select(-id) %>% 
  group_split(group) %>%
  map_dfr(
    ~ .x %>%
      add_row(fillers[1, ], order = "1", number = "one", .after = 3) %>%
      add_row(fillers[2, ], order = "1", number = "one", .after = 7) %>%
      fill(group) %>%
      replace_na(list(target = "1sub"))
  )

## fill design with trial templates

category_dict <- img_tbl %>%
  distinct(domain, type, category) %>%
  filter(type %in% c("sub", "contrast")) %>%
  arrange(domain, desc(type))

gen_learn_set <- function(d, target, order) {
  if (str_detect(d, "^Filler")) {
    if (str_detect(d, "Shape")) {
      "Fillers/Shape/triangle.jpg"
    } else if (str_detect(d, "Color")) {
      "Fillers/Color/red-rect.jpg"
    }
  } else {
    cond_num <- str_extract(target, "^\\d")
    cond_type <- str_extract(target, "\\D+$")
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
# gen_learn_set("animal", "3basic", "3-1")

gen_test_set <- function(d) {
  if (str_detect(d, "^Filler")) {
    filler_dir <- paste0("image_stimuli/Fillers/", str_extract(d, "Color|Shape"), "/")
    if (str_detect(d, "Shape")) {
      c(
        "Fillers/Shape/lines-11.jpg", "Fillers/Shape/lines-13.jpg", "Fillers/Shape/triangle-5.jpg",
        "Fillers/Shape/lines-5.jpg", "Fillers/Shape/lines-3.jpg", "Fillers/Shape/lines-2.jpg",
        "Fillers/Shape/lines-9.jpg", "Fillers/Shape/lines-8.jpg", "Fillers/Shape/triangle-3.jpg",
        "Fillers/Shape/triangle-2.jpg", "Fillers/Shape/triangle-4.jpg", "Fillers/Shape/lines-7.jpg",
        "Fillers/Shape/lines-10.jpg", "Fillers/Shape/lines-6.jpg", "Fillers/Shape/lines-4.jpg",
        "Fillers/Shape/lines-1.jpg", "Fillers/Shape/triangle-1.jpg", "Fillers/Shape/lines-12.jpg"
      )
    } else if (str_detect(d, "Color")) {
      c(
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/green-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/blue-rect.jpg",
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/green-rect.jpg", "Fillers/Color/green-rect.jpg",
        "Fillers/Color/red-rect.jpg", "Fillers/Color/blue-rect.jpg", "Fillers/Color/blue-rect.jpg",
        "Fillers/Color/blue-rect.jpg", "Fillers/Color/red-rect.jpg", "Fillers/Color/red-rect.jpg"
      )
    }
  } else {
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
    learn_set = toJSON(gen_learn_set(domain, target, order)),
    test_set = toJSON(gen_test_set(domain))
  ) %>%
  ungroup()

trial_template_tbl <- trial_template_tbl %>% 
  rename(type = target) %>% 
  mutate(target = ifelse(order == "1-3", "label2", "label1"))

write_csv(trial_template_tbl, here::here("pcibex", "Experiment 3", "01_trial_templates.csv"))

