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
cond_other_set <- c("in", "out")

group_A <- tibble(
  contrast = cond_contrast[c(1, 1, 2, 2)],
  other_set = cond_other_set[c(1, 2, 1, 2)],
  group = "A"
)
group_B <- tibble(
  contrast = cond_contrast[c(2, 2, 1, 1)],
  other_set = cond_other_set[c(2, 1, 2, 1)],
  group = "B"
)
group_C <- tibble(
  contrast = cond_contrast[c(1, 2, 1, 2)],
  other_set = cond_other_set[c(2, 2, 1, 1)],
  group = "C"
)
group_D <- tibble(
  contrast = cond_contrast[c(2, 1, 2, 1)],
  other_set = cond_other_set[c(1, 1, 2, 2)],
  group = "D"
)

group_designs <- bind_rows(group_A, group_B, group_C, group_D) %>% 
  group_by(group) %>% 
  slice(1:n(), 1:n()) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  left_join(nonsense_labels_df, by = "id") %>% 
  select(-id) %>% 
  group_split(group) %>%
  map_dfr(
    ~ .x %>%
      add_row(fillers[1, ], .after = 3) %>%
      add_row(fillers[2, ], .after = 7) %>%
      fill(group)
  ) %>% 
  replace_na(list(contrast = "single"))


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

gen_test_set <- function(d, other_set = c("in", "out")) {
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
    other_set <- match.arg(other_set)
    
    if (other_set == "out") {
      other_refs <- img_tbl %>% 
        filter(
          kind != unique(filter(img_tbl, domain == d)$kind) | domain == "planet",
          type == "sup"
        ) %>% 
        group_by(domain) %>% 
        slice_sample(n = 1) %>% 
        pull(path)
    } else if (other_set == "in") {
      other_refs <- img_tbl %>% 
        filter(domain == d, type == "sup") %>% 
        pull(path)
      if (d == "animal") {
        other_refs <- paste0("in-domain-alts/animal/animal-alt-duck", 1:5, ".jpg")
      }
    }
    domain_refs <- img_tbl %>% 
      filter(domain == d, !is.na(id)) %>% 
      filter(id <= 3) %>%
      pull(path, type)
    domain_refs_sampled_sub <- sample(domain_refs[names(domain_refs) == "sub"], 2)
    domain_refs_sampled_contrast <- sample(domain_refs[names(domain_refs) == "contrast"], 1)
    domain_refs_sampled_basic <- sample(domain_refs[names(domain_refs) == "basic"], 2)
    
    template <- character(10)
    template[c(sample(2:4, 1), 10)] <- domain_refs_sampled_sub
    template[c(5, 9)] <- domain_refs_sampled_basic
    template[7] <- domain_refs_sampled_contrast
    template[template == ""] <- sample(other_refs)
    
    template
  }
}

trial_template_tbl <- group_designs %>% 
  rowwise() %>% 
  mutate(
    learn_set = toJSON(gen_learn_set(domain, contrast)),
    test_set = toJSON(gen_test_set(domain, other_set))
  ) %>% 
  ungroup()

write_csv(trial_template_tbl, here::here("R Scripts", "expt_4", "01_trial_templates.csv"))
