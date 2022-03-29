library(tidyverse)
library(flextable)

tribble(
  ~ type, ~"contrast", ~"no contrast", ~"unlabelled", ~"labelled",
  "narrow", "149 (74.5%)", "NA", "193 (56.1%)", "267 (77.7%)",
  "broad", "26 (13%)", "NA", "9 (2.6%)", "34 (9.9%)",
) %>% 
  rename_with(~ c(" ", "Contrast", "No Contrast", "Un-labelled", "Labelled")) %>% 
  flextable() %>% 
  add_header_row(
    values = c("", "Experiment 1", "Experiment 2"),
    colwidths = c(1, 2, 2)
  ) %>% 
  print(preview = "docx")

chisq.test(matrix(c(193, 9, 267, 34), byrow = FALSE, nrow = 2), simulate.p.value = TRUE)

chisq.test(matrix(c(193, 9, 267, 34), byrow = FALSE, nrow = 2), simulate.p.value = TRUE)

expt1_contrast <- c(149, 26)
expt2_unlabelled <- c(193, 9)
expt2_labelled <- c(267, 34)

pairwise_array <- list(
  "contrast vs. unlabelled"  = matrix(c(expt1_contrast, expt2_unlabelled), nrow = 2),
  "unlabelled vs. labelled" = matrix(c(expt2_unlabelled, expt2_labelled), nrow = 2),
  "contrast vs. labelled" = matrix(c(expt1_contrast, expt2_labelled), nrow = 2)
)

pairwise_chisq <- lapply(pairwise_array, chisq.test)

p.adjust(lapply(pairwise_chisq, `[[`, "p.value"), "bonferroni")
