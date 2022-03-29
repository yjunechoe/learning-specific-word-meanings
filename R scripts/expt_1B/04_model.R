library(tidyverse)
library(lme4)
library(lmerTest)
prop_table <- read_csv("R scripts/expt_1B/03_prop_table_HSP.csv") %>% 
  select(participant, group, labelled, target, item, coding) %>% 
  mutate(
    labelled = ifelse(labelled, "labelled", "unlabelled"),
    across(where(is.logical), as.integer),
    across(where(is.character), as.factor),
    basic_gen = as.integer(coding == "Basic")
  )

contrasts(prop_table$labelled) <- contr.sum(2)/2
contrasts(prop_table$target) <- contr.sum(2)/2

contrasts(prop_table$labelled)
contrasts(prop_table$target)

mdl_converging <- glmer(
  basic_gen ~ 1 + labelled * target +
    (1 + labelled | participant) +
    (1 + labelled || item),
  data = prop_table, family = binomial()
)

summary(mdl_converging)
parameters::model_parameters(mdl_converging)

broom.mixed::tidy(mdl_converging) %>% 
  filter(effect == "fixed") %>% 
  select(-effect, -group) %>% 
  mutate(
    p.value = scales::label_pvalue()(p.value),
    across(c(estimate, statistic), scales::label_number(0.1)),
    std.error = scales::label_number(0.1)(std.error),
    term = c("(Intercept)", "Label", "Order", "Label x Order"),
    estimate = glue::glue("{estimate} ({std.error})")
  ) %>% 
  select(-std.error) %>% 
  flextable::flextable() %>% 
  print(preview = "docx")
