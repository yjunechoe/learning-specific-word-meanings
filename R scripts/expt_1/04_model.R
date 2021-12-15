library(tidyverse)
library(lme4)
library(broom)

prop_table <- read_csv("R scripts/expt_1/03_prop_table.csv") %>% 
  select(participant, group, condition, item, basic_gen, strict_basic_gen) %>% 
  mutate(
    across(where(is.logical), as.integer),
    across(where(is.character), as.factor)
  )

contrasts(prop_table$condition) <- contr.sum(2)

table(prop_table$condition, prop_table$basic_gen)

summary(glm(basic_gen ~ 1 + condition, data = prop_table, family = binomial()))
plogis(-2)
plogis(0)
plogis(-4)
table(prop_table$condition, prop_table$basic_gen)[,2] / 200

full_mod <- glmer(
  basic_gen ~ 1 + condition +
    (1 | item) + (0 + condition | item) + (1 | participant) + (0 + condition | participant),
  data = prop_table, family = binomial()
)

library(afex)
afex::all_fit(full_mod) # bobyqa. : [OK]

full_mod_converge <- glmer(
  basic_gen ~ 1 + condition +
    (0 + condition || item) + (0 + condition || participant),
  data = prop_table, family = binomial(), control = glmerControl("bobyqa")
)

summary(full_mod_converge)
