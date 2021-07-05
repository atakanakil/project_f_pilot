#mechanisms of self-regulation pilot study

library(tidyverse)
library(haven)
library(lmerTest)
library(sjPlot)
library(effects)

selfreg_raw <- read_spss("data/2021_04_24_project_F_pilot_data.sav")

glimpse(selfreg_raw)

selfreg_long <-
  selfreg_raw %>%
  select(PPID1,
         group = Group,
         T1_perc_corr_inhibitions_neutral,
         T3_perc_corr_inhibitions_neutral) %>% 
  pivot_longer(T1_perc_corr_inhibitions_neutral:T3_perc_corr_inhibitions_neutral, 
               
               names_to = "time",
               values_to = "score") %>% 
  arrange(PPID1) %>% 
  mutate(time = str_remove(time, "_perc_corr_inhibitions_neutral") %>% 
                as.factor(),
         group = case_when(group == 1 ~ "tDCS",
                           group == 2 ~ "EEG-Feedback",
                           group == 3 ~ "UMC") %>% 
                 fct_inorder())

# Groups were integers, but turned to factors.

pilot_model <- lmer(score ~ time * group + (1 | PPID1), 
                    data = selfreg_long)

tab_model(pilot_model, show.std = TRUE, show.est = FALSE)

plot(allEffects(pilot_model))
