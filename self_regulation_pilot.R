#mechanisms of self-regulation pilot study

library(tidyverse)
library(haven)
library(lmerTest)
library(sjPlot)
library(effects)

theme_set(theme_light())

selfreg_raw <- read_spss("data/2021_04_24_project_F_pilot_data.sav")

selfreg <- 
  selfreg_raw %>% 
  rename(group = Group) %>% 
  mutate(group = case_when(group == 1 ~ "tDCS",
                           group == 2 ~ "EEG-Feedback",
                           group == 3 ~ "UMC") %>% 
                 fct_inorder())

glimpse(selfreg_raw)


# Data processing ---------------------------------------------------------
vsc_long <-
  selfreg %>%
  select(PPID1, group,
         T1_neutral_neutral_rt, T1_neutral_valid_rt, T3_neutral_neutral_rt, T3_neutral_valid_rt,
         T1_spec_rew_neutral_rt, T1_spec_rew_valid_rt, T3_spec_rew_neutral_rt, T3_spec_rew_valid_rt) %>% 
  pivot_longer(T1_neutral_neutral_rt:T3_spec_rew_valid_rt) %>% 
  mutate(name = str_replace(name, 
                            pattern = "spec_rew", 
                            replacement = "spec-rew")) %>% 
  extract(name, into = c("time", "condition", "cue"), regex = "(T.)_(.*)_(.*)_rt") %>% 
  # Remove outliers
  mutate(outlier = if_else(abs(scale(value)) > 3, TRUE, FALSE)) %>% 
  filter(!outlier)

ssrt_long <-
  selfreg %>% 
  select(PPID1, group, 
         T1_SSRT_neutral, T3_SSRT_neutral, T1_SSRT_spec_rew, T3_SSRT_spec_rew) %>% 
  pivot_longer(T1_SSRT_neutral:T3_SSRT_spec_rew) %>% 
  mutate(name = str_replace(name, 
                            pattern = "spec_rew", 
                            replacement = "spec-rew")) %>% 
  extract(name, into = c("time", "condition"), regex = "(T.)_SSRT_(.*)") %>% 
  # Remove outliers
  mutate(outlier = if_else(abs(scale(value)) > 3, TRUE, FALSE)) %>% 
  filter(!outlier)

# TODO: Calculate alpha power for F4 and F3, and make the comparison on those!
fea_long <-
  selfreg %>% 
  select(PPID1, group, 
         Ln_T1_F4_min_F3_EO, Ln_T2_F4_min_F3_NFT, Ln_T2_F4_min_F3_UMC, Ln_T3_F4_min_F3_EO) %>%
  pivot_longer(Ln_T1_F4_min_F3_EO:Ln_T3_F4_min_F3_EO, 
               values_drop_na = TRUE) %>% 
  extract(name, into = c("time", "task"), regex = "Ln_(T.)_F4_min_F3_(.*)") %>% 
  # Remove outliers
  mutate(outlier = if_else(abs(scale(value)) > 3, TRUE, FALSE)) %>% 
  filter(!outlier)

#  VSC Task --------------------------------------------------------------------
# Attentional bias as reflected in the difference in response time between neutral and validly cued targets
# 1) Neutral Condition: T1_neutral_neutral_rt, T1_neutral_valid_rt, T3_neutral_neutral_rt, T3_neutral_valid_rt  (neutral and validly cued targets in neutral condition) 

# 2) Reward Condition: T1_spec_rew_neutral_rt, T1_spec_rew_valid_rt, T3_spec_rew_neutral_rt, T3_spec_rew_valid_rt (neutral and validly cued targets in reward condition)

vsc_model <- lmer(value ~ time * group * condition * cue + (1 | PPID1), 
     data = vsc_long)

summary(vsc_model)

plot(allEffects(vsc_model))
tab_model(vsc_model)


# SSRT  ------------------------------------------------------------------------
# He said that just SSRT would be enough for both conditions.
# T1_SSRT_neutral, T3_SSRT_neutral, T1_SSRT_spec_rew, T3_SSRT_spec_rew (neutral and specific reward conditions)

ssrt_model <- lmer(value ~ time * group * condition + (1 | PPID1), 
                  data = ssrt_long)

summary(ssrt_model)

plot(allEffects(ssrt_model))
tab_model(ssrt_model)

ssrt_long %>% 
  group_by(group, condition, time) %>% 
  summarise(avg_rt = mean(value),
            se_rt = sd(value)/sqrt(n())) %>% 
  ggplot() + 
  aes(x = time, y = avg_rt, color = group, group = group,
      ymin = avg_rt - se_rt, ymax = avg_rt + se_rt) +
  geom_pointrange(size = 1.2, alpha = .6, 
                  position = position_dodge(width = .1)) +
  geom_line(size = 1.2, 
            alpha = .6,
            position = position_dodge(width = .1)) +
  facet_wrap(~condition)


# FEA --------------------------------------------------------------------------

fea_model <- lmer(value ~ time * group + (1 | PPID1), 
                   data = fea_long)

summary(fea_model)
tab_model(fea_model)

fea_long %>% 
  group_by(group, time) %>% 
  summarise(avg_fea = mean(value),
            se_fea = sd(value)/sqrt(n())) %>% 
  ggplot() + 
  aes(x = time, y = avg_fea, color = group, group = group,
      ymin = avg_fea - se_fea, ymax = avg_fea + se_fea) +
  geom_pointrange(size = 1.2, alpha = .6, 
                  position = position_dodge(width = .1)) +
  geom_line(size = 1.2, 
            alpha = .6,
            position = position_dodge(width = .1))

