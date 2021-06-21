#mechanisms of self-regulation pilot study

library(foreign)
file.choose()
main_dataset <- read.spss("C:\\Users\\theot\\Desktop\\project f\\pilot_study\\pilot_study_analysis\\main_dataset.sav",
                          to.data.frame = TRUE)
library(tidyr)
library(dplyr)
df_perc_corr_inhibitions_neutral <- main_dataset %>%
  select(PPID1, Group, T1_perc_corr_inhibitions_neutral,
         T3_perc_corr_inhibitions_neutral)
df_perc_corr_inhibitions_neutral = tidyr::gather(df_perc_corr_inhibitions_neutral, key=time, value=score, 
                                                 T1_perc_corr_inhibitions_neutral:T3_perc_corr_inhibitions_neutral) %>% arrange(PPID1)
df_perc_corr_inhibitions_neutral$Group <- as.factor(df_perc_corr_inhibitions_neutral$Group) # Groups were integers, but turned to factors.

library(lme4)
library(lmerTest)
my_model <- lmerTest::lmer(score ~ time*Group + (1 | PPID1), data = df_perc_corr_inhibitions_neutral)

library(effects)
plot(allEffects(my_model))