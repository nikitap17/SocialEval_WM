
## Import libraries
library(readr)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lme4)
library(lmerTest)
library(BayesFactor)
library(tidyr)
library(robustlmm)
library(car)
library(emmeans)
library(modelbased)
library(rempsyc)
library(effectsize)
options(es.use_symbols = TRUE)
library(flextable)
library(parameters)


#### Sensitivity exclusion criteria:
# a) Outliers
out_diff_rt <- sapply(out_diff_rt$ID,as.character)
out_diff_err<- sapply(out_diff_err$ID,as.character)
out_diff_di<- sapply(out_diff_di$ID,as.character)
out_diff_eu<- sapply(out_diff_eu$ID,as.character)
out_diff_rat<- sapply(out_diff_ratio$ID,as.character)
# b) Male participants
excl_males <- c("588210","93756","069369","765713","017709")
# c) Failed manipulation check
excl_mcheck <- c("588210", "40710", "848371")


                          #### RT Analyses ####

## Outliers
agg_df1 <- subset(agg_df,!(ID %in% out_diff_rt))
main_df1 <- subset(main_df, !(ID %in% out_diff_rt))

aov <- anova_test(data = agg_df1, RT_Raw ~ CL*Stress + Error(ID/(CL+Stress)), type =3,
                  effect.size="pes")
get_anova_table(aov)

m4 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL+Stress + (CL+Stress|ID),
           data=main_df1, REML = T)
summary(m4)
# no sign. differences


## Exclude males
agg_df1 <- subset(agg_df,!(ID %in% excl_males))
main_df1 <- subset(main_df, !(ID %in% excl_males))

aov <- anova_test(data = agg_df1, RT_Raw ~ CL*Stress + Error(ID/(CL+Stress)), type =3,
                  effect.size="pes")
get_anova_table(aov)

m4 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL+Stress + (CL+Stress|ID),
           data=main_df1, REML = T)
summary(m4)
# no sign. differences


## Exclude manipulation check
agg_df1 <- subset(agg_df,!(ID %in% excl_mcheck))
main_df1 <- subset(main_df, !(ID %in% excl_mcheck))

aov <- anova_test(data = agg_df1, RT_Raw ~ CL*Stress + Error(ID/(CL+Stress)), type =3,
                  effect.size="pes")
get_anova_table(aov)

m4 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL+Stress + (CL+Stress|ID),
           data=main_df1, REML = T)
summary(m4)
# no sign. differences



                          #### Errors Analyses ####

## Outliers
experiment_data1 <- subset(experiment_data,!(ID %in% out_diff_err))

m4 <- glmer(Errors ~ block + trial_block + CL + Stress + (1|ID),
           data=experiment_data1, family = binomial)
summary(m4)
# no sign. differences


## Exclude males
experiment_data1 <- subset(experiment_data,!(ID %in% excl_males))

m4 <- glmer(Errors ~ block + trial_block + CL + Stress + (1|ID),
            data=experiment_data1, family = binomial)
summary(m4)
# no sign. differences


## Exclude manipulation check
experiment_data1 <- subset(experiment_data,!(ID %in% excl_mcheck))

m4 <- glmer(Errors ~ block + trial_block + CL + Stress + (1|ID),
            data=experiment_data1, family = binomial)
summary(m4)
# no sign. differences







