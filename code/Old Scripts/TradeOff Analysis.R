library(tidyverse)
library(readr)
library(dplyr)
library(rempsyc)
library(flextable)
library(readr)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lme4)
library(lmerTest)
library(BayesFactor)
library(tidyr)
#library(robustlmm)
library(car)
library(emmeans)
library(modelbased)
library(rempsyc)
library(effectsize)
options(es.use_symbols = TRUE)
library(flextable)
library(parameters)



control_df <- agg_df[c("ID", "CL", "Stress", "RT_Raw", "Errors")] %>% filter(Stress==0)
stress_df <- agg_df[c("ID", "CL", "Stress", "RT_Raw", "Errors")] %>% filter(Stress==1)

new_df <- inner_join(control_df, stress_df, by=c('ID',"CL"))
new_df$diff_err <- new_df$Errors.y - new_df$Errors.x


# better performance
incl_list2back <- unique(new_df$ID[new_df$CL == "2back" & new_df$diff_err < 0])
incl_list3back <- unique(new_df$ID[new_df$CL == "3back" & new_df$diff_err < 0])
  
analys_df_2back <- subset(agg_df, CL == "2back" & ID %in% incl_list2back)
analys_df_3back <- subset(agg_df, CL == "3back" & ID %in% incl_list3back)
analys_df <- rbind(analys_df_2back,analys_df_3back)

m_better <- lmer(RT_Raw ~ CL+Stress + (CL|ID), data =analys_df, REML = T)  
summary(m_better)
effectsize(m_better)


# worse performance
incl_list2back <- unique(new_df$ID[new_df$CL == "2back" & new_df$diff_err > 0])
incl_list3back <- unique(new_df$ID[new_df$CL == "3back" & new_df$diff_err > 0])

analys_df_2back <- subset(agg_df, CL == "2back" & ID %in% incl_list2back)
analys_df_3back <- subset(agg_df, CL == "3back" & ID %in% incl_list3back)
analys_df <- rbind(analys_df_2back,analys_df_3back)

m_worse <- lmer(RT_Raw ~ CL+Stress + (CL|ID), data =analys_df, REML = T)  
summary(m_worse)
effectsize(m_worse)




###################
incl_list2back <- unique(new_df$ID[new_df$CL == "2back" & new_df$diff_err > 0])
incl_list3back <- unique(new_df$ID[new_df$CL == "3back" & new_df$diff_err > 0])

sum(incl_list2back %in% incl_list3back)


incl_list2back <- unique(new_df$ID[new_df$CL == "2back" & new_df$diff_err < 0])
incl_list3back <- unique(new_df$ID[new_df$CL == "3back" & new_df$diff_err < 0])

sum(incl_list2back %in% incl_list3back)



incl_list2back <- unique(new_df$ID[new_df$CL == "2back" & new_df$diff_err < 0])
incl_list3back <- unique(new_df$ID[new_df$CL == "3back" & new_df$diff_err > 0])

sum(incl_list2back %in% incl_list3back)


###################

list_id <- unique(agg_df$ID[agg_df$Mental < 4])
aggdf2 <- agg_df %>% filter(ID %in% list_id)




lmm <- lmer(RT_Raw ~ CL*Stress + (CL|ID), data =aggdf2, REML = T)  
summary(lmm)
effectsize(lmm)

lmm <- lmer(Errors ~ CL+Stress + (CL|ID), data =aggdf2, REML = T)  
summary(lmm)
effectsize(lmm)












  

