
library(car)
library(readr)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lme4)
library(lmerTest) 
library(BayesFactor)



            "Checking whether participants performed above chance"

successes = c(30,33,34,35,36,37,38,39,40)
n=60
p=0.5
for (k in successes){
  print(paste0(k, ":  ",dbinom(k, n, p)))
}

#Accordingly, a participant performs above chance (= guessing),
#when obtaining at least 35 correct answers/ less than 25 errors across 2 blocks of
#one crossed condition, based on an alpha level of p = .05.

head(agg_df[order(agg_df$Errors, decreasing = T),])
## Highest number of errors = 22. Accordingly, no participant has to be excluded.



                            "Manipulation Checks"

      'CL'
### Participants should report more Mental demand (NASA-TLX) for high tasks,
### regardless of stress condition
                        
descriptives <- agg_df %>% group_by(Evaluation, CL) %>% 
  dplyr::summarise(M = mean(Mental,na.rm = TRUE), SD = sd(Mental,na.rm = TRUE), n = n()) %>% 
  dplyr::mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Mental <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))

ggplot(descriptives_Mental, aes(x = Evaluation, y = M, group = CL, color = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 0.5) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper), width=.2,
                position=position_dodge(0.15),linewidth=0.35,linetype=1)+
  labs(x = "Evaluation", y = "Menatl Demand", color = "CL") +
  theme_minimal()




##

"Temporal"

descriptives <- agg_df %>% group_by(Evaluation, CL) %>% 
  dplyr::summarise(M = mean(Temporal,na.rm = TRUE), SD = sd(Temporal,na.rm = TRUE), n = n()) %>% 
  dplyr::mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Temporal <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))

ggplot(descriptives_Temporal, aes(x = Evaluation, y = M, group = CL, color = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper), width=.2,
                position=position_dodge(0.15),linewidth=0.35,linetype=1)+
  labs(x = "Evaluation", y = "Temporal Demand", color = "CL") +
  theme_minimal()
##

"Effort"

descriptives <- agg_df %>% group_by(Evaluation, CL) %>% 
  dplyr::summarise(M = mean(Effort,na.rm = TRUE), SD = sd(Effort,na.rm = TRUE), n = n()) %>% 
  dplyr::mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Effort <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))

ggplot(descriptives_Effort, aes(x = Evaluation, y = M, group = CL, color = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper), width=.2,
                position=position_dodge(0.15),linewidth=0.35,linetype=1)+
  labs(x = "Evaluation", y = "Effort", color = "CL") +
  theme_minimal()
##


      'Social Evaluation Evaluation'

###
# After the experiment, participants were asked how they felt during the evaluation
# phase and whether they had questions. If they came up themselves with the fact
# that they were sure the observed people were not live, they did not pass the
# manipulation check. If they had doubts but were not sure, they were labelled
# "sceptical", but passed the check. Participants who were surprised by the fake
# observation also passed the check.
# In total, 33 participants passed the check (14 surprised and 19 sceptical).
# 10 participants did not pass the check.
      
# However, since one experimenter was in the same room "observing" the participants,
# participants nevertheless felt observed and stressed, which is visible in the
# physiological data. Thus, we conducted one analysis with the entire sample (N=43)
#and another analysis on the reduced sample (N=33). Subsequently, the results are compared.
      
      
      

      

                          #### Linear Model Assumptions ####
                          

                            ## Reacion Time ####
                          
'Outliers'
         
                                                 
##General
outlier <- agg_df %>% group_by(CL, Evaluation) %>% identify_outliers(RT_Raw)
data.frame(outlier[,c("ID","is.extreme")])
boxplot(RT_Raw ~ CL*Evaluation, data = agg_df,
        xlab = "CL x Evaluation", main = "Mean RT")
# One mild outlier


'Normality of Residuals'

main_df %>% group_by(Evaluation, CL) %>% shapiro_test(RT_Raw)
main_df %>% group_by(Evaluation, CL) %>% shapiro_test(RT_cw)
# violated

RT_mod <- lmer(scale(RT_Raw) ~ CL*Evaluation + (CL+Evaluation|ID), data= main_df, REML=T)
qqnorm(residuals(RT_mod))
qqline(residuals(RT_mod), col = 2)
hist(residuals(RT_mod), breaks=43)
# Looks fine




                            ## ERRORS ####

'Outliers'
                                  
##General
outlier <- agg_df %>% group_by(CL, Evaluation) %>% identify_outliers(Errors)
data.frame(outlier[,c("ID","is.extreme")])
boxplot(Errors ~ CL*Evaluation, data = agg_df,
        xlab = "CL x Evaluation", main = "Mean RT")
# 3 mild outlier



'Normality of Residuals'
ER_mod <- lmer(scale(Errors) ~ CL*Evaluation + (CL+Evaluation|ID), data= main_df, REML=T)

main_df %>% group_by(Evaluation, CL) %>% shapiro_test(Errors)
# violated

qqnorm(residuals(ER_mod))
qqline(residuals(ER_mod), col = 2)
hist(residuals(ER_mod), breaks=43)
# Distribution has a long tail to the right


                            ## EUSTRESS ####

'Outliers'

##General
outlier <- agg_df %>% group_by(CL, Evaluation) %>% identify_outliers(Eustress)
data.frame(outlier[,c("ID","is.extreme")])
boxplot(Eustress ~ CL*Evaluation, data = agg_df,
        xlab = "CL x Evaluation", main = "Mean Eustress")
# no outliers


'Normality of Residuals'
EU_mod <- lmer(scale(Eustress) ~ CL*Evaluation + (CL+Evaluation|ID), data= agg_df, REML=T)

agg_df %>% group_by(Evaluation, CL) %>% shapiro_test(Eustress)
# ok

qqnorm(residuals(EU_mod))
qqline(residuals(EU_mod), col = 2)
hist(residuals(EU_mod), breaks=43)
# Distribution is somewhat off on the left side




                            ## DISTRESS ####

'Outliers'

##General
outlier <- agg_df %>% group_by(CL, Evaluation) %>% identify_outliers(Distress)
data.frame(outlier[,c("ID","is.extreme")])
boxplot(Distress ~ CL*Evaluation, data = agg_df,
        xlab = "CL x Evaluation", main = "Mean Distress")
# no outliers


'Normality of Residuals'
DI_mod <- lmer(scale(Distress) ~ CL*Evaluation + (CL+Evaluation|ID), data= agg_df, REML=T)

agg_df %>% group_by(Evaluation, CL) %>% shapiro_test(Distress)
# violated

qqnorm(residuals(DI_mod))
qqline(residuals(DI_mod), col = 2)
hist(residuals(DI_mod), breaks=43)
# Distribution a bit patchy




                            ## Ratio ####

agg_df$ratio <- agg_df$Eustress/agg_df$Distress

'Outliers'

##General
outlier <- agg_df %>% group_by(CL, Evaluation) %>% identify_outliers(ratio)
data.frame(outlier[,c("ID","is.extreme")])
boxplot(ratio ~ CL*Evaluation, data = agg_df,
        xlab = "CL x Evaluation", main = "Mean Ratio")
# 5 mild outliers


'Normality of Residuals'
rat_mod <- lmer(scale(ratio) ~ CL*Evaluation + (CL+Evaluation|ID), data= agg_df, REML=T)

agg_df %>% group_by(Evaluation, CL) %>% shapiro_test(ratio)
# violated

qqnorm(residuals(rat_mod))
qqline(residuals(rat_mod), col = 2)
hist(residuals(rat_mod), breaks=43)
# Distribution looks fine




                      ### Paired Data Outliers ####

df1 <- agg_df[,c("ID","CL","Evaluation","Errors", "RT_Raw", "Eustress", "Distress", "ratio")]
df1 <- subset(df1, df1$Evaluation == 0)
colnames(df1) <- c("ID","CL", "Stress0",  "Errors0"  , "RT0", "Eustress0", "Distress0", "ratio0")

df2 <- agg_df[,c("ID","CL","Evaluation","Errors", "RT_Raw", "Eustress", "Distress", "ratio")]
df2 <- subset(df2, df2$Evaluation == 1)
colnames(df2) <- c("ID","CL",  "Stress1", "Errors1"  , "RT1", "Eustress1", "Distress1", "ratio1")

df <- merge(df1,df2, by= c("ID", "CL"))
df$err_diff <- df$Errors1-df$Errors0
df$RT_diff <- df$RT1- df$RT0
df$eu_diff <- df$Eustress1-df$Eustress0
df$di_diff <- df$Distress1-df$Distress0
df$rat_diff <- df$ratio1-df$ratio0


boxplot(err_diff ~ CL, data = df,
        xlab = "CL x Evaluation", main = "Errors")
outlier <- df %>% group_by(CL) %>% identify_outliers(err_diff)
out_diff_err <- data.frame(outlier[,c("ID","is.extreme")])
out_diff_err
write.csv(out_diff_err, "Data/clean_data/Outliers_errors.csv", row.names = F)
#No extreme outliers, value distribution seems to be adequate
exclude_list_err <- sapply(out_diff_err$ID,as.character)

boxplot(RT_diff ~ CL, data = df,
        xlab = "CL x Evaluation", main = "RT")
outlier <- df %>% group_by(CL) %>% identify_outliers(RT_diff)
out_diff_rt <- data.frame(outlier[,c("ID","is.extreme")])
out_diff_rt
write.csv(out_diff_err, "Data/clean_data/Outliers_RT.csv", row.names = F)
#No extreme outliers, value distribution seems to be adequate
exclude_list_RT <- sapply(out_diff_rt$ID,as.character)


boxplot(eu_diff ~ CL, data = df,
        xlab = "CL x Evaluation", main = "Eustress")
outlier <- df %>% group_by(CL) %>% identify_outliers(eu_diff)
out_diff_eu <- data.frame(outlier[,c("ID","is.extreme")])
out_diff_eu


boxplot(di_diff ~ CL, data = df,
        xlab = "CL x Evaluation", main = "Distress")
outlier <- df %>% group_by(CL) %>% identify_outliers(di_diff)
out_diff_di <- data.frame(outlier[,c("ID","is.extreme")])
out_diff_di


boxplot(rat_diff ~ CL, data = df,
        xlab = "CL x Evaluation", main = "Ratio")
outlier <- df %>% group_by(CL) %>% identify_outliers(rat_diff)
out_diff_rat <- data.frame(outlier[,c("ID","is.extreme")])
out_diff_rat


