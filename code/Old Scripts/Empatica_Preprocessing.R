library(tidyverse)
library(readr)
library(dplyr)
library(lme4)
library(ggplot2)


## The EDA df was prepared using the python script "Empatica_Processing"
EDA_df <- read_csv("Data/Physio_d/EDA_main_onesec.csv")
View(EDA_df)



## Remove Motion artefacts by linear regression

m1 <- lmer(EDA_zw ~ ACCx + ACCy + ACCz + (1|ID),
           data = EDA_df, REML=T)

m2 <- lmer(EDA_zw ~ ACCx + ACCy + ACCz + (1|ID) + (0 + ACCx + ACCy + ACCz|ID),
           data = EDA_df, REML=T)

m3 <- lmer(scale(EDA_zw) ~ ACCx + ACCy + ACCz + (ACCx + ACCy + ACCz|ID),
           data = EDA_df, REML=T)

m4 <- lm(scale(EDA_zw) ~ ACCx + ACCy + ACCz, data = EDA_df, REML=T)

AIC(m1,m2,m3,m4)[order(AIC(m1,m2, m3,m4)$AIC),]
BIC(m1,m2,m3,m4)[order(BIC(m1,m2, m3,m4)$BIC),]


EDA_p <- m2
EDA_df$EDA_pr <- residuals(EDA_p)

write.csv(EDA_df, "Data/Physio_d/EDA_motion_R.csv")




                    'Prepare data for exploratory analysis'


EDA_df <- read_csv("Data/Physio_d/EDA_main.csv")

EDA_df <- subset(EDA_df, EDA_df$condition != "resting" & EDA_df$condition !=  "break" )

EDA_df$CL <- ifelse(grepl("n2", EDA_df$condition)|grepl("2n", EDA_df$condition),
                    "2back", "3back")
EDA_df$Stress <- ifelse(grepl("stress", EDA_df$condition),"1", "0")

EDA_df$Block <- ifelse(grepl("b2", EDA_df$condition),"2", "1")

EDA_agg <- EDA_df %>%
  group_by(ID,CL,Stress) %>%
  summarise(across(c(EDA,EDA_zw), ~ mean(., na.rm = TRUE)))



