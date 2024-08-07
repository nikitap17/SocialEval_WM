
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
library(car)
library(emmeans)
library(modelbased)
library(rempsyc)
library(effectsize)
options(es.use_symbols = TRUE)
library(flextable)
library(parameters)


## data preparation ------------------------------------------------------------
main_df <- read_csv("Data/clean_data/main_df.csv")
main_df$ID <- factor(main_df$ID)
main_df$Stress <- factor(main_df$Stress)
main_df$CL <- factor(main_df$CL)
main_df$Block <- factor(main_df$Block)
main_df$trial_block <- factor(main_df$trial_block)

agg_df <- read_csv("Data/clean_data/agg_df.csv")
agg_df$ID <- factor(agg_df$ID)
agg_df$Stress <- factor(agg_df$Stress)
agg_df$CL <- factor(agg_df$CL)

experiment_data <- read_csv("Data/clean_data/experiment_data.csv")
experiment_data$ID <- factor(experiment_data$ID)
experiment_data$Stress <- factor(experiment_data$Stress)
experiment_data$CL <- factor(experiment_data$CL)
experiment_data$block <- factor(experiment_data$block)
experiment_data$trial_block <- factor(experiment_data$trial_block)


## calculate the ratio score for eustress and distress
main_df$ratio <- main_df$Eustress/main_df$Distress
agg_df$ratio <- agg_df$Eustress/agg_df$Distress
experiment_data$ratio <- experiment_data$Eustress/experiment_data$Distress



                            #### ANALYSES for RT ####

                            
### Classic ANOVA

# SS type 3
aov <- anova_test(data = agg_df, RT_Raw ~ CL*Stress + Error(ID/(CL+Stress)), type =3,
                  effect.size="pes")
aov_table <- get_anova_table(aov)


# Second option for ANOVA
type3 <- list(CL = contr.sum, Stress = contr.sum)
m_aov3 <- lmer(RT_Raw ~ CL*Stress + (CL+Stress|ID),
               data = agg_df, contrasts = type3)

aov3 <- anova(m_aov3)
aov3
eta_squared(aov3,partial=T)
epsilon_aov3 <- epsilon_squared(aov3, partial=T, alternative = "two.sided")

##partial eta/omega/epsilon is appropriate, if all variables in a design are manipulated
##(Olejnik & Algina, 2003)##


# Main Effects, no interaction effects
aov_table <- aov_table[,1:5]
epsilon_aov3 <- epsilon_aov3 %>% dplyr::select(-"CI")

colnames(aov_table)[1] <- "Parameter"

aovRT <- as_tibble(inner_join(aov_table,epsilon_aov3, by= "Parameter"))
table <- nice_table(aovRT, 
                    title = c("Table X", "Two-Way Repeated-Measures ANOVA (Type III Sum of Squares)"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table

save_as_docx(table, path = "TablesFigures/Classic AOV RT.docx")

estimate_means(m_aov3, ~CL)
estimate_means(m_aov3, ~Stress)


### Linear Mixed Models

##
m1 <- lmer(scale(RT_Raw) ~ CL*Stress + (1|ID),
           data=main_df, REML = T)#, contrasts = type3)

m1a <- lmer(scale(RT_Raw) ~ CL*Stress + (1|ID) + (0 + CL|ID),
            data=main_df, REML = T)#, contrasts = type3)

m1b <- lmer(scale(RT_Raw) ~ CL*Stress + (1|ID) + (0 + Stress|ID),
            data=main_df, REML = T)#, contrasts = type3)

m1c <- lmer(scale(RT_Raw) ~ CL*Stress + (1|ID) + (0 + CL+Stress|ID),
            data=main_df, REML = T)#, contrasts = type3)

m1d <- lmer(scale(RT_Raw) ~ CL*Stress + (CL+Stress|ID),
           data=main_df, REML = T)#, contrasts = type3)

m1e <- lmer(scale(RT_Raw) ~ CL*Stress + (CL*Stress|ID),
           data=main_df, REML = T)#, contrasts = type3)

AIC(m1,m1a,m1b, m1c, m1d, m1e)[order(AIC(m1,m1a,m1b, m1c, m1d, m1e)[,"AIC"]),] # prefers m1e
BIC(m1,m1a,m1b, m1c, m1d, m1e)[order(BIC(m1,m1a,m1b, m1c, m1d, m1e)[,"BIC"]),] # prefers m1d
# We will continue with the less complex model m1d


# Interaction vs. no interaction
m1a <- lmer(scale(RT_Raw) ~ CL+Stress + (CL+Stress|ID),
            data=main_df, REML = T)
m1b <- lmer(scale(RT_Raw) ~ CL*Stress + (CL+Stress|ID),
            data=main_df, REML = T)
AIC(m1a)-AIC(m1b)
BIC(m1a)-BIC(m1b)
RT_mod_simple <- m1a


# Compare different configurations of Block And Timing Block

m2 <- lmer(scale(RT_Raw) ~ Block + CL+Stress + (CL+Stress|ID),
           data=main_df, REML = T)
m3 <- lmer(scale(RT_Raw) ~ trial_block + CL+Stress + (CL+Stress|ID),
           data=main_df, REML = T)
m4 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL+Stress + (CL+Stress|ID),
           data=main_df, REML = T)

m4a <- lmer(scale(RT_Raw) ~ Block + trial_block + CL+Stress + (Block+trial_block+CL+Stress|ID),
            data=main_df, REML = T)

AIC(RT_mod_simple,m2,m3,m4,m4a)[order(AIC(RT_mod_simple,m2,m3,m4,m4a)[,"AIC"]),]
BIC(RT_mod_simple,m2,m3,m4,m4a)[order(BIC(RT_mod_simple,m2,m3,m4,m4a)[,"BIC"]),]

RT_mod <- m4 # m4 preferred
AIC(RT_mod)-AIC(RT_mod_simple)
BIC(RT_mod)-BIC(RT_mod_simple)


###
m4aa <- lmer(scale(RT_Raw) ~ Block*Stress + trial_block*Stress + CL + (CL+Stress|ID),
             data=main_df, REML = T)
BIC(m4)-BIC(m4aa)
summary(m4aa)
###



## Get nice table fro LMM full
t_ <- model_parameters(RT_mod)[1:6,1:9] %>% dplyr::select(-"CI")

# Extract unstandardized Beta
Beta_m <- lmer(RT_Raw ~ Block + trial_block + CL+Stress + (CL+Stress|ID),
               data=main_df, REML = T)
t_Beta <- model_parameters(Beta_m)[1:6,] %>% dplyr::select("Parameter", "Coefficient")
colnames(t_Beta)[2] <- "Unstandardized B"
table <- cbind(t_Beta, t_)[,-3]

table <- nice_table(table, 
                    title = c("Table X", "No Interaction Linear Mixed Model"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table

save_as_docx(table, path = "TablesFigures/FUll LMM RT.docx")


## Visualisation of emm
emm <- estimate_means(RT_mod, ~CL*Stress)

ggplot(emm, aes(x = Stress, y = Mean, group = factor(CL), color=factor(CL),shape=CL, linetype = factor(CL))) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Evaluation", y = "Reaction Time (s)",
       color = "Cognitive Load", shape="Cognitive Load", linetype="Cognitive Load") +
  
  scale_x_discrete(labels = c("control", "evaluation"))+
  
  theme_classic() +

  scale_shape_manual(values = c(15, 19),
                     labels = c("low", "high")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234"),
                     labels = c("low", "high")) +  # Orange color for H0 and dark green for H1
  scale_linetype_manual(values = c(1,2),
                       labels = c("low", "high")) +
  
  #theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15))

ggsave("TablesFigures/EMM RT Stress.jpeg",dpi=300,width = 4, height = 3)

# stress and trial block
mb <- lmer(scale(RT_Raw) ~ Block*trial_block*Stress + CL + (CL+Stress|ID),
           data=main_df, REML = T)
summary(mb)
emm_eu <- estimate_means(mb, ~Stress*trial_block)



ggplot(emm_eu, aes(x = trial_block, y = Mean, group = Stress, shape= Stress, color = Stress,
                   linetype= Stress)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Timing Blocks", y = "Reaction Time (s)",
       color = "Social Evaluation", shape="Social Evaluation", linetype= "Social Evaluation") +
  
  scale_x_discrete(labels = c("1-10 Trials", "10-20 Trials", "20-30 Trials")) +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("control", "evaluation")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("deepskyblue4", "darkorange"),
                     labels = c("control", "evaluation")) +
  scale_linetype_manual(values = c(1,2),
                        labels = c("control", "evaluation")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15))


ggsave("TablesFigures/EMM RT Stress and trial block.jpeg",dpi=300,width = 4, height = 3)



#block and trial_block
emm_eu <- estimate_means(mb, ~Block*trial_block)

ggplot(emm_eu, aes(x = trial_block, y = Mean, group = Block, shape= Block, color = Block,
                   linetype=Block)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Timing Blocks", y = "Reaction Time (s)",
       color = "Block", shape="Block", linetype = "Block") +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("first", "second")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("deepskyblue4", "darkorange"),
                     labels = c("first", "second")) +
  scale_linetype_manual(values = c(1,2),
                        labels = c("first", "second")) +
  

  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("1-10 Trials", "10-20 Trials", "20-30 Trials"))

ggsave("TablesFigures/EMM RT block and trial block.jpeg",dpi=300,width = 4, height = 3)





### Bayesian evidence accumulation RT ####

bf_df <- subset(main_df, is.na(RT_Raw) == F)

  ## Interaction versus no Interaction

h0 = "No interaction model"
h1 = "Interaction model"
s_size = seq(8, 43, by = 7)
bf_collect <- data.frame(sample_size = c(s_size,s_size),
                         Hypothesis = c(rep(h0,length(s_size)), rep(h1,length(s_size))))
permutations <- as.character(seq(1,100))
participants <- unique (bf_df$ID)

for (p in permutations){
  
  bfsH0 = c()
  bfsH1 = c()
  participants <- sample(participants)
  for (n in s_size){
    bf0 = lmBF(RT_Raw ~ Block + trial_block + Stress+CL + ID, data = subset(bf_df, ID %in% participants[1:n]), 
               whichRandom="ID")
    bf1 = lmBF(RT_Raw ~ Block + trial_block + Stress*CL + ID, data = subset(bf_df, ID %in% participants[1:n]), 
               whichRandom="ID")
    
    bfsH0 = c(bfsH0, log10(as.numeric(as.vector(bf0/bf1))))
    bfsH1 = c(bfsH1, log10(as.numeric(as.vector(bf1/bf0))))
  }
  cln <- colnames(bf_collect)
  bf_collect$p <- c(bfsH0,bfsH1)
  colnames(bf_collect) <- c(cln,p)
}

bf_collect1 <- bf_collect %>%
  mutate(logmean = rowMeans(bf_collect[,3:length(bf_collect)]),
         logsd = apply((bf_collect[,3:length(bf_collect)]), 1, sd))

ggplot(bf_collect1, aes(x = sample_size, y = logmean, group = Hypothesis,
                       shape = Hypothesis, color = Hypothesis, fill = Hypothesis)) +
  geom_point(size = 2) +  
  geom_line(size = 0.5) +
  
  geom_ribbon(aes(ymin = logmean-logsd, ymax = logmean+logsd), alpha = 0.3, color=NA) +
  
  labs(x = "Sample Size", y = "log Bayes Factors") +
  scale_shape_manual(values = c(23, 19)) +  # Use square for H0 and circle for H1
  
  scale_color_manual(values = c("#ffc100","#008080")) +
  scale_fill_manual(values = c("#ffc100","#008080")) +
  
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_continuous(breaks = bf_collect$sample_size)

ggsave("TablesFigures/BF_updates int vs no int.jpeg",dpi=300,width = 4, height = 3)


#   ## Versus real H0
# 
# h0 = "No interaction model"
# h1 = "Intercept only model"
# s_size = seq(8, 43, by = 7)
# bf_collect <- data.frame(sample_size = c(s_size,s_size),
#                          Hypothesis = c(rep(h0,length(s_size)), rep(h1,length(s_size))))
# permutations <- as.character(seq(1,100))
# participants <- unique (bf_df$ID)
# 
# for (p in permutations){
#   
#   bfsH0 = c()
#   bfsH1 = c()
#   participants <- sample(participants)
#   for (n in s_size){
#     bf0 = lmBF(RT_Raw ~ Block + trial_block + Stress+CL + ID, data = subset(bf_df, ID %in% participants[1:n]))
#     bf1 = as.numeric(as.vector(bf1))^(-1)
#     
#     bfsH0 = c(bfsH0, log10(bf0))
#     bfsH1 = c(bfsH1, log10(as.numeric(as.vector(bf1))))
#   }
#   cln <- colnames(bf_collect)
#   bf_collect$p <- c(bfsH0,bfsH1)
#   colnames(bf_collect) <- c(cln,p)
# }
# 
# bf_collect2 <- bf_collect
# bf_collect2$mean <- rowMeans(bf_collect[,3:length(bf_collect)])
# bf_collect2$sd <- apply((bf_collect[,3:length(bf_collect)]), 1, sd)
# 
# ggplot(bf_collect2, aes(x = sample_size, y = mean, group = Hypothesis,
#                        shape = Hypothesis, color = Hypothesis, fill = Hypothesis)) +
#   geom_point(size = 2) +  
#   geom_line(size = 0.5) +
#   
#   geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.2, color=NA) +
#   
#   labs(x = "Sample Size", y = "log Bayes Factors") +
#   scale_shape_manual(values = c(17, 19)) +  # Use square for H0 and circle for H1
#   
#   scale_color_manual(values = c("#E34234","#008080")) +  # Orange color for H0 and dark green for H1
#   scale_fill_manual(values = c("#E34234","#008080")) +
#   
#   theme_classic() +
#   theme(legend.title = element_blank(),
#         legend.position = "top",
#         legend.text = element_text(size = 8),
#         axis.title.x = element_text(vjust = -3),
#         axis.title.y = element_text(vjust = 3),
#         plot.margin = margin(b = 15, l=15)) +
#   
#   scale_x_continuous(breaks = bf_collect$sample_size)
# 
# ggsave("TablesFigures/BF_updates h0 vs no int.jpeg",dpi=300,width = 4, height = 3)


  ## CL versus CL and Stress

h0 = "No interaction model"
h1 = "CL model"
s_size = seq(8, 43, by = 7)
bf_collect <- data.frame(sample_size = c(s_size,s_size),
                         Hypothesis = c(rep(h0,length(s_size)), rep(h1,length(s_size))))
permutations <- as.character(seq(1,100))
participants <- unique (bf_df$ID)

for (p in permutations){
  
  bfsH0 = c()
  bfsH1 = c()
  participants <- sample(participants)
  for (n in s_size){
    bf1 = lmBF(RT_Raw ~ Block + trial_block + CL + ID, data = subset(bf_df, ID %in% participants[1:n]), 
               whichRandom="ID")
    bf0 = lmBF(RT_Raw ~ Block + trial_block + Stress+CL + ID, data = subset(bf_df, ID %in% participants[1:n]), 
               whichRandom="ID")
    
    bfsH0 = c(bfsH0, log10(as.numeric(as.vector(bf0/bf1))))
    bfsH1 = c(bfsH1, log10(as.numeric(as.vector(bf1/bf0))))
  }
  cln <- colnames(bf_collect)
  bf_collect$p <- c(bfsH0,bfsH1)
  colnames(bf_collect) <- c(cln,p)
}

bf_collect3 <- bf_collect %>%
  mutate(logmean = rowMeans(bf_collect[,3:length(bf_collect)]),
         logsd = apply((bf_collect[,3:length(bf_collect)]), 1, sd))

ggplot(bf_collect3, aes(x = sample_size, y = logmean, group = Hypothesis,
                       shape = Hypothesis, color = Hypothesis, fill = Hypothesis)) +
  geom_point(size = 2) +  
  geom_line(size = 0.5) +
  
  geom_ribbon(aes(ymin = logmean-logsd, ymax = logmean+logsd), alpha = 0.3, color=NA) +
  
  labs(x = "Sample Size", y = "log Bayes Factors") +
  scale_shape_manual(values = c(15, 19)) +
  
  scale_color_manual(values = c("#E34234","#008080")) +  # Orange color for H0 and dark green for H1
  scale_fill_manual(values = c("#E34234","#008080")) +
  
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_continuous(breaks = bf_collect$sample_size)+
  scale_y_continuous(breaks=c(-4,-3,-2,-1,0,1,2,3,4))

ggsave("TablesFigures/BF_updates CL vs CL+Stress.jpeg",dpi=300,width = 4, height = 3)









                            #### ERRORS ANALYSES ####

## Generalized Linear Model with binomial distribution ####

m1 <- glmer(Errors ~ CL*Stress + (1|ID),
           data=experiment_data, family = binomial,contrasts = type3)

m2 <- glmer(Errors ~ CL*Stress + (Stress|ID),
           data=experiment_data, family = binomial, contrasts = type3)

m3 <- glmer(Errors ~ CL*Stress + (CL+Stress|ID),
            data=experiment_data, family = binomial, contrasts = type3)

m4 <- glmer(Errors ~ CL*Stress + (1|ID) + (0 + Stress|ID),
            data=experiment_data, family = binomial, contrasts = type3)


AIC(m1,m2,m3,m4)[order(AIC(m1,m2,m3,m4)[,"AIC"]),]
BIC(m1,m2,m3,m4)[order(BIC(m1,m2,m3,m4)[,"BIC"]),]
# continue with less complex random structure
summary(m3)
model_parameters(m1)[4,]

## Compare interaction to no interaction model
m1a <- glmer(Errors ~ CL+Stress + (1|ID),
             data=experiment_data, family = binomial)
AIC(m1a)-AIC(m1)
BIC(m1a)-BIC(m1)

errors_m_simple <- m1a

table <- model_parameters(errors_m_simple)[1:3,1:9] %>% dplyr::select(-c("CI","df_error"))
table$OR <- exp(table$Coefficient)
table$ORciL <- exp(table$CI_low)
table$ORciH <- exp(table$CI_high)
table <- table[,c(1,2,3,4,5,8,9,10,7)]

table <- nice_table(table, 
                    title = c("Table X", "Errors - No Interaction Generalized Linear Mixed Model - Simple"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table

save_as_docx(table, path = "TablesFigures/simple GLMM Errors.docx")



## Include Block and Trial Blocks

m3 <- glmer(Errors ~ block + CL+Stress + (1|ID),
           data=experiment_data, family = binomial)

m4 <- glmer(Errors ~ trial_block + CL+Stress + (1|ID),
           data=experiment_data, family = binomial)

m5 <- glmer(Errors ~ block + trial_block + CL+Stress + (1|ID),
           data=experiment_data, family = binomial)


AIC(m3,m4,m5)[order(AIC(m3, m4, m5)$AIC),]
BIC(m3,m4,m5)[order(BIC(m3, m4, m5)$BIC),]
# model m5

errors_m <- m5
summary(errors_m)
car::Anova(errors_m, type=2)

## nice table
table <- model_parameters(errors_m)[1:6,1:9] %>% dplyr::select(-c("CI","df_error"))
table$OR <- exp(table$Coefficient)
table$ORciL <- exp(table$CI_low)
table$ORciH <- exp(table$CI_high)
table <- table[,c(1,2,3,4,5,8,9,10,7)]

table <- nice_table(table, 
                    title = c("Table X", "Errors - No-Interaction Generalized Linear Mixed Model - Full"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table

save_as_docx(table, path = "TablesFigures/Full GLMM Errors.docx")


## simple vs full model
AIC(errors_m)-AIC(errors_m_simple)
BIC(errors_m)-BIC(errors_m_simple)


### Stress * timing block
m5aa <- glmer(Errors ~ block*trial_block*Stress + CL + (1|ID),
            data=experiment_data, family = binomial)

emm_eu <- estimate_means(m5aa, ~Stress*trial_block)

ggplot(emm_eu, aes(x = trial_block, y = Probability, group = Stress, shape= Stress, color = Stress,
                   linetype= Stress)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Timing Blocks", y = "Error Probability",
       color = "Social Evaluation", shape="Social Evaluation", linetype= "Social Evaluation") +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("control", "evaluation")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("deepskyblue4", "darkorange"),
                     labels = c("control", "evaluation")) +
  scale_linetype_manual(values = c(1,2),
                        labels = c("control", "evaluation")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("1-10 Trials", "10-20 Trials", "20-30 Trials"))

ggsave("TablesFigures/EMM Errors Stress and trial block.jpeg",dpi=300,width = 4, height = 3)



## Block and timing block
emm_eu <- estimate_means(m5aa, ~ block*trial_block)

ggplot(emm_eu, aes(x = trial_block, y = Probability, group = block, shape= block, color = block,
                   linetype=block)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Timing Blocks", y = "Error Probability",
       color = "Block", shape="Block", linetype="Block") +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("first", "second")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("deepskyblue4", "darkorange"),
                     labels = c("first", "second")) +
  scale_linetype_manual(values = c(1,2),
                        labels = c("first", "second")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("1-10 Trials", "10-20 Trials", "20-30 Trials"))

ggsave("TablesFigures/EMM Errors block and trial block.jpeg",dpi=300,width = 4, height = 3)





### Bayesian evidence accumulation Errors ####

s_size = seq(8, 43, by = 7)

bf_collect_er1 <- data.frame(sample_size = c(s_size,s_size),
                         Hypothesis = c(rep("No-interaction model",length(s_size)),
                                        rep("Interaction model",length(s_size))))
bf_collect_er2 <- data.frame(sample_size = c(s_size,s_size),
                             Hypothesis = c(rep("No-interaction model",length(s_size)),
                                            rep("Intercept-only model",length(s_size))))
bf_collect_er3 <- data.frame(sample_size = c(s_size,s_size),
                             Hypothesis = c(rep("No-interaction model",length(s_size)),
                                            rep("CL model",length(s_size))))

permutations <- as.character(seq(1,10))
participants <- unique (experiment_data$ID)

for (p in permutations){
  
  # comparison no int vs. int
  no.int1 <- c()
  int = c()
  # comparison no int vs. intercept
  no.int2 = c()
  null <- c()
  # comparison no int vs. cl
  no.int3 <- c()
  cl <- c()
  
  participants <- sample(participants)
  
  for (n in s_size){
    
    # int_m <- BIC(glmer(Errors ~ block + trial_block + CL*Stress + (1|ID),
    #                data=subset(experiment_data, ID %in% participants[1:n]), family = binomial))
    no.int_m <- BIC(glmer(Errors ~ block + trial_block + CL+Stress + (1|ID),
                      data=subset(experiment_data, ID %in% participants[1:n]), family = binomial))
    # null_m <- BIC(glmer(Errors ~ 1 + (1|ID),
    #                 data=subset(experiment_data, ID %in% participants[1:n]), family = binomial))
    cl_m <- BIC(glmer(Errors ~ block + trial_block + CL + (1|ID),
                  data=subset(experiment_data, ID %in% participants[1:n]), family = binomial))
    
    # approximation BF == exp( (BIC(denominator m) -  BIC(nominator m)) /2 )
    
    # comparison no int vs. int
    #no.int1 <- c(no.int1, log10(exp((int_m-no.int_m)/2)))
    #int <- c(int, log10(exp((no.int_m-int_m)/2)))
    # comparison no int vs. intercept
    #no.int2 = c(no.int2, log10(exp((null_m-no.int_m)/2)))
    #null <- c(null, log10(exp((no.int_m-null_m)/2)))
    # comparison no int vs. cl
    no.int3 <- c(no.int3, log10(exp((cl_m-no.int_m)/2)))
    cl <- c(cl, log10(exp((no.int_m-cl_m)/2)))
    
    
  }
  
  # cln <- colnames(bf_collect_er1)
  # bf_collect_er1$p <- c(no.int1,int)
  # colnames(bf_collect_er1) <- c(cln,p)
  # 
  # cln <- colnames(bf_collect_er2)
  # bf_collect_er2$p <- c(no.int2,null)
  # colnames(bf_collect_er2) <- c(cln,p)
  
  cln <- colnames(bf_collect_er3)
  bf_collect_er3$p <- c(no.int3,cl)
  colnames(bf_collect_er3) <- c(cln,p)
}


## no interaction vs. interaction
bf_collect_er1 <- bf_collect_er1 %>%
  mutate(logmean = rowMeans(bf_collect_er1[,3:length(bf_collect_er1)]),
         logsd = apply((bf_collect_er1[,3:length(bf_collect_er1)]), 1, sd))
#bf_collect_er1$logsd <- replace(bf_collect_er1, bf_collect_er1$logsd== -Inf,0)

ggplot(bf_collect_er1, aes(x = sample_size, y = logmean, group = Hypothesis,
                        shape = Hypothesis, color = Hypothesis, fill = Hypothesis)) +
  geom_point(size = 2) +  
  geom_line(size = 0.5) +
  
  geom_ribbon(aes(ymin = logmean-logsd, ymax = logmean+logsd), alpha = 0.3, color=NA) +
  
  labs(x = "Sample Size", y = "log Bayes Factors") +
  scale_shape_manual(values = c(23, 19)) +  # Use square for H0 and circle for H1
  
  scale_color_manual(values = c("#ffc100","#008080")) +
  scale_fill_manual(values = c("#ffc100","#008080")) + # Orange color for H0 and dark green for H1
  
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_continuous(breaks = bf_collect$sample_size)

ggsave("TablesFigures/BF_updates ER int vs no int.jpeg",dpi=300,width = 4, height = 3)



## no interaction vs. intercept only
bf_collect_er2 <- bf_collect_er2 %>%
  mutate(logmean = rowMeans(bf_collect_er2[,3:length(bf_collect_er2)]),
         logsd = apply((bf_collect_er2[,3:length(bf_collect_er2)]), 1, sd))

ggplot(bf_collect_er2, aes(x = sample_size, y = logmean, group = Hypothesis,
                           shape = Hypothesis, color = Hypothesis, fill = Hypothesis)) +
  geom_point(size = 2) +  
  geom_line(size = 0.5) +
  
  geom_ribbon(aes(ymin = logmean-logsd, ymax = logmean+logsd), alpha = 0.3, color=NA) +
  
  labs(x = "Sample Size", y = "log Bayes Factors") +
  scale_shape_manual(values = c(17, 15)) +  # Use square for H0 and circle for H1
  
  scale_color_manual(values = c("#E34234","#008080")) +
  scale_fill_manual(values = c("#E34234","#008080")) +# Orange color for H0 and dark green for H1
  
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_continuous(breaks = bf_collect$sample_size)

ggsave("TablesFigures/BF_updates ER intercept vs no int.jpeg",dpi=300,width = 4, height = 3)


## no interaction vs. CL model
bf_collect_er3 <- bf_collect_er3 %>%
  mutate(logmean = rowMeans(bf_collect_er3[,3:length(bf_collect_er3)]),
         logsd = apply((bf_collect_er3[,3:length(bf_collect_er3)]), 1, sd))

ggplot(bf_collect_er3, aes(x = sample_size, y = logmean, group = Hypothesis,
                           shape = Hypothesis, color = Hypothesis, fill = Hypothesis)) +
  geom_point(size = 2) +  
  geom_line(size = 0.5) +
  
  geom_ribbon(aes(ymin = logmean-logsd, ymax = logmean+logsd), alpha = 0.3, color=NA) +
  
  labs(x = "Sample Size", y = "log Bayes Factors") +
  scale_shape_manual(values = c(15, 19)) +  # Use square for H0 and circle for H1
  
  scale_color_manual(values = c("darkorange","#008080")) +
  scale_fill_manual(values = c("darkorange","#008080")) +# Orange color for H0 and dark green for H1
  
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_continuous(breaks = bf_collect_er3$sample_size)+
  scale_y_continuous(breaks = c(-2,-1,-0.5,0,0.5,1,2))

ggsave("TablesFigures/BF_updates ER cl vs no int.jpeg",dpi=300,width = 4, height = 3)



### Visualisation ####

# Stress  
emm <- estimate_means(errors_m, ~Stress)

ggplot(emm, aes(x = Stress, y = Probability, group = Stress)) +
  geom_point(size = 2.5, color = "azure4") +
  geom_line(aes(group = 0),size = 0.5, color = "azure4") +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.15),linewidth=0.35,linetype=1, color = "azure4")+
  labs(x = "Stress", y = "Errors") +
  
  labs(x = "Social Stress", y = "Error Probability") +
  #scale_shape_manual(values = c(15, 19)) +  # Use square for H0 and circle for H1
  
  #scale_color_manual(values = c("orange", "darkgreen")) +  # Orange color for H0 and dark green for H1
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("Control", "Stress"))

ggsave("TablesFigures/EMM Errors GLMM.jpeg",dpi=300,width = 4, height = 3)


#Stress*CL
emm_eu <- estimate_means(errors_m, ~CL*Stress)

ggplot(emm_eu, aes(x = Stress, y = Probability, group = CL, shape= CL, color = CL,
                   linetype = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Evaluation", y = "Error Probability",
       color = "Cognitive Load", shape="Cognitive Load", linetype= "Cognitive Load") +
  
  scale_x_discrete(labels = c("Control", "Evaluation")) +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("low", "high")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234"),
                     labels = c("low", "high")) +  # Orange color for H0 and dark green for H1
  scale_linetype_manual(values=c(1,2),
                        labels=c("low","high")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15))
  

ggsave("TablesFigures/EMM Errors interaction GLMM.jpeg",dpi=300,width = 4, height = 3)


# block and trial block
mb <- glmer(Errors ~ block*trial_block + CL+Stress + (1|ID),
            data=experiment_data, family = binomial)
emm_eu <- estimate_means(mb, ~block*trial_block)

ggplot(emm_eu, aes(x = trial_block, y = Probability, group = block, shape= block, color = block)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Timing Blocks", y = "Error Probability",
       color = "Block", shape="Block") +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("first", "second")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("deepskyblue4", "darkorange"),
                     labels = c("first", "second")) +  # Orange color for H0 and dark green for H1
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("1-10 Trials", "10-20 Trials", "20-30 Trials"))

ggsave("TablesFigures/EMM Errors block and trial block.jpeg",dpi=300,width = 4, height = 3)



## Compare ERRORS, cER, and oER descriptively/ visually ####


#cER
cER_m <- glmer(cER ~ block + trial_block + CL+Stress + (1|ID),
               data=experiment_data, family = binomial)
summary(cER_m)
emm <- estimate_means(cER_m, ~CL*Stress)

ggplot(emm, aes(x = Stress, y = Probability, group = CL, shape= CL, color = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Stress", y = "Error Probability", color = "CL") +
  
  scale_shape_manual(values = c(15, 19)) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234")) +  # Orange color for H0 and dark green for H1
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("Control", "Stress"))


#oER
oER_m <- glmer(oER ~ block + trial_block + CL+Stress + (1|ID),
            data=experiment_data, family = binomial)
summary(oER_m)
emm <- estimate_means(oER_m, ~CL*Stress)

ggplot(emm, aes(x = Stress, y = Probability, group = CL, shape= CL, color = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Stress", y = "Error Probability", color = "CL") +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("low", "high")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234"),
                     labels = c("low", "high")) +  # Orange color for H0 and dark green for H1
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("Control", "Stress"))


## Same direction of effects









                            #### Perceived stress ####

## Relation to performance and manipulation ####

## Reaction Time
m1 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL + Stress + scale(Eustress) + (CL + Stress|ID),
           data=main_df, REML = T)
m2 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL + Stress + scale(Distress) + (CL + Stress|ID),
           data=main_df, REML = T)
m3 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL + Stress + scale(ratio) + (CL + Stress|ID),
           data=main_df, REML = T)
m4 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL + Stress + scale(Eustress) + scale(Distress) + (CL + Stress|ID),
           data=main_df, REML = T)
m5 <- lmer(scale(RT_Raw) ~ Block + trial_block + CL + Stress + scale(Eustress)*scale(Distress) + (CL + Stress|ID),
           data=main_df, REML = T)

AIC(m1,m2,m3,m4,m5)[order(AIC(m1,m2,m3,m4,m5)$AIC),] #m3
BIC(m1,m2,m3,m4,m5)[order(BIC(m1,m2,m3,m4,m5)$BIC),] #m3

perc.stress.m <- m1
summary(perc.stress.m)
model_parameters(perc.stress.m)
summary(lmer(RT_Raw ~ Block + trial_block + CL + Stress + Eustress + (CL + Stress|ID),
           data=main_df, REML = T))




## Errors
m1 <- glmer(Errors ~ block + trial_block + CL + scale(Eustress) + (1|ID),
           data=experiment_data, family = binomial)
m2 <- glmer(Errors ~ block + trial_block + CL + scale(Distress) + (1|ID),
           data=experiment_data, family = binomial)
m3 <- glmer(Errors ~ block + trial_block + CL + scale(ratio) + (1|ID),
           data=experiment_data, family = binomial)
m4 <- glmer(Errors ~ block + trial_block + CL + scale(Eustress) + scale(Distress) + (1|ID),
           data=experiment_data, family = binomial)
m5 <- glmer(Errors ~ block + trial_block + CL + scale(Eustress)*scale(Distress) + (1|ID),
           data=experiment_data, family = binomial)

AIC(m1,m2,m3,m4,m5)[order(AIC(m1,m2,m3,m4,m5)$AIC),] #m3
BIC(m1,m2,m3,m4,m5)[order(BIC(m1,m2,m3,m4,m5)$BIC),] #m3

ratio_er_m <- m3
summary(ratio_er_m)
model_parameters(ratio_er_m)
model_parameters(glmer(Errors ~ block + trial_block + CL + ratio + (1|ID),
                       data=experiment_data, family = binomial))



### Differences between conditions ####
m_eustress1 <- lmer(Eustress ~ CL*Stress + (CL + Stress|ID),
           data=main_df, REML = T)
m_eustress2 <- lmer(Eustress ~ CL+Stress + (CL + Stress|ID),
                    data=main_df, REML = T)

AIC(m_eustress1) - AIC(m_eustress2)
summary(m_eustress1)
effectsize(m_eustress1)

estimate_means(m_eustress1, ~CL*Stress)




m_distress1 <- lmer(Distress ~ CL*Stress + (CL + Stress|ID),
                   data=main_df, REML = T)
m_distress2 <- lmer(Distress ~ CL+Stress + (CL + Stress|ID),
                    data=main_df, REML = T)

AIC(m_distress1) - AIC(m_distress2)
summary(m_distress1)
effectsize(m_distress1)

estimate_means(m_distress1, ~CL*Stress)




m_rat1 <- lmer(ratio ~ CL*Stress + (CL + Stress|ID),
                    data=main_df, REML = T)
m_rat2 <- lmer(ratio ~ CL+Stress + (CL + Stress|ID),
                    data=main_df, REML = T)

AIC(m_rat1) - AIC(m_rat2)
summary(m_rat1)
effectsize(m_rat1)

estimate_means(m_rat1, ~CL*Stress)



                        #### EXPLORATION ####

### RT and Errors exploration ####

df_perceived <- agg_df[,c("ID","CL","Stress","RT_Raw","Errors")]

wide_df <- pivot_wider(df_perceived, id_cols = c(ID,CL), names_from = Stress,
                          values_from = c(RT_Raw,Errors))

wide_df <- subset(wide_df, is.na(RT_Raw_0) == F & is.na(RT_Raw_1) == F)


## Reaction Time

lm.rt <- lm(scale(RT_Raw_1) ~ scale(RT_Raw_0), data=wide_df)
cor.test(wide_df$RT_Raw_0,wide_df$RT_Raw_1)
lm.exp <- lm(scale(RT_Raw_1) ~ scale(RT_Raw_0) + I(exp(scale(RT_Raw_0))), data=wide_df)
lm.qu <- lm(scale(RT_Raw_1) ~ scale(RT_Raw_0) + I(scale(RT_Raw_0^2)), data=wide_df)
lm.cub <- lm(scale(RT_Raw_1) ~ scale(RT_Raw_0) + I(scale(RT_Raw_0^2))+ I(scale(RT_Raw_0^3)), data=wide_df)

BIC(lm,lm.exp,lm.qu,lm.cub)[order(BIC(lm,lm.exp,lm.qu,lm.cub)$BIC),]

# Visualising
lm <- lm(RT_Raw_1 ~ RT_Raw_0, data=wide_df)
lm.exp <- lm(RT_Raw_1 ~ RT_Raw_0 + I(exp(RT_Raw_0)), data=wide_df)
lm.qu <- lm(RT_Raw_1 ~ RT_Raw_0 + I(RT_Raw_0^2), data=wide_df)

ggplot(wide_df, aes(x = RT_Raw_0, y = RT_Raw_1)) +
  geom_point(color="azure4", alpha=0.7) +  # Add points
  #geom_smooth(method = "lm", se = FALSE) +
  geom_line(aes(y=predict(lm.qu)),color="deepskyblue4")+
  geom_line(aes(y=predict(lm)), color="coral")+
  geom_line(aes(y=predict(lm.exp)), color="gold")+
  labs(x = "RT control", y = "RT stress", title = "Scatter plot with Regression Line")+
  theme_classic()


## Errors

lm.er <- lm(scale(Errors_1) ~ scale(Errors_0), data=wide_df)
cor.test(wide_df$Errors_0,wide_df$Errors_1)
lm.exp <- lm(scale(Errors_1) ~ scale(Errors_0) + I(exp(scale(Errors_0))), data=wide_df)
lm.qu <- lm(scale(Errors_1) ~ scale(Errors_0) + I(scale(Errors_0^2)), data=wide_df)
lm.cub <- lm(scale(Errors_1) ~ scale(Errors_0) + I(scale(Errors_0^2))+ I(scale(Errors_0^3)), data=wide_df)

BIC(lm,lm.exp,lm.qu,lm.cub)[order(BIC(lm,lm.exp,lm.qu,lm.cub)$BIC),]

# Visualising
lm <- lm(Errors_1 ~ Errors_0, data=wide_df)
lm.exp <- lm(Errors_1 ~ Errors_0 + I(exp(Errors_0)), data=wide_df)
lm.qu <- lm(Errors_1 ~ Errors_0 + I(Errors_0^2), data=wide_df)

ggplot(wide_df, aes(x = Errors_0, y = Errors_1)) +
  geom_point(color="azure4", alpha=0.7) +  # Add points
  #geom_smooth(method = "lm", se = FALSE) +
  geom_line(aes(y=predict(lm.qu)),color="deepskyblue4")+
  geom_line(aes(y=predict(lm)), color="coral")+
  geom_line(aes(y=predict(lm.exp)), color="gold")+
  labs(x = "Errors control", y = "Errors social stress")+
  theme_classic()


## Coeficients
lm.rt <- lm(RT_Raw_1 ~ RT_Raw_0, data=wide_df)
lm.er <- lm(Errors_1 ~ Errors_0, data=wide_df)
model_parameters(lm.rt)
effectsize(lm.rt)
model_parameters(lm.er)
effectsize(lm.er)




#### Evaluation and CL effects on Target trials only

target_df <- experiment_data %>% filter(Target == 1)


## Errors
m1 <- glmer(Errors ~ CL+Stress + (1|ID),
             data=target_df, family = binomial)
m1a <- glmer(Errors ~ CL*Stress + (1|ID),
             data=target_df, family = binomial)
m0 <- glmer(Errors ~ CL + (1|ID),
            data=target_df, family = binomial)

AIC(m1)- AIC(m1a)
BIC(m1)- BIC(m1a)

AIC(m0)- AIC(m1)
BIC(m0)- BIC(m1)

AIC(m0,m1,m1a)
BIC(m0,m1,m1a)
summary(m1)
summary(m1a)

parameters(m1, exponentiate = T)


#Stress*CL
emm_eu <- estimate_means(m1a, ~CL*Stress)

ggplot(emm_eu, aes(x = Stress, y = Probability, group = CL, shape= CL, color = CL,
                   linetype = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Evaluation", y = "Error Probability",
       color = "Cognitive Load", shape="Cognitive Load", linetype= "Cognitive Load") +
  
  scale_x_discrete(labels = c("Control", "Evaluation")) +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("low", "high")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234"),
                     labels = c("low", "high")) +  # Orange color for H0 and dark green for H1
  scale_linetype_manual(values=c(1,2),
                        labels=c("low","high")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15))


ggsave("TablesFigures/EMM TARGET Errors interaction GLMM.jpeg",dpi=300,width = 4, height = 3)



### RT
m1 <- lmer(RT_Raw ~ CL+Stress + (1|ID),
            data=target_df, REML=T)
m1a <- lmer(RT_Raw ~ CL*Stress + (1|ID),
             data=target_df, REML=T)
m0 <- lmer(RT_Raw ~ CL + (1|ID),
            data=target_df, REML=T)

AIC(m0,m1,m1a)
BIC(m0,m1,m1a)
summary(m1)
summary(m1a)


#Stress*CL
emm_eu <- estimate_means(m1a, ~CL*Stress)

ggplot(emm_eu, aes(x = Stress, y = Mean, group = CL, shape= CL, color = CL,
                   linetype = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Evaluation", y = "Error Probability",
       color = "Cognitive Load", shape="Cognitive Load", linetype= "Cognitive Load") +
  
  scale_x_discrete(labels = c("Control", "Evaluation")) +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("low", "high")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234"),
                     labels = c("low", "high")) +  # Orange color for H0 and dark green for H1
  scale_linetype_manual(values=c(1,2),
                        labels=c("low","high")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15))


ggsave("TablesFigures/EMM TARGET RT interaction GLMM.jpeg",dpi=300,width = 4, height = 3)









## Perceived Performance ####
                          
## step 1
m <- lmer(scale(RT_Raw) ~ Block + trial_block + CL + Stress + scale(Eustress) + scale(Performance) + (CL + Stress|ID),
           data=main_df, REML = T)

## Get nice table fro LMM full
t_ <- model_parameters(m)[1:8,1:9] %>% dplyr::select(-"CI")

# Extract unstandardized Beta
Beta_m <- lmer(RT_Raw ~ Block + trial_block + CL + Stress + Eustress + Performance + (CL + Stress|ID),
               data=main_df, REML = T)
t_Beta <- model_parameters(Beta_m)[1:8,] %>% dplyr::select("Parameter", "Coefficient")
colnames(t_Beta)[2] <- "Unstandardized B"
table <- cbind(t_Beta, t_)[,-3]

table <- nice_table(table, 
                    title = c("Table X", "perceived performance and perceived stress in ralation to rt"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table


#errors
m <- glmer(Errors ~ block + trial_block + CL + scale(ratio) + scale(Performance) + (1|ID),
            data=experiment_data, family = binomial)
model_parameters(m)
summary(m)



# step 2
m <- lmer(scale(Performance) ~ CL + scale(RT_Raw) + scale(Errors) +(1|ID),
           data=agg_df, REML=T)

## Get nice table fro LMM full
t_ <- model_parameters(m)[1:4,1:9] %>% dplyr::select(-"CI")

# Extract unstandardized Beta
Beta_m <- lmer(Performance ~ CL + RT_Raw + Errors +(1|ID),
               data=agg_df, REML=T)
t_Beta <- model_parameters(Beta_m)[1:4,] %>% dplyr::select("Parameter", "Coefficient")
colnames(t_Beta)[2] <- "Unstandardized B"
table <- cbind(t_Beta, t_)[,-3]

table <- nice_table(table, 
                    title = c("Table X", "Perceived Performance and Errors/RT"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table



### Perceived Performance and eustress/distress ####

gl.perf.er <- glmer(Errors ~ block + trial_block + CL + Stress + scale(Performance)+scale(ratio) + (1|ID),
            data=experiment_data, family = binomial)
summary(gl.perf.er)
model_parameters(gl.perf.er)

lm.perf.er <- lmer(scale(Errors) ~ CL + Stress + scale(Performance)+scale(ratio) + (1|ID),
            data=agg_df, REML=T)
summary(lm.perf.er)


perform.rt.m <- lmer(scale(RT_Raw) ~ Block + trial_block + CL + Stress + scale(Performance)+scale(Eustress) + (CL+Stress|ID),
           data=main_df, REML=T)
perform.rt.m.uns <- lmer(RT_Raw ~ Block + trial_block + CL + Stress + scale(Performance)+scale(Eustress) + (CL+Stress|ID),
                     data=main_df, REML=T)
model_parameters(perform.rt.m)
model_parameters(perform.rt.m.uns)





### Mental Demand ####
mental.m <- lmer(Mental ~ CL*Stress + (CL+Stress|ID),
           data=agg_df, REML=T, contrasts=type3)
aov_mental <- anova(mental.m,type=3)
aov_mental
epsilon_squared(aov_mental)
effectsize(m1)
model_parameters(m1)

emm <- estimate_means(mental.m, ~CL*Stress)

ggplot(emm, aes(x = Stress, y = Mean, group = CL, color=CL,shape=CL, linetype = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Evaluation", y = "Mental Demand",
       color = "Cognitive Load", shape="Cognitive Load", linetype= "Cognitive Load") +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("low", "high")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234"),
                     labels = c("low", "high")) +  # Orange color for H0 and dark green for H1
  scale_linetype_manual(values = c(1,2),
                        labels = c("low", "high")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("Control", "Evaluation"))

ggsave("TablesFigures/Perceived Mental.jpeg",dpi=300,width = 4, height = 3)
# greater effect in CL high group



### Temporal Demand ####

temp.m <- lmer(Temporal ~ CL*Stress + (CL+Stress|ID),
                 data=agg_df, REML=T, contrasts=type3)
aov_temp <- anova(temp.m,type=3)
aov_temp
epsilon_squared(aov_temp)
effectsize(temp.m)
model_parameters(temp.m)

emm <- estimate_means(temp.m, ~CL*Stress)

ggplot(emm, aes(x = Stress, y = Mean, group = CL, color=CL,shape=CL, linetype = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.2),linewidth=0.35,linetype=1)+
  labs(x = "Social Evaluation", y = "Temporal Demand",
       color = "Cognitive Load", shape="Cognitive Load", linetype= "Cognitive Load") +
  
  scale_shape_manual(values = c(15, 19),
                     labels = c("low", "high")) +  # Use square for H0 and circle for H1
  scale_color_manual(values = c("#008080", "#E34234"),
                     labels = c("low", "high")) +  # Orange color for H0 and dark green for H1
  scale_linetype_manual(values = c(1,2),
                        labels = c("low", "high")) +
  
  theme_classic() +
  theme(legend.text = element_text(size = 8),
        axis.title.x = element_text(vjust = -3),
        axis.title.y = element_text(vjust = 3),
        plot.margin = margin(b = 15, l=15)) +
  
  scale_x_discrete(labels = c("Control", "Evaluation"))

#ggsave("TablesFigures/Perceived Temporal.jpeg",dpi=300,width = 4, height = 3)




### EDA ####

## Differences across conditions
m1 <- lmer(scale(EDA) ~ CL+Stress + (1|ID),
           data=agg_EDA, REML=T)
m2 <- lmer(scale(EDA) ~ CL+Stress + (CL+Stress|ID),
           data=agg_EDA, REML=T)
m3 <- lmer(scale(EDA) ~ CL*Stress + (CL+Stress|ID),
           data=agg_EDA, REML=T)
AIC(m1,m2,m3)
BIC(m1,m2,m3)
summary(m2)

aov <- anova_test(data = agg_EDA, EDA ~ CL*Stress + Error(ID/(CL*Stress)), type =3,
                  effect.size="pes")
get_anova_table(aov)


emm_eu <- estimate_means(m2, ~CL*Stress)

ggplot(emm_eu, aes(x = Stress, y = Mean, group = CL, color = CL)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) +
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,
                position=position_dodge(0.15),linewidth=0.35,linetype=1)+
  labs(x = "Stress", y = "EDA", color = "CL") +
  theme_minimal()

ggsave("TablesFigures/EDA across conditions.jpeg",dpi=300,width = 4, height = 3)
# greater effect in CL high group








## EDA prediction of RT and Errors

m1 <- lmer(scale(RT_Raw) ~ scale(EDA) + (1|ID),
           data=agg_EDA, REML=T)

m2 <- lmer(scale(RT_Raw) ~ scale(EDA) + (EDA|ID),
           data=agg_EDA, REML=T)
AIC(m1,m2) #m1
BIC(m1,m2) #m1

summary(m1) # The higher EDA, the faster RT


m2 <- lmer(scale(RT_Raw) ~ CL+Stress + scale(EDA) + (1|ID),
           data=agg_EDA, REML=T)

m3 <- lmer(scale(RT_Raw) ~ CL+Stress + scale(EDA) + (CL+Stress|ID),
           data=agg_EDA, REML=T)

m4 <- lmer(scale(RT_Raw) ~ CL*Stress*scale(EDA) + (1|ID),
           data=agg_EDA, REML=T)

m5 <- lmer(scale(RT_Raw) ~ CL*Stress*scale(EDA) + (CL+Stress|ID),
           data=agg_EDA, REML=T)

AIC(m1,m2,m3,m4, m5)[order(AIC(m1,m2, m3, m4, m5)$AIC),] # prefers m3
BIC(m1,m2,m3,m4, m5)[order(BIC(m1,m2, m3, m4, m5)$BIC),] # prefers m2

summary(m2) 
summary(m3) #EDA sign.

'The higher EDA, the faster RT'


## Get nice table fro LMM full
t_ <- model_parameters(m3)[1:4,1:9] %>% dplyr::select(-"CI")

# Extract unstandardized Beta
Beta_m <- lmer(scale(RT_Raw, scale=F) ~ CL+Stress + scale(EDA) + (CL+Stress|ID),
               data=agg_EDA, REML=T)
t_Beta <- model_parameters(Beta_m)[1:4,] %>% dplyr::select("Parameter", "Coefficient")
colnames(t_Beta)[2] <- "Unstandardized B"
table <- inner_join(t_Beta, t_, c="Parameter")

table <- nice_table(table, 
                    title = c("Table X", "EDA and Reaction Time (Type I Sum of Squares)"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table

save_as_docx(table, path = "TablesFigures/EDA RT LMM.docx")






m1 <- lmer(scale(Errors) ~ scale(EDA) + (1|ID),
           data=agg_EDA, REML=T)

m2 <- lmer(scale(Errors) ~ scale(EDA) + (EDA|ID),
           data=agg_EDA, REML=T)
AIC(m1,m2) #m1
BIC(m1,m2) #m1

summary(m1) # No effect


m2 <- lmer(scale(Errors) ~ CL+Stress + scale(EDA) + (1|ID),
           data=agg_EDA, REML=T)

m3 <- lmer(scale(Errors) ~ CL+Stress + scale(EDA) + (CL+Stress|ID),
           data=agg_EDA, REML=T)

m4 <- lmer(scale(Errors) ~ CL*Stress*scale(EDA) + (1|ID),
           data=agg_EDA, REML=T)

m5 <- lmer(scale(Errors) ~ CL*Stress*scale(EDA) + (CL+Stress|ID),
           data=agg_EDA, REML=T)

AIC(m1,m2,m3,m4, m5)[order(AIC(m1,m2, m3, m4, m5)$AIC),] # prefers m3
BIC(m1,m2,m3,m4, m5)[order(BIC(m1,m2, m3, m4, m5)$BIC),] # prefers m2

summary(m2) 
summary(m3) #EDA sign. and Stress sign.
'Tendency, the higher EDA, the less errors'#Seems to be similar to eustress


## Get nice table fro LMM full
t_ <- model_parameters(m3)[1:4,1:9] %>% dplyr::select(-"CI")

# Extract unstandardized Beta
Beta_m <- lmer(scale(Errors, scale=F) ~ CL+Stress + scale(EDA) + (CL+Stress|ID),
               data=agg_EDA, REML=T)
t_Beta <- model_parameters(Beta_m)[1:4,] %>% dplyr::select("Parameter", "Coefficient")
colnames(t_Beta)[2] <- "Unstandardized B"
table <- inner_join(t_Beta, t_, c="Parameter")

table <- nice_table(table, 
                    title = c("Table X", "EDA and Errors (Type I Sum of Squares)"),
                    note = c(
                      "* p < .05, ** p < .01, *** p < .001. CL = Cognitive Load; CI = Confidence Interval."
                    ))
table

save_as_docx(table, path = "TablesFigures/EDA Errors LMM.docx")








### Eustress and Distress

m1 <- lmer(scale(EDA) ~ scale(Eustress) + (1|ID),
           data=agg_EDA, REML = T)
m1a <- lmer(scale(EDA) ~ scale(Eustress) + (Eustress|ID),
           data=agg_EDA, REML = T)
AIC(m1,m1a)
BIC(m1,m1a)
summary(m1) # no correlation


m1 <- lmer(scale(EDA) ~ scale(Distress) + (1|ID),
           data=agg_EDA, REML = T)
m1a <- lmer(scale(EDA) ~ scale(Distress) + (Distress|ID),
            data=agg_EDA, REML = T)
AIC(m1,m1a)
BIC(m1,m1a)
summary(m1) # no correlation


m1 <- lmer(scale(EDA) ~ scale(Eustress)*scale(Distress) + (1|ID),
           data=agg_EDA, REML = T)
summary(m1) # no effect


'Eda seems not related to perceived ratings of Eustress or Distress'


## EDA and Frustration
m1 <- lmer(scale(EDA) ~ scale(Frustration) + (1|ID),
           data=agg_EDA, REML = T)
m1a <- lmer(scale(EDA) ~ scale(Frustration) + (Frustration|ID),
            data=agg_EDA, REML = T)
AIC(m1,m1a)
BIC(m1,m1a)
summary(m1) # no correlation
















## Version infomations

devtools::session_info()






