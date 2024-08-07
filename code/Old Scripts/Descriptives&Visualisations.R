if(!require("groundhog", quietly = T)){
  install.packages("groundhog")
}
library(groundhog)

pkgs <- c("tidyverse",
          "rempsyc",
          "flextable",
          "plyr",
          "lattice",
          "tidyverse",
          "rmarkdown",
          "Rmisc",
          "cowplot",
          "devtools",
          "gghalves")


date = "2024-01-01" # year-month-day
groundhog.library(pkgs,date)



### load data ------------------------------------------------------------------
TLX_and_ADES <- read_csv("Data/TLX_and_ADES_clean.csv")
View(TLX_and_ADES)

PostExp <- read_csv("Data/PostExp_clean.csv")
View(PostExp)




                      #### Descriptive Statistics ####


### Errors ####

## Errors
descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Errors,na.rm = TRUE), SD = sd(Errors,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Errors <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))


descriptives <- main_df %>% group_by(Stress, CL, Block, trial_block) %>% 
  dplyr::summarise(M = mean(Errors,na.rm = TRUE), SD = sd(Errors,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Errors_all <- cbind(descriptives[,1:4],round(descriptives[,5:10],2))
##

# split errors ####
# ## cER
# descriptives <- agg_df %>% group_by(Stress, CL) %>% 
#   dplyr::summarise(M = mean(cER,na.rm = TRUE), SD = sd(cER,na.rm = TRUE), n = n()) %>% 
#   mutate(SE = SD / sqrt(n),
#          CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
#          CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)
# 
# descriptives_cER <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
# 
# 
# descriptives <- main_df %>% group_by(Stress, CL, Block, trial_block) %>% 
#   dplyr::summarise(M = mean(cER,na.rm = TRUE), SD = sd(cER,na.rm = TRUE), n = n()) %>% 
#   mutate(SE = SD / sqrt(n),
#          CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
#          CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)
# 
# descriptives_cER_all <- cbind(descriptives[,1:4],round(descriptives[,5:10],2))
# ##
# 
# 
# ## oER
# descriptives <- agg_df %>% group_by(Stress, CL) %>% 
#   dplyr::summarise(M = mean(oER,na.rm = TRUE), SD = sd(oER,na.rm = TRUE), n = n()) %>% 
#   mutate(SE = SD / sqrt(n),
#          CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
#          CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)
# 
# descriptives_oER <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
# 
# 
# descriptives <- main_df %>% group_by(Stress, CL, Block, trial_block) %>% 
#   dplyr::summarise(M = mean(oER,na.rm = TRUE), SD = sd(oER,na.rm = TRUE), n = n()) %>% 
#   mutate(SE = SD / sqrt(n),
#          CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
#          CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)
# 
# descriptives_oER_all <- cbind(descriptives[,1:4],round(descriptives[,5:10],2))
# ##
# 


### Reaction Time ####

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(RT_Raw,na.rm = TRUE), SD = sd(RT_Raw,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_RT_Raw <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))


descriptives <- main_df %>% group_by(Stress, CL, Block, trial_block) %>% 
  dplyr::summarise(M = mean(RT_Raw,na.rm = TRUE), SD = sd(RT_Raw,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_RT_Raw_all <- cbind(descriptives[,1:4],round(descriptives[,5:10],2))
##



### Merge descriptives into one table

##
c_names <- colnames(descriptives_RT_Raw)[-1:-2]
colnames(descriptives_RT_Raw)[-1:-2] <- paste("RT.", c_names)

c_names <- colnames(descriptives_Errors)[-1:-2]
colnames(descriptives_Errors)[-1:-2] <- paste("Errors.", c_names)

dscr_er <- inner_join(descriptives_RT_Raw[,1:4], descriptives_Errors[,1:4],
                      by = c("Stress", "CL"))

table <- nice_table(dscr_er, separate.header = TRUE)#, italics = seq(dscr_er))
table

save_as_docx(table, path = "TablesFigures/Descr Performance.docx")
##

##
c_names <- colnames(descriptives_RT_Raw_all)[-1:-4]
colnames(descriptives_RT_Raw_all)[-1:-4] <- paste("RT.", c_names)

c_names <- colnames(descriptives_Errors_all)[-1:-4]
colnames(descriptives_Errors_all)[-1:-4] <- paste("Errors.", c_names)

c_names <- colnames(descriptives_cER_all)[-1:-4]
colnames(descriptives_cER_all)[-1:-4] <- paste("cER.", c_names)

c_names <- colnames(descriptives_oER_all)[-1:-4]
colnames(descriptives_oER_all)[-1:-4] <- paste("oER.", c_names)

dscr_er <- inner_join(descriptives_RT_Raw_all[,1:6], descriptives_Errors_all[,1:6], 
                      by = c("Stress", "CL", "Block", "trial_block")) %>% 
  inner_join(descriptives_cER_all[,1:6], by = c("Stress", "CL", "Block", "trial_block")) %>%
  inner_join(descriptives_oER_all[,1:6], by = c("Stress", "CL", "Block", "trial_block"))

table <- nice_table(dscr_er, separate.header = TRUE)#, italics = seq(dscr_er))
table

save_as_docx(table, path = "TablesFigures/Descr Performance all.docx")
##


### ADES ####

## Eustress

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Eustress,na.rm = TRUE), SD = sd(Eustress,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Eustress <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
##



## Distress

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Distress,na.rm = TRUE), SD = sd(Distress,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Distress <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
##


## Ratio

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(ratio,na.rm = TRUE), SD = sd(ratio,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_ratio <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))


##
c_names <- colnames(descriptives_Eustress)[-1:-2]
colnames(descriptives_Eustress)[-1:-2] <- paste("Eustress.", c_names)

c_names <- colnames(descriptives_Distress)[-1:-2]
colnames(descriptives_Distress)[-1:-2] <- paste("Distress.", c_names)

c_names <- colnames(descriptives_ratio)[-1:-2]
colnames(descriptives_ratio)[-1:-2] <- paste("Eustress/Distress-Ratio.", c_names)

dscr_er <- inner_join(descriptives_Eustress[,1:4], descriptives_Distress[,1:4],
                      by = c("Stress", "CL")) %>%
  inner_join(descriptives_ratio[,1:4],
             by = c("Stress", "CL"))

table <- nice_table(dscr_er, separate.header = TRUE)#, italics = seq(dscr_er))
table

save_as_docx(table, path = "TablesFigures/Descr EU_Distress.docx")
##



### NASA-TLX ####

## Mental

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Mental,na.rm = TRUE), SD = sd(Mental,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Mental <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
##



## Temporal

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Temporal,na.rm = TRUE), SD = sd(Temporal,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Temporal <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
##



## Effort

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Effort,na.rm = TRUE), SD = sd(Effort,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Effort <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
##



## Perceived Performance

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Performance,na.rm = TRUE), SD = sd(Performance,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Performance <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))
##



## Frustration

descriptives <- agg_df %>% group_by(Stress, CL) %>% 
  dplyr::summarise(M = mean(Frustration,na.rm = TRUE), SD = sd(Frustration,na.rm = TRUE), n = n()) %>% 
  mutate(SE = SD / sqrt(n),
         CI_lower = M - qt(1 - (0.05 / 2), n - 1) * SE,
         CI_upper = M + qt(1 - (0.05 / 2), n - 1) * SE)

descriptives_Frustration <- cbind(descriptives[,1:2],round(descriptives[,3:8],2))



c_names <- colnames(descriptives_Mental)[-1:-2]
colnames(descriptives_Mental)[-1:-2] <- paste("Mental.", c_names)

c_names <- colnames(descriptives_Temporal)[-1:-2]
colnames(descriptives_Temporal)[-1:-2] <- paste("Temporal.", c_names)

c_names <- colnames(descriptives_Effort)[-1:-2]
colnames(descriptives_Effort)[-1:-2] <- paste("Effort.", c_names)

c_names <- colnames(descriptives_Performance)[-1:-2]
colnames(descriptives_Performance)[-1:-2] <- paste("Performance.", c_names)

c_names <- colnames(descriptives_Frustration)[-1:-2]
colnames(descriptives_Frustration)[-1:-2] <- paste("Frustration.", c_names)

dscr_er <- inner_join(descriptives_Mental[,1:4], descriptives_Temporal[,1:4],
                      by = c("Stress", "CL")) %>%
  inner_join(descriptives_Effort[,1:4], by = c("Stress", "CL")) %>%
  inner_join(descriptives_Performance[,1:4], by = c("Stress", "CL")) %>%
  inner_join(descriptives_Frustration[,1:4], by = c("Stress", "CL"))

table <- nice_table(dscr_er, separate.header = TRUE)#, italics = seq(dscr_er))
table

save_as_docx(table, path = "TablesFigures/Descr NASA.docx")




                          #### Visualisation ####
                          

# width and height variables for saved plots
w = 6
h = 4
### Raincloud generating function specific to this data

raincloud_func <- function(data, y_var, str_y, seed=7){
  
  d <- data
  d$x <- ifelse(d$CL == "2back" & d$Stress == 0,  1,
                ifelse(d$CL == "2back" & d$Stress == 1, 2, NA)) # x CL low under Stress & no stress
  d$z <- ifelse(d$CL == "3back" & d$Stress == 0,  3,
                ifelse(d$CL == "3back" & d$Stress == 1, 4, NA)) # z == CL high under Stress and no stress
  d$y <- d[,y_var , drop = TRUE] # y == dependent variable
  
  #set.seed(seed)
  d$xj <- jitter(d$x, amount = .09)
  d$xj_2 <- jitter(d$z, amount = .09)
  
  
  summary_df <- data %>%
    group_by(Stress, CL) %>%
    dplyr::summarise(score_mean = mean(get(y_var), na.rm = TRUE),
                     sd = sd(get(y_var), na.rm = TRUE),N = n()) %>%
    mutate(se = sd / sqrt(N),
           ci = qt(1 - (0.05 / 2), N - 1) * se)
  summary_df$group <- c(1,3,2,4)
  summary_df <- summary_df[order(summary_df$group),]
  summary_df_l <- summary_df %>% filter(group=="1"|group=="2")
  summary_df_h <- summary_df %>% filter(group=="3"|group=="4")
  
  #print(summary_df)
  
  
  x_tick_means_x <- c(.87, 2.15)
  x_tick_means_z <- c(2.87, 4.15)
  
  f9 <- ggplot(data = d, aes(y = y)) +
    #Add geom_() objects
    geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'dodgerblue', size = 1.5,
               alpha = .6) +
    geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'darkorange', size = 1.5,
               alpha = .6) +
    geom_point(data = d %>% filter(z =="3"), aes(x = xj_2), color = 'dodgerblue', size = 1.5,
               alpha = .6) +
    geom_point(data = d %>% filter(z =="4"), aes(x = xj_2), color = 'darkorange', size = 1.5,
               alpha = .6) +
    geom_line(aes(x = xj, group = ID), color = 'lightgray', alpha = .3) +
    geom_line(aes(x = xj_2, group = ID), color = 'lightgray', alpha = .3) +
    geom_half_boxplot(
      data = d %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.35),
      side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'dodgerblue', alpha = .6) +
    geom_half_boxplot(
      data = d %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.16),
      side = "l",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'darkorange', alpha = .6) +
    geom_half_boxplot(
      data = d %>% filter(z=="3"), aes(x=z, y = y), position = position_nudge(x = 1.3),
      side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'dodgerblue', alpha = .6) +
    geom_half_boxplot(
      data = d %>% filter(z=="4"), aes(x=z, y = y), position = position_nudge(x = .2),
      side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'darkorange', alpha = .6) +
    geom_half_violin(
      data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.40),
      side = "l", fill = 'dodgerblue', alpha = .6) +
    geom_half_violin(
      data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = -1.40),
      side = "l", fill = "darkorange", alpha = .6) +
    geom_half_violin(
      data = d %>% filter(z=="3"),aes(x = z, y = y), position = position_nudge(x = 1.45),
      side = "r", fill = 'dodgerblue', alpha = .6) +
    geom_half_violin(
      data = d %>% filter(z=="4"),aes(x = z, y = y), position = position_nudge(x = .45),
      side = "r", fill = "darkorange", alpha = .6) +
    
    #Add lines connecting the two means
    geom_line(data = summary_df_l, aes(x = x_tick_means_x, y = score_mean),
              color = 'gray', linewidth = .8) +
    geom_line(data = summary_df_h, aes(x = x_tick_means_z, y = score_mean),
              color = 'gray', linewidth = .8) +
    
    # Add condition means and ci bars
    geom_point(data = d %>% filter(x=="1"), aes(x = x, y = summary_df$score_mean[1]),
               position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
    geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = summary_df$score_mean[1],
                                                   ymin = summary_df$score_mean[1]-summary_df$ci[1], ymax = summary_df$score_mean[1]+summary_df$ci[1]),
                  position = position_nudge(-.13),
                  color = "dodgerblue", width = 0.05, linewidth = 0.4, alpha = .6) +
    geom_point(data = d %>% filter(x=="2"), aes(x = x, y = summary_df$score_mean[2]),
               position = position_nudge(x = .15), color = "darkorange", alpha = .6, size = 1.5)+
    geom_errorbar(data = d %>% filter(x=="2"), aes(x = x, y = summary_df$score_mean[2],
                                                   ymin = summary_df$score_mean[2]-summary_df$ci[2],
                                                   ymax = summary_df$score_mean[2]+summary_df$ci[2]), position = position_nudge(x = .15), color = "darkorange",
                  width = 0.05, linewidth = 0.4, alpha = .6) +
    
    geom_point(data = d %>% filter(z=="3"), aes(x = z, y = summary_df$score_mean[3]),
               position = position_nudge(x = -.13), color = "dodgerblue", alpha = .5) +
    geom_errorbar(data = d %>% filter(z=="3"), aes(x = z, y = summary_df$score_mean[3],
                                                   ymin = summary_df$score_mean[3]-summary_df$ci[3],
                                                   ymax = summary_df$score_mean[3]+summary_df$ci[3]), position = position_nudge(-.13),
                  color = "dodgerblue", width = 0.05, linewidth = 0.4, alpha = .5)+
    
    geom_point(data = d %>% filter(z=="4"), aes(x = z, y = summary_df$score_mean[4]),
               position = position_nudge(x = .15), color = "darkorange", alpha = .5)+
    geom_errorbar(data = d %>% filter(z=="4"), aes(x = z, y = summary_df$score_mean[4],
                                                   ymin = summary_df$score_mean[4]-summary_df$ci[4],
                                                   ymax = summary_df$score_mean[4]+summary_df$ci[4]),
                  position = position_nudge(.15),
                  color = "darkorange", width = 0.05, linewidth = 0.4, alpha = .5) +
    
    
    #Define additional settings
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("Control", "Stress","Control", "Stress"),
                       limits=c(0, 5))+
    annotate("text", x = 1.5, y = max(d$y)*1.1, label = "Cognitive Load low", hjust = 0.5) +
    annotate("text", x = 3.5, y = max(d$y)*1.1, label = "Cognitive Load high", hjust = 0.5) +
    xlab("Social Stress") + ylab(str_y) +
    theme_classic()+
    theme(axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 3),
          plot.margin = margin(b = 15, l=15))
  
  print(f9)
  ggsave(paste("TablesFigures/", str_y, " Raincloud.jpeg", sep = ""),width = 6, height = 4, dpi = 300)
}



## Reaction Time

#raincloud_func(agg_df,"RT_cw","Reaction Time (cw)")

raincloud_func(agg_df,"RT_Raw","Reaction Time (s)")


## Errors

#raincloud_func(agg_df,"Errors_cw","Errors (cw)")

raincloud_func(agg_df,"Errors","Errors")


## Eustress

#raincloud_func(agg_df,"Eustress_cw","Eustress (cw)")

raincloud_func(agg_df,"Eustress","Eustress")


# Distress

#raincloud_func(agg_df,"Distress_cw","Distress (cw)")

raincloud_func(agg_df,"Distress","Distress")


# ratio
raincloud_func(agg_df,"ratio","Eustress-Distress-Ratio")


# Mental Demand
raincloud_func_2 <- function(data, y_var, str_y, seed=7){
  
  d <- data
  d$x <- ifelse(d$CL == "2back" & d$Stress == 0,  1,
                ifelse(d$CL == "3back" & d$Stress == 0, 2, NA)) # x == CL low & high under control
  d$z <- ifelse(d$CL == "2back" & d$Stress == 1,  3,
                ifelse(d$CL == "3back" & d$Stress == 1, 4, NA)) # z == CL low & high under stress
  d$y <- d[,y_var , drop = TRUE] # y == dependent variable
  
  #set.seed(seed)
  d$xj <- jitter(d$x, amount = .09)
  d$xj_2 <- jitter(d$z, amount = .09)
  
  
  summary_df <- data %>%
    group_by(Stress, CL) %>%
    dplyr::summarise(score_mean = mean(get(y_var), na.rm = TRUE),
                     sd = sd(get(y_var), na.rm = TRUE),N = n()) %>%
    mutate(se = sd / sqrt(N),
           ci = qt(1 - (0.05 / 2), N - 1) * se)
  summary_df$group <- c(1,3,2,4)
  summary_df <- summary_df[order(summary_df$group),]
  summary_df_l <- summary_df %>% filter(group=="1"|group=="2")
  summary_df_h <- summary_df %>% filter(group=="3"|group=="4")
  
  #print(summary_df)
  
  
  x_tick_means_x <- c(.87, 2.15)
  x_tick_means_z <- c(2.87, 4.15)
  
  f9 <- ggplot(data = d, aes(y = y)) +
    #Add geom_() objects
    geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'dodgerblue', size = 1.5,
               alpha = .6) +
    geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'darkorange', size = 1.5,
               alpha = .6) +
    geom_point(data = d %>% filter(z =="3"), aes(x = xj_2), color = 'dodgerblue', size = 1.5,
               alpha = .6) +
    geom_point(data = d %>% filter(z =="4"), aes(x = xj_2), color = 'darkorange', size = 1.5,
               alpha = .6) +
    geom_line(aes(x = xj, group = ID), color = 'lightgray', alpha = .3) +
    geom_line(aes(x = xj_2, group = ID), color = 'lightgray', alpha = .3) +
    geom_half_boxplot(
      data = d %>% filter(x=="1"), aes(x=x, y = y), position = position_nudge(x = -.35),
      side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'dodgerblue', alpha = .6) +
    geom_half_boxplot(
      data = d %>% filter(x=="2"), aes(x=x, y = y), position = position_nudge(x = -1.16),
      side = "l",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'darkorange', alpha = .6) +
    geom_half_boxplot(
      data = d %>% filter(z=="3"), aes(x=z, y = y), position = position_nudge(x = 1.3),
      side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'dodgerblue', alpha = .6) +
    geom_half_boxplot(
      data = d %>% filter(z=="4"), aes(x=z, y = y), position = position_nudge(x = .2),
      side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
      fill = 'darkorange', alpha = .6) +
    geom_half_violin(
      data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.40),
      side = "l", fill = 'dodgerblue', alpha = .6) +
    geom_half_violin(
      data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = -1.40),
      side = "l", fill = "darkorange", alpha = .6) +
    geom_half_violin(
      data = d %>% filter(z=="3"),aes(x = z, y = y), position = position_nudge(x = 1.45),
      side = "r", fill = 'dodgerblue', alpha = .6) +
    geom_half_violin(
      data = d %>% filter(z=="4"),aes(x = z, y = y), position = position_nudge(x = .45),
      side = "r", fill = "darkorange", alpha = .6) +
    
    #Add lines connecting the two means
    geom_line(data = summary_df_l, aes(x = x_tick_means_x, y = score_mean),
              color = 'gray', linewidth = .8) +
    geom_line(data = summary_df_h, aes(x = x_tick_means_z, y = score_mean),
              color = 'gray', linewidth = .8) +
    
    # Add condition means and ci bars
    geom_point(data = d %>% filter(x=="1"), aes(x = x, y = summary_df$score_mean[1]),
               position = position_nudge(x = -.13), color = "dodgerblue", alpha = .6, size = 1.5) +
    geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = summary_df$score_mean[1],
                                                   ymin = summary_df$score_mean[1]-summary_df$ci[1], ymax = summary_df$score_mean[1]+summary_df$ci[1]),
                  position = position_nudge(-.13),
                  color = "dodgerblue", width = 0.05, linewidth = 0.4, alpha = .6) +
    geom_point(data = d %>% filter(x=="2"), aes(x = x, y = summary_df$score_mean[2]),
               position = position_nudge(x = .15), color = "darkorange", alpha = .6, size = 1.5)+
    geom_errorbar(data = d %>% filter(x=="2"), aes(x = x, y = summary_df$score_mean[2],
                                                   ymin = summary_df$score_mean[2]-summary_df$ci[2],
                                                   ymax = summary_df$score_mean[2]+summary_df$ci[2]), position = position_nudge(x = .15), color = "darkorange",
                  width = 0.05, linewidth = 0.4, alpha = .6) +
    
    geom_point(data = d %>% filter(z=="3"), aes(x = z, y = summary_df$score_mean[3]),
               position = position_nudge(x = -.13), color = "dodgerblue", alpha = .5) +
    geom_errorbar(data = d %>% filter(z=="3"), aes(x = z, y = summary_df$score_mean[3],
                                                   ymin = summary_df$score_mean[3]-summary_df$ci[3],
                                                   ymax = summary_df$score_mean[3]+summary_df$ci[3]), position = position_nudge(-.13),
                  color = "dodgerblue", width = 0.05, linewidth = 0.4, alpha = .5)+
    
    geom_point(data = d %>% filter(z=="4"), aes(x = z, y = summary_df$score_mean[4]),
               position = position_nudge(x = .15), color = "darkorange", alpha = .5)+
    geom_errorbar(data = d %>% filter(z=="4"), aes(x = z, y = summary_df$score_mean[4],
                                                   ymin = summary_df$score_mean[4]-summary_df$ci[4],
                                                   ymax = summary_df$score_mean[4]+summary_df$ci[4]),
                  position = position_nudge(.15),
                  color = "darkorange", width = 0.05, linewidth = 0.4, alpha = .5) +
    
    
    #Define additional settings
    scale_x_continuous(breaks=c(1,2,3,4), labels=c("low","high","low","high"),
                       limits=c(0, 5))+
    annotate("text", x = 1.5, y = max(d$y)*1.1, label = "Control", hjust = 0.5) +
    annotate("text", x = 3.5, y = max(d$y)*1.1, label = "Stress", hjust = 0.5) +
    xlab("CL") + ylab(str_y) +
    theme_classic()+
    theme(axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 3),
          plot.margin = margin(b = 15, l=15))
  
  print(f9)
  ggsave(paste("TablesFigures/", str_y, " Raincloud.jpeg", sep = ""),width = 6, height = 4, dpi = 300)
}

raincloud_func_2(agg_df, "Mental", "Mental Demand")



