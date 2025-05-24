pkgs <- c("tidyverse",
          "readxl",
          "ltm",
          "car")

lapply(pkgs, library, character.only=TRUE)

proj_dir <- getwd()


##### NASA-TLX and ADES ----------------------------------------------------------

### Add condition labels -------------------------------------------------------

# first participant in experiment: 792672; started with 2-back

TLX_and_ADES <- read_csv("../Data/raw_data/raw_questionnaires/TLX and ADES.csv")
View(TLX_and_ADES)

TLX_and_ADES <- TLX_and_ADES[,-2:-10] #exclude irrelevant columns
TLX_and_ADES <- TLX_and_ADES[-1:-2,] #exclude 2 first irrelevant rows
TLX_and_ADES <- TLX_and_ADES[-1:-24,] #exclude all pilot participants (up to 792672)

TLX_and_ADES[,2:length(TLX_and_ADES)] <- sapply(TLX_and_ADES[,2:length(TLX_and_ADES)], as.numeric)


## make sure the order aligns with increasing date
TLX_and_ADES <- TLX_and_ADES[order(TLX_and_ADES$StartDate),]


## add counterbalance order
pattern <- c("2back", "3back", "2back", "3back", "3back", "2back", "3back", "2back")
TLX_and_ADES$order <- rep(pattern, length.out = nrow(TLX_and_ADES))

## add condition CL
TLX_and_ADES$CL <- rep(c("low","high","low","high","high","low","high","low"),
                       length.out = nrow(TLX_and_ADES))

## add condition social evaluation
TLX_and_ADES$Evaluation <- rep(c(0,1), each = 2, length.out = nrow(TLX_and_ADES))

## add condition CL*social evaluation
TLX_and_ADES$CL_evaluation <- rep(c("low_0","high_0","low_1","high_1",
                                "high_0","low_0","high_1","low_1"),
                              length.out = nrow(TLX_and_ADES))


### Calculate total scores for questionnaires ----------------------------------

## Reverse Performance scale to calculate the total score for NASA TLX
TLX_and_ADES$Q6 <- recode(TLX_and_ADES$Q6, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") 

## Rename Items
TLX_and_ADES <- TLX_and_ADES %>% dplyr::rename(Mental = Q4, Temporal = Q8...13,
                                        Performance = Q6, Effort = Q5, Frustration = Q9...16)

TLX_and_ADES <- TLX_and_ADES %>% dplyr::rename(E1 = Q7, E2 = Q9...19,
                                        E3 = Q11, E4 = Q10, E5 = Q15)

TLX_and_ADES <- TLX_and_ADES %>% dplyr::rename(D1 = Q8...18, D2 = Q12,
                                        D3 = Q13, D4 = Q14, D5 = Q16)

TLX_and_ADES <- TLX_and_ADES%>% dplyr::rename(ID = Q17)


## Total mean scores of ADES and NASA TLX
TLX_and_ADES$Eustress <- rowMeans(TLX_and_ADES[c("E1", "E2", "E3", "E4", "E5")])

TLX_and_ADES$Distress <- rowMeans(TLX_and_ADES[c("D1", "D2", "D3", "D4", "D5")])

TLX_and_ADES$NASA_TLX <- rowMeans(TLX_and_ADES[c("Mental", "Temporal",
                                                 "Performance", "Effort", "Frustration")])

## Reverse Performance scale for better interpretation (low numbers correspond to low perceived performance)
TLX_and_ADES$Performance <- recode(TLX_and_ADES$Performance, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") 


## Safe data frames for later reliability analysis

rel_eustress <- TLX_and_ADES[c("E1", "E2", "E3", "E4", "E5")]
rel_distress <- TLX_and_ADES[c("D1", "D2", "D3", "D4", "D5")]
rel_NASA <- TLX_and_ADES[c("Mental", "Temporal", "Performance",
                           "Effort", "Frustration")]


## Retain only relevant variables
TLX_and_ADES <- cbind(TLX_and_ADES[,1:7], TLX_and_ADES[,19:length(TLX_and_ADES)])
TLX_and_ADES <- TLX_and_ADES[, c(1,2,8,9,10,3,4,5,6,7,11,12,13)]


## Centering values within participant (_cw) for better visualisations
TLX_and_ADES <- TLX_and_ADES %>% group_by(ID) %>% 
  mutate(Eustress_cw = round(scale(Eustress, scale=F)[,1],3), Distress_cw = round(scale(Distress, scale=F)[,1],3),
         NASA_TLX_cw = round(scale(NASA_TLX, scale=F)[,1],3),Mental_cw = round(scale(Mental, scale=F)[,1],3),
         Temporal_cw = round(scale(Temporal, scale=F)[,1],3),Performance_cw = round(scale(Performance, scale=F)[,1],3),
         Effort_cw = round(scale(Effort, scale=F)[,1],3), Frustration_cw = round(scale(Frustration, scale=F)[,1],3))


## Save dataframe
write.csv(TLX_and_ADES, "../Data/clean_data/TLX_and_ADES_clean.csv", row.names=FALSE)


##### Reliability analysis of questionnaire scales -------------------------------

cronbach.alpha(rel_eustress, CI = TRUE)
cronbach.alpha(rel_distress, CI = TRUE)
cronbach.alpha(rel_NASA, CI = TRUE)




##### Socio-Demographic Questionnaire (Post-Experiment Questionnaire) ----------

PostExp <- read_csv("../Data/raw_data/raw_questionnaires/PostExperimentQuestions.csv")
View(PostExp)
PostExp <- PostExp[,11:length(PostExp)]
colnames(PostExp) <- c('ID','Age','Sex', "Gender", "Studies", "Medication",
                       "Meds_specific", "Consumption_1hour", "Alc_yesterday",
                       "Sleep", "Evaluators", "Nback_encounter", "English_Comprehension")
PostExp <- PostExp[-1:-2,] #exclude 2 first irrelevant rows
PostExp <- PostExp[-1:-6,] #exclude all pilot participants (up to 792672)

write.csv(PostExp, "../Data/clean_data/PostExp_clean.csv", row.names=FALSE)




##### Performance Data ---------------------------------------------------------

setwd("../Data/raw_data/raw_performance")

files = list.files(pattern="\\.xlsx$")
id_list <- files
id_list <- gsub(".xlsx","", id_list)

sheets <- c("s1n2b2","s1n3b2", "s2n2b1","s2n3b1",
            "control_n2_b1","control_n2_b2","control_n3_b1","control_n3_b2",
            "STRESS_2n_b1","STRESS_2n_b2" ,"STRESS_3n_b1" ,"STRESS_3n_b2" )

df_list <- c()
for (nf in 1:length(files)){
  for (n in 1:length(sheets)){
    a <- read_excel(as.character(files[nf]), sheet = sheets[n])
    
    if (all(is.na(a[25,]))){
      a <- a[4:24,]
    } else{
      a <- a[4:33,]
    }
    
    a <- cbind(a[,7],a[,2], a[,12], a[,length(a)])
    a[,1] <- 1- a[,1]
    colnames(a) <- c("Errors", "Target", "RT_Raw", "Trial")
    a$session <- rep(sheets[n],nrow(a))
    
    
    assign(sheets[n], a)
  }
  
  big_df <- rbind(s1n2b2,s1n3b2, s2n2b1,s2n3b1,
                  control_n2_b1,control_n2_b2,control_n3_b1,control_n3_b2,
                  STRESS_2n_b1,STRESS_2n_b2 ,STRESS_3n_b1 ,STRESS_3n_b2)
  big_df$ID <- rep(id_list[nf],nrow(big_df))
  #big_df$RT <- big_df$RT_Raw - mean(big_df$RT_Raw, na.rm = TRUE)    # Centering RT for better visualisation
  #big_df$Errors_z <- round(big_df$Errors - mean(big_df$Errors), 3)    # Centering Errors for better visualisation
  big_df$cER  <- big_df$Errors
  big_df$cER[is.na(big_df$RT_Raw)] <- 0   # commission errors/ incorrect key press
  big_df$oER   <-  big_df$Errors
  big_df$oER[is.na(big_df$RT_Raw)==F] <- 0   # omission errors/ no key press
  
  big_df$RT_Raw <- round(big_df$RT_Raw,3)
  #big_df$RT <- round(big_df$RT,3)
  
  big_df <- big_df[,c(6,2,1,7,8,3,5,4)]
  assign(id_list[nf], big_df)
  df_list[[id_list[nf]]] <- get(id_list[nf])
}
performance_df <- bind_rows(df_list)

remove(s1n2b2,s1n3b2, s2n2b1,s2n3b1,
       control_n2_b1,control_n2_b2,control_n3_b1,control_n3_b2,
       STRESS_2n_b1,STRESS_2n_b2 ,STRESS_3n_b1 ,STRESS_3n_b2,a,big_df)

setwd(proj_dir)

performance_df$RT_Raw <- performance_df$RT_Raw * 1000


write.csv(performance_df, "../Data/clean_data/Performance_clean.csv", row.names = F)
for (i in id_list){rm(list = i)}



### split data into practice and experiment phases

performance_df$phase <- ifelse(grepl("control", performance_df$session)|
                                 grepl("STRESS", performance_df$session),
                               "experiment", "training")

### Prepare training data
training_data <- subset(performance_df, performance_df$phase != "experiment")
training_data$sessions <- ifelse(grepl("s1", training_data$session),
                                "1", "2")
training_data$CL <- ifelse(grepl("n2", training_data$session),
                                 "low", "high")

## Aggregate values for Errors and RT
training_aggregated <- aggregate(training_data$Errors, by = list(training_data$ID,
                                                                 training_data$CL,
                                                                 training_data$sessions),
                                 FUN = sum, na.rm = T)

colnames(training_aggregated) <- c("ID", "CL", "Session", "Errors")


training_RT <- training_data
training_aggregated_RT <- aggregate(training_RT$RT, by = list(training_RT$ID,
                                                              training_RT$CL,
                                                              training_RT$sessions),
                                 FUN = mean, na.rm = T)

colnames(training_aggregated_RT) <- c("ID", "CL", "Session", "RT_mean")


final_training <- inner_join(training_aggregated, training_aggregated_RT, by=c('ID',"CL","Session"))


remove(raining_aggregated_RT,training_aggregated,training_RT)



### Experiment Data
all_trials_df <- subset(performance_df, performance_df$phase == "experiment")

all_trials_df$Evaluation <- ifelse(grepl("STRESS", all_trials_df$session),
                                 "1", "0")
all_trials_df$CL <- ifelse(grepl("n2", all_trials_df$session)|grepl("2n", all_trials_df$session),
                           "low", "high")
all_trials_df$block <- ifelse(grepl("b2", all_trials_df$session),
                             "2", "1")


# Split each main block into 3 time blocks, separate for two main blocks (6 blocks in total)

all_trials_df$trial_block <- ifelse(all_trials_df$Trial <= 12, 0, ifelse(all_trials_df$Trial <= 22,1,2))




## Aggregate across ID,CL,Evaluation,block, and trial_block

exp_1 <- aggregate(all_trials_df$Errors, by = list(all_trials_df$ID,
                                                     all_trials_df$CL,
                                                     all_trials_df$Evaluation,
                                                     all_trials_df$block,
                                                     all_trials_df$trial_block),
                   FUN = sum, na.rm = T)
colnames(exp_1) <- c("ID", "CL", "Evaluation", "block", "trial_block", "Errors")


exp_2 <- aggregate(all_trials_df$cER, by = list(all_trials_df$ID,
                                                     all_trials_df$CL,
                                                     all_trials_df$Evaluation,
                                                     all_trials_df$block,
                                                     all_trials_df$trial_block),
                   FUN = sum, na.rm = T)
colnames(exp_2) <- c("ID", "CL", "Evaluation", "block", "trial_block", "cER")


exp_3 <- aggregate(all_trials_df$oER, by = list(all_trials_df$ID,
                                                     all_trials_df$CL,
                                                     all_trials_df$Evaluation,
                                                     all_trials_df$block,
                                                     all_trials_df$trial_block),
                   FUN = sum, na.rm = T)
colnames(exp_3) <- c("ID", "CL", "Evaluation", "block", "trial_block", "oER")


exp_RT <- all_trials_df
exp_aggregated_RT <- aggregate(exp_RT$RT_Raw, by = list(exp_RT$ID,
                                                    exp_RT$CL,
                                                    exp_RT$Evaluation,
                                                    exp_RT$block,
                                                    exp_RT$trial_block),
                               FUN = mean, na.rm = T)
colnames(exp_aggregated_RT) <- c("ID", "CL", "Evaluation", "block", "trial_block", "RT_Raw")


main_df <- inner_join(exp_1, exp_aggregated_RT, by=c('ID',"CL","Evaluation", "block","trial_block"))
main_df <- inner_join(main_df, exp_2, by=c('ID',"CL","Evaluation", "block", "trial_block"))
main_df <- inner_join(main_df, exp_3, by=c('ID',"CL","Evaluation", "block", "trial_block"))


main_df$ID <- as.numeric(main_df$ID)
main_df$RT_Raw <- round(main_df$RT_Raw, 0)


## Center values within participant (_cw) for better visualisations
main_df <- main_df %>% group_by(ID) %>% 
  mutate(Errors_cw = scale(Errors, scale=F)[,1], cER_cw = scale(cER, scale=F)[,1],
         oER_cw = scale(oER, scale=F)[,1], RT_cw=scale(RT_Raw, scale=F)[,1])
##



##### Merge Data Frames --------------------------------------------------------

TLX_and_ADES2 <- TLX_and_ADES
TLX_and_ADES2$ID <- as.numeric(TLX_and_ADES2$ID)
TLX_and_ADES2$Evaluation <- as.character(TLX_and_ADES2$Evaluation)
TLX_and_ADES2$CL <- as.character(TLX_and_ADES2$CL)

TLX_and_ADES2 <- TLX_and_ADES2[, -1]

main_df <- left_join(main_df, TLX_and_ADES2, by=c('ID',"CL","Evaluation"))


all_trials_df$ID <- as.numeric(all_trials_df$ID)
all_trials_df$Evaluation <- as.character(all_trials_df$Evaluation)
all_trials_df$CL <- as.character(all_trials_df$CL)
all_trials_df <- left_join(all_trials_df, TLX_and_ADES2, by=c('ID',"CL","Evaluation"))




##### Manual Adjustments -------------------------------------------------------
                          
## There have been slight complications with two participants. Those cases are briefly explained
# and corresponding data will be omitted (turned into Na)

                          
# Participant 727655:
  # At the end of block 1 in the 3back under Evaluation condition, the participant missed
  # the correct keys (C and M), resulting in several non-responses. This is clearly visible
  # in the data, especially in the high omission errors.
  #Accordingly, trial_block 2 - block 1 -3back - Evaluation will be omitted
  # for this participant.

manual <- subset(all_trials_df, ID=="727655" & block=="1" & trial_block=="2"
                 & CL=="high" &Evaluation== "1")
manual[2:5] <- NA
manual

all_trials_df[all_trials_df$ID=="727655" & all_trials_df$block=="1" & all_trials_df$trial_block=="2"
        & all_trials_df$CL=="high" & all_trials_df$Evaluation== "1",] <- manual
               
           
manual <- subset(main_df, ID=="727655" & block=="1" & trial_block=="2"
                 & CL=="high" &Evaluation== "1")
manual[6:13] <- NA
manual

main_df[main_df$ID=="727655" & main_df$block=="1" & main_df$trial_block=="2"
        & main_df$CL=="high" & main_df$Evaluation== "1",] <- manual


# Participant 511327:
  # Unfortunately, this participant had to cough heavily at the end of
  # block 2, 2back under Evaluation. This can be seen in the unusually high RT and most committed
  #errors.
  # Accordingly, results for trial_block 2, block 2, 2back under Evaluation will be omitted.
manual <- subset(all_trials_df, ID=="511327" & block=="1" & trial_block=="2"
                 & CL=="high" &Evaluation== "1")
manual[2:5] <- NA
manual

all_trials_df[all_trials_df$ID=="511327" & all_trials_df$block=="1" & all_trials_df$trial_block=="2"
                & all_trials_df$CL=="high" & all_trials_df$Evaluation== "1",] <- manual

## calculate the ratio score for eustress and distress
all_trials_df$ratio <- all_trials_df$Eustress/all_trials_df$Distress


manual <- subset(main_df, ID=="511327" & block=="2" & trial_block=="2"
                 & CL=="low" &Evaluation== "1")
manual[6:13] <- NA
manual

main_df[main_df$ID=="511327" & main_df$block=="1" & main_df$trial_block=="2"
        & main_df$CL=="high" & main_df$Evaluation== "1",] <- manual

## calculate the ratio score for eustress and distress
main_df$ratio <- main_df$Eustress/main_df$Distress


### Save data frames -----------------------------------------------------------
write.csv(all_trials_df, "../Data/clean_data/all_trials_df.csv", row.names = F)
write.csv(main_df, "../Data/clean_data/main_df.csv", row.names = F)




### Aggregate by main conditions CL and Evaluation ---------------------------------

agg_df <- main_df %>%
  group_by(ID,CL,Evaluation) %>%
  dplyr::summarise(across(c(Errors, Errors_cw, cER, cER_cw, oER, oER_cw), ~ sum(., na.rm = TRUE)))

agg_df[,7:9] <- round(agg_df[,7:9],3)

agg_df2 <- main_df %>%
  group_by(ID,CL,Evaluation) %>%
  dplyr::summarise(across(c(RT_Raw, RT_cw, Eustress, Eustress_cw,
                     Distress, Distress_cw, NASA_TLX, NASA_TLX_cw,
                     Mental, Mental_cw, Temporal, Temporal_cw,
                     Effort, Effort_cw, Performance, Performance_cw,
                     Frustration, Frustration_cw),
                   ~ mean(., na.rm = TRUE)))

agg_df <- inner_join(agg_df, agg_df2, by=c('ID',"CL","Evaluation"))
agg_df$RT_Raw <- round(agg_df$RT_Raw, 0)

## calculate the ratio score for eustress and distress
agg_df$ratio <- agg_df$Eustress/agg_df$Distress

write.csv(agg_df, "../Data/clean_data/agg_df.csv", row.names = F)




##### Prepare EDA data for exploratory analysis --------------------------------

# EDA was organised using the python script "Empatica_Processing.py"
EDA_df <- read_csv("../Data/clean_data/EDA_main.csv")
EDA_df <- subset(EDA_df, EDA_df$condition != "resting" & EDA_df$condition !=  "break" )

# Derive conditions
EDA_df$CL <- ifelse(grepl("n2", EDA_df$condition)|grepl("2n", EDA_df$condition),
                    "low", "high")
EDA_df$Evaluation <- ifelse(grepl("stress", EDA_df$condition),"1", "0")
EDA_df$block <- ifelse(grepl("b2", EDA_df$condition),"2", "1")
EDA_agg <- EDA_df %>%
  group_by(ID,CL,Evaluation) %>%
  dplyr::summarise(across(c(EDA,EDA_zw), ~ mean(., na.rm = TRUE)))


## Merge with agg_df

# Since the EDA response is missing for participant 40122, their data has to be excluded from agg_df
agg_df_eda <- subset(agg_df, ID != "40122")
aggEDA_df <- inner_join(agg_df_eda, EDA_agg, by=c('ID',"CL","Evaluation"))

write.csv(aggEDA_df, "../Data/clean_data/aggEDA_df.csv", row.names = F)


remove(df_list,exp_1,exp_2,exp_3,agg_df_eda,agg_df2,EDA_agg,EDA_df,exp_aggregated_RT,
       exp_RT,manual)








