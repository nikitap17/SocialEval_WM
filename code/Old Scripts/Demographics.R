
library(tidyverse)
library(readr)
library(dplyr)
library(car)
library("readxl")


demo_df <- read_csv("Data/PostExp_clean.csv")


age <- data.frame(summary(demo_df[,c("Age")]))
demo_df %>% filter(Sex==2) %>% summarise(n())

age <- demo_df %>%
  summarise(M = mean(Age), SD = sd(Age), min = min(Age), max = max(Age), n = n())
