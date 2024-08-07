if(!require("renv", quietly = T)){
  install.packages("renv")
}

library(plyr)
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(rstatix)
library(BayesFactor)
library(car)
library(emmeans)
library(modelbased)
library(rempsyc)
library(effectsize)
library(flextable)
library(parameters)
library(patchwork)
library(lattice)
library(Rmisc)
library(cowplot)
library(readxl)
library(devtools)
library(gghalves)
library(ltm)


renv::init()

