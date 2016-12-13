setwd("~/Documents/MSDS/DataMiningFinal")
suppressMessages({
  library(caret); library(DAAG); library(car); library(PerformanceAnalytics); library(dplyr); library(readr); 
  library(glmnet); library(htmlTable); library(ggthemes); library(boot); library(lubridate); library(randomForest); 
  library(party); library(ggplot2); library(scales); library(ROCR); library(lubridate)
})

set.seed(12345)

##############################################################
##
## LOAD DATA
## 
##############################################################
caseCentered     <- read_csv('../Data/ED_Visit_Dx.csv')
justiceCentered  <- read_csv('../Data/ED_Visit_Ins.csv')
ed_visit_loc  <- read_csv('../Data/ED_Visit_Loc.csv')
ed_visit_phys <- read_csv('../Data/ED_Visit_Phys.csv')
ed_visit      <- read_csv('../Data/ED_Visit.csv')
ed_visit2     <- read_csv('../Data/ED_Visit.csv')
