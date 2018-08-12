#calculating boat performance

rm(list=ls())

### Library
library(tidyverse)
library(lubridate)
library(stringr)

### Load Data
load("./data/positions.Rdata")
load("./data/marks.Rdata")
load("./data/results.Rdata")
load("../polarAnalysis/polarmodel.rda")

### Source other functions

source("src/helperFunctions.R")
source("src/calcCorrection.R")
source("src/optimalPerformanceFunctions.R")

### Join the Yellowbrick data with the results to the positions
dat <- left_join(pos, results)

### Cleanup some data naming
dat <- rename(dat, corr_finish = Corrected)

### Remove Retired, pre-start , post-finish data
dat <- dat %>% filter(!is.na(place))
dat <- dat %>% filter(time >= start_time)
dat <- dat %>% filter(time <= finish_time)


### Calculating Corrected Time for all boats ============================

#calculate corrected time for each position. 

dat$elapsed_secs <- dat$time - dat$start_time

dat$corr_elapsed <- as.numeric(dat$elapsed_secs) * as.numeric(dat$rating) 


dat$corr_time <- dat$corr_elapsed + dat$start_time



#Calculate the Zube's position vs. other boats ================================
zube <- dat %>% filter(boat=="Zubenelgenubi")




