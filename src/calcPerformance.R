#calculating boat performance

rm(list=ls())

### Library
library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(arima)

### Load Data
load("./data/positions.Rdata")
load("./data/marks.Rdata")
load("./data/results.Rdata")
load("../polarAnalysis/polarmodel.rda")
load("./data/expeditionclean.rda")

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

### remove and duplicate data

dat <- dat %>% distinct(boat, time, .keep_all=TRUE)



### Calculating Corrected Time for all boats ============================

#calculate corrected time for each position. 

dat$elapsed_secs <- dat$time - dat$start_time

dat$corr_elapsed <- as.numeric(dat$elapsed_secs) * as.numeric(dat$rating) 


dat$corr_time <- dat$corr_elapsed + dat$start_time





#Calculate the Zube's position vs. other boats ================================
classo <- dat %>% filter(class == "Class O Class")

#we only really care about a few boats
# Flying Buffalo, Albacore, Courage, Defiant, JamJam, Shillelagh, Zubenelgenubi

diff <- classo %>% select(boat, corr_time, dmg) 

diff <- diff %>% group_by(boat, corr_time) %>% summarise(dmg = max(dmg))

diff <- diff %>% spread(key=boat, value=dmg)

#impute the data between the points
imputeValues <- function(x){
  
  first_value <- min(x, na.rm=TRUE)
  
  n <- length(x)      #Length 
  
  x[1] <- first_value

  x <- na.approx(x)   #impute the values
  
  n <- n - length(x)
  
  x <- c(x, rep_len(NA, n))  #add NA values so the length is matching
  
  return(x)
  
}

diff_imp <- as.data.frame(apply(diff[, -1], 2, FUN=imputeValues))

diff <- cbind(diff[, c("corr_time")], diff_imp)

zube <- dat %>% filter(boat == "Zubenelgenubi")

diff <- diff[diff$corr_time %in% zube$corr_time, ]

zube$vsAlbacore <- diff$Albacore - diff$Zubenelgenubi 
zube$vsBuffalo <- diff$`Flying Buffalo` - diff$Zubenelgenubi
zube$vsCourage <- diff$Courage - diff$Zubenelgenubi 
zube$vsDefiant <- diff$Defiant - diff$Zubenelgenubi  
zube$vsJamJam <- diff$Jamjam - diff$Zubenelgenubi 
zube$vsShillelagh <- diff$Shillelagh - diff$Zubenelgenubi


#Calculate Zube's performance ===========================================================

#select relevant data from Expedition
exp <- exp %>% select(Boat, time, Utc, Bsp, Awa, Aws, Twa, Tws, Twd, Lat, Lon, Cog, Sog)

#calculate performance



#join zube Yellowbrick position data and expedition performance data





