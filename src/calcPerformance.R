#calculating boat performance

rm(list=ls())

### Library
library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(akima)

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
results <- results %>% select(-Corrected, -elapsed_time)

dat <- left_join(pos, results)

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

zube_yb <- dat %>% filter(boat == "Zubenelgenubi")

diff <- diff[diff$corr_time %in% zube_yb$corr_time, ]

zube_yb$vsAlbacore <- diff$Albacore - diff$Zubenelgenubi
zube_yb$vsBuffalo <- diff$`Flying Buffalo` - diff$Zubenelgenubi
zube_yb$vsCourage <- diff$Courage - diff$Zubenelgenubi
zube_yb$vsDefiant <- diff$Defiant - diff$Zubenelgenubi 
zube_yb$vsJamJam <- diff$Jamjam - diff$Zubenelgenubi
zube_yb$vsShillelagh <- diff$Shillelagh - diff$Zubenelgenubi


#Calculate Zube's performance ===========================================================

#select relevant data from Expedition
exp <- exp %>% select(Boat, time, Utc, Bsp, Awa, Aws, Twa, Tws, Twd, Lat, Lon, Cog, Sog)

exp <- exp[!is.na(exp$Twa), ]

#average over 5 seconds
exp$time <- round_date(exp$time, unit="5 seconds")

exp <- exp %>% group_by(time) %>% summarise_all(mean)

#calculate performance
exp$opt_bsp <- getOptV(btw = abs(exp$Twa), vtw = exp$Tws, pol.model)

exp$pol_perc <- (exp$Sog / exp$opt_bsp) * 100

#calculate optimal speed and angle for VMC. 
#need Bearing to Mark.  To calculate, we need to know which mark we're on. 

#join zube Yellowbrick position data and expedition performance data
exp$time <- round_date(exp$time, unit="second")

#discard duplicate times
exp <- exp %>% distinct(time, .keep_all = TRUE)

zube <- full_join(zube_yb, exp) %>% arrange(time)

#remove anything before the start
zube <- zube %>% filter(time >= min(zube$start_time, na.rm=TRUE))

#need to impute the mark information through out the entire data frame to calculate




#just looking at the time where we know both
zube_yb_exp <- zube[zube$time %in% zube_yb$time, ]


