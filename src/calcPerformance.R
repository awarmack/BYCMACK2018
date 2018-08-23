#calculating boat performance

rm(list=ls())

### Library
library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(akima)
library(geosphere)

### Load Data
load("./data/positions.Rdata")
load("./data/marks.Rdata")
load("./data/results.Rdata")
load("./data/polarmodel.rda")
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

dat <- dat %>% ungroup()


### Calculating Corrected Time for all boats ============================

#calculate corrected time for each position. 

dat$elapsed_secs <- dat$time - dat$start_time

dat$corr_elapsed <- as.numeric(dat$elapsed_secs) * as.numeric(dat$rating) 

dat$corr_time <- dat$corr_elapsed + dat$start_time

posn <- dat

save(posn, file="./data/positions_wcorr.Rdata")



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

zube_yb$vsAlbacore <- diff$Zubenelgenubi - diff$Albacore
zube_yb$vsAlbacoreGain <- c(NA, diff(zube_yb$vsAlbacore))
zube_yb$vsTunaSpeedDiff <- zube_yb$vsAlbacoreGain / .25    #15 min time diff
zube_yb$vsBuffalo <- diff$Zubenelgenubi - diff$`Flying Buffalo` 
zube_yb$vsCourage <- diff$Zubenelgenubi - diff$Courage  
zube_yb$vsDefiant <- diff$Zubenelgenubi - diff$Defiant 
zube_yb$vsJamJam <- diff$Zubenelgenubi- diff$Jamjam 
zube_yb$vsShillelagh <- diff$Zubenelgenubi - diff$Shillelagh

save(zube_yb, file="./data/zube_yb_withdiff.rda")


#Calculate Zube's performance ===========================================================

#smooth the data =======

#rolling 30 second average
exp$bsp <- rollapply(exp$bsp, width=30, FUN = function(x) mean(x, na.rm=TRUE), fill=NA)

#rolling 10 second average
exp$sog <- rollapply(exp$sog, width=10, FUN = function(x) mean(x, na.rm=TRUE), fill=NA)
exp$awa <- rollapply(exp$awa, width=10, FUN = function(x) mean(x, na.rm=TRUE), fill=NA)
exp$aws <- rollapply(exp$aws, width=10, FUN = function(x) mean(x, na.rm=TRUE), fill=NA)
exp$twa <- rollapply(exp$twa, width=10, FUN = function(x) mean(x, na.rm=TRUE), fill=NA)

rollmeanbear <- function(bearing, width){
  #rolling mean for a bearing
  
  bearing <- fromNorth(bearing)
  
  rollbear <- rollapply(bearing, width, FUN = function(x) mean(x, na.rm=TRUE), fill=NA)
  
  rollbear <- normbear(rollbear)
  
  return(rollbear)
}

exp$cog <- rollmeanbear(exp$cog, width=10)

#handle missing COG
exp$cog[is.nan(exp$cog)] <- NA

imputeBearing <- function(bearing){
  bearing <- fromNorth(bearing)
  
  bearing <- na.approx(exp$cog, maxgap = 30, na.rm = FALSE)
  
  bearing <- normbear(bearing)
  
  return(bearing)
  
}


exp$cog <- imputeBearing(exp$cog)

exp$twd[is.na(exp$twd)]<- getTWD(exp$twa[is.na(exp$twd)], exp$cog[is.na(exp$twd)])

exp$twd <- rollmeanbear(exp$twd, width=10)



#remove any lines where TWA is na
exp <- exp[!is.na(exp$twa), ]



#calculate performance to polars ========
exp$opt_bsp <- getOptV(btw = abs(exp$twa), vtw = exp$tws, pol.model)

exp$pol_perc <- (exp$sog / exp$opt_bsp) * 100

exp$off_bsp <- exp$opt_bsp - exp$bsp

save(exp, file="expeditiondata_perf.rda")


#calculate optimal speed and angle for VMC =========


  
  ##need Bearing to Mark.  To calculate, we need to know which mark we're on. 
  
    #join zube Yellowbrick position data and expedition performance data
    #discard duplicate times
    
    #round up to the nearest second
    exp$time <- ceiling_date(exp$time, unit = "second" )
    exp <- exp %>% distinct(time, .keep_all = TRUE) 

      
    zube_yb$time <- ceiling_date(zube_yb$time, unit="second")



    zube <- full_join(zube_yb, exp, by="time", suffix=c(".yb", ".exp")) %>% arrange(time)
  
    #remove anything before the start
    zube <- zube %>% filter(time >= min(zube$start_time, na.rm=TRUE))
    
    #need to impute the mark information through out the entire data frame to calculate
    #carry the mark forward
    zube$mark <- na.locf(zube$mark)    #locf = last observation carried forward
    zube$markLat <- na.locf(zube$markLat)
    zube$markLon <- na.locf(zube$markLon)
    


  
  ##clean up and impute missing positional data
  #linear interpolation of points
  impcols <- c("lat.yb", "lon.yb", "SOG", "dtm", "dtf", "dmg", "vsAlbacore", "vsBuffalo", "vsCourage", 
               "vsDefiant", "vsJamJam", "vsShillelagh", "bsp", "tws", "lat.exp", "lon.exp")
  
  toimpute <- zube[, impcols]
  toimpute <- zoo(toimpute, zube$time)
  
  imputted <- na.approx(toimpute, na.rm = FALSE)
  
  zube[, impcols] <- imputted
  
  
  zube$tdiff <- c(0, diff(zube$time))
  
  
  #gain/loss vs. albacore
  zube$gain <- c(0, diff(zube$vsAlbacore)) 
  zube$closerate <- zube$gain / ((zube$tdiff)/3600)
  
  

  ##Calculate Bearing to the Mark
  p1 <- zube[!is.na(zube$lon.exp), c("lon.exp", "lat.exp")]
  p2 <- zube[!is.na(zube$lon.exp), c(c("markLon", "markLat"))]
  
  zube$btm[!is.na(zube$lon.exp)] <- bearingRhumb(p1, p2)
  

  # get the optimal bearing off the wind
  optimalcourse <- mapply(FUN = optvmc, zube$btm, zube$twd, zube$tws, MoreArgs = list(pol.model), SIMPLIFY = FALSE)

  optimalcourse <- do.call(bind_rows, optimalcourse)
  
  stopifnot(nrow(optimalcourse)==nrow(zube))

  #add the optimal course back to the data frame
  zube <- bind_cols(zube, optimalcourse[, 4:9])

zube$act_vmc <- getVMC(btm = zube$btm, cog = zube$cog, bsp = zube$bsp)
zube$off_vmc <- zube$opt_vmc - zube$act_vmc
zube$vmc_perc <- (zube$act_vmc/zube$opt_vmc) * 100





### Comparison of Performance vs. Position vs. Other Boats
# First, we need to interpolate our position vs other boats since the points do not fully line up. 





save(zube, file="./data/zube_performance.rda")



