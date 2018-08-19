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
zube_yb$vsBuffalo <- diff$Zubenelgenubi - diff$`Flying Buffalo` 
zube_yb$vsCourage <- diff$Zubenelgenubi - diff$Courage  
zube_yb$vsDefiant <- diff$Zubenelgenubi - diff$Defiant 
zube_yb$vsJamJam <- diff$Zubenelgenubi- diff$Jamjam 
zube_yb$vsShillelagh <- diff$Zubenelgenubi - diff$Shillelagh

save(zube_yb, file="./data/zube_yb_withdiff.rda")


#Calculate Zube's performance ===========================================================

#select relevant data from Expedition
exp <- exp %>% select(Boat, time, Utc, Bsp, Awa, Aws, Twa, Tws, Twd, Lat, Lon, Cog, Sog)

names(exp) <- tolower(names(exp))

exp <- exp[!is.na(exp$twa), ]

#average over 5 seconds
exp$time <- round_date(exp$time, unit="5 seconds")

exp <- exp %>% group_by(time) %>% summarise_all(mean)


#calculate performance to polars ========
exp$opt_bsp <- getOptV(btw = abs(exp$twa), vtw = exp$tws, pol.model)

exp$pol_perc <- (exp$sog / exp$opt_bsp) * 100

exp$off_bsp <- exp$opt_bsp - exp$bsp



#calculate optimal speed and angle for VMC =========


  
  ##need Bearing to Mark.  To calculate, we need to know which mark we're on. 
  
    #join zube Yellowbrick position data and expedition performance data
    #discard duplicate times
    exp <- exp %>% distinct(time, .keep_all = TRUE) %>% select(-boat)
    
    zube <- full_join(zube_yb, exp, by="time", suffix=c(".yb", ".exp")) %>% arrange(time)
  
    #remove anything before the start
    zube <- zube %>% filter(time >= min(zube$start_time, na.rm=TRUE))
    
    #need to impute the mark information through out the entire data frame to calculate
    #carry the mark forward
    zube$mark <- na.locf(zube$mark)    #locf = last observation carried forward
    zube$markLat <- na.locf(zube$markLat)
    zube$markLon <- na.locf(zube$markLon)
  
  ##clean up and impute missing positional data
    ###Impute any TWD where we have TWA
    zube$twd[!is.na(zube$tws)] <- getTWD(twa=zube$twa[!is.na(zube$tws)], hdg = zube$cog[!is.na(zube$tws)])
  
  
    #Impute Expedition Lat/Lon by linear interpolation for any that have TWD
    hastwd <- !is.na(zube$twd)
    
    zube$lat.exp[hastwd] <- apply(zube[hastwd, "lat.exp"], 2, FUN=imputeValues)
    zube$lon.exp[hastwd] <- apply(zube[hastwd, "lon.exp"], 2, FUN=imputeValues)
  
  ##Calculate Bearing to the Mark
  p1 <- zube[hastwd, c("lon.exp", "lat.exp")]
  p2 <- zube[hastwd, c(c("markLon", "markLat"))]
  
  zube$btm[hastwd] <- bearingRhumb(p1, p2)
  

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

zube$vsAlbacore <- imputeValues(zube$vsAlbacore)

zube$gain <- c(diff(zube$vsAlbacore) ,0) 
zube$gainrate <- zube$gain / (as.numeric(c(diff(zube$time), 0))/3600)




save(zube, file="./data/zube_performance.rda")



