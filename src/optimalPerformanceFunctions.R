#functions for calculating optimal performace



matchLength <- function(v1, v2){
  
  if(length(v1)==length(v2)){
    return(list(v1,v2))
  }
  
  if(length(v1)>length(v2)){
    v2 <- rep_len(v2, length.out = length(v1))
  } else {
    v1 <- rep_len(v1, length.out = length(v2))
  }
  return(list(v1,v2))
}


getOptV <- function(btw, vtw, pol.model){
  #returns Optimal Velocity at a given Bearing to Wind (BTW) and Wind Speed (VTW)
  btw <- normbear(btw)
  
  btw <- convBearing(btw)

  btw <- abs(btw)
    
  #ensure that the length of both btw and vtw are the same...
  inputvectors <- matchLength(btw, vtw)
  btw <- inputvectors[[1]]
  vtw <- inputvectors[[2]]
  
  #output the optimum Velocity for a given BTW and wind speed. 
  v <- bilinear(x=pol.model$twa, y=pol.model$tws, z=pol.model$pol.matrix, x0=btw, y0=vtw)
  
  return(v$z)
  
}



getBTMvBTW <- function(btm, btw){
  #returns the difference between bearing to mark and wind
  
  diff <- btm-btw
  
  diff <- ifelse(diff > 180 | diff < -180, 360-diff, diff)

  return(diff)
    
}


#convert bearing to +/- 0 (north) from 0~360
convBearing <- function(bearing){
  ifelse(bearing>180, bearing-360, bearing)
}


#convert bearing from +/-0 to 0~360
convBearingBack <- function(bearing){
  bearing <- ifelse(bearing>360, bearing-360, bearing)
  bearing <- ifelse(bearing< -360, bearing+360, bearing)
  bearing <- ifelse(bearing<0, bearing+360, bearing)
  
  return(bearing)
}



getDiffMarkWind <- function(btm, twd){
  #gets the difference between the mark and bearing
  # + = Stbd
  # - = port
  
  twd <- convBearing(twd)
  btm <- convBearing(btm)
  
  diffmark <- twd - btm
  
  return(diffmark)
  
}



getOptVMC <- function(diffwind, tws, pol.model){
  #variable = offmark distance between wind and mark
  
  twa <- seq(0,180, by=1)
  
  #test angles off the mark
  offmark <- twa - diffwind
  
  #get the polar curve
  v <- getOptV(twa, tws, pol.model)
  
  #vmc 
  vmc <- v*cos(offmark*(pi/180))
  
  vmcMax <- max(vmc)
  
  opt <- data.frame(twa, offmark, v, vmc)
  
  return(opt[which(vmcMax == vmc), ])
         
  
}





#figuring out max VMC Angle....
# We'll check the VMC of both tacks seperately and decide if we're on the wrong tack elsewhere. 
#note that this function is not vectorized.

optvmc <- function(btm, twd, tws, pol.model){
  #given a bearing to the mark from wind  and vtw
  
  #twd = True Wind Direction (between 0 - 360)

  #create a vector of angles +/- from the btm
  twa <- seq(-179, 179, by=1)
  
  #find all bsp on the polar
  v <- getOptV(twa, tws, pol.model)  
  
  twd <- convBearing(twd)
  
  #Add true wind direction to true wind angle to get actual bearings 
  bearings <- twd + twa
  
  #bearings <- ifelse(bearings <0, 360+bearings, bearings)
  bearings <- ifelse(bearings > 360, bearings-360, bearings)
  
  #find difference between each bearing and the mark
  btm <- convBearing(btm)  #convert to bearing +/- 180 degrees from north (rather than 0~360)
  offmark <- bearings - btm   
  
  vmc <- v*cos(offmark*(pi/180))
  
  #return bearing to 0-360 formant
  bearings <- convBearingBack(bearings)
  
  opt <- data.frame(btm, twd, twa, bearings, offmark, bsp=v, vmc)
  names(opt) <- c("btm", "twd", "opt_twa", "opt_bear", "DegOffMark", "opt_bsp", "vmc")
  
  vmcopt <- max(vmc)  ### Could be multiple points (how to handle if on opp tacks?)
  
  opt$vmc_gain <- vmcopt - getOptV(abs(btm-twd), vtw=tws, pol.model)
  
  #optimal angle to the mark
  return_opt <- opt[which(vmcopt==vmc), ]
  
  return(return_opt)
  
}



