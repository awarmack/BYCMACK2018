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



#figuring out max VMC Angle....
# We'll check the VMC of both tacks seperately and decide if we're on the wrong tack elsewhere. 

optvmc <- function(btm, twd, tws, pol.model){
  #given a bearing to the mark from wind  and vtw

  #create a vector of angles +/- from the btm
  twa <- seq(-179, 179, by=1)
  
  #find all bsp on the polar
  v <- getOptV(abs(twa), tws, pol.model)  
  
  #Add true wind direction to true wind angle to get actual bearing
  bearings <- twd - twa
  #bearings <- ifelse(bearings <0, 360+bearings, bearings)
  
  #find difference between each bearing and the mark
  btm <- ifelse(btm>180, btm-360, btm)
  diff <- bearings - btm
  
  vmc <- v*cos(diff*(pi/180))
  
  opt <- data.frame(twa, diff, bsp=v, vmc)
  
  vmcopt <- max(vmc)  ### Could be multiple points (how to handle if on opp tacks?)
  
  #optimal angle to the mark
  opttwa <- testtwa[which(vmcopt==vmc)]
  
  #optimal boat speed at optimal vmc
  bsp <- v[which(vmcopt==vmc)]
  
  return(data.frame(bsp=bsp, vmcopt=vmcopt, optangle=opttwa, bow=bow))
  
}



