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
  
  diff <- btw-btm
  
  diff <- ifelse(diff > 180 | diff < -180, 360-diff, diff)

  return(diff)
    
}



#figuring out max VMC Angle....
# We'll check the VMC of both tacks seperately and decide if we're on the wrong tack elsewhere. 

optvmc <- function(btm, btw, vtw, pol.model){
  #given a bearing to the mark from wind  and vtw
  
  # Bearing Off Wind = Bearing to Wind - Bearing to Mark
  bow <- getBTMvBTW(btm, btw)
  
  #create a vector of angles +/- from the btm
  testbtw <- c(seq(bow-40, bow, by=0.5), seq(bow+0.1, bow+40, by=0.5))
  cosangle <- cos((testbtw - bow)*(pi/180))
  
  #find max VMC to the mark
  v <- getOptV(testbtw, vtw, pol.model)
  vmc <- v*cosangle
  vmcopt <- max(vmc)
  
  #optimal angle to the mark
  optvmcangle <- testbtw[which(vmcopt==vmc)]
  
  #optimal boat speed at optimal vmc
  bsp <- v[which(vmcopt==vmc)]
  
  return(list(bsp=bsp, vmcopt=vmcopt, optangle=optvmcangle, optheading = ,bow=bow))
  
}



