#correction for rating

#Create vector of cumulative elapsed time since start for each boat. 

calcCorrected<- function(time, rating){
  # Time = column of time spans
  # returns a column of elapsed times
  
  startTime <- time[1]
  
  #number of seconds
  secs <- as.numeric(difftime(time, lag(time, 1), units="secs"))
  
  secs[1] <- 0
  
  #cumulative number of seconds 
  elapsed_secs <- cumsum(secs)
  
  elapsed_corrected <-elapsed_secs * rating 
  
  corrected_time <- startTime + elapsed_corrected
  
  return(corrected_time)
  
}


#at each point, calculate elapsed time X rating


