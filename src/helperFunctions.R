#Helper functions
#library(geosphere)

calcdist <- function(lon1, lat1, lon2, lat2){
  pos1 <- as.matrix(data.frame(lon1, lat1))
  pos2 <- as.matrix(data.frame(lon2, lat2))
  
  dist <- distHaversine(pos1, pos2, r=3440)
  return(dist)
}



#convert bearings to radians (for plotting)

#example -10 degrees north = pi/2 + 10*pi/180k