rm(list=ls())

#parse Positions
library(xml2)
library(tidyr)
library(geosphere)
library(lubridate)


source("./src/parseXMLfunctions.R")

filename <- "./data/raw_positions.xml"

pos <- parse_yb_XML(filename)

#### clean up
pos$GpsAt <- as.POSIXct(pos$GpsAt)

pos <- pos %>% rename(lat = Latitude, lon = Longitude, time = GpsAt) %>% select(-year)

#arrange in descending order
pos <- pos %>% arrange(boat, time)

pos$time <- with_tz(pos$time, tzone="America/Detroit")


### Feature Engineering ###
calcdist <- function(lon1, lat1, lon2, lat2){
  pos1 <- as.matrix(data.frame(lon1, lat1))
  pos2 <- as.matrix(data.frame(lon2, lat2))
  
  dist <- distHaversine(pos1, pos2, r=3440)
  return(dist)
}

#distance travelled between last point
pos <- pos %>% 
  group_by(boat) %>% 
  arrange(time, .by_group=TRUE) %>% 
  mutate(lat2 = lag(lat, 1), 
         lon2 = lag(lon, 1)) 

pos1 <- matrix(c(pos$lon, pos$lat), nrow=nrow(pos), ncol=2)
pos2 <- matrix(c(pos$lon2, pos$lat2), nrow=nrow(pos), ncol=2)

pos$d <- distHaversine(pos1, pos2, r=3440)
pos$d[is.na(pos$d)] <- 0

pos <- pos %>% group_by(boat) %>% mutate(cumd = cumsum(d))


# Get the distance to finish
source("./src/downloadMarks.R")

totalLength <- max(marks$dtf)

#identify which mark the boat is at...
# Because the marks are progressivly north, going north of the mark is rounding. 

whichMark <- function(lat, marks, returns="mark"){
  
  if(lat > max(marks$lat)){
    return(marks[nrow(marks), returns])
  }
  
  mark <- marks[min(which(lat < marks$lat)), returns]
  return(mark)
}

pos$mark <- sapply(pos$lat, whichMark, marks, returns="mark")
pos$markLat <- as.numeric(sapply(pos$lat, whichMark, marks, returns="lat"))
pos$markLon <- as.numeric(sapply(pos$lat, whichMark, marks, returns="lon"))

#calculate the distance to finish (Distance to the Mark + Mark to finish)
pos$dtm <- calcdist(lon1=pos$lon, lat1=pos$lat, lon2=pos$markLon, lat2=pos$markLat)
mark_dtf <- as.numeric(sapply(pos$lat, whichMark, marks, returns="dtf"))

pos$dtf <- mark_dtf + pos$dtm
#pos$markdist <- as.numeric(sapply(pos$lat, whichMark, marks, returns="dist"))

#distance made good to the mark
pos$dmg <- totalLength - pos$dtf









save(pos, file="./data/positions.Rdata")
