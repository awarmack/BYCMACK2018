#Download Yellowbrick Position Data
library(xml2)
library(tidyr)
library(geosphere)

rm(list=ls())

posurl <- "http://live.adventuretracking.com/xml/bayviewmack2018?n=1000"

filename <- "./data/raw_positions.xml"

download_html(url=posurl, file=filename)

#parse the xml file

source("./src/parseXMLfunctions.R")

pos <- parse_yb_XML(filename)

pos$GpsAt <- as.POSIXct(pos$GpsAt)

#### clean up

pos <- pos %>% rename(lat = Latitude, lon = Longitude)


### Feature Engineering ###


#distance travelled between last point
pos <- pos %>% 
        group_by(boat) %>% 
        arrange(GpsAt, .by_group=TRUE) %>% 
        mutate(lat2 = lag(lat, 1), 
               lon2 = lag(lon, 1)) 

pos1 <- matrix(c(pos$lon, pos$lat), nrow=nrow(pos), ncol=2)
pos2 <- matrix(c(pos$lon2, pos$lat2), nrow=nrow(pos), ncol=2)

pos$d <- distHaversine(pos1, pos2, r=3440)
pos$d[is.na(pos$d)] <- 0

pos <- pos %>% group_by(boat) %>% mutate(cumd = cumsum(d))

#distance made good




#vmg


#distance to the finish


