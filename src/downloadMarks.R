#download marks

#library
library(geosphere)

source("./src/helperFunctions.R")

#distance made good
marks <- read.csv("./data/marks.csv", stringsAsFactors = FALSE)
marks$lat <- as.numeric(marks$lat)
marks$lon <- as.numeric(marks$lon)


#need calcDist from download positions file

marks$dist <- calcdist(marks$lon, marks$lat, lag(marks$lon, 1), lag(marks$lat, 1))
marks$dist[1] <- 0

marks$dtf <- sum(marks$dist)-cumsum(marks$dist)

marks$dmg <- max(marks$dtf)-marks$dtf

to <- matrix(c(marks$lon, marks$lat), ncol=2)
from <- head(rbind(to[1, ], to), -1)

marks$bearing <- bearingRhumb(from, to)



save(marks, file = "./data/marks.Rdata")
