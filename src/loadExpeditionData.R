#load & format expedition data

#library
library(tidyverse)
library(dplyr)
library(zoo)



dat_folder <- "./data/expeditiondata"

dat_files <- list.files(dat_folder, full.names = TRUE)

exp <- lapply(dat_files, read.csv, stringsAsFactors=FALSE)
exp_raw <- do.call(rbind, exp)

#remove any columns where the data is entirely NA

allna <- which(apply(exp_raw, 2, function(x) all(is.na(x))))   #Find columns where all is not NA

exp <- exp_raw %>% select(-allna)

#Convert Time
exp$time <- as.POSIXct((exp$Utc) * (60*60*24), origin = "1899-12-30", tz = "America/Detroit")

#Select only needed columns
exp <- exp %>% select(time, Bsp, Awa, Aws, Twa, Tws, Twd, Lat, Lon, Cog, Sog)

#change names to lowercase for easier handling
names(exp) <- tolower(names(exp))


#remove pre-start and post-finsih
exp <- exp %>% dplyr::filter(time > as.POSIXct("2018-7-14 16:00:00"))

exp <- exp %>% dplyr::filter(time < as.POSIXct("2018-07-17"))


#remove any rows where all values are NA
exp <- exp[rowSums(is.na(exp[, -1:-2]))<10, ]

#set any extreme outliers to NA
exp$tws[exp$tws > 25] <- NA
exp$sog[exp$sog > 15] <- NA
exp$bsp[exp$bsp > 15] <- NA

#impute missing values using a linear interpolation
exp$tws <- na.approx(exp$tws)
exp$bsp <- na.approx(exp$bsp)
exp$twa <- na.approx(exp$twa)
exp$tws <- na.approx(exp$tws)
exp$lat <- na.approx(exp$lat)
exp$lon <- na.approx(exp$lon)
exp$sog <- na.approx(exp$sog)

save(exp, file="./data/expeditionclean.rda")

