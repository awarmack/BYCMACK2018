#load & format expedition data

#library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)


dat_folder <- "./data/expeditiondata"

dat_files <- list.files(dat_folder, full.names = TRUE)

exp <- lapply(dat_files, read.csv, stringsAsFactors=FALSE)
exp_raw <- do.call(rbind, exp)

#remove any columns where the data is entirely NA

allna <- which(apply(exp_raw, 2, function(x) all(is.na(x))))   #Find columns where all is not NA

exp <- exp_raw %>% select(-allna)


#Convert Time
exp$time <- as.POSIXct((exp$Utc) * (60*60*24), origin = "1899-12-30", tz = "America/Detroit")

#remove pre-start
startTime <- as.POSIXct("2018-7-14 11:00:00")

exp <- exp %>% filter(time > startTime)


#Remove outliers
#e.g any speeds over 20 kts
exp <- exp %>% filter(exp$time < as.POSIXct("2018-07-17"))
exp <- exp %>% filter(exp$Sog < 15)
exp <- exp %>% filter(exp$Bsp < 15)

#e.g any Tws over 30 kts

save(exp, file="./data/expeditionclean.rda")

