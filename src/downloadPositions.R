#Download Yellowbrick Position Data
library(xml2)
library(tidyr)
library(geosphere)

rm(list=ls())

posurl <- "http://live.adventuretracking.com/xml/bayviewmack2018?n=1000"

filename <- "./data/raw_positions.xml"

download_html(url=posurl, file=filename)

