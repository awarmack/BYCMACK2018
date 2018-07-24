##### Parse Yellowbrick XML data ######


library(xml2)
#library(XML)
library(tidyverse)

#Gets the value of each position text value
get_values <- function(boat_path){
  datues <- xml_children(boat_path) %>% xml_text()
  return(datues)
}

#Takes the boat data and reformats it
clean_boat_data <- function(dat){
  
  numeric_cols <- c("Latitude", "Longitude", "SOG", "COG")
  date_cols <- c("GpsAt")
  
  dat <- dat %>% mutate_all(funs(as.character))
  
  dat <- dat %>% select("boat", date_cols, numeric_cols)
  
  dat <- dat %>% mutate_at(vars(numeric_cols), funs(as.numeric))
  dat$GpsAt <- strptime(dat$GpsAt, "%FT%T", tz="UTC")
  
  return(dat)
}

#Combines the position data for each boat
get_boat_data <- function(yb, target_boat){
  # extracts data for a single boat from yellowbrick data
  # Arguements
  #   yb = Yellowbrick data
  #   target_boat = Character name of boat
  #print(target_boat)
  
  boat_xpath <- paste("//Device[@name=\"", target_boat, "\"]", sep="")
  
  # Get the node with the boat name
  boat_path <- xml_find_all(yb, boat_xpath) 
  
  # Get the column names for each position
  if (xml_length(boat_path)<1){
    dat <- NA
  } else {
    col_names <- xml_child(boat_path) %>% xml_children() %>% xml_name()
    
    boat_positions <- boat_path %>% xml_children()
    
    dat <- lapply(boat_positions, get_values)
    
    dat <- do.call(rbind, dat)
    
    dat <- as.data.frame(dat)
    names(dat) <- col_names
    
    dat$boat <- target_boat
    
    dat <- clean_boat_data(dat)
  }

  return(dat)
}

#gets list of all boats
get_boats <- function(yb){
  
  boats <- xml_find_all(yb, "//Device") %>% xml_attr("name")
  return(boats)
}

#Returns a dataframe for all boats and positions for the given yb file
parse_yb_XML <- function(yb_file){
  #yb_file = yellowbrick file for the entire fleet for one year. 

  
  #print(yb_file)
  yb <- read_xml(yb_file)
  
  boats <- get_boats(yb)
  
  allboats <- lapply(boats, function(x) get_boat_data(yb, x))
  
  allboats <- do.call(rbind, allboats)
  
  allboats$year <- substr(yb_file, 27, 30)
  
  return(allboats)
}




