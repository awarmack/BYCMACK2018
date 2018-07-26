#Download class data

#get ratings and results
library(rvest)
library(tidyr)
library(dplyr)
library(tidyverse)
library(xml2)
library(stringr)
library(lubridate)





### Function Definitions ###
scrape_results <- function(results_page){
  #get the results and rankings
  results <- read_html(results_page)
  
  all_tables <- results %>% html_nodes("table")
  return(all_tables)
}


#find the table that contains "Albacore"
contains_tuna <- function(node){
  #searches the text of the node and returns TRUE/FALSE if it contains Albacore
  
  text_data <- node %>% html_children %>% html_text()
  
  contains_tuna <- any(grepl("*[aA]lbacore", text_data))
  
  return(contains_tuna)
}


format_results <- function(scraped_data, tableno){
  
  #convert the html to a dataframe
  # we know that the shore corse is table number 9
  results <- html_table(scraped_data[[tableno]], fill=TRUE)
  
  #Apply a column names
  sail_col <- head(grep("[sS]ail", results),1)
  name_row <- head(grep("[sS]ail", results[, sail_col]),1)
  names(results) <- results[name_row, ]
  
  #remove the column after home_city if it isn't finish_time
  if(any(grepl("City, State", names(results)))){
    col_after_city <- which(names(results)=="City, State")+1
    
    if(names(results)[col_after_city]==""){
      #remove blank column after city if it exists
      results <- results[, -col_after_city]
      
    }
    names(results)[which(names(results)=="City, State")] <- "home_city"
  } else {
    results$home_city <- NA
  }
  
  
  if(!any(grepl("[rR]ating", names(results)))){
    #there is no rating column, so add one. 
    results$rating <- NA
  }
  
  if(any(is.na(names(results)))){
    na_col <- which(is.na(names(results)))
    results <- results[, -na_col]
  }
  
  if(any(names(results)=="Rounding Time")){
    results <- results[, -which(names(results)=="Rounding Time")]
    
  }
  
  
  #we know these two from looking at the table
  finish_col <- grep("[fF]inish.*", names(results))
  if(length(finish_col)>1){
    stop("more than one finish column")
  } 
  
  if(length(finish_col)==0){
    results$finish_time <- NA
  }
  
  if(length(finish_col)==1){
    names(results)[finish_col] <- "finish_time"
  }
  
  
  names(results)[which(names(results)=="Elapsed Time")] <- "elapsed_time"
  #names(results)[which(names(results)==".1")] <- "place"   ### can't do this...
  names(results)[grepl("[rR]ating", names(results))] <- "rating"
  
  #find the place column. Check for all columns that have only 1 digit. 
  
  isPlace <- function(column){
    #does the row contain greater than 70% 1 or 2 digit numbers? 
    sum(grepl("^\\d{1,2}$", column))/length(column)>.50
    
  }
  
  place_col <- apply(results, 2, FUN=isPlace)
  
  if(all(!place_col)){
    stop("Could not find PLace column")
  }
  
  if(sum(place_col)>1){
    stop("more than one place column found")
  }
  
  names(results)[place_col]<- "place"
  
  
  #results <- results %>% select(Sail, Boat, home_city, rating)
  #remove any duplicate column names or blank column names
  results <- results[, !duplicated(colnames(results))]
  
  
  results <- results %>% select(Sail, Boat, home_city, rating, finish_time, elapsed_time, Corrected, place)
  
  #results <- results[ ,-1]   # we don't need the first column
  
  
  uneeded_rows <- grep("Sail", results$Sail) #find the uneeded rows that contain "Sail"
  
  results <- results[-uneeded_rows, ]  #remove the rows
  
  blank_rows <- results[, 1] == ""  #logical vector showing TRUE if it should be removed
  
  #remove blank rows
  results <- results[!blank_rows, ]
  
  #remove protest row
  protest_row <- grepl("[pP]rotest", results$Sail)
  
  results <- results[!protest_row, ]
  
  # rows_removed <- grep("finished", results[, 1])
  # results <- results[!rows_removed, ]
  # 
  # rows_removed <- grep("[cC]lass", results[, 1])
  # results <- results[!rows_removed, ]
  
  return(results)
}


get_class_data <- function(results){
  
  #get the positions of the classes 
  class_name_rows <- grep("[cC]lass", results[ ,1])
  
  class_names <- results[class_name_rows, 1]
  
  #get the class data
  #we need the data between the class_name_rows
  start <- class_name_rows + 1  #start after the class name rows
  end <- c(tail(class_name_rows, -1) - 1, nrow(results))  #subtract 1 and add on the last column
  
  #fill the class rows
  class_data_rows <- mapply(seq, start, end)
  
  class_data_ls <- lapply(class_data_rows, function(i) results[i, ])
  names(class_data_ls) <- class_names
  
  #combine into a data frame
  class_data <- do.call(rbind, class_data_ls)
  
  class_data$class <- row.names(class_data)
  
  row.names(class_data) <- NULL
  
  class_data$class <- str_extract(class_data$class, ".*(?=\\.\\d*)")
  
  return(class_data)
}


### Get the data ###
results_page <- "http://www.bycmack.com/results_shore.cfm"

scraped_data <- scrape_results(results_page)

tableno <- max(which(sapply(scraped_data, contains_tuna)))

results <- format_results(scraped_data, tableno)

rm(scraped_data)

results <- get_class_data(results)


## Clean Up ###########

retired <- results$Corrected=="RET"

#set retired to place
results$place[retired] <- "RET"
results$finish_time[retired] <- NA
results$elapsed_time[retired] <- NA
results$Corrected[retired] <- NA

results <- results %>% rename(boat = Boat, sail = Sail)


#Convert data types
results$elapsed_time <- hms(results$elapsed_time)
results$Corrected <- hms(results$Corrected)

#finish time
# find the start date


save(results, file="./data/results.Rdata")

