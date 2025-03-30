##########################################
# Step 6 - EMDAT Geocoding RC            #
#                                        #
# 2024-08-03                             #
# Ryan Calder                            #
##########################################

# This is the code Ryan used to geocode using the geocode() function the 
# locations identified in "6 EMDAT split locations RC.R". Part I of this code
# defines a function called geocode_range() which accepts index positions
# (1,2,3, etc.) of the object 
# location_split_no_generic_no_parentheses_country.fixed, created in 
# "6a EMDAT lat lon.R". It uses the Open Street Map interface
# via function geocode() to return OSM data including lat lon for every
# location supplied. The first time I ran this code, there were many non-
# matches to the OSM database. Part I of this code documents how the results from 
# geocode_range() (including the failed matches) were obtained and written to .csv.
# Then, in an offline process, we (Ryan and Julia) manually recoded the 
# locations and fed that back into "6 EMDAT split locations RC.R" via the file
# osm.replace.csv (or some version of that). Part II of this code uses a function
# geocode_range_try_db_first(), which recodes the locations that didn't match
# the first (or second, or third...) time, starting with the database of 
# successful matches that is updated every time the code is run with new
# location names. This minimizes the number of times that the OSM database
# has to be queried because queries are limited to 1 per second. Ultimately,
# this code checks that all locations in the filtered EMDAT database are 
# successfully geocoded and creates a geocoded object that can be passed to
# Step 7. 


# I. Read libraries and define functions

library(tidygeocoder)
library(dplyr)
library(openxlsx)
library(tidyr)
library(purrr)
library(readxl)

log_message <- function(message) {
  cat(message, "\n")
}

convert_boundingbox <- function(df) {
  if ("boundingbox" %in% names(df)) {
    df <- df %>%
      mutate(boundingbox = 
               ifelse(is.list(boundingbox), 
                      sapply(boundingbox, function(x) paste(x, collapse = ", ")), 
                      boundingbox))
  }
  return(df)
}


escape_parentheses <- function(x) {
  gsub("([()])", "\\\\\\1", x)
}

  
geocode_range <- function(range) {
  all_columns <- c()
  result_list <- list()
  
  for (i in range) {
    location <- unlist(location_split_no_generic_no_parentheses_country.fixed.unique[i])
    location_escaped <- escape_parentheses(location)
    
    temp_df <- data.frame(address = location, stringsAsFactors = FALSE)
    
    # Attempt geocoding
    result <- tryCatch(
      {
        tidygeocoder::geocode(temp_df, address = address, method = "osm", full_results = TRUE)
      },
      error = function(e) {
        log_message(paste("Error geocoding address:", location))
        return(NULL)
      }
    )
    
    if (!is.null(result) && nrow(result) > 0) {
      if ("osm_type" %in% colnames(result)) {
        preferred_result <- result %>% filter(osm_type %in% c("relation", "boundary"))
        if (nrow(preferred_result) == 0) {
          preferred_result <- result
        }
      } else {
        preferred_result <- result
      }
      
      matches <- grep(paste(location_escaped, collapse = "|"), 
                      location_split_no_generic_no_parentheses_country.fixed)
      
      preferred_result <- preferred_result %>%
        mutate(original_location = location, 
               item_number = paste(matches, collapse = ","),
               disaster_number = paste(emdat.data.filtered$DisNo.[matches], collapse = ","))
      
      result <- preferred_result
      log_message(paste("Successfully geocoded address:", location))
    } else {
      log_message(paste("Failed to geocode address:", location))
    }
    
    log_message(paste("Completed geocoding for item number:", i))
    
    if (!is.null(result)) {
      all_columns <- union(all_columns, colnames(result))
      result_list[[length(result_list) + 1]] <- result
    }
  }
  
  combined_results <- do.call(rbind, lapply(result_list, function(df) {
    missing_cols <- setdiff(all_columns, colnames(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    return(df[, all_columns])
  }))
  
  return(combined_results)
}


# Part I: Only run this section if you want to generate your first database
# of OSM data linked to EM-DAT data. This is very time-consuming, so most times
# you will want to skip directly to Part II, which reads in a database of locations
# already geocoded and corrects individual geocoding results, or Part III,
# which reads in a database of locations already geocoded and queries 
# only those which are still unmatched. Note that originally I tried to 
# parallelize this code so it would go faster but that got me temporarily 
# blocked from the OSM API because queries are limited to one per second. 


# Example ranges
range1 <- 1:10
geocode_results_df1 <- geocode_range(range1)

range2 <- 11:165
geocode_results_df2 <- geocode_range(range2)

range3 <- 166:180
geocode_results_df3 <- geocode_range(range3)

range4 <- 181:length(location_split_no_generic_no_parentheses_country.fixed.unique)
geocode_results_df4 <- geocode_range(range4)

# Combine all results
geocode_results_20240718_1408 <- rbind(geocode_results_df1,
                                       geocode_results_df2,
                                       geocode_results_df3,
                                       geocode_results_df4)



# Flatten bounding box list into string for export to XLSX

geocode_results_20240718_1408$boundingbox.flat = 
  sapply(geocode_results_20240718_1408$boundingbox,function(x) paste(unlist(x),collapse=","))

# Use XLSX instead of CSV because of issues with special characters in place names

write.xlsx(geocode_results_20240718_1408[,
                                        which(colnames(geocode_results_20240718_1408)%in%
                                                setdiff(names(geocode_results_20240718_1408),"boundingbox"))],
           "Geocode results - 2024-07-18 - output from Step 6b.xlsx")

# Look at the output. Save the entries that worked as an XLSX file that is 
# passed to Step 2 and delete the ones that didn't code properly. Put the
# "original locations" from that file into osm_replace.csv and rerun 
# Step 6 in order to replace the names of those locations in 
# location_split_no_generic_no_parentheses_country.fixed.
# Then run Part II to re-geocode those entries. 

table(geocode_results_20240718_1408$type)

# Part II: Here we read in a database of geocoded locations and supplement it
# with new geocodes for locations that haven't been processed yet

geocode_previous_results = 
  read_excel("../Step06/6 output - Geocode database.xlsx")

location_index_to_geocode = 
  
  which(is.na(match(str_trim(location_split_no_generic_no_parentheses_country.fixed.unique),
                    str_trim(geocode_previous_results$original_location))))

geocode_results_add_df1 <- geocode_range(location_index_to_geocode)


geocode_results_add_df1$boundingbox.flat = 
  sapply(geocode_results_add_df1$boundingbox,function(x) paste(unlist(x),collapse=","))

geocode_results_add_df1 = 
  geocode_results_add_df1[,match(setdiff(colnames(geocode_results_add_df1),
                                         "boundingbox"),
                                 colnames(geocode_results_add_df1))]

geocode_combined_results = 
  rbind.data.frame(geocode_previous_results,
                   geocode_results_add_df1)

# Make sure that geocoded events have all relevant disaster numbers

for(i in 1:nrow(emdat.data.filtered)){
  disaster.number = emdat.data.filtered$DisNo.[i]
  locations.fixed = unlist(location_split_no_generic_no_parentheses_country.fixed[i])
  for(j in 1:length(locations.fixed)){
    geocode.row = match(str_trim(locations.fixed[j]),geocode_combined_results$original_location)
    
    disasters.listed = geocode_combined_results$disaster_number[geocode.row]
    
    if(length(grep(disaster.number,disasters.listed))==0){
      geocode_combined_results$disaster_number[geocode.row] = 
        paste(geocode_combined_results$disaster_number[geocode.row],",",disaster.number,sep="")    
    }
    
  }
}

## Go through database and remove and consolidate duplicate locations; note that 
## there might still be duplicate geometries coded using alternate place names
## (e.g., "Boston, Massachusetts, USA" vs. "Boston, USA") with different
## EMDAT events associated with each. That is OK because Step 7 handles 
## overlapping location bounding boxes.

dup.location.index = 
  which(unlist(lapply(str_trim(geocode_combined_results$address), function(x)
    sum(str_trim(geocode_combined_results$address)==x)))>1)

remove.rows = c()

for(i in dup.location.index){
  
  dup.location.index.i = which(str_trim(geocode_combined_results$address)==
                                 str_trim(geocode_combined_results$address)[i])
  
  if(is.na(match(i,remove.rows))){
    
    remove.rows = c(remove.rows,setdiff(dup.location.index.i,i))
    
    geocode_combined_results$disaster_number[i] = 
      paste(unique(unlist(strsplit(paste(
        geocode_combined_results$disaster_number[
          dup.location.index.i],collapse=","),","))),collapse=",")  
  }
  

}

geocode_combined_results = 
  geocode_combined_results[setdiff(1:nrow(geocode_combined_results),
                                   remove.rows),]

write.xlsx(geocode_combined_results, 
           "../Step06/6 output - Geocode database.xlsx")

