##########################################
# Step 5 - EMDAT lat lon RC            #
#                                        #
# 2024-08-10                             #
# Ryan Calder                            #
##########################################

# This is the code Ryan used to take raw EMDAT data, subset it based on disaster
# types of interest, and extract subnational locations for geocoding. Subnational
# locations are split up into jagged lists according to the observation that
# the EMDAT locations field separates discrete locations using commas. There
# are however many exceptions, and locations with alternate/unconventional/wrong
# spellings that need to be corrected manually. This code therefore also
# reads in a file called osm.replace (or some version of that), which replaces
# the EMDAT locations with locations that are confirmed to work with Open
# Street Maps. This file also corrects several recurring issues that interfere
# with Open Street Maps pairing, like formal country names, and place names
# that are paired with some generic descriptor (which is frequently 
# incorrect anyway) like "Mississippi province". The culmination of this file is
# location_split_no_generic_no_parentheses_country.fixed, the jagged list
# of locations with the same length as the subsetted EMDAT disaster data has rows
# and which is passed to 6 EMDAT Geocoding RC for geocoding. The osm.replace
# file that is used to update the locations was developed iterating back and
# forth between this file and 6 EMDAT Geocoding.

# I. Clear environment, set WD, and read in (1) EMDAT data, (2) our 
# "country data" sheet that maps overseas territories to parent countries for 
# the purposes of matching them to World Bank data, and (3) manual overrides
# to location data to query in Open Street Maps (OSM)

rm(list=ls())

library(readxl)
library(stringr)
library(openxlsx)

# Getting the path of your current open file
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

emdat.data = read_excel("public_emdat_incl_hist_2025-01-05.xlsx")

country.data = 
  read.csv(paste(
    "../Step04/Identify mismatched and missing countries - step 4/",
    "SLIIDERS country codes with names and territorial mapping.csv",
    sep=""))

osm.replace = read.csv("osm_replace.csv")

# II. Subset EMDAT data based on disasters of interest and years since 1990

disasters.consider = c("Tropical cyclone", 
                        "Coastal flood", 
                        "Extra-tropical storm",
                        "Tsunami",
                        "Storm surge")

disasters.exclude = 
  setdiff(emdat.data$`Disaster Subtype`, disasters.consider)

emdat.data.filtered = 
  emdat.data[emdat.data$`Disaster Subtype` %in% disasters.consider &
               emdat.data$`Start Year` >= 1985,]

# III. Make alternate "total deaths" column where NAs are treated as 0s 

emdat.data.filtered$total.deaths.reported = emdat.data.filtered$`Total Deaths`
emdat.data.filtered$total.deaths.na.as.0 = 
  emdat.data.filtered$total.deaths.reported

emdat.data.filtered$total.deaths.na.as.0[
  is.na(emdat.data.filtered$total.deaths.na.as.0)] = 0

emdat.data.filtered = 
  emdat.data.filtered[,colnames(emdat.data.filtered)%in%
                        setdiff(colnames(emdat.data.filtered),
                                "`Total Deaths`")]

# III. Go through EMDAT data and add country code to pair with SLIIDERS. Use list
# structure because EMDAT lists as countries areas that are considered 
# as separate countries in SLIIDERS (e.g., "Netherlands Antilles" in EMDAT is 
# Curacao, Aruba, etc. in SLIIDERS).

emdat.data.filtered$SLIIDERS.country.code = list(NA)

for(i in 1:nrow(emdat.data.filtered)){
  
  emdat.country = emdat.data.filtered$Country[i]
  country.code.index = which(country.data$Name..ISO.3166.if.available.==
                               emdat.country)
  
  if(length(country.code.index)==0){
    country.code.index = which(country.data$Alternate.name==emdat.country)
  }
  
  
  emdat.data.filtered$SLIIDERS.country.code[[i]] = 
    country.data$SLIIDERS.country.code[country.code.index]
  
  
}

emdat.data.filtered$SLIIDERS.country.code[
  which(emdat.data.filtered$ISO!=emdat.data.filtered$SLIIDERS.country.code)]



## Ensure that countries that didn't find country code match in SLIIDERS
## are all landlocked (e.g., Laos, Bhutan, Zimbabwe)

emdat.data.filtered$Country[which(unlist(lapply(
  emdat.data.filtered$SLIIDERS.country.code,function(x) length(x)))==0)]

## Delete from EMDAT database events in landlocked countries 

emdat.data.filtered = 
  emdat.data.filtered[setdiff(1:nrow(emdat.data.filtered),
                              which(unlist(lapply(
                                emdat.data.filtered$SLIIDERS.country.code,
                                function(x) length(x)))==0)),]

## Make flat SLIIDERS country code variable for exporting 

emdat.data.filtered$SLIIDERS.country.code.flat = 
  sapply(emdat.data.filtered$SLIIDERS.country.code, function(x) 
    paste(unlist(x), collapse = ","))


# IV. Set up EMDAT location data to geocode in Open Street Maps

## Define generic words to scan for and delete from place name. These generic
## names interfere with place matching in Open Street Maps.

generic_words = 
  c("islands", "island", "atolls", "atoll", "provinces", "province", 
    "districts", "district", "region", "regions", "county",
    "counties", "Islands", "Isl.", "isl.",
    "state", "states", "territory", "territories", "municipality", 
    "city", "cities", "town", "towns", "village", "villages", "area", 
    "zone", "division", "divisions", "isl", "prov", "dist")


## Function to split location by commas while respecting parentheses

split_location_by_commas = function(location) {
  if (is.na(location)) {
    return(character(0))
  }
  parts = unlist(strsplit(location, ",(?![^()]*\\))", perl = TRUE))
  parts = str_trim(parts)
  return(parts)
}

location_split = 
  lapply(emdat.data.filtered$Location, split_location_by_commas)


## Function to remove generics  (state, province, etc.) from location names

identify_and_remove_generics = function(parts, generic_words) {
  if (length(parts) == 0) {
    return(list(location_split_no_generic = parts, generic = parts))
  }
  
  cleaned_parts = c()
  generic_parts = rep(NA, length(parts))
  
  for (i in seq_along(parts)) {
    part = as.character(parts[[i]]) # Ensure part is a character
    words = unlist(strsplit(part, "\\s+"))
    lower_words = tolower(words)
    
    # Identify and keep generic words
    if (any(lower_words %in% generic_words)) {
      generic_parts[i] = paste(words[lower_words %in% generic_words], 
                               collapse = " ")
      non_generic_words = words[!lower_words %in% generic_words]
      if (length(non_generic_words) > 0) {
        cleaned_part = paste(non_generic_words, collapse = " ")
        cleaned_parts = c(cleaned_parts, cleaned_part)
      }
    } else {
      cleaned_parts = c(cleaned_parts, part)
    }
  }
  
  return(list(location_split_no_generic = cleaned_parts, generic = generic_parts))
}

## Apply the identify_and_remove_generics function to each entry in location_split

processed_split = 
  lapply(location_split, identify_and_remove_generics, generic_words)

## Extract cleaned locations and generics. Save generics for future reference.
location_split_no_generic =
  lapply(processed_split, function(x) x$location_split_no_generic)
generic = lapply(processed_split, function(x) x$generic)

## Function to remove contents of parentheses from location names

remove_and_store_parentheses = function(parts) {
  cleaned_parts = sapply(parts, function(part) {
    cleaned_part = gsub("\\s*\\([^\\)]+\\)", "", part)
    str_trim(cleaned_part)
  })
  
  parentheses_parts = sapply(parts, function(part) {
    if (grepl("\\(", part)) {
      parentheses_content = gsub(".*\\(([^\\)]+)\\).*", "\\1", part)
      return(parentheses_content)
    } else {
      return(NA)
    }
  })
  
  return(list(cleaned_parts = 
                cleaned_parts, parentheses_parts = parentheses_parts))
}


processed_parentheses_split = 
  lapply(location_split_no_generic, remove_and_store_parentheses)

## Extract cleaned locations and removed parentheses content

location_split_no_generic_no_parentheses = 
  lapply(processed_parentheses_split, function(x) x$cleaned_parts)
location_split_no_generic_only_parentheses =
  lapply(processed_parentheses_split, function(x) x$parentheses_parts)

processed_parentheses_no_generics = 
  lapply(location_split_no_generic_only_parentheses, function(parts) {
  sapply(parts, function(part) {
    if (is.na(part)) {
      return(NA)
    } else {
      cleaned_result = identify_and_remove_generics(list(part), generic_words)
      return(cleaned_result$location_split_no_generic[[1]])
    }
  })
})

location_split_no_generic_only_parentheses_no_generic = 
  lapply(processed_parentheses_no_generics, function(parts) parts)

## Function to combine split up location names with the country to facilitate
## matching in Open Street Maps

combine_location_country = function(locations, country) {
  combined = sapply(locations, function(location) {
    paste(location, country, sep = ", ")
  })
  return(combined)
}

## Extract country names and simplify some complicated ones for matching to Open
## Street Maps

country = emdat.data.filtered$Country

country = gsub("\\(Bolivarian Republic of\\)", "", country)
country = gsub("\\(Islamic Republic of\\)", "", country)
country = gsub("\\(Province of China\\)", "", country)
country = gsub("\\(Kingdom of the Netherlands\\)", "", country)
country = gsub("\\(Kingdom of the\\)", "", country)
country = gsub("\\(Plurinational State of\\)", "", country)
country = gsub("\\(Federated States of\\)", "", country)

## Keep track of disaster numbers

disaster_numbers = emdat.data.filtered$DisNo.

## Combine location names with country names

location_split_no_generic_no_parentheses_country = 
  lapply(seq_along(location_split_no_generic_no_parentheses), function(x) 
  combine_location_country(location_split_no_generic_no_parentheses[[x]], 
                           country[x]))

# We have now combined split up locations with the corresponding country. In
# theory we can now geocode these locations. However, many of the locations
# are misspelled or use alternate spellings not recognized by OSM. Therefore,
# we read in a file to override the EMDAT names with names confirmed to
# match in OSM. This osm.replace file was developed iterating back and forth
# with 6 EMDAT Geocoding.R.

# V. Replace certain location names with location names known to work with
# Open Street Maps


location_split_no_generic_no_parentheses_country.fixed = 
  location_split_no_generic_no_parentheses_country


## Check for duplicates in the osm.replace object; duplicates will be skipped
## because otherwise it messes things up.

osm.duplicates = which(unlist(lapply(osm.replace$emdat_location, function(x) 
  sum(osm.replace$emdat_location==x)))>1)

if(length(osm.duplicates)>0){
  print("Warning! Duplicates in osm.replace. Duplicates will be skipped.")}

osm.replace = osm.replace[setdiff(1:nrow(osm.replace),
                                  osm.duplicates),]

## Function to facilitate comparison between strings with different encoding

normalize_string = function(s) {
  s = str_trim(s)               # Trim whitespace
  s = tolower(s)                # Convert to lowercase
  s = gsub("[[:space:]]+", " ", s) # Replace multiple spaces with a single space
  s = gsub("\u00A0", " ", s)    # Replace non-breaking space with a regular space
  s = gsub("[^[:print:]]", "", s)  # Remove non-printable characters
  return(s)
}

## Loop through osm.replace and update the locations used for geocoding
for(i in 1:nrow(osm.replace)){
  
  location_find = osm.replace$emdat_location[i]
  location_replace = osm.replace$new_location_1[i]
  location_replace_2 = osm.replace$new_location_2[i]
  location_replace_3 = osm.replace$new_location_3[i]
  location_replace_4 = osm.replace$new_location_4[i]
  location_replace_5 = osm.replace$new_location_5[i]
  location_replace_6 = osm.replace$new_location_6[i]
  
  for(j in 1:length(location_split_no_generic_no_parentheses_country)){
    
    location_bad_position = 
      which(normalize_string(unlist(location_split_no_generic_no_parentheses_country[j]))==
              normalize_string(osm.replace$emdat_location[i]))
    
    if(length(location_bad_position)>0){
      
      print(paste("location ",j," in ",i,sep=""))
      
      location_split_no_generic_no_parentheses_country.fixed[[j]][
        location_bad_position] =
        location_replace
      
      if(location_replace_2 != ""){
        location_split_no_generic_no_parentheses_country.fixed[[j]][
          length(location_split_no_generic_no_parentheses_country.fixed[[j]]) + 1] =
          location_replace_2
      }
      
      if(location_replace_3 != ""){
        location_split_no_generic_no_parentheses_country.fixed[[j]][
          length(location_split_no_generic_no_parentheses_country.fixed[[j]]) + 1] =
          location_replace_3
      }
      
      if(location_replace_4 != ""){
        location_split_no_generic_no_parentheses_country.fixed[[j]][
          length(location_split_no_generic_no_parentheses_country.fixed[[j]]) + 1] =
          location_replace_4
      }
      
      if(location_replace_5 != ""){
        location_split_no_generic_no_parentheses_country.fixed[[j]][
          length(location_split_no_generic_no_parentheses_country.fixed[[j]]) + 1] =
          location_replace_5
      }
      
      if(location_replace_6 != ""){
        location_split_no_generic_no_parentheses_country.fixed[[j]][
          length(location_split_no_generic_no_parentheses_country.fixed[[j]]) + 1] =
          location_replace_6
      }
      
    }
  }
  
}

location_split_no_generic_no_parentheses_country.fixed.unique = 
  unique(unlist(location_split_no_generic_no_parentheses_country.fixed))

## Save using XLSX to preserve special characters
write.xlsx(emdat.data.filtered[,setdiff(colnames(emdat.data.filtered),
                                        "SLIIDERS.country.code")], 
           "5 output - EMDAT filtered events with deaths.xlsx")

## Mismatches between EMDAT country codes and SLIIDERS country codes (excluding
## landlocked countries)

countries.mismatch = 
  emdat.data.filtered$ISO[
    which(emdat.data.filtered$ISO!=emdat.data.filtered$SLIIDERS.country.code)]


write.csv(cbind.data.frame(EMDAT.countries=unique(countries.mismatch),
                           SLIIDERS.countries.matched=
                             emdat.data.filtered$SLIIDERS.country.code.flat[
                               match(unique(countries.mismatch),
                                     emdat.data.filtered$ISO)]),
          file = "5 output - SLIIDERS countries subbed into EMDAT countries.csv")


# If this code executed properly, you now have an object 
# location_split_no_generic_no_parentheses_country.fixed that will be used
# by "6 EMDAT Geocoding RC" to geocode all the subnational locations in the
# EMDAT data. 


