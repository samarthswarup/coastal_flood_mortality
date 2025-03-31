##########################################
# Step 7b - GNI, HDI, nat pop.R          #
#                                        #
# 2025-03-04                             #
# Ryan Calder                            #
##########################################

# This script adds to our EMDAT database information on GNI, HDI, and national
# population for use in regression.

# I. Clear environment, set WD, and read in (1) geocoded EMDAT data, (2) our 
# "country data" sheet that maps overseas territories to parent countries for 
# the purposes of matching them to World Bank data, (3) population data
# for each country from World Bank, and (4) HDI and GNI data from World Bank.

rm(list=ls())
library(openxlsx)
library(dplyr)
library(stringr)
library(tidyr)


# Getting the path of your current open file
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

gni.cutoff.data = 
  read.csv(paste("World Bank GNI HDR data/GNI_group metadata 2025-01-16.csv",
                   sep=""))

hdr.data = read.csv(paste("World Bank GNI HDR data/",
                          "HDR data 2025-01-16.csv",
                          sep=""))

gni.atlas.data.raw = read.csv("World Bank GNI HDR data/GNI Atlas 2025-01-16.csv")


country.data =
  read.csv(paste("../Step04/Identify mismatched and missing countries - step 4/",
                 "SLIIDERS country codes with names and territorial mapping.csv",
                  sep=""))


emdat.data.filtered = 
  read.xlsx(paste("../Step07/7 output - EMDAT events with exposed pops.xlsx"))

## This is the national population data used in Step 3 consisting of data
## from World Bank supplemented with other data sources (e.g., for Taiwan)

WB.supp.pop.data = 
  read.csv("../Step04/4 output - All pops by year (past and future) by country.csv")

# II. Map EMDAT countries to countries for which HDI/GNI data are available

##  Make column of country codes to match with EMDAT data
emdat.data.filtered$hdr.country.code = NA

emdat.country.code.in.hdr = unique(emdat.data.filtered$SLIIDERS.country.code.flat)[
  which(unique(emdat.data.filtered$SLIIDERS.country.code.flat)%in%
          hdr.data$countryIsoCode==TRUE)]

##  Loop through EMDAT assigning HDR countries

for(i in 1:nrow(emdat.data.filtered)){
  
  if(emdat.data.filtered$SLIIDERS.country.code.flat[i]%in%
     hdr.data$countryIsoCode){
    
    emdat.data.filtered$hdr.country.code[i] = 
      emdat.data.filtered$SLIIDERS.country.code.flat[i]
    
  }else{
    if(emdat.data.filtered$SLIIDERS.country.code.flat[i]%in%
       country.data$SLIIDERS.country.code){
      
      emdat.data.filtered$hdr.country.code[i] =
       
        country.data$Group.with..for.World.Bank.[
          match(emdat.data.filtered$SLIIDERS.country.code.flat[i],
              country.data$SLIIDERS.country.code)]
      
      if(emdat.data.filtered$hdr.country.code[i]==""){
        print(paste("Error matching at i =",i))
        break
      }
      
    }else{
      if(emdat.data.filtered$SLIIDERS.country.code.flat[i]==
         "BES,CUW,ABW,SXM"){
        emdat.data.filtered$hdr.country.code[i] = "NLD"
      }else{
        print(paste("Error matching at ",i,sep=""))
        break
        
      }
    }
  }
}

## For GNI and HDR, give Macao and Taiwan the values for Hong Kong instead of
## mainland China. Note that Macao has World Bank pop data but not GNI/HDR data
## while Taiwan doesn't have any World Bank data; pop data for Taiwan was
## downloaded directly from the Taiwan National Development Council, but 
## reliable statistics for HDI are not available (other than that it is
## highly developed). 

emdat.data.filtered$hdr.country.code[which(emdat.data.filtered$ISO=="MAC")] =
  "HKG"

emdat.data.filtered$hdr.country.code[which(emdat.data.filtered$ISO=="TWN")] =
  "HKG"

## Do HDR data country codes match with Atlas GNI?

which(is.na(match(unique(emdat.data.filtered$hdr.country.code),
      unique(gni.atlas.data.raw$Country.Code))))

## Summarize all the mismatches for a summary file outputted at the end

hdr.country.exceptions = 
  cbind.data.frame(emdat.country.code = unique(emdat.data.filtered$ISO),
                   hdr.country.code = 
                     emdat.data.filtered$hdr.country.code[
                       match(unique(emdat.data.filtered$ISO),
                             emdat.data.filtered$ISO)])

hdr.country.exceptions = 
  hdr.country.exceptions[which(hdr.country.exceptions$emdat.country.code!=
                                 hdr.country.exceptions$hdr.country.code),]


hdr.country.exceptions$emdat.country = 
  emdat.data.filtered$Country[match(hdr.country.exceptions$emdat.country.code,
        emdat.data.filtered$ISO)]

hdr.country.exceptions$hdr.country = 
  emdat.data.filtered$Country[match(hdr.country.exceptions$hdr.country.code,
                                    emdat.data.filtered$ISO)]


## All matched?

which(is.na(hdr.data$countryIsoCode))
which(hdr.data$countryIsoCode=="")

# II and half -- fix GNI Atlas data

gni.atlas.data = pivot_longer(gni.atlas.data.raw,cols=5:ncol(gni.atlas.data.raw),
                             names_to = "year", values_to = "gnipc.atlas")

gni.atlas.data = gni.atlas.data %>%
  mutate(year = as.numeric(substr(year, 2,5)))

gni.atlas.data$gnipc.atlas[gni.atlas.data$gnipc.atlas==".."] = NA
gni.atlas.data$gnipc.atlas = as.numeric(gni.atlas.data$gnipc.atlas)


gni.atlas.data <- gni.atlas.data %>%
  group_by(Country.Name, Country.Code) %>%
  arrange(year) %>%
  mutate(gnipc.atlas.imputed = if (sum(!is.na(gnipc.atlas)) >= 2) {
    approx(year, gnipc.atlas, year, rule = 2)$y
  } else {
    gnipc.atlas # Keep original values if interpolation is not possible
  }) %>%
  ungroup()



# III. Add HDI, GNI per capita, and GNI group (event year and at baseline) to 
# EMDAT dataset

emdat.data.filtered$hdi = NA
emdat.data.filtered$gnipc = NA
emdat.data.filtered$gnipc.atlas = NA
emdat.data.filtered$gnipc.atlas.imputed = NA
emdat.data.filtered$gni.group = NA
emdat.data.filtered$gni.group.baseline = NA

## Function to map GNI per capita to GNI group as a function of a country's
## GNI and year of event

get_income_group = function(year, gni_per_capita) {
  
  ### Assume 2023 data for 2024
  
  if(year==2024){
    year = 2023
  }
  
  ### Filter the data for the given year
  year_data = gni.cutoff.data %>%
    filter(Data.for.calendar.year..== year)
  
  ### Check if the year exists in the data
  if (nrow(year_data) == 0) {
    stop("Year not found in the data.")
  }
  
  ### Extract the cutoffs
  low_income_max = as.numeric(gsub(",","",gsub("[<= ]", "", year_data$Low.income..L.)))
  lower_middle_income = as.numeric(str_split(gsub(",", "", year_data$Lower.middle.income..LM.), "-")[[1]]) 
  upper_middle_income = as.numeric(str_split(gsub(",", "", year_data$Upper.middle.income..UM.), "-")[[1]]) 
  high_income = as.numeric(gsub("[>, ]", "", year_data$High.income..H.))
  
  ### Determine the income group
  if (gni_per_capita <= low_income_max) {
    return("LI")
  } else if (gni_per_capita >= lower_middle_income[1] & gni_per_capita <= lower_middle_income[2]) {
    return("LMI")
  } else if (gni_per_capita >= upper_middle_income[1] & gni_per_capita <= upper_middle_income[2]) {
    return("UMI")
  } else if (gni_per_capita > high_income) {
    return("HI")
  } else {
    return("NC")
  }
}

## Loop through EMDAT assigning HDI/GNI data

na.countries = c()

backfill.report = cbind.data.frame(real.year=NA,fill.year=NA,country=NA,data.type=NA)

for(i in 1:nrow(emdat.data.filtered)){
  
  if(emdat.data.filtered$hdr.country.code[i]=="PRK"){
    next
  }
  
  if(emdat.data.filtered$Start.Year[i]<1990){
    next
  }
  
  
  
  if(emdat.data.filtered$Start.Year[i]>=2022){ # if event since 2022
    
    
    hdr.row.lookup = which(hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
                             hdr.data$year==2022&
                             hdr.data$indicatorCode=="hdi")
    
    gnipc.row.lookup = which(hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
                             hdr.data$year==2022&
                             hdr.data$indicatorCode=="gnipc")
    
    emdat.data.filtered$hdi[i] = hdr.data$value[hdr.row.lookup]
    emdat.data.filtered$gnipc[i] = hdr.data$value[gnipc.row.lookup]
    
    if(emdat.data.filtered$Start.Year[i]<=2023){ # Atlas data available for 2023
      
      gnipc.row.lookup = which(gni.atlas.data$Country.Code==emdat.data.filtered$hdr.country.code[i]&
                                 gni.atlas.data$year==emdat.data.filtered$Start.Year[i])
      
      
      emdat.data.filtered$gnipc.atlas[i] =gni.atlas.data$gnipc.atlas[gnipc.row.lookup]
      emdat.data.filtered$gnipc.atlas.imputed[i] =gni.atlas.data$gnipc.atlas.imputed[gnipc.row.lookup]
      
      
    }else{
      
      gnipc.atlas.row.lookup = which(gni.atlas.data$Country.Code==emdat.data.filtered$hdr.country.code[i]&
                                 gni.atlas.data$year==2023)
      
      
      emdat.data.filtered$gnipc.atlas[i] =gni.atlas.data$gnipc.atlas[gnipc.atlas.row.lookup]
      emdat.data.filtered$gnipc.atlas.imputed[i] =gni.atlas.data$gnipc.atlas.imputed[gnipc.atlas.row.lookup]
      
    }
    
    
  }else{ # if event before 2022
    
    earliest.year.of.hdi.data = min(hdr.data$year[
      hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
        hdr.data$indicatorCode=="hdi"])
    
    earliest.year.of.gnipc.data = min(hdr.data$year[
      hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
        hdr.data$indicatorCode=="gnipc"])
    
    
    if(emdat.data.filtered$Start.Year[i]<earliest.year.of.hdi.data){
      
      year.sub = earliest.year.of.hdi.data
      
      backfill.report = rbind.data.frame(backfill.report,
                                         c(emdat.data.filtered$Start.Year[i],earliest.year.of.hdi.data,emdat.data.filtered$Country[i],"HDI"))
      
      
      hdr.row.lookup = which(hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
                               hdr.data$year==year.sub&
                               hdr.data$indicatorCode=="hdi")
      
    }else{
      
      hdr.row.lookup = which(hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
                               hdr.data$year==emdat.data.filtered$Start.Year[i]&
                               hdr.data$indexCode=="HDI"&
                               hdr.data$indicatorCode=="hdi")
      
    }
    
    if(emdat.data.filtered$Start.Year[i]<earliest.year.of.gnipc.data){
      
      year.sub = earliest.year.of.gnipc.data
      
      backfill.report = rbind.data.frame(backfill.report,
                                         c(emdat.data.filtered$Start.Year[i],earliest.year.of.hdi.data,emdat.data.filtered$Country[i],"GNI"))
      
      
      gnipc.row.lookup = which(hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
                                 hdr.data$year==year.sub&
                                 hdr.data$indicatorCode=="gnipc")
    }else{
      
      gnipc.row.lookup = which(hdr.data$countryIsoCode==emdat.data.filtered$hdr.country.code[i]&
                                 hdr.data$year==emdat.data.filtered$Start.Year[i]&
                                 hdr.data$indicatorCode=="gnipc")
      
    }
    
    
    
    gnipc.atlas.row.lookup = which(gni.atlas.data$Country.Code==emdat.data.filtered$hdr.country.code[i]&
                                     gni.atlas.data$year==emdat.data.filtered$Start.Year[i])
    
    emdat.data.filtered$gnipc.atlas[i] =gni.atlas.data$gnipc.atlas[gnipc.atlas.row.lookup]
    emdat.data.filtered$gnipc.atlas.imputed[i] =gni.atlas.data$gnipc.atlas.imputed[gnipc.atlas.row.lookup]
    
    emdat.data.filtered$hdi[i] = hdr.data$value[hdr.row.lookup]
    emdat.data.filtered$gnipc[i] = hdr.data$value[gnipc.row.lookup]
    
    

      
    
  }
  

  
  emdat.data.filtered$gni.group[i] = 
    get_income_group(emdat.data.filtered$Start.Year[i],
                     emdat.data.filtered$gnipc.atlas.imputed[i])
  
  # Get baseline GNI group
  
  baseline.year = 1990
  
  emdat.data.filtered$gni.group.baseline[i] =
        get_income_group(baseline.year,
                         gni.atlas.data$gnipc.atlas.imputed[
                           which(gni.atlas.data$Country.Code==emdat.data.filtered$hdr.country.code[i]&
                                   gni.atlas.data$year==baseline.year)])
}
  

# Make sure the only events with missing data are either in North Korea or
# before 1990

  ## HDI

range(emdat.data.filtered$Start.Year[which(is.na(emdat.data.filtered$hdi))[
  which(emdat.data.filtered$hdr.country.code[which(is.na(emdat.data.filtered$hdi))]!=
          "PRK")]])

  ## GNI

range(emdat.data.filtered$Start.Year[which(is.na(emdat.data.filtered$gnipc))[
  which(emdat.data.filtered$hdr.country.code[which(is.na(emdat.data.filtered$gnipc))]!=
          "PRK")]])

  ## GNI group

range(emdat.data.filtered$Start.Year[which(is.na(emdat.data.filtered$gni.group))[
  which(emdat.data.filtered$hdr.country.code[which(is.na(emdat.data.filtered$gni.group))]!=
          "PRK")]])


## GNI baseline

range(emdat.data.filtered$Start.Year[which(is.na(emdat.data.filtered$gni.group.baseline))[
  which(emdat.data.filtered$hdr.country.code[which(is.na(emdat.data.filtered$gni.group.baseline))]!=
          "PRK")]])

# IV. Add national population in the year of the event using population data
# from Step 3

emdat.data.filtered$pop.total.adm0 = NA
emdat.data.filtered$pop.total.adm0.2050 = NA

for(i in 1:nrow(emdat.data.filtered)){
  
  year = emdat.data.filtered$Start.Year[i]
  
    emdat.data.filtered$pop.total.adm0[i] =
      WB.supp.pop.data$Population[
        WB.supp.pop.data$Year==emdat.data.filtered$Start.Year[i]&
          WB.supp.pop.data$Country_Code==emdat.data.filtered$hdr.country.code[i]]
    
    emdat.data.filtered$pop.total.adm0.2050[i] =
      WB.supp.pop.data$Population[
        WB.supp.pop.data$Year==2050&
          WB.supp.pop.data$Country_Code==emdat.data.filtered$hdr.country.code[i]]
    

}
  
## Note the mismatched countries 

hdr.country.exceptions = cbind.data.frame(emdat.countries = unique(emdat.data.filtered$ISO),
                                          wb.undp.countries = NA)


for(i in 1:nrow(hdr.country.exceptions)){
  hdr.country.exceptions$wb.undp.countries[i] = 
    emdat.data.filtered$hdr.country.code[
      match(hdr.country.exceptions$emdat.countries[i],
            emdat.data.filtered$ISO)]
  
}

hdr.country.exceptions = 
  hdr.country.exceptions[which(hdr.country.exceptions$emdat.countries!=
                                 hdr.country.exceptions$wb.undp.countries),]

backfill.print = cbind.data.frame(country=
                                   rep(unique(backfill.report$country)[!is.na(unique(
                                     backfill.report$country))],2))

backfill.print$type = c(rep("GNI",nrow(backfill.print)/2),
                        rep("HDI",nrow(backfill.print)/2))

backfill.print$events = unlist(lapply(1:nrow(backfill.print),function(x)
  sum(backfill.report$country==backfill.print$country[x]&
    backfill.report$data.type==backfill.print$type[x],na.rm=T)))

backfill.print$earliest.year = unlist(lapply(1:nrow(backfill.print),function(x)
  backfill.report$fill.year[which(backfill.report$country==backfill.print$country[x]&
                                    backfill.report$data.type==backfill.print$type[x])][1]))

backfill.print = backfill.print[backfill.print$events>0,]

backfill.print$country.code = 
  emdat.data.filtered$ISO[match(backfill.print$country,emdat.data.filtered$Country)]

# VI. Save results to CSV/XLSX.


write.xlsx(emdat.data.filtered, 
           paste("8 output - EMDAT events with exposed pops,",
           " HDI GNI and prev events.xlsx",sep=""))

write.csv(hdr.country.exceptions,
          "8 output - Countries aggregated for GNI, HDI, and national pop.csv")

write.csv(backfill.print,
          "8 output - Backfilled HDI and GNI data.csv")


