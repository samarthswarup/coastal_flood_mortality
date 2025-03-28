##########################################
# Step 4 - Calculate yearly populations  #
# at different surge heights and for     #
# countries as a whole                   #
#                                        #
# 2025-05-06                             #
# Ryan Calder                            #
##########################################

# I. Clear environment, set WD, and read in (1) 2019 surge height populations 
# data from Step 2, (2) population scale data from SLIIDERS, (3)
# World Bank data to generate population estimates for years not covered
# by SLIIDERS "pop_scale.data" variable, and (4) our "country data" sheet that
# maps overseas territories to parent countries for the purposes of matching
# them to World Bank data.

rm(list = ls())

# Getting the path of your current open file
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

library(dplyr)
library(stringr)
library(tidyr)
library(readxl)

WB_pop.data = read_excel("World Bank pop data/WB pop data 2024-01-16.xlsx")

seg_adm_pop_exposed.df = 
  read.csv("../Step03/3 output - Population below surge return periods (present + future) by tile no pop growth.csv")

country.data =
  read.csv(paste("Identify mismatched and missing countries - step 4/",
           "SLIIDERS country codes with names and territorial mapping.csv",
           sep=""))

pop_scale.data = 
  read.csv(paste("../Step01/",
                 "updated_pop.csv",sep=""))

taiwan.pop.data = read.csv("Taiwan data/Total population_20240810005643.csv",
                           skip = 9)

# II. Transform World Bank population data to allow matching

names(WB_pop.data)[5:95] = unlist(lapply(names(WB_pop.data)[5:95],function(x) substr(x, 1, 4)))

WB_pop.data[, 5:95] <- lapply(WB_pop.data[, 5:95], as.numeric)

WB_pop.data = WB_pop.data[!is.na(WB_pop.data$`Country Name`),]
year_columns <- names(WB_pop.data)[5:95]


WBpop.transformed <- do.call(rbind, lapply(year_columns, function(year) {
  data.frame(
    `Country Name` = WB_pop.data$`Country Name`,
    `Country Code` = WB_pop.data$`Country Code`,
    `Series Name` = WB_pop.data$`Series Name`,
    `Series Code` = WB_pop.data$`Series Code`,
    year = as.numeric(year),  # Convert year to numeric
    Population = as.numeric(WB_pop.data[[year]])  # Convert population to numeric
  )
}))

## Rename columns and drop irrelevant columns

WBpop.transformed = 
  rename(WBpop.transformed, 
         Country=Country.Name, 
         Country_Code=Country.Code, 
         Year=year)

WBpop.transformed = 
  WBpop.transformed[,
                    which(names(WBpop.transformed)%in%
                            setdiff(names(WBpop.transformed),
                                    c("X","Indicator.Name",
                                      "Indicator.Code","Series.Name","Series.Code")))]

WBpop.transformed$Population = as.numeric(WBpop.transformed$Population)

WBpop.transformed = WBpop.transformed[!is.na(WBpop.transformed$Population),]

# III. Append Taiwan data to WB data

taiwan.data.to.add = cbind.data.frame(
  Country = rep("Taiwan",length(1970:2070)),
  Country_Code = rep("TWN",length(1970:2070)),
  Year = 1970:2070,
  Population = 
    as.numeric(taiwan.pop.data[match("Total",taiwan.pop.data$X.1),
                    match(paste("X",1970:2070,sep=""),
                          colnames(taiwan.pop.data))]))

WBpop.transformed = 
  rbind.data.frame(WBpop.transformed,taiwan.data.to.add)


# IV. Isolate country codes from SLIIDERS tiles

seg_adm_pop_exposed.df = seg_adm_pop_exposed.df %>% 
  mutate(country_code = str_extract(seg_adm, "(?<=_)[A-Z]+(?=\\.)"))


## Fix broken country codes

index.country.na = which(is.na(seg_adm_pop_exposed.df$country_code))
country.code.na = substr(seg_adm_pop_exposed.df$seg_adm[index.country.na],16,18)
seg_adm_pop_exposed.df$country_code[index.country.na] = country.code.na

# V. Get rid of superfluous columns 

seg_adm_pop_exposed.df = 
  seg_adm_pop_exposed.df[,
  which(colnames(seg_adm_pop_exposed.df)%in%setdiff(colnames(seg_adm_pop_exposed.df),
                                                    c("X.1","X")))]
  
# VI. Isolate World Bank data for 2019


WBpop.transformed.2019 =
  WBpop.transformed %>%
  filter(Year == 2019) %>%
  select(Country_Code, pop2019.whole.country = Population)



# VII. Reconcile SLIIDERS country codes with World Bank country codes

unique(WBpop.transformed.2019$Country_Code)
unique(seg_adm_pop_exposed.df$country_code)

## Which countries do not have WB data? -- Here we have data for Guam etc.

countries.to.replace.WB = 
  setdiff(unique(seg_adm_pop_exposed.df$country_code),
          unique(WBpop.transformed.2019$Country_Code))

## Use SLIIDERS country code if not in the above mismatch object. If found
## in mismatch object then group with appropriate country as defined in
## country.data.

seg_adm_pop_exposed.df$country_code_WB = 

  unlist(lapply(seg_adm_pop_exposed.df$country_code, function(x)
    if(x%in%countries.to.replace.WB){
      return(country.data$Group.with..for.World.Bank.[
        match(x,country.data$SLIIDERS.country.code)])}else{return(x)}))


## All fixed? 

setdiff(unique(seg_adm_pop_exposed.df$country_code_WB),
        unique(WBpop.transformed.2019$Country_Code))

# VIII. Join World Bank 2019 population data with surge height specific data 
# and calculate proportion of each surge height population as fraction of
# total country population in 2019; this 2019 proportion is used in the
# next step to calculate surge-height-specific populations as a funciton of
# annual total-country populations


seg_adm_pop_exposed.df = seg_adm_pop_exposed.df %>%
  left_join(WBpop.transformed.2019, by = c("country_code_WB" = "Country_Code"))

seg_adm_pop_exposed.df$pop2019.whole.country = as.numeric(seg_adm_pop_exposed.df$pop2019.whole.country)

seg_adm_pop_exposed.df = seg_adm_pop_exposed.df %>%
  mutate(prop.2019.10yr = base.2019.pop.10yr / pop2019.whole.country)

seg_adm_pop_exposed.df = seg_adm_pop_exposed.df %>%
  mutate(prop.2019.100yr = base.2019.pop.100yr / pop2019.whole.country)

seg_adm_pop_exposed.df = seg_adm_pop_exposed.df %>%
  mutate(prop.2019.1000yr = base.2019.pop.1000yr / pop2019.whole.country)

seg_adm_pop_exposed.df = seg_adm_pop_exposed.df %>%
  mutate(prop.2019.10000yr = base.2019.pop.10000yr / pop2019.whole.country)



# IX. Use annual World Bank population 1985-1999 to calculate surge-height
# specific populations


for(y in 1985:1999) {
  pop_year = WBpop.transformed %>%
    filter(Year == y) %>%
    select(Country_Code, Year_Pop = Population)

  seg_adm_pop_exposed.df = seg_adm_pop_exposed.df %>%
    mutate(
      !!paste0("base.", y, ".pop.10yr") := 
        prop.2019.10yr * pop_year$Year_Pop[
          match(seg_adm_pop_exposed.df$country_code_WB, pop_year$Country_Code)],
      !!paste0("base.", y, ".pop.100yr") := 
        prop.2019.100yr * pop_year$Year_Pop[
          match(seg_adm_pop_exposed.df$country_code_WB, pop_year$Country_Code)],
      !!paste0("base.", y, ".pop.1000yr") := 
        prop.2019.1000yr * pop_year$Year_Pop[
          match(seg_adm_pop_exposed.df$country_code_WB, pop_year$Country_Code)],
      !!paste0("base.", y, ".pop.10000yr") := 
        prop.2019.10000yr * pop_year$Year_Pop[
          match(seg_adm_pop_exposed.df$country_code_WB, pop_year$Country_Code)]
    )
  
}
# X. Go through pop.scale data from SLIIDERS and fix countries where
# pop.scale factor is zero for that country -- group them with parent country

pop_scale.data.0.index = 
  which(pop_scale.data$pop_scale==0)

pop_scale.data.0.countries = 
  unique(pop_scale.data$country[pop_scale.data.0.index])

pop_scale.data$pop_scale[pop_scale.data.0.index] =
  unlist(lapply(pop_scale.data.0.index,function(x) 
    pop_scale.data$pop_scale[pop_scale.data$ssp==pop_scale.data$ssp[x]&
                               pop_scale.data$year==pop_scale.data$year[x]&
                               pop_scale.data$country==
                               country.data$Group.with..for.World.Bank.[
                                 match(pop_scale.data$country[x],
                                       country.data$SLIIDERS.country.code)]]))

# XI. Use SLIIDERS population scale to calculate surge-height-specific
# populations under different SSP scenarios for years 2000 to 2100

 ssp.vec = unique(pop_scale.data$ssp)
 #pop_scale.data.year.vec = min(pop_scale.data$year):max(pop_scale.data$year)
 pop_scale.data.year.vec = c(min(pop_scale.data$year):2025,2050,2100)
 surge.period.vec = c("10yr","100yr","1000yr","10000yr")
 
 ## Make sure all countries in seg_adm_pop_expsed.df are in pop_scale.data
 
 countries.to.replace.popscale = 
   setdiff(unique(seg_adm_pop_exposed.df$country_code),
           unique(pop_scale.data$country))
 
 countries.to.replace.popscale # should be empty
 

 ## Initialize columns for pop scale analysis - SSP only
 
 for (year in pop_scale.data.year.vec) {
   for (ssp in ssp.vec) {
     for (surge in surge.period.vec) {
       col_name = paste0("base.", year, ".", ssp, ".pop.", surge)
       seg_adm_pop_exposed.df[[col_name]] = NA
     }
   }
 }

 
 ## Loop over each row and calculate yearly populations at every
 ## surge height 
 

 for (i in 1:nrow(seg_adm_pop_exposed.df)) {
   country = seg_adm_pop_exposed.df$country_code[i]
   
   for (year in pop_scale.data.year.vec) {
     for (ssp in ssp.vec) {
       for (surge in surge.period.vec) {
         col_name = paste0("base.", year, ".", ssp, ".pop.", surge)
         pop_col = paste0("base.2019.pop.", surge)
         
         pop_scale_value = pop_scale.data$pop_scale[
           which(pop_scale.data$country == country &
                   pop_scale.data$ssp == ssp &
                   pop_scale.data$year == year)]
         
         # if(pop_scale_value ==0){
         #   
         #   print(paste("error",i))
         #   
         # }
         # 
         if (length(pop_scale_value) == 1 && 
             !is.na(seg_adm_pop_exposed.df[[pop_col]][i])) {
           
           seg_adm_pop_exposed.df[[col_name]][i] = 
             pop_scale_value * seg_adm_pop_exposed.df[[pop_col]][i]
         }
       }
     }
   }
   
   if(i%%10==0){
     print(paste(i," out of ",nrow(seg_adm_pop_exposed.df),
                 " (", round(i/nrow(seg_adm_pop_exposed.df)*100,2), "%) completed",sep=""))
     
   }
 }
 
 
 seg_adm_pop_exposed.df = 
   seg_adm_pop_exposed.df[,setdiff(colnames(seg_adm_pop_exposed.df),"X")]
 
 # XI. Go through population forecast scenarios based on SLR and superimpose
 # "base" (no SLR-related increases) demographic changes for composite
 # estimates of future exposed populations by scenario
 
 pop_scale.data
 
 for(i in 1:length(colnames(seg_adm_pop_exposed.df))){
   
   if(grepl("SP1", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP1",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]] *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP1"&
                                    pop_scale.data$year==2050])[
                                      match(seg_adm_pop_exposed.df$country_code_WB,
                                            (pop_scale.data[pop_scale.data$ssp=="SSP1"&
                                                              pop_scale.data$year==2050,])$country)]
       
         print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
      
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP1",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP1"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP1"&
                                                               pop_scale.data$year==2100,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
       
     
   }
   
   if(grepl("SP2", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP2",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP2"&
                                     pop_scale.data$year==2050])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP2"&
                                                               pop_scale.data$year==2050,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP2",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP2"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP2"&
                                                               pop_scale.data$year==2100,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
     
     
   }
   
   if(grepl("SP3", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP3",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP3"&
                                     pop_scale.data$year==2050])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP3"&
                                                               pop_scale.data$year==2050,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP3",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP3"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP3"&
                                                               pop_scale.data$year==2100,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
     
     
   }
   
   
   if(grepl("SP5", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP5",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP5"&
                                     pop_scale.data$year==2050])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP5"&
                                                               pop_scale.data$year==2050,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP5",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP5"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP5"&
                                                               pop_scale.data$year==2100,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
     
     
   }
   
   
   
   if(grepl("k14_RCP26", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP1",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP1"&
                                     pop_scale.data$year==2050])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP1"&
                                                               pop_scale.data$year==2050,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP1",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP1"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP1"&
                                                               pop_scale.data$year==2100,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
     
     
   }
   
   if(grepl("k14_RCP45", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP2",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP2"&
                                     pop_scale.data$year==2050])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP2"&
                                                               pop_scale.data$year==2050,])$country)]
       print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP2",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP2"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP2"&
                                                               pop_scale.data$year==2100,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
     
     
   }
   
   if(grepl("k14_RCP85", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP5",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP5"&
                                     pop_scale.data$year==2050])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP5"&
                                                               pop_scale.data$year==2050,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP5",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]   *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP5"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP5"&
                                                               pop_scale.data$year==2100,])$country)]
       
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
     
     
   }
   
   if(grepl("NCC_AR6", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("base", colnames(seg_adm_pop_exposed.df)[i])&
      !grepl("popscale", colnames(seg_adm_pop_exposed.df)[i])){
     
     if(grepl(".2050.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP1",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP1"&
                                     pop_scale.data$year==2050])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP1"&
                                                               pop_scale.data$year==2050,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
       
     }
     
     if(grepl(".2100.", colnames(seg_adm_pop_exposed.df)[i])){
       
       seg_adm_pop_exposed.df[
         paste(colnames(seg_adm_pop_exposed.df)[i],".popscale.SSP1",sep="")] =
         
         
         seg_adm_pop_exposed.df[colnames(seg_adm_pop_exposed.df)[i]]  *
         
         (pop_scale.data$pop_scale[pop_scale.data$ssp=="SSP1"&
                                     pop_scale.data$year==2100])[
                                       match(seg_adm_pop_exposed.df$country_code_WB,
                                             (pop_scale.data[pop_scale.data$ssp=="SSP1"&
                                                               pop_scale.data$year==2100,])$country)]
       
       print(colnames(seg_adm_pop_exposed.df)[i])
     }
     
     
     
     
   }
   
 }
 
 
 
 # XII. Output files
 
 write.csv(WBpop.transformed,
           "4 output - All pops by year (past and future) by country.csv")
 
 write.csv(seg_adm_pop_exposed.df, 
           "4 output - All pops by year (past and future) by SSP by return period by tile with pop growth.csv")
 

 write.csv(cbind.data.frame(SLIIDERS.country.code = 
                              countries.to.replace.WB,
                            SLIIDERS.country.name = 
                              country.data$Name..ISO.3166.if.available.[
                                match(countries.to.replace.WB,
                                    country.data$SLIIDERS.country.code)],
                            WB.country.code = 
                              country.data$Group.with..for.World.Bank.[
                                match(countries.to.replace.WB,
                                      country.data$SLIIDERS.country.code)]),
           "4 output - Countries aggregated for WB pops 1985-1999.csv")
 
 write.csv(cbind.data.frame(SLIIDERS.country.code = 
                              pop_scale.data.0.countries,
                            SLIIDERS.country.name = 
                              country.data$Name..ISO.3166.if.available.[
                                match(pop_scale.data.0.countries,
                                      country.data$SLIIDERS.country.code)],
                            WB.country.code = 
                              country.data$Group.with..for.World.Bank.[
                                match(pop_scale.data.0.countries,
                                      country.data$SLIIDERS.country.code)]),
           "4 output - Countries aggregated for pop scale 2000-2100.csv")
 
 
 