##########################################
# Step 7 - Calculate exposed pops RC    #
#                                        #
# 2025-02-13                             #
# Ryan Calder                            #
##########################################

# This script calculates "exposed" populations for every event in our subset of
# EMDAT data using various definitions of exposure. It loops through
# EMDAT events and tabulates national and subnational estimates of coastal
# populations based on the SLIIDERS population data (adjusted to the year of
# the event). 

# I. Clear environment, set WD, and read in (1) geocoded EMDAT data, (2) our 
# "country data" sheet that maps overseas territories to parent countries for 
# the purposes of matching them to World Bank data, (3) population data
# for each tile, (4) various World Bank data, and (5) shapefile data for ADM0&1

rm(list=ls())
library(openxlsx)

# Getting the path of your current open file
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


library(sf)
library(openxlsx)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

tile.shapefile.data =
  st_read(paste("SLIIDERS_SHP/sliiders_v1_1_seg_voronoi.shp",sep=""))


seg_adm_pop_exposed.df = 
  read.csv(paste("../Step04/",
                 "4 output - All pops by year (past and future) by SSP by return period by tile with pop growth.csv",sep=""))

emdat.data.filtered = 
  read.xlsx(paste("../Step05/5 output - EMDAT filtered events with deaths",
  ".xlsx",sep=""))

geocode.data =
  read.xlsx(paste("../Step06/6 output - Geocode database.xlsx",sep=""))

download.file(url='https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-levels.zip',
              destfile='gadm_410-levels.zip', method='curl')
unzip('gadm_410-levels.zip',exdir='./')
gadm.path = paste("gadm_410-levels.gpkg",sep="")

gadm.data = st_read(gadm.path)
gadm.layers = st_layers(gadm.path)
gadm.data.adm1 = st_read(gadm.path, layer = "ADM_1")
gadm.data.adm1_simplified = 
  st_simplify(st_make_valid(gadm.data.adm1), 
              preserveTopology = TRUE, dTolerance = 0.01)

country.data = 
  read.csv(paste("../Step04/Identify mismatched and missing countries - step 4/",
                 "SLIIDERS country codes with names and territorial mapping.csv",
                 sep=""))

# I. Calculate ADM0 (country-level) exposed populations by summing up populations
# below surge heights of different return periods

surge.period.vec = c("10yr","100yr","1000yr","10000yr")

# Initialize columns for population exposed analysis in year of event + in 
# 2050 and in 2100 for all SSPs - ADM0

for (surge in surge.period.vec) {
  col_name = paste0("pop.exposed.adm0.", surge)
  emdat.data.filtered[[col_name]] = NA
  
  for(j in 1:5){
    col_name = paste0("base.2050.SSP", j,".","pop.exposed.adm0.",surge)
    
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP", j,".","pop.exposed.adm0.",surge)
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
}

# Initialize columns for population exposed analysis in year of event + in 
# 2050 and in 2100 for future scenarios handled by SLIIDERS - ADM0

# slr.filenames = 
#   dir(paste("/Users/rsdc/Library/CloudStorage/",
#             "OneDrive-VirginiaTech/NASA HAQ June 2021/",
#             "Paper 1 – EM-DAT SLIIDERS/replication output/",
#             "Julia output step 1b - sea level rise by tile",sep=""))

slr.filenames = 
  dir(paste("../Step02/",sep=""))


slr.colnames.root = slr.filenames

slr.colnames.root = 
  gsub(".csv","",slr.colnames.root)

for(i in 1:5){
  slr.colnames.root = 
    gsub(paste("ssp",i,sep=""),paste("SSP",i,".",sep=""),slr.colnames.root)
  
}

slr.colnames.root = 
  gsub("rcp","RCP",slr.colnames.root)

for(i in c("2050","2100")){
  slr.colnames.root = 
    gsub(paste("_",i,"_",sep=""),paste(".",i,".",sep=""),slr.colnames.root)
  
  gsub(paste("_",i,sep=""),paste(".",i,sep=""),slr.colnames.root)
}

surge.period.vec = c("10yr","100yr","1000yr","10000yr")


# slr.colnames = paste(slr.colnames.root,".pop.",surge.period.vec,sep="")
slr.colnames = expand.grid(paste(slr.colnames.root,".pop.",sep=""), surge.period.vec,sep="")
slr.colnames = apply(slr.colnames, 1, function(x) paste0(x, collapse=""))

slr.popscale.colnames = colnames(seg_adm_pop_exposed.df)[grep(".popscale.",colnames(seg_adm_pop_exposed.df))]

for (i in 1:length(slr.colnames.root)) {
  for(surge in surge.period.vec){
    col_name = paste0(slr.colnames.root[i],".pop.exposed.adm0.",surge)
    
    emdat.data.filtered[[col_name]] = NA

    for(popscale.var in slr.popscale.colnames[grep(slr.colnames.root[i],slr.popscale.colnames)]){
      
      col_name = gsub("\\.pop\\.",".pop.exposed.adm0.",popscale.var)
      
      emdat.data.filtered[[col_name]] = NA
      
      
    }
    
  }
  
}

## Identify which tiles are Canary Islands (SPI)

spi_bbox <- st_as_sf(data.frame(
  id = 1,
  geometry = st_sfc(st_polygon(list(matrix(c(
    -18.2, 27.5,  # Bottom-left (SW)
    -18.2, 29.6,  # Top-left (NW)
    -13.3, 29.6,  # Top-right (NE)
    -13.3, 27.5,  # Bottom-right (SE)
    -18.2, 27.5   # Close the polygon
  ), ncol = 2, byrow = TRUE))), crs = 4326)
))

# Find which segments intersect with the Canary Islands
spi_segments <- tile.shapefile.data %>%
  st_filter(spi_bbox)

str(spi_segments)


# Loop through EMDAT data and add up the populations below different
# surge heights for all SLIIDERS tiles in whatever country is listed in 
# EMDAT. Note that SLIIDERS does not use a separate country code for 
# Canary Islands (SPI in EMDAT) and considers this to be part of Spain (ESP).
# EMDAT also lists many countries as "Netherlands Antilles" (ANT), which are
# decomposed into different countries in SLIIDERS (SNT,CUW,ABW,BES,SXM). This
# calculates exposed populations for the year of the EMDAT event plus 2050 and
# 2100 for all SSPs.

for(i in 1:nrow(emdat.data.filtered)){
  
  emdat.country.code = unlist(emdat.data.filtered$SLIIDERS.country.code.flat[i])
  emdat.year = emdat.data.filtered$Start.Year[i]
  
  
  for (surge in surge.period.vec) {
    
    if(emdat.year<2000){
      col_name_lookup = paste0("base.",emdat.year,".pop.", surge)}else{
        col_name_lookup = paste0("base.",emdat.year,".SSP1.pop.", surge)
      }
    
    pop.sum = sum(seg_adm_pop_exposed.df[which(
      seg_adm_pop_exposed.df$country_code==emdat.country.code),
      which(colnames(seg_adm_pop_exposed.df)==
                             col_name_lookup)],na.rm=T)
    
    if(emdat.data.filtered$ISO[i]=="SPI"){
      
      pop.sum = sum(seg_adm_pop_exposed.df[which(
        substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%spi_segments$seg),
        which(colnames(seg_adm_pop_exposed.df)==
                col_name_lookup)],na.rm=T)
      
      
    }
    
    if(pop.sum==0){
      
      pop.sum = sum(seg_adm_pop_exposed.df[
        which(unlist(lapply(lapply(seg_adm_pop_exposed.df$country_code, function(x) 
        grep(x,emdat.country.code)),function(y) length(y))>0)),
        which(colnames(seg_adm_pop_exposed.df)==col_name_lookup)],na.rm=T)}
    
    col_name_fill = paste0("pop.exposed.adm0.", surge)
    
    emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
      pop.sum
    
    
    for(j in 1:5){
    
      # 2050 future pop for all SSPs
      
      col_name_lookup = paste0("base.2050.SSP",j,".pop.", surge)
      
      pop.sum = sum(seg_adm_pop_exposed.df[which(
        seg_adm_pop_exposed.df$country_code==emdat.country.code),
        which(colnames(seg_adm_pop_exposed.df)==
                col_name_lookup)],na.rm=T)
      
      if(pop.sum==0){
        
        pop.sum = sum(seg_adm_pop_exposed.df[
          which(unlist(lapply(lapply(seg_adm_pop_exposed.df$country_code, function(x) 
            grep(x,emdat.country.code)),function(y) length(y))>0)),
          which(colnames(seg_adm_pop_exposed.df)==col_name_lookup)],na.rm=T)}
      
      if(emdat.data.filtered$ISO[i]=="SPI"){
        
        pop.sum = sum(seg_adm_pop_exposed.df[which(
          substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%spi_segments$seg),
          which(colnames(seg_adm_pop_exposed.df)==
                  col_name_lookup)],na.rm=T)
        
        
      }
      
      
      
      col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      # 2100 future pop for all SSPs
      
      col_name_lookup = paste0("base.2100.SSP",j,".pop.", surge)
      
      pop.sum = sum(seg_adm_pop_exposed.df[which(
        seg_adm_pop_exposed.df$country_code==emdat.country.code),
        which(colnames(seg_adm_pop_exposed.df)==
                col_name_lookup)],na.rm=T)
      
      if(pop.sum==0){
        
        pop.sum = sum(seg_adm_pop_exposed.df[
          which(unlist(lapply(lapply(seg_adm_pop_exposed.df$country_code, function(x) 
            grep(x,emdat.country.code)),function(y) length(y))>0)),
          which(colnames(seg_adm_pop_exposed.df)==col_name_lookup)],na.rm=T)}
      
      if(emdat.data.filtered$ISO[i]=="SPI"){
        
        pop.sum = sum(seg_adm_pop_exposed.df[which(
          substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%spi_segments$seg),
          which(colnames(seg_adm_pop_exposed.df)==
                  col_name_lookup)],na.rm=T)
        
        
      }
      
      
      col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
    }
    
    for(m in 1:length(slr.colnames.root)){
      
      col_name_lookup = paste0(slr.colnames.root[m],".pop.",surge)
      
      col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm0.",surge)
      
      pop.sum = sum(seg_adm_pop_exposed.df[which(
        seg_adm_pop_exposed.df$country_code==emdat.country.code),
        which(colnames(seg_adm_pop_exposed.df)==
                col_name_lookup)],na.rm=T)
      
      if(pop.sum==0){
        
        pop.sum = sum(seg_adm_pop_exposed.df[
          which(unlist(lapply(lapply(seg_adm_pop_exposed.df$country_code, function(x) 
            grep(x,emdat.country.code)),function(y) length(y))>0)),
          which(colnames(seg_adm_pop_exposed.df)==col_name_lookup)],na.rm=T)}
      
      if(emdat.data.filtered$ISO[i]=="SPI"){
        
        pop.sum = sum(seg_adm_pop_exposed.df[which(
          substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%spi_segments$seg),
          which(colnames(seg_adm_pop_exposed.df)==
                  col_name_lookup)],na.rm=T)
        
        
      }
      
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
      
    }
    
    slr.popscale.colnames.loop =
      slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
    
    for(n in 1:length(slr.popscale.colnames.loop)){
      
      col_name_lookup = slr.popscale.colnames.loop[n]
      
      col_name_fill = gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n])
      
      
      pop.sum = sum(seg_adm_pop_exposed.df[which(
        seg_adm_pop_exposed.df$country_code==emdat.country.code),
        which(colnames(seg_adm_pop_exposed.df)==
                col_name_lookup)],na.rm=T)
      
      if(pop.sum==0){
        
        pop.sum = sum(seg_adm_pop_exposed.df[
          which(unlist(lapply(lapply(seg_adm_pop_exposed.df$country_code, function(x) 
            grep(x,emdat.country.code)),function(y) length(y))>0)),
          which(colnames(seg_adm_pop_exposed.df)==col_name_lookup)],na.rm=T)}
      
      if(emdat.data.filtered$ISO[i]=="SPI"){
        
        pop.sum = sum(seg_adm_pop_exposed.df[which(
          substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%spi_segments$seg),
          which(colnames(seg_adm_pop_exposed.df)==
                  col_name_lookup)],na.rm=T)
        
        
      }
      
      
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
    }
    
    
    
    
    
  }
  print(paste("I. ", i ," out of ", nrow(emdat.data.filtered), " done",sep=""))
  
}

save(emdat.data.filtered,file="emdat.data.filtered after Step I")

# II. Pool events that affect more than one country in separate "pooled"
# columns; all populations exposed and fatalities are grouped in first
# row where event occurs and subsequent rows for same event have NA. 


for (surge in surge.period.vec) {
  col_name = paste0("pop.exposed.adm0.", surge,".pooled")
  emdat.data.filtered[[col_name]] = NA
  
  for(j in 1:5){
    
    col_name = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge,".pooled")
      emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
  }
  
  for(m in 1:length(slr.colnames.root)){
    col_name = paste0(slr.colnames.root[m],".pop.exposed.adm0.",surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
  }
  
  slr.popscale.colnames.loop =
    slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
  
  for(n in 1:length(slr.popscale.colnames.loop)){
    
    col_name = paste(gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n]),".pooled",sep="")
    
    emdat.data.filtered[[col_name]] = NA
    
    
    
  }
  
}


emdat.data.filtered$total.deaths.na.as.0.pooled = NA
emdat.data.filtered$total.deaths.reported.pooled = NA

multi.country.events = list(NA)
k = 0

for(i in 1:nrow(emdat.data.filtered)){
  
  disaster.number = substr(emdat.data.filtered$DisNo.[i],1,9)
  disaster.rows = grep(disaster.number,emdat.data.filtered$DisNo.)
  
  if(length(disaster.rows)>1){
    if(length(grep(disaster.number,multi.country.events))==0){
     k = k + 1 
      multi.country.events[[k]] = emdat.data.filtered$DisNo.[disaster.rows]
      

      emdat.data.filtered$total.deaths.na.as.0.pooled[i] = 
        sum(emdat.data.filtered$total.deaths.na.as.0[disaster.rows])
      
      emdat.data.filtered$total.deaths.reported.pooled[i] =
        sum(emdat.data.filtered$total.deaths.reported[disaster.rows])
      
      
      
      for (surge in surge.period.vec) {
        
        col_name_lookup = paste0("pop.exposed.adm0.", surge)
        
        pop.sum = sum(emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
        
        col_name_fill = paste0("pop.exposed.adm0.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        for(j in 1:5){
          
          # 2050 future pop for all SSPs
          
          col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge)
          
          pop.sum = sum(emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
          
          col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge,".pooled")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          # 2100 future pop for all SSPs
          
          col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge)
          
          pop.sum = sum(emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
          
          col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge,".pooled")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
        }
        
        for(m in 1:length(slr.colnames.root)){
          
          col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm0.", surge)
          
          pop.sum = sum(emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
          
          col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm0.", surge,".pooled")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
        }
        
        slr.popscale.colnames.loop =
          slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
        
        for(n in 1:length(slr.popscale.colnames.loop)){
          
          col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n]),sep="")
          
          pop.sum = sum(emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
          
          
          col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n]),".pooled",sep="")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          
          
        }

      }
      
    }
  }else{
    
    emdat.data.filtered$total.deaths.na.as.0.pooled[i] = 
      emdat.data.filtered$total.deaths.na.as.0[i]
    
    emdat.data.filtered$total.deaths.reported.pooled[i] =
      emdat.data.filtered$total.deaths.reported[i]
    
    
    for (surge in surge.period.vec) {
      
      col_name_lookup = paste0("pop.exposed.adm0.", surge)

      col_name_fill = paste0("pop.exposed.adm0.", surge,".pooled")
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
      
      for(j in 1:5){
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge)
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge)
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
      }
      for(m in 1:length(slr.colnames.root)){
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm0.", surge)
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm0.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
      }
      
      slr.popscale.colnames.loop =
        slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
      
      for(n in 1:length(slr.popscale.colnames.loop)){
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n]),sep="")

        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n]),".pooled",sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
        
      }
    }
    
  }
  
}

# III. Loop through EMDAT data and make list of SLIIDERS tiles covered by the
# bounding boxes of all locations. This will allow us to sum exposed populations
# in SLIIDERS tiles that intersect with the bounding boxes (IV and V) and
# those that intersect with the ADM Level I areas (VI and VII).

emdat.data.filtered$tiles_covered.local = vector("list", nrow(emdat.data.filtered))

# Allow for making maps of the SLIIDERS tiles relevant to each disaster. Set
# plot_tiles = FALSE for faster processing (but you won't get the maps).

plot_tiles = FALSE
world = ne_countries(scale = "medium", returnclass = "sf")

expand_bbox_square = function(bbox, expand_factor) {
  x_range = bbox["xmax"] - bbox["xmin"]
  y_range = bbox["ymax"] - bbox["ymin"]
  max_range = max(x_range, y_range)
  
  bbox["xmin"] = bbox["xmin"] - (max_range - x_range) / 2 - max_range * expand_factor
  bbox["xmax"] = bbox["xmax"] + (max_range - x_range) / 2 + max_range * expand_factor
  bbox["ymin"] = bbox["ymin"] - (max_range - y_range) / 2 - max_range * expand_factor
  bbox["ymax"] = bbox["ymax"] + (max_range - y_range) / 2 + max_range * expand_factor
  
  return(bbox)
}


make_valid_with_snap = function(geom, snap_tolerance = 0.0001) {
  geom = st_make_valid(geom)
  if (!st_is_valid(geom)) {
    geom = st_snap(geom, st_make_valid(geom), tolerance = snap_tolerance)
  }
  return(geom)
}

# Adding a small buffer to avoid degenerate geometries
buffer_small = function(geom, distance = 0.0001) {
  st_buffer(geom, distance)
}

for(i in 1:nrow(emdat.data.filtered)) {
  disaster.number = emdat.data.filtered$DisNo.[i]
  geocode.rows = grep(disaster.number, geocode.data$disaster_number)
  sliiders.countries = 
    unlist(strsplit(emdat.data.filtered$SLIIDERS.country.code.flat[i],","))
  
  if(length(geocode.rows) > 0) {
    combined_bbox = NULL
    for(row in geocode.rows) {
      bbox = as.numeric(unlist(strsplit(geocode.data$boundingbox.flat[row], ",")))
      
      # Expand bounding box of points if necessary
      if (bbox[1] == bbox[2]) {
        bbox[1] = bbox[1] - 0.0001
        bbox[2] = bbox[2] + 0.0001
      }
      if (bbox[3] == bbox[4]) {
        bbox[3] = bbox[3] - 0.0001
        bbox[4] = bbox[4] + 0.0001
      }
      
      bbox_polygon = 
        st_as_sfc(st_bbox(c(xmin = bbox[3], xmax = bbox[4], 
                            ymin = bbox[1], ymax = bbox[2]), 
                          crs = st_crs(tile.shapefile.data)))
      bbox_polygon = make_valid_with_snap(bbox_polygon)  # Ensure the geometry is valid
      bbox_polygon = buffer_small(bbox_polygon)  # Add a small buffer to avoid degenerate geometries
      
      if (is.null(combined_bbox)) {
        combined_bbox = bbox_polygon
      } else {
        combined_bbox = st_union(make_valid_with_snap(combined_bbox), 
                                 bbox_polygon)
      }
    }
    
    intersection = st_intersects(tile.shapefile.data, 
                                 st_make_valid(combined_bbox), sparse = FALSE)
    intersected_tiles = which(intersection)
    
    tile_names = tile.shapefile.data$seg[intersected_tiles]
    
    tile_names_country = 
      seg_adm_pop_exposed.df$
      country_code[match(tile_names,
                         substr(seg_adm_pop_exposed.df$seg_adm,1,9))]
    
    tiles_include = which(tile_names_country%in%sliiders.countries)
    
    tile_names = tile_names[tiles_include]
    
    emdat.data.filtered$tiles_covered.local[[i]] = 
      unique(c(emdat.data.filtered$tiles_covered.local[[i]], tile_names))
    
    if(plot_tiles) {
      plot_data = tile.shapefile.data[intersected_tiles, ]
      plot_bbox = st_bbox(plot_data)
      expanded_bbox = expand_bbox_square(plot_bbox, expand_factor = 3)
      
      gg_combined_bbox = ggplot() +
        geom_sf(data = world, fill = "gray80") +
        geom_sf(data = combined_bbox, fill = "blue", alpha = 0.5) + # Combined bbox in blue
       geom_sf(data = plot_data, fill = "red", alpha = 0.5) + # 50% transparency for intersected tiles
        coord_sf(xlim = c(expanded_bbox["xmin"], 
                          expanded_bbox["xmax"]), 
                 ylim = c(expanded_bbox["ymin"], 
                          expanded_bbox["ymax"]), 
                 expand = FALSE) +
        ggtitle(paste("Disaster Number:", disaster.number)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Save the plot
      ggsave(filename = 
               paste0("Maps of SLIIDERS tiles covered by each disaster/", 
                      disaster.number, "_combined_bbox.png"), 
             plot = gg_combined_bbox, width = 10, height = 8)

      gg_tiles = ggplot() +
        geom_sf(data = world, fill = "gray80") +
        geom_sf(data = plot_data, fill = "red", alpha = 0.5) + # 50% transparency
        coord_sf(xlim = c(expanded_bbox["xmin"], expanded_bbox["xmax"]), 
                 ylim = c(expanded_bbox["ymin"], expanded_bbox["ymax"]), 
                 expand = FALSE) +
        ggtitle(paste("Disaster Number:", disaster.number)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      
      ggsave(filename = paste0("Maps of SLIIDERS tiles covered by each disaster/"
                               , disaster.number, "_tiles.png"), 
             width = 10, height = 8,plot = gg_tiles)
    }
  }
  print(paste("III. ", i ," out of ", nrow(emdat.data.filtered), " done",sep=""))
}




# IV. Sum up SLIIDERS tiles populations for different surge heights based on 
# which SLIIDERS tiles intersect with the bounding box of the events


# Initialize columns for population exposed analysis - local SLIIDERS tiles

for (surge in surge.period.vec) {
  col_name = paste0("pop.exposed.local.if.available.", surge)
  emdat.data.filtered[[col_name]] = NA
  
  col_name = paste0("pop.exposed.local.else.national.", surge)
  emdat.data.filtered[[col_name]] = NA
  
  
  for(j in 1:5){
    col_name = paste0("base.2050.SSP", j,".pop.exposed.local.if.available.",surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2050.SSP", j,".pop.exposed.local.else.national.",surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP", j,".pop.exposed.local.if.available.",surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP", j,".pop.exposed.local.else.national.",surge)
    emdat.data.filtered[[col_name]] = NA
    
    
    
    
  }
  
  for(m in 1:length(slr.colnames.root)){
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.local.if.available.",surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.local.else.national.",surge)
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
  slr.popscale.colnames.loop =
    slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
  
  for(n in 1:length(slr.popscale.colnames.loop)){
  
    col_name = gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n])
    
    emdat.data.filtered[[col_name]] = NA
    
    col_name = gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n])
    
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
}

for(i in 1:nrow(emdat.data.filtered)){
  

  tiles_covered = unlist(emdat.data.filtered$tiles_covered.local[i])
  
  # If no SLIIDERS tiles identified (no specific location data) just use
  # national (ADM0) data
  
  if(length(tiles_covered)==0){
    
    for (surge in surge.period.vec) {
      col_name_lookup = paste0("pop.exposed.adm0.", surge)
      col_name_fill_national = paste0("pop.exposed.local.else.national.", surge)
      col_name_fill_NA = paste0("pop.exposed.local.if.available.", surge)
     
       emdat.data.filtered[i,col_name_fill_national] = 
        emdat.data.filtered[i,col_name_lookup]
       
       
       emdat.data.filtered[i,col_name_fill_NA] = NA
       
       for(j in 1:5){
         
         # 2050
         
         col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge)
         col_name_fill_national = paste0("base.2050.SSP",j, ".pop.exposed.local.else.national.",surge)
         
         col_name_fill_NA = paste0("base.2050.SSP",j, ".pop.exposed.local.if.available.",surge)
         
         emdat.data.filtered[i,col_name_fill_national] = 
           emdat.data.filtered[i,col_name_lookup]
         
         
         emdat.data.filtered[i,col_name_fill_NA] = NA
         
         # 2100 
         
         col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge)
         col_name_fill_national = paste0("base.2100.SSP",j, ".pop.exposed.local.else.national.",surge)
         col_name_fill_NA = paste0("base.2100.SSP",j, ".pop.exposed.local.if.available.",surge)
         
         emdat.data.filtered[i,col_name_fill_national] = 
           emdat.data.filtered[i,col_name_lookup]
         
         
         emdat.data.filtered[i,col_name_fill_NA] = NA
         
         
         
       }
       
       for(m in 1:length(slr.colnames.root)){
         
         col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm0.", surge)
         col_name_fill_national = paste0(slr.colnames.root[m], ".pop.exposed.local.else.national.",surge)
         col_name_fill_NA = paste0(slr.colnames.root[m], ".pop.exposed.local.if.available.",surge)
         
         
         emdat.data.filtered[i,col_name_fill_national] = 
           emdat.data.filtered[i,col_name_lookup]
         
         
         emdat.data.filtered[i,col_name_fill_NA] = NA
         
         
         
       }
       
       slr.popscale.colnames.loop =
         slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
       
       for(n in 1:length(slr.popscale.colnames.loop)){
         
         col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n]),sep="")
         
         col_name_fill_national = paste(gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n]),sep="")
         col_name_fill_NA = paste(gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n]),sep="")
         
         emdat.data.filtered[i,col_name_fill_national] = 
           emdat.data.filtered[i,col_name_lookup]
         
         
         emdat.data.filtered[i,col_name_fill_NA] = NA
         
         

         
         
       }
       
       
       
      
    }
    
    
    
    
  }else{
    
    emdat.year = emdat.data.filtered$Start.Year[i]
    
    for(surge in surge.period.vec){
      
      if(emdat.year<2000){
        
        col_name_lookup = paste0("base.",emdat.year,".pop.", surge)
      
      }else{
        
        col_name_lookup = paste0("base.",emdat.year,".SSP1.pop.", surge)
        
      }
    
    pop.sum = sum(seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                        tiles_covered)&
                                          (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                             seg_adm_pop_exposed.df$country_code%in%
                                             country.data$SLIIDERS.country.code[
                                               grep(emdat.data.filtered$ISO[i],
                                                     country.data$Alternate.ISO.code)]))
                                ,which(colnames(seg_adm_pop_exposed.df)==
                                         col_name_lookup)],na.rm=T)
    
    
    
    
    col_name_fill = paste0("pop.exposed.local.else.national.", surge)
    
    emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
      pop.sum
    
    col_name_fill = paste0("pop.exposed.local.if.available.", surge)
    
    emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
      pop.sum
    
    for(j in 1:5){
      
      # 2050
      
      col_name_lookup = paste0("base.2050.SSP",j,".pop.",surge)
      
      pop.sum = sum(seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                    tiles_covered)&
                                                   (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                      seg_adm_pop_exposed.df$country_code%in%
                                                      country.data$SLIIDERS.country.code[
                                                        grep(emdat.data.filtered$ISO[i],
                                                              country.data$Alternate.ISO.code)]))
                                           ,which(colnames(seg_adm_pop_exposed.df)==
                                                    col_name_lookup)],na.rm=T)
    
    
      col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.local.else.national.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.local.if.available.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
      # 2100
      
      col_name_lookup = paste0("base.2100.SSP",j,".pop.",surge)
      
      
      pop.sum = sum(seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                    tiles_covered)&
                                                   (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                      seg_adm_pop_exposed.df$country_code%in%
                                                      country.data$SLIIDERS.country.code[
                                                        grep(emdat.data.filtered$ISO[i],
                                                              country.data$Alternate.ISO.code)]))
                                           ,which(colnames(seg_adm_pop_exposed.df)==
                                                    col_name_lookup)],na.rm=T)
      
      col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.local.else.national.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.local.if.available.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
    }
    
    for(m in 1:length(slr.colnames.root)){
      
      col_name_lookup = paste0(slr.colnames.root[m],".pop.", surge)

      
      pop.sum = sum(seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                   tiles_covered)&
                                                  (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                     seg_adm_pop_exposed.df$country_code%in%
                                                     country.data$SLIIDERS.country.code[
                                                       grep(emdat.data.filtered$ISO[i],
                                                             country.data$Alternate.ISO.code)]))
                                          ,which(colnames(seg_adm_pop_exposed.df)==
                                                   col_name_lookup)],na.rm=T)
      
      col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.local.else.national.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
      col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.local.if.available.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
    }
    
    slr.popscale.colnames.loop =
      slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
    
    for(n in 1:length(slr.popscale.colnames.loop)){
      
      col_name_lookup = slr.popscale.colnames.loop[n]
      
      pop.sum = sum(seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                    tiles_covered)&
                                                   (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                      seg_adm_pop_exposed.df$country_code%in%
                                                      country.data$SLIIDERS.country.code[
                                                        grep(emdat.data.filtered$ISO[i],
                                                              country.data$Alternate.ISO.code)]))
                                           ,which(colnames(seg_adm_pop_exposed.df)==
                                                    col_name_lookup)],na.rm=T)
      
      col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n]),sep="")
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
      col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n]),sep="")
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
      
      
      
    }
    
    
    }
  }
  
}


# V. Pool events that span multiple countries as in Step II

for (surge in surge.period.vec) {
  col_name = paste0("pop.exposed.local.else.national.", surge,".pooled")
  emdat.data.filtered[[col_name]] = NA
  
  col_name = paste0("pop.exposed.local.if.available.", surge,".pooled")
  emdat.data.filtered[[col_name]] = NA
  
  for(j in 1:5){
    
    col_name = paste0("base.2050.SSP",j,".pop.exposed.local.if.available.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2050.SSP",j,".pop.exposed.local.else.national.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    
    col_name = paste0("base.2100.SSP",j,".pop.exposed.local.if.available.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP",j,".pop.exposed.local.else.national.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA    
    
  }
  
  for(m in 1:length(slr.colnames.root)){
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.local.if.available.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.local.else.national.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
  slr.popscale.colnames.loop =
    slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
  
  for(n in 1:length(slr.popscale.colnames.loop)){
    
    col_name = paste(gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n]),".pooled",sep="")
    
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste(gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n]),".pooled",sep="")
    
    emdat.data.filtered[[col_name]] = NA
    
    
    
  }
  
  
}

multi.country.events = list(NA)

k = 0
for(i in 1:nrow(emdat.data.filtered)){
  
  
  disaster.number = substr(emdat.data.filtered$DisNo.[i],1,9)
  disaster.rows = grep(disaster.number,emdat.data.filtered$DisNo.)
  
  if(length(disaster.rows)>1){
    if(length(grep(disaster.number,multi.country.events))==0){
      k = k + 1 
      multi.country.events[[k]] = emdat.data.filtered$DisNo.[disaster.rows]

      for (surge in surge.period.vec) {
        
        # Local else national
        
        col_name_lookup = paste0("pop.exposed.local.else.national.", surge)
        
        pop.sum = sum(emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
        
        col_name_fill = paste0("pop.exposed.local.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
      
        
        # Local if available
   
        
        col_name_lookup = paste0("pop.exposed.local.if.available.", surge)
        
        pops.to.sum = emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        if(any(is.na(pops.to.sum))){
          
          pop.sum = NA
          
        }else{
          pop.sum = sum(pops.to.sum,na.rm=T)
          
        }
        
        
        col_name_fill = paste0("pop.exposed.local.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
      }
      
      for(j in 1:5){
        
        # Local else national
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.local.else.national.", surge)
        
        pop.sum = sum(emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.local.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.local.else.national.", surge)
        
        pop.sum = sum(emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.local.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        # Local if available
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.local.if.available.", surge)
        
        pops.to.sum = emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        if(any(is.na(pops.to.sum))){
          
          pop.sum = NA
          
        }else{
          pop.sum = sum(pops.to.sum,na.rm=T)
          
        }
        
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.local.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.local.if.available.", surge)
        
        pops.to.sum = emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        if(any(is.na(pops.to.sum))){
          
          pop.sum = NA
          
        }else{
          pop.sum = sum(pops.to.sum,na.rm=T)
          
        }
        
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.local.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
       
       
        
         
      }
      for(m in 1:length(slr.colnames.root)){
        
        # Local else national
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.local.else.national.", surge)
        
        pop.sum = sum(emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.local.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
        # Local if available
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.local.if.available.", surge)
        
        pops.to.sum = emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        if(any(is.na(pops.to.sum))){
          
          pop.sum = NA
          
        }else{
          pop.sum = sum(pops.to.sum,na.rm=T)
          
        }
        
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.local.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
      }
      
      slr.popscale.colnames.loop =
        slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
      
      for(n in 1:length(slr.popscale.colnames.loop)){
        
        # Local else national
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n]),sep="")
        
        pop.sum = sum(emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)],na.rm=T)
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n]),".pooled",sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        # Local if available
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n]),sep="")
        
        pops.to.sum = emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        if(any(is.na(pops.to.sum))){
          
          pop.sum = NA
          
        }else{
          pop.sum = sum(pops.to.sum,na.rm=T)
          
        }
        
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n]),".pooled",sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
        
      }
      
    }
  }else{
    
    
    for (surge in surge.period.vec) {
      
      # Local else national
      
      col_name_lookup = paste0("pop.exposed.local.else.national.", surge)
      
      col_name_fill = paste0("pop.exposed.local.else.national.", surge,".pooled")
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
      
      # Local if available
      
      col_name_lookup = paste0("pop.exposed.local.if.available.", surge)
      
      col_name_fill = paste0("pop.exposed.local.if.available.", surge,".pooled")
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
      
      for(j in 1:5){
        
        # Local else national
        
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.local.else.national.", surge)
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.local.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.local.else.national.", surge)
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.local.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
        # Local if available
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.local.if.available.", surge)
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.local.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.local.if.available.", surge)
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.local.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
        
        
      }
      
      for(m in 1:length(slr.colnames.root)){
        
        # Local else national
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.local.else.national.", surge)
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.local.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        # Local if available
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.local.if.available.", surge)
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.local.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
      }
      
      slr.popscale.colnames.loop =
        slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
      
      for(n in 1:length(slr.popscale.colnames.loop)){
       
        # Local else national
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n]),sep="")
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.local.else.national.",slr.popscale.colnames.loop[n]),".pooled",sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
         
        
        # Local if available
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n]),sep="")
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.local.if.available.",slr.popscale.colnames.loop[n]),".pooled",sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
      }
      
    }
    
    
  }
  
}
# save(file="emdat.data.filtered.after.V",emdat.data.filtered)
# save(file="emdat.data.filtered.after.VI.1495",emdat.data.filtered)



# VI. Loop through EMDAT data and make list of SLIIDERS tiles covered by the
# ADM1 level jurisdiction (state/province) that intersects with the bounding
# boxes of locations in SLIIDERS. 

emdat.data.filtered$tiles_covered.adm1 = vector("list", nrow(emdat.data.filtered))

# Allow for making maps of the SLIIDERS tiles relevant to each disaster. Set
# plot_tiles = FALSE for faster processing (but you won't get the maps).

plot_tiles = FALSE

# "world" GIS feature and expanded_bbox_square, make_valid_with_snap,
# and buffer_small functions defined in Step III above. 


# VII. Sum up SLIIDERS tiles populations for different surge heights based on 
# which SLIIDERS tiles intersect with the ADM1 region that intersects with
# the bounding box of the events


# Initialize columns for population exposed analysis - local SLIIDERS tiles

for (surge in surge.period.vec) {
  col_name = paste0("pop.exposed.adm1.if.available.", surge)
  emdat.data.filtered[[col_name]] = NA
  
  col_name = paste0("pop.exposed.adm1.else.national.", surge)
  emdat.data.filtered[[col_name]] = NA
  
  
  for(j in 1:5){
    
    col_name = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge)
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
  for(m in 1:length(slr.colnames.root)){
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge)
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge)
    emdat.data.filtered[[col_name]] = NA
    
  }
  
  slr.popscale.colnames.loop =
    slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
  
  for(n in 1:length(slr.popscale.colnames.loop)){
    
    # Local else national
    
    
    col_name = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),sep="")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),sep="")
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
  
}

for(i in 1:nrow(emdat.data.filtered)){
  
  tiles_covered = unlist(emdat.data.filtered$tiles_covered.adm1[i])
  
  # If no SLIIDERS tiles identified (no specific location data) just use
  # national (ADM0) data
  
  if(length(tiles_covered)==0){
    
    for (surge in surge.period.vec) {
      col_name_lookup = paste0("pop.exposed.adm0.", surge)
      col_name_fill_national = paste0("pop.exposed.adm1.else.national.", surge)
      col_name_fill_NA = paste0("pop.exposed.adm1.if.available.", surge)
      
      emdat.data.filtered[i,col_name_fill_national] = 
        emdat.data.filtered[i,col_name_lookup]
      
      
      emdat.data.filtered[i,col_name_fill_NA] = NA
      
      
      for(j in 1:5){
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm0.", surge)
        
        col_name_fill_national = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge)
        
        col_name_fill_NA = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge)
        
        emdat.data.filtered[i,col_name_fill_national] = 
          emdat.data.filtered[i,col_name_lookup]
        
        
        emdat.data.filtered[i,col_name_fill_NA] = NA
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm0.", surge)
        
        col_name_fill_national = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge)
        
        col_name_fill_NA = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge)
        
        emdat.data.filtered[i,col_name_fill_national] = 
          emdat.data.filtered[i,col_name_lookup]
        
        
        emdat.data.filtered[i,col_name_fill_NA] = NA
        
        
        
        
        
      }
      
      for(m in 1:length(slr.colnames.root)){
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm0.", surge)
        
        col_name_fill_national = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge)
        
        col_name_fill_NA = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge)
        
        emdat.data.filtered[i,col_name_fill_national] = 
          emdat.data.filtered[i,col_name_lookup]
        
        emdat.data.filtered[i,col_name_fill_NA] = NA
        
        
        
      }
      
      slr.popscale.colnames.loop =
        slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
      
      for(n in 1:length(slr.popscale.colnames.loop)){
        
        # Local else national
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm0.",slr.popscale.colnames.loop[n]),sep="")
        
        col_name_fill_national = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),sep="")
        
        col_name_fill_NA = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),sep="")
        
        emdat.data.filtered[i,col_name_fill_national] = 
          emdat.data.filtered[i,col_name_lookup]
        
        emdat.data.filtered[i,col_name_fill_NA] = NA
        
      }
      
      
    }
    
    
  }else{
    
    emdat.year = emdat.data.filtered$Start.Year[i]
    
    for(surge in surge.period.vec){
      
      if(emdat.year<2000){
        
        col_name_lookup = paste0("base.",emdat.year,".pop.", surge)
        
      }else{
        
        col_name_lookup = paste0("base.",emdat.year,".SSP1.pop.", surge)
        
      }
      
      
      pops.to.sum = seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                   tiles_covered)&
                                                  (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                     seg_adm_pop_exposed.df$country_code%in%
                                                     country.data$SLIIDERS.country.code[
                                                       grep(emdat.data.filtered$ISO[i],
                                                             country.data$Alternate.ISO.code)]))
                                          ,which(colnames(seg_adm_pop_exposed.df)==
                                                   col_name_lookup)]
      
      pop.sum = sum(pops.to.sum,na.rm=T)

      col_name_fill = paste0("pop.exposed.adm1.else.national.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      col_name_fill = paste0("pop.exposed.adm1.if.available.", surge)
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        pop.sum
      
      
      for(j in 1:5){
        col_name_lookup = paste0("base.2050.SSP",j,".pop.", surge)
        
        pops.to.sum = seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                      tiles_covered)&
                                                     (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                        seg_adm_pop_exposed.df$country_code%in%
                                                        country.data$SLIIDERS.country.code[
                                                          grep(emdat.data.filtered$ISO[i],
                                                                country.data$Alternate.ISO.code)]))
                                             ,which(colnames(seg_adm_pop_exposed.df)==
                                                      col_name_lookup)]
        
        pop.sum = sum(pops.to.sum,na.rm=T)
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge)
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge)
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.", surge)
        
        pops.to.sum = seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                      tiles_covered)&
                                                     (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                        seg_adm_pop_exposed.df$country_code%in%
                                                        country.data$SLIIDERS.country.code[
                                                          grep(emdat.data.filtered$ISO[i],
                                                                country.data$Alternate.ISO.code)]))
                                             ,which(colnames(seg_adm_pop_exposed.df)==
                                                      col_name_lookup)]
        
        pop.sum = sum(pops.to.sum,na.rm=T)
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge)
        
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge)
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
        
      }
      
      for(m in 1:length(slr.colnames.root)){

        col_name_lookup = paste0(slr.colnames.root[m],".pop.", surge)
        
        pops.to.sum = seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                      tiles_covered)&
                                                     (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                        seg_adm_pop_exposed.df$country_code%in%
                                                        country.data$SLIIDERS.country.code[
                                                          grep(emdat.data.filtered$ISO[i],
                                                                country.data$Alternate.ISO.code)]))
                                             ,which(colnames(seg_adm_pop_exposed.df)==
                                                      col_name_lookup)]
        
        pop.sum = sum(pops.to.sum,na.rm=T)
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge)
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge)
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
      }
      
      slr.popscale.colnames.loop =
        slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
      
      for(n in 1:length(slr.popscale.colnames.loop)){
        
        # Local else national
        
        col_name_lookup = slr.popscale.colnames.loop[n]
        
        pops.to.sum = seg_adm_pop_exposed.df[which((substr(seg_adm_pop_exposed.df$seg_adm,1,9)%in%
                                                      tiles_covered)&
                                                     (seg_adm_pop_exposed.df$country_code==emdat.data.filtered$ISO[i]|
                                                        seg_adm_pop_exposed.df$country_code%in%
                                                        country.data$SLIIDERS.country.code[
                                                          grep(emdat.data.filtered$ISO[i],
                                                                country.data$Alternate.ISO.code)]))
                                             ,which(colnames(seg_adm_pop_exposed.df)==
                                                      col_name_lookup)]
        
        pop.sum = sum(pops.to.sum,na.rm=T)
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        

        
      }
      
      
      
    }
  }
  
}

# Some location data actually only specifies the country; remove those from
# the column that is reserved for only subnational data.

col_name_fill_NA = unique(c(grep("local.if.available",colnames(emdat.data.filtered)),
                            grep("adm1.if.available",colnames(emdat.data.filtered))))

for(geocode.row in which(geocode.data$addresstype=="country")){
  
  disaster.numbers.fix = 
    unlist(strsplit(geocode.data$disaster_number[geocode.row],split = ","))
  
  emdat.rows.fix = match(disaster.numbers.fix,
                         emdat.data.filtered$DisNo.)
  
  for(i in emdat.rows.fix[!is.na(emdat.rows.fix)]){
    emdat.data.filtered[i,col_name_fill_NA] = NA
  }
}

# VIII. Pool events that span multiple countries as in Steps II and V

for (surge in surge.period.vec) {
  col_name = paste0("pop.exposed.adm1.else.national.", surge,".pooled")
  emdat.data.filtered[[col_name]] = NA
  
  col_name = paste0("pop.exposed.adm1.if.available.", surge,".pooled")
  emdat.data.filtered[[col_name]] = NA
  
  for(j in 1:5){
    
    col_name = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    
    col_name = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
  for(m in 1:length(slr.colnames.root)){
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge,".pooled")
    emdat.data.filtered[[col_name]] = NA
    
    
  }
  
  slr.popscale.colnames.loop =
    slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
  
  for(n in 1:length(slr.popscale.colnames.loop)){
    
    
    col_name = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),".pooled",sep="")
    emdat.data.filtered[[col_name]] = NA
    
    col_name = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),".pooled",sep="")
    emdat.data.filtered[[col_name]] = NA
    
    
    
  }
}

multi.country.events = list(NA)

k = 0
for(i in 1:nrow(emdat.data.filtered)){
  
  disaster.number = substr(emdat.data.filtered$DisNo.[i],1,9)
  disaster.rows = grep(disaster.number,emdat.data.filtered$DisNo.)
  
  if(length(disaster.rows)>1){
    if(length(grep(disaster.number,multi.country.events))==0){
      k = k + 1 
      multi.country.events[[k]] = emdat.data.filtered$DisNo.[disaster.rows]
      
      for (surge in surge.period.vec) {
        
        # Local else national
        
        col_name_lookup = paste0("pop.exposed.adm1.else.national.", surge)
        
        pops.to.sum = emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        if(any(is.na(pops.to.sum))){
          
          pop.sum = NA
          
        }else{
          pop.sum = sum(pops.to.sum,na.rm=T)
          
        }
        
        col_name_fill = paste0("pop.exposed.adm1.else.national.", surge,".pooled")
        
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        for(j in 1:5){
          
          col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge)
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            
            pop.sum = NA
            
          }else{
            pop.sum = sum(pops.to.sum,na.rm=T)
            
          }
          
          col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge,".pooled")
          
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          
          
          col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge)
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            
            pop.sum = NA
            
          }else{
            pop.sum = sum(pops.to.sum,na.rm=T)
            
          }
          
          col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge,".pooled")
          
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum

          
        }
        
        
        
        # Local if available
        
        col_name_lookup = paste0("pop.exposed.adm1.if.available.", surge)
        
        pops.to.sum = emdat.data.filtered[
          match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
          which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        if(any(is.na(pops.to.sum))){
          pop.sum = NA
        }else{
          pop.sum = sum(pops.to.sum)
        }
        
        col_name_fill = paste0("pop.exposed.adm1.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          pop.sum
        
        
        for(j in 1:5){
          
          col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge)
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            pop.sum = NA
          }else{
            pop.sum = sum(pops.to.sum)
          }
          
          col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge,".pooled")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge)
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            pop.sum = NA
          }else{
            pop.sum = sum(pops.to.sum)
          }
          
          col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge,".pooled")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
        }
        
        for(m in 1:length(slr.colnames.root)){
          
          # Local else national
          
          col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge)
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            pop.sum = NA
          }else{
            pop.sum = sum(pops.to.sum)
          }
          
          col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge,".pooled")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          # Local if available
          
          col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge)
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            pop.sum = NA
          }else{
            pop.sum = sum(pops.to.sum)
          }
          
          col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge,".pooled")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          
        }
        
        slr.popscale.colnames.loop =
          slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
        
        for(n in 1:length(slr.popscale.colnames.loop)){
          
          
          # Local else national
          
          col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),sep="")
          emdat.data.filtered[[col_name]] = NA
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            pop.sum = NA
          }else{
            pop.sum = sum(pops.to.sum)
          }
          
          col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),".pooled",sep="")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          # Local if available
          
          col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),sep="")
          emdat.data.filtered[[col_name]] = NA
          
          pops.to.sum = emdat.data.filtered[
            match(unlist(multi.country.events[[k]]),emdat.data.filtered$DisNo.),
            which(colnames(emdat.data.filtered)==col_name_lookup)]
          
          if(any(is.na(pops.to.sum))){
            pop.sum = NA
          }else{
            pop.sum = sum(pops.to.sum)
          }
          
          col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),".pooled",sep="")
          
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
            pop.sum
          
          
        }
        
      }
      
    }
    
  }else{
    
    for (surge in surge.period.vec) {
      
      # Local else national
      
      col_name_lookup = paste0("pop.exposed.adm1.else.national.", surge)
      
      col_name_fill = paste0("pop.exposed.adm1.else.national.", surge,".pooled")
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
      
      # Local if available
      
      col_name_lookup = paste0("pop.exposed.adm1.if.available.", surge)
      
      col_name_fill = paste0("pop.exposed.adm1.if.available.", surge,".pooled")
      
      emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
      
      for(j in 1:5){
        
        # Local else national
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge)
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm1.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge)
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm1.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        # Local if available
        
        col_name_lookup = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge)
        
        col_name_fill = paste0("base.2050.SSP",j,".pop.exposed.adm1.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        col_name_lookup = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge)
        
        col_name_fill = paste0("base.2100.SSP",j,".pop.exposed.adm1.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
      }
      
      for(m in 1:length(slr.colnames.root)){
        
        # Local else national
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge)
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm1.else.national.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
      
        
        # Local if available
        
        col_name_lookup = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge)
        
        col_name_fill = paste0(slr.colnames.root[m],".pop.exposed.adm1.if.available.", surge,".pooled")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
        
      }
      
      slr.popscale.colnames.loop =
        slr.popscale.colnames[grep(surge,slr.popscale.colnames)]
      
      for(n in 1:length(slr.popscale.colnames.loop)){
        
        # Local else national
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),sep="")
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm1.else.national.",slr.popscale.colnames.loop[n]),".pooled",sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
      
        # Local if available
        
        col_name_lookup = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),sep="")
        
        col_name_fill = paste(gsub("\\.pop\\.",".pop.exposed.adm1.if.available.",slr.popscale.colnames.loop[n]),".pooled",sep="")
        
        emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_fill)] =
          emdat.data.filtered[i,which(colnames(emdat.data.filtered)==col_name_lookup)]
        
        
        
      }
      
    }
  }
  
}

emdat.data.filtered.to.save = 
  emdat.data.filtered[,which(colnames(emdat.data.filtered)%in%
                               setdiff(colnames(emdat.data.filtered),
                               c("tiles_covered.adm1",
                                 "tiles_covered.local",
                                 "tiles_covered")))]

# Make column names easier to understand
emdat.data.filtered.to.save.new.colnames = 
  emdat.data.filtered.to.save
colnames(emdat.data.filtered.to.save.new.colnames)[
  grep("base.",colnames(emdat.data.filtered.to.save.new.colnames))] =
  gsub("base.","popscale.",
       colnames(emdat.data.filtered.to.save.new.colnames)[
         grep("base.",colnames(emdat.data.filtered.to.save.new.colnames))])

cols.to.fix = 
  grep("local.else.national",colnames(emdat.data.filtered.to.save.new.colnames))

fix.summary = 0*1:length(cols.to.fix)

for(i in 1:length(cols.to.fix)){
  colname.to.fix = colnames(emdat.data.filtered.to.save.new.colnames)[cols.to.fix][i]
  
  fix.summary[i] = 
    length(which(emdat.data.filtered.to.save.new.colnames[colname.to.fix]<
                emdat.data.filtered.to.save.new.colnames$total.deaths.na.as.0))
}

for(i in 1:length(cols.to.fix)){
  
  colname.to.fix = colnames(emdat.data.filtered.to.save.new.colnames)[cols.to.fix][i]
  colname.new = gsub("local.else.national","local.else.adm1.else.national",colname.to.fix)
  
  emdat.data.filtered.to.save.new.colnames[colname.new] = 
    emdat.data.filtered.to.save.new.colnames[colname.to.fix]
  
  rows.to.fix = 
    which(emdat.data.filtered.to.save.new.colnames[colname.to.fix]<
            emdat.data.filtered.to.save.new.colnames$total.deaths.na.as.0)
  
  emdat.data.filtered.to.save.new.colnames[rows.to.fix,colname.new] <-
    emdat.data.filtered.to.save.new.colnames[rows.to.fix,
                                             gsub("local.else.national",
                                                  "adm1.else.national",
                                                  colname.to.fix)]
    
  
}

cols.to.fix = 
  grep("local.else.adm1.else.national",colnames(emdat.data.filtered.to.save.new.colnames))


for(i in 1:length(cols.to.fix)){
  
  colname.to.fix = colnames(emdat.data.filtered.to.save.new.colnames)[cols.to.fix][i]
  
  
  rows.to.fix = 
    which(emdat.data.filtered.to.save.new.colnames[colname.to.fix]<
            emdat.data.filtered.to.save.new.colnames$total.deaths.na.as.0)
  
  emdat.data.filtered.to.save.new.colnames[rows.to.fix,colname.to.fix] <-
    emdat.data.filtered.to.save.new.colnames[rows.to.fix,
                                             gsub("local.else.adm1.else.national",
                                                  "adm0",
                                                  colname.to.fix)]
  
  
}

  

write.xlsx(emdat.data.filtered.to.save.new.colnames,
           "7 output - EMDAT events with exposed pops.xlsx")


