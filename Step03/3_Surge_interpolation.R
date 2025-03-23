##########################################
# Step 3 - Interpolate population at     #
# different surge return intervals       #
#                                        #  
# 2025-02-05                             #
# Ryan Calder                            #
##########################################

# I. Clear environment, set WD, and read in SLIIDERS data generated from Step 1
# Python code

rm(list = ls())
# Getting the path of your current open file
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

library(dplyr)

surge_height.data = read.csv("../Step01/surge_height.csv")
pop2019_10cm_incr = read.csv("../Step01/pop2019_new.csv")
  
# II. Set up objects for different return periods and group surge heights
# at different return periods into a data frame with 2019 population


filtered_surge_10yr = surge_height.data %>% filter(return_period==10)
filtered_surge_100yr = surge_height.data %>% filter(return_period==100)
filtered_surge_1000yr = surge_height.data %>% filter(return_period==1000)
filtered_surge_10000yr = surge_height.data %>% filter(return_period==10000)

## Check for mismatched tiles

unique(pop2019_10cm_incr$seg_adm)[
  which((unique(pop2019_10cm_incr$seg_adm)%in%unique(surge_height.data$seg_adm))==
          FALSE)]

unique(surge_height.data$seg_adm)[
  which((unique(surge_height.data$seg_adm)%in%unique(pop2019_10cm_incr$seg_adm))==
          FALSE)]

## Join filtered surge height and 2019 pops and identify surge heights clearly

df = merge(pop2019_10cm_incr, filtered_surge_10yr, by="seg_adm")
colnames(df)[colnames(df)=="surge_height"] = "surge_height_10yr"
df = full_join(df, filtered_surge_100yr, by="seg_adm")
colnames(df)[colnames(df)=="surge_height"] = "surge_height_100yr"
df = full_join(df, filtered_surge_1000yr, by="seg_adm")
colnames(df)[colnames(df)=="surge_height"] = "surge_height_1000yr"
df = full_join(df, filtered_surge_10000yr, by="seg_adm")
colnames(df)[colnames(df)=="surge_height"] = "surge_height_10000yr"

## Make data frame with relevant columns and delete df

pop_surge.df = cbind.data.frame(seg_adm = df$seg_adm,
                                adm1 = df$adm1,
                                seg = df$seg,
                                seg_country = df$seg_country,
                                elev = df$elev,
                                pop_2019_at_elev_band = df$pop_2019,
                                surge_height_10yr = df$surge_height_10yr,
                                surge_height_100yr = df$surge_height_100yr,
                                surge_height_1000yr = df$surge_height_1000yr,
                                surge_height_10000yr = df$surge_height_10000yr)

rm(df)

# III. Prepare future scenarios based on output of Step 2

slr.filenames = fs::dir_ls("../Step02/", recurse = FALSE, glob = "*.csv")
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


slr.colnames = paste(slr.colnames.root,".",surge.period.vec,sep="")

### IV. Calculate 2019 populations below different surge heights 

## Make vector of unique seg_adm tiles

seg_adm.vec = unique(pop_surge.df$seg_adm)

## Make data frame listing seg_adm tiles with total population at all elevations
## and population at elevations below surge heights with different return 
## frequencies

seg_adm_pop_exposed.df = data.frame(seg_adm = seg_adm.vec,
                                 pop_2019_whole_tile = as.numeric(NA),
                                 pop_2019_below_10yr_surge = as.numeric(NA),
                                 pop_2019_below_100yr_surge = as.numeric(NA),
                                 pop_2019_below_1000yr_surge = as.numeric(NA),
                                 pop_2019_below_10000yr_surge = as.numeric(NA))


for(i in 1:length(slr.colnames)){
  
  seg_adm_pop_exposed.df[[slr.colnames[i]]] = as.numeric(NA)
}

for(i in 1:nrow(seg_adm_pop_exposed.df)){

  ### Sum total population in that tile across all heights 
  
  seg_adm_pop_exposed.df$pop_2019_whole_tile[i] = 
    
    sum(pop_surge.df$pop_2019_at_elev_band[
      which(pop_surge.df$seg_adm==seg_adm_pop_exposed.df$seg_adm[i])])
  
  ### Make vector of 10, 100, 1000, 10000 year surge heights for that tile
  
  surge_height.vec = c((pop_surge.df$surge_height_10yr[
    pop_surge.df$seg_adm==seg_adm_pop_exposed.df$seg_adm[i]])[1], 
    
    (pop_surge.df$surge_height_100yr[
      pop_surge.df$seg_adm==seg_adm_pop_exposed.df$seg_adm[i]])[1],
  
      (pop_surge.df$surge_height_1000yr[
        pop_surge.df$seg_adm==seg_adm_pop_exposed.df$seg_adm[i]])[1],
    
    (pop_surge.df$surge_height_10000yr[
      pop_surge.df$seg_adm==seg_adm_pop_exposed.df$seg_adm[i]])[1])
  
  ### Make vector of elevations in order
  
  elev.vec = pop_surge.df$elev[pop_surge.df$seg_adm==seg_adm_pop_exposed.df$seg_adm[i]]
  elev.vec.reordered = elev.vec[order(elev.vec)]
  elev.vec.reordered.top = elev.vec.reordered + 0.05
  
  ### Make vector of populations in order
  
  surge.vec = pop_surge.df$pop_2019_at_elev_band[
    pop_surge.df$seg_adm==seg_adm_pop_exposed.df$seg_adm[i]]
  surge.vec_reordered = surge.vec[order(elev.vec)]
  
  for(j in 1:length(surge_height.vec)){
    
    ### If surge height is above maximum elevation reported then just set exposed
    ### equal to total population 
    
    if(surge_height.vec[j]>max(elev.vec.reordered.top)){
      
      
      seg_adm_pop_exposed.df[i,2+j] = seg_adm_pop_exposed.df$pop_2019_whole_tile[i]
      
      
    }else{
      
      ### For each surge height, find the first elevation height that is above 
      ### that surge height
      
      
      elev_above_index = which(elev.vec.reordered.top>surge_height.vec[j])[1]
      
      elev_above = elev.vec.reordered.top[elev_above_index]
      
      ### For each surge height, find the first elevation height that is below 
      ### that surge height and sum up the population there
      
      elev_below_index = elev_above_index - 1
      
      elev_below = elev.vec.reordered.top[elev_below_index]
      
      pop_exposed_below = sum(surge.vec_reordered[1:elev_below_index])
      
      ### Calculate what fraction to include of the next highest level
      
      interp.frac = (surge_height.vec[j]-elev_below)/(elev_above-elev_below)
      
      pop_exposed_above = interp.frac * surge.vec_reordered[elev_above_index]
      
      seg_adm_pop_exposed.df[i,2+j] = pop_exposed_below + pop_exposed_above
      
    }
    
  }
  
  ## Recalculate population for future scenarios
  
  for(k in 1:length(slr.colnames.root)){
    
    slr.data = 
      read.csv(paste("../Step02/",
                     slr.filenames[k],sep=""))
    
    slr.offset = slr.data$lsl_msl05[match(substr(seg_adm_pop_exposed.df$seg_adm[i],
                                                 1,9),slr.data$seg)]
    
    if(is.null(slr.offset)){
      slr.offset = slr.data$lsl_ncc_msl05[match(substr(seg_adm_pop_exposed.df$seg_adm[i],
                                                       1,9),slr.data$seg)]
      
    }
    
    
    if(!is.na(slr.offset)){
      surge_height.vec.offset = surge_height.vec + slr.offset  
    }else{
      surge_height.vec.offset = surge_height.vec
    }
    
    surge_height.vec.offset[surge_height.vec.offset<0] = 0
    
    for(j in 1:length(surge.period.vec)){
      
      colname.fill = 
        
        paste(slr.colnames.root[k],".",surge.period.vec[j],sep="")
      
      if(surge_height.vec.offset[j]>max(elev.vec.reordered)){
        
        
        seg_adm_pop_exposed.df[[colname.fill]][i] = 
          seg_adm_pop_exposed.df$pop_2019_whole_tile[i]
        
        
      }else{
        
        if(surge_height.vec.offset[j]==0){
          
          seg_adm_pop_exposed.df[[colname.fill]][i] = 0
          
        }else{
          
          ### For each surge height, find the first elevation height that is above 
          ### that surge height
          
          
          elev_above_index = which(elev.vec.reordered.top>surge_height.vec.offset[j])[1]
          
          elev_above = elev.vec.reordered.top[elev_above_index]
          
          ### For each surge height, find the first elevation height that is below 
          ### that surge height and sum up the population there
          
          elev_below_index = elev_above_index - 1
          
          elev_below = elev.vec.reordered.top[elev_below_index]
          
          pop_exposed_below = sum(surge.vec_reordered[1:elev_below_index])
          
          ### Calculate what fraction to include of the next highest level
          
          interp.frac = (surge_height.vec.offset[j]-elev_below)/(elev_above-elev_below)
          
          pop_exposed_above = interp.frac * surge.vec_reordered[elev_above_index]
          
          if(length(pop_exposed_above)==0){
            seg_adm_pop_exposed.df[[colname.fill]][i] = pop_exposed_below
            
          }else{
            seg_adm_pop_exposed.df[[colname.fill]][i] = pop_exposed_below + pop_exposed_above
            
          }
        }
      }
    }
  }
  ### Print every 10 rows done
  
  if(i %% 10 == 0){
    print(paste(i," out of ", nrow(seg_adm_pop_exposed.df)," completed",
                " (",round(i*100/nrow(seg_adm_pop_exposed.df),1),"%)",sep=""))
    
  }
  
}

colnames(seg_adm_pop_exposed.df)

# Ensure consistent column names
for(i in 1:length(surge.period.vec)){
  colnames(seg_adm_pop_exposed.df) =
    gsub(paste("\\.",surge.period.vec[i],"$",sep=""),
         paste(".pop.",surge.period.vec[i],sep=""), 
         colnames(seg_adm_pop_exposed.df))
}


colnames(seg_adm_pop_exposed.df)[
  colnames(seg_adm_pop_exposed.df)=="pop_2019_whole_tile"] =
  "base.2019.pop.whole.tile"

for(i in 1:length(surge.period.vec)){
  colnames(seg_adm_pop_exposed.df)[
    colnames(seg_adm_pop_exposed.df)==paste("pop_2019_below_",surge.period.vec[i],
    "_surge",sep="")] = paste("base.2019.pop.",surge.period.vec[i],sep="")
}
  
  
### IV. Output file for use in subsequent steps


write.csv(seg_adm_pop_exposed.df, 
          "3 output - Population below surge return periods (present + future) by tile no pop growth.csv")
