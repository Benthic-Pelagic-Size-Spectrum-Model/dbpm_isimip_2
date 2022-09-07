
rm(list=ls())

# ---------------------------------- STEP 1: GET GCM INPUTS FOR DYNAMIC BENTHIC-PELAGIC SIZE SPECTRUM MODEL

#### CN has not run this step for CMIP63b. This step was run by Ryan who then provided the inputs. 
#### CN is runnig this step for CMIP63a, LME scale (first added part) and CMIP63a gridcell scale. 

#### CMIP63a LME scale inputs adding by CN -----
# explore LME inputs provided by Ryan on 25/07/2022

library(raster)
library(stringr)
library(tidyverse)
library(tictoc)
library(data.table)
library(parallel)

#### 1. Define main function: for each LME, and each variable, apply the calc_inputs_LME function -----

# 1A. calc_inputs_LME()
# this function calculates LME fixed weighted.mean depth and total area, 
# and monthly weighted.mean for each climate variable. 
# it then add spin up from control to the cliamte variables.  
calc_inputs_LME<-function(file_name_obs, file_name_crtl, file_path_crtl, file_path_obs){
  
  # extract variable name
  variable<-str_match(file_name_obs, "gfdl-mom6-cobalt2_obsclim_\\s*(.*?)\\s*_15arcmin")[2]
  
  # work on CONTROLCLIM first 
  lme_crtl<-read.csv(file.path(file_path_crtl, file_name_crtl))
  
  # then work on OBSERVED 
  lme<-read.csv(file.path(file_path_obs, file_name_obs))
  
  # 2 methods to calculate weighed mean, 1 based on dplyer and cell area, 2 based on raster and cos(lat)
  # same results for LME 1, depth: method 1 gives 1797.203 and method 2 gives 1797.202
  # for depth and area, for LME 1 results are the same for obs and crtl as expected   
  # for LME 1 results are the same for expc-bot_mol_m-2_s-1 (first of inputs) across methods (checked trends). 
  # assuming this is the case for all inputs... 
  # NOTE, the advantage of method 2 is plotting LME but this is much slower than method 1. 
  
  # # METHOD 1
  if (variable == "deptho_m"){
    
    # calculate fixed variables - mean depth and area of LME 
    weighted_mean_obs<-lme %>% 
      summarise(deptho_m = weighted.mean(m, area_m2),
                area_m2 = sum(area_m2)) %>% 
      mutate(LME = str_extract(file_name_obs,"(?<=LME_).+(?=_monthly)"))
    
    weighted_mean_obs_final<-weighted_mean_obs
    weighted_mean_crtl_final<-weighted_mean_obs # shortcut if you need to extract crtl values too for all variables
    
  }else{
    
    # CONTROL 
    weighted_mean_crtl<-lme_crtl %>% 
      gather(key = "Date",
             value = "value",
             -c("lat", "lon","area_m2")) %>% 
      group_by(Date) %>% 
      summarise(weighted_mean_crtl = weighted.mean(value, area_m2)) %>% 
      ungroup() %>% 
      mutate(LME = str_extract(file_name_crtl,"(?<=LME_).+(?=_monthly)"),
             Month = str_extract(Date,"[[:upper:]]+[[:lower:]]+"),
             Year = str_extract(Date, "\\d+"), 
             Date = lubridate::my(paste(Month,Year, sep = "." ))) %>% 
      arrange(Date)
    
    # # to plot and compare 
    # trial<-weighted_mean_crtl %>% 
    #   group_by(Year) %>% 
    #   summarise(weighted_mean_crtl = mean(weighted_mean_crtl)) %>% 
    #   ungroup() %>% 
    #   mutate(Year = as.numeric(Year))
    # 
    # # to compare with methods below ... 
    # pdf("Output/plot1.pdf")
    # ggplot(trial, aes(x = Year, y = weighted_mean_crtl))+
    #   geom_point()+
    #   geom_line()
    # dev.off()
    
    # calculate spinup which will then be used for the observed
    spinup<-weighted_mean_crtl %>%
      select(-Date) %>% 
      filter(Year >= 1961, Year <=1980) %>% 
      slice(rep(1:n(), times = 6)) %>%
      mutate(Year = as.character(rep(1841:1960, each = 12)),
             Date = lubridate::my(paste(Month,Year, sep = "." ))) 
    
    # add spinup to control and check that's all OK 
    weighted_mean_crtl_final<-weighted_mean_crtl %>% 
      full_join(spinup) %>% 
      arrange(Date)
    
    # reorder columns 
    weighted_mean_crtl_final<-weighted_mean_crtl_final[,c("LME", "Date", "Year", "Month", "weighted_mean_crtl")]
    
    # rename weighed_mean column according to variable 
    names(weighted_mean_crtl_final)[5] <- variable
    
    # OBSERVED 
    weighted_mean_obs<-lme %>% 
      gather(key = "Date",
             value = "value",
             -c("lat", "lon","area_m2")) %>% 
      group_by(Date) %>% 
      summarise(weighted_mean_obs = weighted.mean(value, area_m2)) %>% 
      ungroup() %>% 
      mutate(LME = str_extract(file_name_crtl,"(?<=LME_).+(?=_monthly)"),
             Month = str_extract(Date,"[[:upper:]]+[[:lower:]]+"),
             Year = str_extract(Date, "\\d+"), 
             Date = lubridate::my(paste(Month,Year, sep = "." ))) %>% 
      arrange(Date)
    
    # add spin up to observed and plot to check 
    spinup<-spinup %>% 
      rename(weighted_mean_obs = weighted_mean_crtl)
    
    weighted_mean_obs_final<-weighted_mean_obs %>% 
      full_join(spinup) %>% 
      arrange(Date) 
    
    # reorder columns 
    weighted_mean_obs_final<-weighted_mean_obs_final[,c("LME", "Date", "Year", "Month", "weighted_mean_obs")]
    
    # rename weighed_mean column according to variable 
    names(weighted_mean_obs_final)[5] <- variable
    
  }
  
  # # METHOD 2
  # # create raster (dim 1 = long, dim 2 = lat and dim 3 = time)  
  # lme_crtl_for_raster<-lme_crtl %>% 
  #   dplyr::select(-area_m2) %>% 
  #   relocate(lat, .after = lon)
  # lmeRaster_crtl <- rasterFromXYZ(lme_crtl_for_raster)
  # 
  # # WARNING - should I add a crs?!?
  # 
  # # # CHECK dimentions were correctly taken into account
  # # trial<-lme_crtl[,c(2,1,6)]
  # # trial <- rasterFromXYZ(trial)
  # # dim(trial)
  # 
  # # pdf("/Output/plot.pdf")
  # # plot(lmeRaster_crtl[[3]])
  # # dev.off()
  # 
  # # pdf("/Output/plot1.pdf")
  # # plot(trial)
  # # dev.off()
  # 
  # # Calculate the weighted average of climate input (for all inputs including depth)
  # # mean values - weighted by grid cell latitude 
  # # https://stackoverflow.com/questions/55230510/calculating-weighted-spatial-global-annual-averages-across-grid-cells-using-netc
  # 
  # # raster with latitude cell values 
  # w_crtl <- init(lmeRaster_crtl, 'y')
  # # cosine after transforming to radians
  # w_crtl <- cos(w_crtl  * (pi/180))
  # # multiply weights with values
  # x_crtl <- lmeRaster_crtl * w_crtl
  # 
  # # CHECK 
  # # # need to mask the weights too (x_crtl) otherwise denominator is too high
  # # pdf("Output/plot1.pdf", height = 8, width = 6)
  # # plot(x_crtl[[1]]) # MULTIPLE DIMENTION 
  # # dev.off()
  # # 
  # # pdf("Output/plot2.pdf", height = 8, width = 6)
  # # plot(w_crtl) # ONE DIMENTION if using lmeRaster_crtl[[1]] above 
  # # dev.off()
  # 
  # w2_crtl <- mask(w_crtl, lmeRaster_crtl, updatevalue=NA) # MULTIPLE DIMENTIONS 
  # # w2_crtl<-mask(w_crtl, lmeRaster_crtl[[1]], updatevalue=NA) # ONE DIMENTION - it does not matter but need to check 
  # 
  # # # CHECK
  # # pdf("Output/plot3.pdf", height = 8, width = 6)
  # # plot(w2_crtl)
  # # dev.off()
  # 
  # # compute weighted average - WARNING is na.rm = TRUE ok??  
  # weighted_mean_crtl<-cellStats(x_crtl , sum, na.rm = TRUE) / cellStats(w2_crtl , sum, na.rm = TRUE)
  # 
  # # transform into data frame 
  # weighted_mean_crtl<-data.frame(Year = colnames(dplyr::select(lme_crtl,-c("lat","lon","area_m2"))), weighted_mean_crtl = weighted_mean_crtl)
  # weighted_mean_crtl$LME <- str_extract(file_name_crtl,"(?<=LME_).+(?=_monthly)") # extract LME # keep even if not needed to double check the files
  # rownames(weighted_mean_crtl)<-NULL
  # 
  # weighted_mean_crtl<-weighted_mean_crtl %>% 
  #   mutate(Month = str_extract(Year,"[[:upper:]]+[[:lower:]]+"),
  #          Year = str_extract(Year, "\\d+"))
  # 
  # # # compare with METHOD 1 
  # # trial<-weighted_mean_crtl %>% 
  # #   group_by(Year) %>% 
  # #   summarise(weighted_mean_crtl = mean(weighted_mean_crtl)) %>% 
  # #   ungroup() %>% 
  # #   mutate(Year = as.numeric(Year))
  # # 
  # # pdf("Output/plot2.pdf")
  # # ggplot(trial, aes(x = Year, y = weighted_mean_crtl))+
  # #   geom_point()+
  # #   geom_line()
  # # dev.off()
  # 
  # # calculate spinup which will then be used for the observed
  # spinup<-weighted_mean_crtl %>%
  #   filter(Year >= 1961, Year <=1980) %>% 
  #   slice(rep(1:n(), times = 6)) %>%
  #   mutate(Year = as.character(rep(1841:1960, each = 12))) 
  # 
  # # add spinup to control and check that's all OK 
  # weighted_mean_crtl_final<-weighted_mean_crtl %>% 
  #   full_join(spinup) %>% 
  #   mutate(Year = as.numeric(Year)) %>% 
  #   arrange(Year, Month)
  # 
  # # # CHECK 
  # # # check by year 
  # # weighted_mean_crtl_final_plot<-weighted_mean_crtl_final %>% 
  # #   group_by(Year, LME) %>% 
  # #   summarise(weighted_mean_crtl = mean(weighted_mean_crtl)) %>% 
  # #   ungroup()
  # # 
  # # # plot to check
  # # pdf("Output/plot.pdf", height = 4, width = 6)
  # # ggplot(weighted_mean_crtl_final_plot, aes(x = Year, y = weighted_mean_crtl))+
  # #   geom_line()+
  # #   annotate("rect",xmin=1961, xmax=1980, ymin=-Inf, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)
  # # dev.off()
  # 
  # # work on OBSERVED
  # # create raster (dim 1 = long, dim 2 = lat and dim 3 = time)   
  # lme_for_raster<-lme %>% 
  #   dplyr::select(-area_m2) %>% 
  #   relocate(lat, .after = lon)
  # lmeRaster <- rasterFromXYZ(lme_for_raster)
  # 
  # # CHECK 
  # # trial<-lme[,c(2,1,6)]
  # # trial <- rasterFromXYZ(trial)
  # # dim(trial)
  # 
  # # pdf("/Output/plot.pdf")
  # # plot(lmeRaster[[3]])
  # # dev.off()
  # 
  # # pdf("Output/plot1.pdf")
  # # plot(trial)
  # # dev.off()
  # 
  # # mean values - weighted by grid cell latitude 
  # # raster with latitude cell values 
  # w <- init(lmeRaster, 'y')
  # # cosine after transforming to radians
  # w <- cos(w  * (pi/180))
  # # multiply weights with values
  # x <- lmeRaster * w
  # 
  # # # need to mask the weights too otherwise the denominator is too high
  # # pdf("Output/plot1.pdf", height = 8, width = 6)
  # # plot(x[[1]]) # MULTIPLE DIMENTION
  # # dev.off()
  # # 
  # # pdf("Output/plot2.pdf", height = 8, width = 6)
  # # plot(w) # ONE DIMENTION 
  # # dev.off()
  # 
  # w2<-mask(w, lmeRaster, updatevalue=NA) # MULTIPLE DIMENTIONS 
  # # w2<-mask(w, lmeRaster[[1]], updatevalue=NA) # ONE DIMENTION - it does not seem to matter but need to check 
  # 
  # # # CHECK 
  # # pdf("Output/plot3.pdf", height = 8, width = 6)
  # # plot(w2) 
  # # dev.off()
  # 
  # # compute weighted average - WARNING is na.rm = TRUE ok??  
  # weighted_mean_obs<-cellStats(x, sum, na.rm = TRUE) / cellStats(w2, sum, na.rm = TRUE)
  # 
  # # data frame 
  # weighted_mean_obs<-data.frame(Year = colnames(lme[,-c(1,2,3)]), weighted_mean_obs = weighted_mean_obs)
  # weighted_mean_obs$LME <- str_extract(file_name_obs,"(?<=LME_).+(?=_monthly)")# extract number after LME_ # keep this to double check the files
  # rownames(weighted_mean_obs)<-NULL
  # 
  # # fix date - should we do this and should this be done for both control and observed?  
  # weighted_mean_obs<-weighted_mean_obs %>% 
  #   mutate(Month = str_extract(Year,"[[:upper:]]+[[:lower:]]+"),
  #          Year = str_extract(Year, "\\d+"))
  # 
  # # add spin up to observed and plot to check 
  # spinup<-spinup %>% 
  #   rename(weighted_mean_obs = weighted_mean_crtl)
  # 
  # weighted_mean_obs_final<-weighted_mean_obs %>% 
  #   full_join(spinup) %>% 
  #   mutate(Year = as.numeric(Year)) %>% 
  #   arrange(Year, Month)
  # 
  # # add a date column to be able to plot by year and month - again, should we do this? 
  # weighted_mean_obs_final$Date<-paste(weighted_mean_obs_final$Month,weighted_mean_obs_final$Year, sep = "." )
  # weighted_mean_obs_final$Date<-lubridate::my(weighted_mean_obs_final$Date)
  # 
  # # CHECK
  # # try by year 
  # # weighted_mean_final_plot<-weighted_mean_obs_final %>% 
  # #   group_by(Year, LME) %>% 
  # #   summarise(weighted_mean_obs = mean(weighted_mean_obs)) %>% 
  # #   ungroup()
  # 
  # # pdf("Output/plot1.pdf", height = 4, width = 6)
  # # ggplot(weighted_mean_final_plot, aes(x = Year, y = weighted_mean_obs))+
  # #   geom_line()+
  # #   annotate("rect",xmin=1961, xmax=1980, ymin=-Inf, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)
  # # dev.off()
  # 
  # # calculate the total area of LME (save this as part of the inputs file - one file per inputs)
  # # weighted_mean_obs_final$area_m2<-sum(lme$area_m2)
  # 
  # # reorder columns 
  # weighted_mean_obs_final<-weighted_mean_obs_final[,c("LME", "area_m2","Date", "Year", "Month", "weighted_mean_obs")]
  # 
  # # rename weighed_mean column according to variable 
  # names(weighted_mean_obs_final)[6] <- variable
  
  return(list(weighted_mean_obs_final = weighted_mean_obs_final, weighted_mean_crtl_final = weighted_mean_crtl_final)) 
  
}

# 1.B calc_inputs_all_LME()
# this function extracts depth adn all climate variable files
# applies calc_inputs_LME() to each file 
# and saves a csv with depth and all climate variables as columns for one LME

calc_inputs_all_LME<-function(this_LME){
  
  # # trial 
  # this_LME = 1

  file_path_obs<-"/rd/gem/private/fishmip_inputs/ISIMIP3a/lme_inputs/obsclim/0.25deg"
  file_path_crtl<-"/rd/gem/private/fishmip_inputs/ISIMIP3a/lme_inputs/ctrlclim/0.25deg"
  
  this_LME_new<-paste0("LME_", this_LME, "_")
  
  lme_obs<-list.files(file_path_obs, pattern = this_LME_new, full.names = TRUE) 
  lme_ctrl<-list.files(file_path_crtl, pattern = this_LME_new, full.names = TRUE) 
  
  tic()
  output_obs<-list()
  output_crtl<-list()
  
  for(i in 1:length(lme_obs)){
  
    a<-calc_inputs_LME(file_name_obs = lme_obs[[i]], 
                       file_name_crtl = lme_ctrl[[i]], 
                       file_path_crtl = file_path_crtl, 
                       file_path_obs = file_path_obs)
    output_obs[[i]]<-a$weighted_mean_obs_final
    output_crtl[[i]]<-a$weighted_mean_crtl_final
  
  }
  toc() 
  
  # Method 2 based on raster: 2.8 min (no depth calcualtion)
  # Method 1 based on dplyr: 1.4 min (with depth)
  # Method 1 using read_csv(): 1.22 min but see warnings ... 

  # all inputs together for one LME 
  output_obs_all_variables<-Reduce(merge,output_obs)
  output_crtl_all_variables<-Reduce(merge,output_crtl)
  
  # write output files - temporary path - need to save on gem48!
  # this_destination_path_obs <- paste0("/data/home/camillan/dbpm/Output/", "observed_LME_", this_LME, ".csv")
  # this_destination_path_ctrl <- paste0("/data/home/camillan/dbpm/Output/", "control_LME_", this_LME, ".csv")
  this_destination_path_obs <- paste0("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/lme_inputs/obsclim/0.25deg", "observed_LME_", this_LME, ".csv")
  this_destination_path_ctrl <- paste0("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/lme_inputs/crtlclim/0.25deg", "control_LME_", this_LME, ".csv")

  fwrite(x = output_obs_all_variables, file = file.path(this_destination_path_obs))
  fwrite(x = output_crtl_all_variables, file = file.path(this_destination_path_ctrl))

  # return(output_obs_all_variables = output_obs_all_variables, output_crtl_all_variables = output_crtl_all_variables)
}

#### 2. apply the functions above to each LME -----

this_LME = seq(1:66)

tic()
for (i in 1:length(this_LME)){

  # i = 1
  message("Processing #", i, " of ", length(this_LME))
  calc_inputs_all_LME(this_LME[[i]])

}
toc() # 8405.771 - 2.3h

# # in parallel... try again maybe just need time
# 
# chunk_size <- 10 # chunk size for processing
# this_LME = seq(1:66)
# lme_obs_new <- split(this_LME, ceiling(seq_along(this_LME)/chunk_size))
# 
# tic()
# for(i in 1:length(lme_obs_new)){
#   
#   # i = 1
#   
#   file_chunk_obs <- lme_obs_new[[i]]
#   
#   message("Processing chunk #", i, " of ", length(lme_obs_new))
#   
#   mclapply(X = file_chunk_obs, FUN = calc_inputs_all_LME, mc.cores = 40)
#   
# }
# toc()

#### 3. read in printed csv file for each LME and merge info into a unique file ----- 

# newly_written_files_observed <- list.files("/data/home/camillan/dbpm/Output", pattern = "observed", full.names = TRUE)
newly_written_files_observed <- list.files("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/lme_inputs/obsclim/0.25deg", pattern = "observed", full.names = TRUE)

# pick one randomly and check 
# map(newly_written_files_observed[[8]], fread)

# combine files 
combined_LME_inputs <- rbindlist(mclapply(X = newly_written_files_observed, FUN = fread, mc.cores = 40))
# head(combined_LME_inputs)
# sort(unique(combined_LME_inputs$LME)) # WARNING LME 0 missing. 

#### 4. check calculation below in terms of sphy and sphy and adopt same variable names -----
# function below: 
# names(pp) <- c("lon", "lat", "t", "lphy", "sphy", "sbt", "sst")
# depth file: lat, lon, depth 
# the 2 files are saved directly withing the function as RData  
# the function is applied to different protocols
# for CMIP63a protocols are observed at 0.25 deg, control at 0.25 deg, and observed at 1 deg resolution. 
# however, you are only using observed at LME scale for calibration - so run only this scenario. 

# checked with Julia 1/09/2022
# sphy = phypico-vint_mol_m-2
# lphy =  phyc-vint_mol_m-2 - phypico-vint_mol_m-2 

combined_LME_inputs<-combined_LME_inputs %>% 
  mutate(sphy = `phypico-vint_mol_m-2`, 
         lphy = `phyc-vint_mol_m-2` - `phypico-vint_mol_m-2`) %>% 
  select(-c(`phyc-vint_mol_m-2`,`phypico-vint_mol_m-2`))

#### 5. add effort and catches ------

effort<-read_csv("/rd/gem/private/users/yannickr/DKRZ_EffortFiles/effort_histsoc_1841_2010.csv")

# calculate climate inputs by Year as effort is by Year 
# no - skyp as Julia would like monthly inputs
# WARNING - should this be annual mean or sum? - CHECKED with JULIA, do mean()
# WARNING - you've always used means but should this be sum? 
# CAMI THIS ARE RATES (value/m2) and what you are after is the mean annual RATE mean(value/m2) - 
# this is not annual absolute value sum(value) as you keep thinking! STOP ASKING. 
# 1 CMIPs paper comparison (I think, code by Derek - should be OK as this is considered % changes anyways) 
# + maps of % changes CMIP5 to Andrea
# for the above, if you did sum() across months instead of mean for all inputs and outputs, 
# then temperature would be problematic.
# # checks 
# a<-c(2,3,4,6,7,8,10)
# b<-c(3,4,7,8,10,11,12)
# # percentage change - it's the same 
# # ((new-original)/original)*100
# ((mean(b)-mean(a))/mean(a))*100 # 37.5
# ((sum(b)-sum(a))/sum(a))*100 # 37.5  
# # difference as used for temperature - it's NOT the same 
# mean(b)-mean(a) # 2.1 
# sum(b)-sum(a) # 15 # I guess the code was calculating means ... 
# (sum(b)-sum(a))/length(a) # 2.1  
# 2. fishing_effort/08_plotGFDLinputs + code given to Denisse and Romain (so check) 
# 3. emergentConstraints to check and change 
# 4. biodiversity work when comparing total abundance - but not used in the end
#  
# combined_LME_inputs<-combined_LME_inputs %>% 
#   group_by(LME, Year, deptho_m, area_m2) %>% 
#   summarise(`expc-bot_mol_m-2_s-1` = mean(`expc-bot_mol_m-2_s-1`),  
#             `phyc-vint_mol_m-2` = mean(`phyc-vint_mol_m-2`),
#             `phypico-vint_mol_m-2` = mean(`phypico-vint_mol_m-2`),
#             `tob_degC` = mean(`tob_degC`),
#             `tos_degC` = mean(`tos_degC`)) %>% 
#   ungroup()

DBPM_LME_climate_inputs<-combined_LME_inputs

# add LME total area and calculate effort/m2  
LME_area<-combined_LME_inputs %>% 
  select(LME, area_m2, deptho_m) %>% # this is LME total area and weighted mean depth
  unique()

# calculate sum of effort by LME/by total area of LME 
DBPM_LME_effort_input<-effort %>% 
  group_by(Year, LME) %>% 
  summarize(NomActive = sum(NomActive)) %>% 
  ungroup() %>% 
  full_join(LME_area) %>% 
  mutate(NomActive_area_m2 = NomActive/area_m2)

# do the same with catches 
catch<-read_csv("/rd/gem/private/users/yannickr/DKRZ_EffortFiles/calibration_catch_histsoc_1850_2004.csv")

DBPM_LME_catch_input<-catch %>% 
  mutate(catch_tonnes = Reported+IUU) %>% 
  group_by(Year, LME) %>% 
  summarize(catch_tonnes = sum(catch_tonnes)) %>% # catch is in tonnes, checked in FishingEffort Rproject, 
  # also Reg advise to exclude dischards 
  ungroup() %>% 
  full_join(LME_area) %>% 
  mutate(catch_tonnes_area_m2 = catch_tonnes/area_m2)

DBPM_LME_effort_catch_input<-DBPM_LME_effort_input %>% 
  full_join(DBPM_LME_catch_input)

head(DBPM_LME_effort_catch_input)

#### 6. Plot to check ----

# WARNING - keep going with the checks

my_theme<-theme_bw()+
  theme(text = element_text(size = 10), # this should be overwritten by the below
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.title=element_text(size = 9), 
        legend.text=element_text(size = 8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.key.size = unit(0.1, "cm")) 


plot_df<-split(DBPM_LME_effort_catch_input, DBPM_LME_effort_catch_input$LME)

head(plot_df[[2]])

Value = "NomActive"
toKeep = "1"

plot<-ggplot(data = plot_df[[2]], aes(x = Year, y = NomActive)) +
  ggtitle(paste("LME", toKeep, sep = " "))+
  annotate("rect",xmin=1841, xmax=1960, ymin=0, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)+ # spin-up edf8fb
  annotate("rect",xmin=1961, xmax=2010, ymin=0, ymax=Inf, fill = "#238b45", alpha = 0.4)+ # projection 66c2a4
  geom_point(size=1)+
  geom_line() +
  my_theme

pdf("Output/plot1.pdf")
plot
dev.off()

#### 7. go to step 2 - batch create inputs ----
# in batch create input, the code uses getgridin_ISIMIP3b to calculate intercept and slope 
# the below are the steps you need from that function (checked with Julia - line 14 to 22)
source("input_funcs.R")

# name columns as in code
# head(DBPM_LME_climate_inputs)
DBPM_LME_climate_inputs_renamed<-DBPM_LME_climate_inputs %>% 
  select(LME, Date, deptho_m, area_m2, `expc-bot_mol_m-2_s-1`, tob_degC, tos_degC, sphy, lphy) %>% 
  rename(t = Date, depth = deptho_m, sbt = tob_degC, sst = tos_degC, expcbot = `expc-bot_mol_m-2_s-1`)

DBPM_LME_climate_inputs_renamed<-DBPM_LME_climate_inputs_renamed[,c("LME", "t", "lphy", "sphy", "sbt", "sst", "depth", "area_m2", "expcbot")]

# dplyr method
DBPM_LME_climate_inputs_slope<-DBPM_LME_climate_inputs_renamed %>%
  mutate(er = getExportRatio(sphy,lphy,sst,depth),
         er = ifelse(er<0,0, ifelse(er>1,1,er)),
         intercept = GetPPIntSlope(sphy,lphy,mmin=10^-14.25,mmid=10^-10.184,mmax=10^-5.25,depth,output="intercept"),
         slope = GetPPIntSlope(sphy,lphy,mmin=10^-14.25,mmid=10^-10.184,mmax=10^-5.25,depth,output="slope")) %>% 
  relocate("LME","t","sst","sbt","er","intercept","slope", "sphy", "lphy", "depth", "area_m2","expcbot")

head(DBPM_LME_climate_inputs_slope)

# # tests 
# getExportRatio<-function(sphy,lphy,sst,depth){
#   # trial 
#   sphy = 0.05354391
#   lphy = 0.006422868
#   sst = 23.153202
#   depth = 4123.2588
#     
#   
#   ptotal=sphy+lphy
#   plarge <- lphy/ptotal
#   psmall <- sphy/ptotal
#   er <- (exp(-0.032*sst)*((0.14*psmall)+(0.74*(plarge)))+(0.0228*(plarge)*(depth*0.004)))/(1+(depth*0.004))
#   return(er)
# }

# mapply method bease on gridcell inputs and matrix format (as per )
# # example mapply
# set.seed(10)
# 
# n=10
# x<-rnorm(n)
# e<-rnorm(n,0,2)
# y=0.5+2*x+e
# 
# df<-as.data.frame(cbind(y,x))
# 
# sf<-function(x,y){
#   sc = x*y
#   return(sc)
# }
# 
# df[,"sc"]<-mapply(sf,x = df[,"x"],y = df[,"y"])
# df[,"check"]<-df[,"x"]*df[,"y"]

#
# gridinputs<-DBPM_LME_climate_inputs_renamed
# 
# gridinputs[,"er"] <- mapply(getExportRatio,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],sst=gridinputs[,"sst"],depth=gridinputs[,"depth"])
# gridinputs[which(gridinputs[,"er"]<0),"er"]<-0
# gridinputs[which(gridinputs[,"er"]>1),"er"]<-1
# 
# gridinputs[,"intercept"]<-mapply(GetPPIntSlope,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=gridinputs[,"depth"],output="intercept")
# gridinputs[,"slope"]<-mapply(GetPPIntSlope,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=gridinputs[,"depth"],output="slope")
# 
# gridinputs <- gridinputs[,c("LME","t","sst","sbt","er","intercept","slope", "sphy", "lphy", "depth", "area_m2","expcbot")]
# 
# head(gridinputs)
# 
# DBPM_LME_climate_inputs_slope<-gridinputs

#### 8. Save results ---- 

# # WARNING - change location of the temporary and this final files to Gem48
# fwrite(x = DBPM_LME_climate_inputs_slope, file.path("Output/", "DBPM_LME_climate_inputs_slope.csv"))
# fwrite(x = DBPM_LME_effort_catch_input, file.path("Output/", "DBPM_LME_effort_catch_input.csv"))

fwrite(x = DBPM_LME_climate_inputs_slope, file.path("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/lme_inputs/obsclim/0.25deg/", "DBPM_LME_climate_inputs_slope.csv"))
fwrite(x = DBPM_LME_effort_catch_input, file.path("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/lme_inputs/obsclim/0.25deg/", "DBPM_LME_effort_catch_input.csv"))

##### END CN at LME scale -----


# get gridded GCM inputs for ISIMIP3b phase 1 protocol - from GFDL-ESM4, IPSL-CM6A-LR
# monthly time steps, so need to set up time-varying plankton input into code (as in Q_F, start with climatology, then apply dynamical forcing) 
# gridded values 1 by 1 degree lat/lon
# use parallel to do runs for a bunch of grids cell at the same time

# ------------------------------------------------------ 
# What inputs are needed from GCMS?
# Depends on method used to get the plankton size spectrum:

#  use Woodworth-Jefcoats 2013 GCB paper method:
# get small and large phytoplankton densities (if diazotroph density provided, add to large phyto),
# to get slope and intercept, also get median size of consumer and minimum size of phytoplankton everything else same as above

# --------------------------------------------------------

### CN prepare input files at for CMIP63a ----
# download controlclim files from DKRZ on 02/09/2022
# download all file in this directory to ctrlclim 
# scp -r b381217@levante.dkrz.de:/work/bb0820/ISIMIP/ISIMIP3a/SecondaryInputData/climate/ocean/ctrlclim/global/monthly/historical/GFDL-MOM6-COBALT2/* ./
# move one degree files to 1deg folder (from inside 0.25deg folder): mv *onedeg* ../1deg
# obsclim file dowloaded earlier (no record kept) - obsclim are also in SecondaryInputData folder (but from CESM2 not GFDL)
# move all files into a subfolder: mv * subfolder (in case you need) 

# Depth files download on 03/09/2022 
# ctrlclim: /work/bb0820/ISIMIP/ISIMIP3a/SecondaryInputData/climate/ocean/ctrlclim/global/fixed/historical/GFDL-MOM6-COBALT2
# files: gfdl-mom6-cobalt2_ctrlclim_deptho_15arcmin_global_fixed.nc; gfdl-mom6-cobalt2_ctrlclim_deptho_onedeg_global_fixed.nc
# obsclim: /work/bb0820/ISIMIP/ISIMIP3a/InputData/climate/ocean/obsclim/global/fixed/historical/GFDL-MOM6-COBALT2
# files: gfdl-mom6-cobalt2_obsclim_deptho_15arcmin_global_fixed.nc; gfdl-mom6-cobalt2_obsclim_deptho_onedeg_global_fixed.nc

# RECAP on files 
# obsclim 0.25deg = 39 files 
# obsclim 1deg = 39 files (78 in all)
# obsclime DKRZ = 76 files 

# crtlclim all folder = 74 files (2 of which are obsclim)
# crtlclim DKRZ = 74 files (2 of which are obsclim)

#### code starting ----

rm(list=ls())

#install.packages('RNetCDF', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
#install.packages('reshape2', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
#install.packages('abind', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')

# # CN 
# install.packages("RNetCDF")
# install.packages("plyr")
# install.packages("reshape2")

library(RNetCDF) #library(RNetCDF, lib.loc = '/home/rhenegha/R_package_library') # For reading/manipulating netCDFs
library(reshape2) #library(reshape2, lib.loc = '/home/rhenegha/R_package_library')
library(abind) #library(abind, lib.loc = '/home/rhenegha/R_package_library')
library(tictoc)

# setwd('/Users/ryanheneghan 1/Desktop/Papers/FishMIP_CMIP6/')

### FishMIP Phase 1 protocols for ISIMIP3b,
# 1. picontrol, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')
# 2. historical, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')
# 3. ssp126, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')
# 4. ssp585, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')

#### CN ISMIP63a adapted function -----

getGCM<-function(gcmPath = './inputs/', protocol, gcm = 'IPSL-CM6A-LR', savepath, getdepth = T, vers = 2){
  
  # # CN trial 
  # gcmPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/"
  # protocol = "0.25deg"
  # gcm = "obsclim"
  # savepath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/"
  # getdepth = T
  # vers = 3 # see meaning below 
  
  # getGCM(gcmPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/", 
  #        protocol = "0.25deg", 
  #        gcm = "obsclim", 
  #        savepath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
  #        getdepth = T, 
  #        vers = 3)
  
  
  # phydiat_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*phydiat-vint*', full.names = TRUE)
  # phyc_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*phyc-vint*', full.names = TRUE)
  # to_zb_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*thetao-bot*', full.names = TRUE)
  # to_zs_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*_tos_*', full.names = TRUE)
  
  # CN
  phypico_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*phypico-vint*', full.names = TRUE)
  phyc_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*phyc-vint*', full.names = TRUE)
  to_zb_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*_tob_*', full.names = TRUE)
  to_zs_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*_tos_*', full.names = TRUE)
  
  # get large phy
  
  # lphy <- var.get.nc(open.nc(phydiat_file), 'phydiat-vint')
  
  # CN 
  # checked with Julia 1/09/2022
  # sphy = phypico-vint_mol_m-2
  # lphy =  phyc-vint_mol_m-2 - phypico-vint_mol_m-2 
  
  lphy <- var.get.nc(open.nc(phyc_file), 'phyc-vint') - var.get.nc(open.nc(phypico_file), 'phypico-vint')
  
  # t <- var.get.nc(open.nc(phydiat_file), 'time')
  # lon <- var.get.nc(open.nc(phydiat_file), 'lon')
  # lat <- var.get.nc(open.nc(phydiat_file), 'lat')
  
  # CN 
  t <- var.get.nc(open.nc(phypico_file), 'time')
  lon <- var.get.nc(open.nc(phypico_file), 'lon')
  lat <- var.get.nc(open.nc(phypico_file), 'lat')
  
  # Format lphy
  dimnames(lphy) <- list(lon=lon,lat=lat,t=t)
  pp <- melt(lphy)
  names(pp) <-  c("lon","lat","t","lphy")
  pp <- pp[!is.na(pp[,"lphy"]),]
  rm(list = ('lphy'))
  
  # sphy
  # sphy <- var.get.nc(open.nc(phyc_file), 'phyc-vint')- var.get.nc(open.nc(phydiat_file), 'phydiat-vint')
  # CN - see above checked with Julia
  sphy <- var.get.nc(open.nc(phypico_file), 'phypico-vint')
  
  sphy <- as.vector(sphy)
  sphy <- sphy[!is.na(sphy)]
  pp$sphy <- sphy
  rm(list = ('sphy'))
  
  # bottom temperature
  to_zb <- var.get.nc(open.nc(to_zb_file), 'tob')
  
  to_zb <- as.vector(to_zb)
  to_zb <- to_zb[!is.na(to_zb)]
  pp$sbt <- to_zb
  rm(list = ('to_zb'))
  
  # Surface temperature
  to_zs <- var.get.nc(open.nc(to_zs_file), 'tos')

  to_zs <- as.vector(to_zs)
  to_zs <- to_zs[!is.na(to_zs)]
  pp$sst <- to_zs
  rm(list = ('to_zs'))
  
  # Standardise colnames
  names(pp) <- c("lon", "lat", "t", "lphy", "sphy", "sbt", "sst")

  # CN - explore
  # head(pp)
  
  if(getdepth == T){
  # get depth
  # depth_nc <- open.nc(list.files(path = paste(gcmPath, gcm, '/', sep = ''), pattern = 'depth', full.names = TRUE))
  # CN - NOTE depth files are now in the same folder as input (one depth file for gcm(e.g. obsclim)/protocol(e.g. 0.25deg) combination)
  depth_nc <- open.nc(list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = 'deptho', full.names = TRUE))
  depth <- var.get.nc(depth_nc, 'deptho') # Depth in metres
  dimnames(depth) <- list(lon=var.get.nc(depth_nc, 'lon'), lat=var.get.nc(depth_nc, 'lat'))
  depth <- melt(depth)
  names(depth) <- c("lon", "lat", "depth")
  depth$gridnum <- 1:length(depth[,1])
  # Remove land values (na and 0 depth)
  depth <- depth[!is.na(depth[,"depth"]),]
  depth <- depth[depth[,'depth'] != 0,]
  
  ## Save depth
  depth_save_name <- paste(savepath, gcm, '/', protocol, '/', gcm, "_", protocol, "_depth.RData", sep = '')
  save(depth, file = depth_save_name, version = vers)
  }
  
  ## Save processed forcings
  print(paste('Now saving forcings for', gcm, protocol, sep = ' '))
  pp_save_name <- paste(savepath, gcm, '/', protocol, '/', gcm, "_", protocol, ".RData", sep = '')
  save(pp, file = pp_save_name, version = vers) # version = the workspace format version to use. 
  # NULL specifies the current default format (3). 
  # Version 1 was the default from R 0.99.0 to R 1.3.1 and 
  # version 2 from R 1.4.0 to 3.5.0. 
  # Version 3 is supported from R 3.5.0.
  # "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/obsclim/0.25deg/obsclim_0.25deg.RData"
  
  #remove any objects no longer needed 
  if(getdepth == T){
  rm(pp, depth)
  }else{rm(pp)}
}

#### apply getGCM() to all combo of protocols ----

# getGCM(gcmPath = './inputs/', protocol = 'picontrol', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = T, vers = 3)
# getGCM(gcmPath = './inputs/', protocol = 'historical', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
# getGCM(gcmPath = './inputs/', protocol = 'ssp126', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
# getGCM(gcmPath = './inputs/', protocol = 'ssp585', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
# 
# getGCM(gcmPath = './inputs/', protocol = 'picontrol', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = T, vers = 3)
# getGCM(gcmPath = './inputs/', protocol = 'historical', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
# getGCM(gcmPath = './inputs/', protocol = 'ssp126', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
# getGCM(gcmPath = './inputs/', protocol = 'ssp585', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)

# CN 
tic()
getGCM(gcmPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/", protocol = "0.25deg", gcm = "obsclim", savepath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", getdepth = T, vers = 3)
toc() # 8.533967 min 
getGCM(gcmPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/", protocol = "0.25deg", gcm = "ctrlclim", savepath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", getdepth = T, vers = 3)

getGCM(gcmPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/", protocol = "1deg", gcm = "obsclim", savepath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", getdepth = T, vers = 3)
getGCM(gcmPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/", protocol = "1deg", gcm = "ctrlclim", savepath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", getdepth = T, vers = 3)

#### CN Calculate spin-up for CMIP63a climate forcings ----
## NO - go to batch_run_create_inputs_ISIMIP3b and getgridin_ISIMIP3b.R 

# rm(list=ls())
# 
# library(tidyverse)
# library(tictoc)
# library(data.table)
# library(parallel)
# library(dtplyr)
# 
# # # calc spinup for ctrlclim 
# # load("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/ctrlclim/1deg/ctrlclim_1deg.RData")
# # ctrlclim<-pp
# # 
# # load("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/ctrlclim/1deg/ctrlclim_1deg_depth.RData")
# # gridnum_depth<-select(depth, - depth)
# # nrow(gridnum_depth) # 41934 lat/lon cell
# # 
# # load("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/obsclim/1deg/obsclim_1deg.RData")
# # obsclim<-pp
# 
# # assume they are the same... 
# # load("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/obsclim/1deg/obsclim_1deg_depth.RData")
# # gridnum_depth<-select(depth, - depth)
# # nrow(gridnum_depth) # 41934 lat/lon cell
# 
# calc_input_spinup_gridcell<-function(inputPath, protocol, subset){
#   
#   # # # trial
#   # inputPath<-"/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/"
#   # protocol = "1deg"
# 
#   input_file_crtlclim <- file.path(paste0(inputPath, "ctrlclim", '/', protocol), paste0("ctrlclim", '_', protocol, ".RData"))
#   load(input_file_crtlclim)
#   ctrlclim<-lazy_dt(pp)
#   # ctrlclim<-pp
#   rm(input_file_crtlclim, pp)
#   
#   input_file_obsclim <- file.path(paste0(inputPath, "obsclim", '/', protocol), paste0("obsclim", '_', protocol, ".RData"))
#   load(input_file_obsclim)
#   obsclim<-lazy_dt(pp)
#   # obsclim<-pp
#   rm(input_file_obsclim, pp)
#   
#   
#   gridnum_depth <- file.path(paste0(inputPath, "obsclim", '/', protocol), paste0("obsclim", '_', protocol,"_depth", ".RData"))
#   load(gridnum_depth)
#   gridnum_depth<-lazy_dt(depth) %>% select(- depth)
# 
#   # calculate timesteps, as t is confusing (inputs: monthly_1961_2010.nc) 
#   # and add gridnumber from depth file 
#   Date<-seq(as.Date("1961-01-01"), as.Date("2010-12-01"), by="month")
#   t<-ctrlclim %>% select(t) %>% unique()
#   time<-data.frame(t = t, Date = Date)
#   time<-lazy_dt(time)
#   rm(Date,t)
#   
#   # CONTROL 
#   tic()
#   ctrlclim<-ctrlclim %>% 
#     full_join(time) %>%  
#     full_join(gridnum_depth) %>% 
#     arrange(Date, gridnum)
#   toc() # 21 sec # 5 with lazy
# 
#   # calculate spin-up
#   tic()
#   spinup<-ctrlclim %>%
#     filter(Date >= "1961-01-01", Date <="1980-12-01") %>%
#     slice(rep(1:n(), times = 6)) #%>% 
#     # arrange(Date, gridnum)
#   toc() # 13 sec # 0 with lazy
# 
#   # calc new date and t for spin-up 
#   Date = seq(as.Date("1841-01-01"), as.Date("1960-12-01"), by="months")
#   new_t<-seq((720-length(Date)),720-1) # WARNING - need to fix
#   # new_t<-seq((t[1]-length(Date)),t[1]-1)
#   Date_df<-data.frame(Date = Date, t = new_t)
#   Date_df<-lazy_dt(Date_df)
#   rm(Date, new_t)
#   # head(Date_df)
#   
#   tic()
#   new_date<-gridnum_depth %>% 
#     full_join(Date_df, by = character()) %>% 
#     arrange(Date, gridnum)
#   toc() # 30 sec # 0 with lazy
#   # rm(gridnum_depth)
#   
#   # replace new date to spin-up file 
#   tic()
#   spinup<-spinup %>% 
#     select(-Date, -t) 
#   toc()
#   
#   # trying to figure out problem 
#   new_date<-as.data.frame(new_date)
#   Date_new<-new_date$Date
#   t_new<-new_date$t
#   rm(new_date)
#   # length(Date_new) # 60384960
#   # length(t_new) # 60384960
#   
#   tic()
#   spinup<-spinup %>% 
#     mutate(Date = Date_new, 
#            t = t_new) 
#   toc()# 0.01
#   # rm(Date_new, t_new)
#   
#   # add spinup to ctrlclim  
#   tic()
#   ctrlclim<-ctrlclim %>% 
#     full_join(spinup) %>% 
#     arrange(Date, gridnum)
#   toc() # 20 sec # 1 with lazy 
#   # rm(spinup)
#   
#   tic()
#   ctrlclim<-as_tibble(ctrlclim)
#   toc() # 593.884 # 10 min 
# 
#   # print final input_file_crtlclim file and remove from environment 
#   crtlclim_withSpinUP <- file.path(paste0(inputPath, "ctrlclim", '/', protocol), paste0("ctrlclim", '_', protocol,"_withSpinUp", ".RData"))
#   save(ctrlclim, file = crtlclim_withSpinUP, version = 3)
#   # fwrite(x = ctrlclim, file = crtlclim_withSpinUP)
#   rm(ctrlclim)
#   
#   # OBSERVED 
#   # add spinup to obsclim file 
#   tic()
#   obsclim<-obsclim %>% 
#     full_join(time) %>%  
#     full_join(gridnum_depth) %>% 
#     arrange(Date, gridnum)
#   toc() # 21 sec
#   
#   # add spinup to crtlclim
#   tic()
#   obsclim<-obsclim %>% 
#     full_join(spinup)
#   toc() # 20 sec 
#   
#   # arrange file as per original
#   tic()
#   obsclim<-obsclim %>% 
#     arrange(Date, gridnum)
#   toc() # 24 sec
#   
#   # # Plot one gridcell to check - seems OK
#   # trial<-obsclim %>%
#   #   filter(gridnum == 1) %>%
#   #   mutate(Year = format(as.Date(Date), "%Y"))  %>%
#   #   group_by(Year) %>%
#   #   summarise(lphy = mean(lphy)) %>%
#   #   ungroup() %>%
#   #   mutate(Year = as.numeric(Year))
#   # 
#   # plot<-ggplot(trial, aes(x = Year, y = lphy))+
#   #   geom_point()+
#   #   geom_line()+
#   #   annotate("rect", xmin=1961, xmax=1980, ymin=-Inf, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)
#   # 
#   # pdf("Output/plot1_lazy.pdf", height = 4, width = 6)
#   # plot
#   # dev.off()
#   
#   tic()
#   obsclim<-as_tibble(obsclim)
#   toc() # 593.884 # 10 min 
#   
#   # print final input_file_crtlclim file and remove from environment 
#   obsclim_withSpinUP <- file.path(paste0(inputPath, "obsclim", '/', protocol), paste0("obsclim", '_', protocol,"_withSpinUp", ".RData"))
#   save(obsclim, file = obsclim_withSpinUP, version = 3)
#   rm(obsclim, spinup, time) # ANYTHING ELSE? 
# 
# }
# 
# tic()
# calc_input_spinup_gridcell(inputPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", protocol = "1deg", subset = NA)
# toc() # 235.699 # 4 min
# 
# # tic() # FILE TOO LARGE
# # calc_input_spinup_gridcell(inputPath = "/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", protocol = "0.25deg")
# # toc() # ERRROR Error: cannot allocate vector of size 10.7 Gb

# # OPTION 2 at gridcell level ...
# 
# # get gridnumber from depth file 
# load("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/ctrlclim/1deg/ctrlclim_1deg_depth.RData")
# gridnum_depth<-select(depth, - depth)
# 
# # # how many grid cell?
# # ncell<-ctrlclim %>% filter(t == 720) # consider one time-step
# # nrow(ncell) # 670589 0.25deg deg # 41934 1deg - 16 times more (4*4)
# # nrow(gridnum_depth)
# 
# # prepare data - get gridcell ID 
# tic()
# ctrlclim<-ctrlclim %>% 
#   full_join(gridnum_depth)
# toc() # 19 sec  
# 
# this_cell<-gridnum_depth$gridnum
# 
# # calculate Date and t of spinup 
# tic()
# trial<-ctrlclim %>% filter(gridnum == this_cell[1])
# toc()# 1 sec
# 
# # inputs: monthly_1961_2010.nc (this shouod be done once)
# Date<-seq(as.Date("1961-01-01"), as.Date("2010-12-01"), by="month")
# t<-unique(trial$t)
# time<-data.frame(t = t, Date = Date)
# 
# # define function that caclautes spin up for each grid cell 
# calc_spinup_gridcell<-function(this_cell){
#   
#   # # trial  
#   this_cell_new = this_cell
#   # protocol = "1deg"
#   
#   trial<-ctrlclim %>% filter(gridnum == this_cell_new)
#   
#   trial<-trial %>% 
#     full_join(time) %>%
#     mutate(Year = format(as.Date(Date), "%Y"),  
#            Month = format(as.Date(Date), "%m"))
#   
#   trial_spinup<-trial %>% 
#     filter(Year >= 1961, Year <=1980) %>%
#     slice(rep(1:n(), times = 6)) %>% 
#     mutate(Year = as.character(rep(1841:1960, each = 12)),
#            Date = lubridate::my(paste(Month,Year, sep = "." ))) %>% 
#     select(-t)
#   
#   # calcualte t 
#   # runs : 50 year 
#   # 50*12 = 600 months 
#   # spinup : 120 years 
#   # 120*12 = 1440 months 
#   # t1 should be 720 - 1440 = -720
#   
#   Date_spinup<-unique(trial_spinup$Date)
#   t_spinup<-seq((t[1]-length(Date_spinup)),t[1]-1)
#   
#   time_spinup<-data.frame(t = t_spinup, Date = Date_spinup)
#   trial_spinup<-trial_spinup %>% 
#     full_join(time_spinup)
#   
#   # print spin up for each grid cell. 
#   this_destination_path <- paste0("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
#                                   "ctrlclim/", 
#                                   protocol, 
#                                   "/TempSpinup/",
#                                   "gridnum_", 
#                                   this_cell, 
#                                   ".csv")
#   
#   fwrite(x = trial_spinup, file = file.path(this_destination_path))
#   
# }
# 
# # try function 
# tic()
# calc_spinup_gridcell(this_cell = this_cell[1], time, protocol = "1deg")
# toc()

# tic() # 9 min = 150 files. 30 min for 500 files (1 chunk below). 46 h for all files - NOT POSSIBLE 
# for(i in 1:length(this_cell)){
#   
#   # i = 1
#   message("Processing #", i, " of ", length(this_cell))
#   calc_spinup_gridcell(this_cell[[i]])
#   
# }
# toc()

# # prepare for loop in //
# # 30 min - not even 500 1st chunk ... 
# chunk_size <- 500 # chunk size for processing
# cell_chunk <- split(this_cell, ceiling(seq_along(this_cell)/chunk_size))
# length(cell_chunk) # 84 for deg1 
# 
# # trial 
# cell_chunk<-cell_chunk[1] # it shiould take less than 30 min 
# 
# protocol = "1deg"
# 
# tic() # strt 9:30 - it should take much less than 30 min ... 
# for(i in 1:length(cell_chunk)){
# 
#   # i = 1
#   file_chunk <- cell_chunk[[i]]
#   message("Processing chunk #", i, " of ", length(cell_chunk))
#   mclapply(X = file_chunk, FUN = calc_spinup_gridcell, mc.cores = 40)
# 
# }
# toc()

#-------------------------------------STEP 2: DISAGGREGATE TIME SERIES INPUTS FOR MODEL TO WEEKLY (OR DAILY) TIME STEPS


# CN Step 2 is now part of batch_run_create_inputs_ISIMIP3b.R. Ste p 1 above could also be moved to this file 

# rm(list=ls())
# #install.packages(pkgs = 'http://zoo.r-forge.r-project.org/', lib = './R_package_library')
# library(zoo)
# library(parallel)
# setwd('/Users/ryanheneghan 1/Desktop/Papers/FishMIP_CMIP6/DBPM')
# 
# esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
# scens <- c("picontrol", "historical", "ssp126", "ssp585")
# num_cells <- c(44564, 41328)
# 
# for(i in 1:length(esms)){
#   curr_esm <- esms[i]
#   for(j in 1:length(scens)){
#     curr_scen <- scens[j]
#     load(file=paste("./processed_forcings/", curr_esm, "/", curr_scen, "/", curr_esm, "_", curr_scen, ".RData", sep ="")) # load forcings
#     load(file=paste("./processed_forcings/", curr_esm, "/",  curr_esm, "_depth.RData", sep ="")) # load depth data
#     
#     source("./dbpm_CMIP6/getgridin.R")
#     
#     # set up cluster 
#     numcores= detectCores()-2
#     
#     # cl <- makeCluster(numcores,type="FORK",outfile='')
#     cl <- makeForkCluster(getOption("cl.cores", numcores))
#     
#     # grids to read in are sequential for the depth file
#     grids<-1:num_cells[i]
#     
#     # Running the model
#     ptm=proc.time()
#     options(warn=-1)
#     
#     clusterApply(cl,x=grids,fun=getgridin,curr_esm = curr_esm, curr_scen = curr_scen)
#     
#     print((proc.time()-ptm)/60.0)
#     
#     stopCluster(cl)
#     
#   }
#   
# }




