
# CN this is an updated file from Ryan to run the model in parallel.
# it calls runmodel_yearly.R (or runmodel.R, depending on whether you need to run yearly or monthly). NOTE: You might need to load these files from Ryan's folder too. 
# in Julia's older version, this codes is included in runmodel_calls.r but it's very different!

#### STEP 3: RUN THE MODEL
# Set to the base folder for the DBPM runs
setwd("/data/home/camillan/dbpm")

rm(list=ls())

# install.packages('zoo', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
# library(zoo, lib.loc = '/home/rhenegha/R_package_library')
library(zoo)
library(parallel)

esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
scenario <- c("picontrol", "historical", "ssp126", "ssp585")

source('runmodel_yearly.R') 

# igrid <-1
# readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/GFDL-ESM4/historical/grid_1_GFDL-ESM4_historical.rds") # try reading file 
# gcm = curr_esm 
# protocol = curr_scen
# output = "partial"
# input_files_location = input_loc 
# output_files_location = output_loc

### protocols requiring spin up ----

for(i in 1:length(esms)){ # Loop over esms
  
  i = 1
  curr_esm <- esms[i]
  
  load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  
  for(j in 1:(length(scenario)-2)){ # Loop over scenario
  
    # historical saved weekly outputs + spin up = 4.9T, 4 days to run (but on 25 cores)
    # picontrol saved montly outputs = 340G, 2.5 days to run on 45 cores  
  
    # new runs without temp effect on senescenace:
    # historical: montly outputs 2.2 days; need to run separately as you are also saving growth - and picontrol is not necessary at this stage. 
    # note that you can either use the dynamics_sizebased_model_function.R or the dynamics_sizebased_model_function_TempOnSenescence.R in runmodel_yearly.R  

    # new runs without temp effect on senescenace and other mortalities for detritus. 
    # historical IPSL: montly outputs saving growth also, ~ 2.2 days
    # historical + picontrol GFDL: montly outputs saving growth also, ~ 7 not sure why so long (picontrol has a time dimention of 3012, hisitircal of 1980). 
    
    curr_scen <- scenario[j]
    
    input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "")
    output_loc <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "") 
    
    # set up cluster
    numcores= 45 # gem48 has 48 cpu 
    
    cl <- makeForkCluster(getOption("cl.cores", numcores))
    
    # grids to read in are sequential for the depth file
    grids<-1:dim(depth)[1]

    ptm=proc.time()
    options(warn=-1)
    
    parallel::clusterApply(cl,x=grids,fun=rungridsep, gcm = curr_esm, protocol = curr_scen, output = "partial",  
                           input_files_location = input_loc, output_files_location = output_loc)
    
    print((proc.time()-ptm)/60.0)
    
    stopCluster(cl)
  }

}
    
### projections protocols ssp ----
    
 for(i in 1:length(esms)){ # Loop over esms
  
  i = 1  
  curr_esm <- esms[i]
  
  load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  
  for(j in 3:length(scenario)){ # Loop over scenario
    
    # ssp126 saved weekly outputs starting from last historical week = 13 h to run; 117G  
    # ssp585 saved weekly outputs starting from last historical week = 12 h to run; 117G 
    
    # runs without temperature effect on senescence - see above. both ssp/s
    # saved weekly outputs starting from last historical week = 25 h to run together
    
    # run without temp effect on senescence and on detritus other mortalities 
    # strange error with GFDL: I rerun teh code and only 2 grid cells are now not run , but the error still remains. If run one by one grid cell, it all seems OK
    # Error in checkForRemoteErrors(val) : 
    #   15 nodes produced errors; first error: invalid 'description' argument
    j = 4 
    
    curr_scen <- scenario[j]
    
    input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen, sep = "")
    output_loc <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, "/", curr_scen, sep = "") 
    output_loc_hist <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, '/historical', sep = "")
    input_loc_hist <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/historical', sep = "")
    
    # set up cluster
    numcores= 45 # 48 cpu 
    
    cl <- makeForkCluster(getOption("cl.cores", numcores))
    
    # grids to read in are sequential for the depth file
    grids<-1:dim(depth)[1]
    # not for second run 
    # grids<-grids[grids!=21747] # this is the only grid from the historical run with greater size dimentions, meaning that result_set$notrun == TRUE
    # see why this grid could be problematic below 'check time dimention of outputs in historical'
    
    ptm=proc.time()
    options(warn=-1)
    
    parallel::clusterApply(cl,x=grids,fun=rungridsep_ssp, gcm = curr_esm, protocol = curr_scen, output = "partial",  
                           input_files_location = input_loc, output_files_location = output_loc, 
                           input_historical_location = input_loc_hist, output_historical_location = output_loc_hist)
    
    print((proc.time()-ptm)/60.0)
  
    stopCluster(cl)
   }
}

### explore effect of senescence and fix 'bug' which increases biomass ----

rm(list=ls())
    
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)

# the function below 
    # uploaads outputs from 1 (specified) grid cell,
    # runs the model for the same grid cell using a different dynamic_sizebased_model_functions.R 
    # saves the new run in output_loc_trial (rd/gem/private/fishmip_outputs/temp_trials/) which is different from the path to the original runs, 
    # so you are not overwriting original files. 
    # plots biomass trends, size-spcrea and growth for the 2 (original ad new) runs 
# Steps 
    # 1 - delete files in temp_trials otherwise the function sees taht the grid cell has already been run 
    # 2 - specify the dynamic_sizebased_model_functions.R that you want ot use and commnet the dynamic_sizebased_model_functions.R inside runmodel_yearly.r
    
compare_senarios<-function(grids){
    
  # trial 
  # grids = 22430
  
  plot_grid<-function(result_set_h, 
                    result_set_585,
                    result_set_126, 
                    input_h,
                    input_585, 
                    input_126){
  
  # pelagic for all scenarios
  U_h<-as_data_frame(result_set_h$U) %>% 
    mutate(bin =result_set_h$x, 
           period = "hist") %>% 
    `colnames<-`(c(seq(1, ncol(result_set_h$U)),"bin", "period")) %>% 
    gather(step, bioU, -bin, -period)
  
  U_585<-as_data_frame(result_set_585$U) %>% 
    mutate(bin =result_set_585$x, 
           period = "585")%>% 
    `colnames<-`(c(seq(ncol(result_set_h$U)+1 ,(ncol(result_set_585$U) + ncol(result_set_h$U))),"bin", "period")) %>% 
    gather(step, bioU, -bin, -period)
  
  U_126<-as_data_frame(result_set_126$U) %>% 
    mutate(bin =result_set_126$x, 
           period = "126")%>% 
    `colnames<-`(c(seq(ncol(result_set_h$U)+1 ,(ncol(result_set_126$U) + ncol(result_set_h$U))),"bin", "period")) %>% 
    gather(step, bioU, -bin, -period)
  
  U<-U_h %>% 
    full_join(U_126) %>% 
    full_join(U_585)
  
  # demersal for all scenarios
  V_h<-as_data_frame(result_set_h$V) %>% 
    mutate(bin =result_set_h$x, 
           period = "hist") %>% 
    `colnames<-`(c(seq(1, ncol(result_set_h$V)),"bin", "period")) %>% 
    gather(step, bioV, -bin, -period)
  
  V_585<-as_data_frame(result_set_585$V) %>% 
    mutate(bin =result_set_585$x, 
           period = "585")%>% 
    `colnames<-`(c(seq(ncol(result_set_h$V)+1 ,(ncol(result_set_585$V) + ncol(result_set_h$V))),"bin", "period")) %>% 
    gather(step, bioV, -bin, -period)
  
  V_126<-as_data_frame(result_set_126$V) %>% 
    mutate(bin =result_set_126$x, 
           period = "126")%>% 
    `colnames<-`(c(seq(ncol(result_set_h$V)+1 ,(ncol(result_set_126$V) + ncol(result_set_h$V))),"bin", "period")) %>% 
    gather(step, bioV, -bin, -period)
  
  V<-V_h %>% 
    full_join(V_126) %>% 
    full_join(V_585)
  
  # all Biomass 
  biomass<-U %>% full_join(V)
  biomass<-biomass %>% 
    gather(trait, bio, -c(bin, step, period) )
  
  # spectrum 
  plot_spectrum<-ggplot(filter(biomass, bio>0, step == max(as.numeric(biomass$step))-1), 
                        aes(x=bin, y=log10(bio), group = trait, color = trait))+
    geom_line()+
    facet_wrap(~period)
  
  # calcualte tcb as per netcdf outputs 
  biomass_time<-biomass %>% 
    filter(bin>=-7) %>% 
    mutate(bio = bio*0.1*10^bin) %>% # this should be as bove TotalUbiomass and Totalvbiomass
    mutate(step = as.numeric(step)) %>% 
    group_by(step, period) %>%  # trait
    dplyr::summarise(bio = sum(bio))
  
  # add temperature 
  temp_h<-input_h$ts[,c(1,2)] %>% 
    mutate(period = "hist") %>% 
    select(-t) %>% 
    mutate(step = c(rep(1:(nrow(input_h$ts)/4), each = 4),NA)) %>% # yearly mean 
    group_by(step,period) %>%  
    dplyr::summarise(sst = mean(sst, na.rm=TRUE))
  
  temp_585<-input_585$ts[,c(1,2)] %>% 
    mutate(period = "585") %>% 
    select(-t) %>% 
    mutate(step = c(rep(1:(nrow(input_585$ts)/4), each = 4), NA)) %>% # yearly mean
    group_by(step, period) %>%  
    dplyr::summarise(sst = mean(sst, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(step = step+nrow(temp_h))
  
  temp_126<-input_126$ts[,c(1,2)] %>% 
    mutate(period = "126") %>% 
    select(-t) %>% 
    mutate(step = c(rep(1:(nrow(input_126$ts)/4), each = 4), NA)) %>% # montly mean
    group_by(step, period) %>%  
    dplyr::summarise(sst = mean(sst, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(step = step+nrow(temp_h))
  
  temp<-temp_h %>% full_join(temp_585) %>%  full_join(temp_126)
  
  # plot trends 
  # NOTE - temp need to be adjusted - don't know how ... maybe as per model? 
  plot_tcb<-ggplot(biomass_time, aes(x=step, y=bio, color = period))+
    geom_line()
  
  plot_temp<-ggplot(temp, aes(x=step, y=sst, color = period))+
    geom_smooth()
  
  # plot_trend<-plot_tcb/plot_temp
  
  # growth from historical  
  GU<-as_data_frame(result_set_h$GGU) %>% 
    mutate(bin =result_set_h$x) %>% 
    `colnames<-`(c(seq(1, ncol(result_set_h$GGU)),"bin")) %>% 
    gather(step, GU, -bin)
  GV<-as_data_frame(result_set_h$GGV) %>% 
    mutate(bin =result_set_h$x) %>% 
    `colnames<-`(c(seq(1, ncol(result_set_h$GGV)),"bin")) %>% 
    gather(step, GV, -bin)
  
  growth<-GU %>% full_join(GV) %>% 
    gather(trait, growth, -c(bin, step)) %>%
    filter(bin >= -7)
  
  # try your code
  plot_growth<-ggplot(filter(growth, step == max(as.numeric(step))-1), aes(x=bin, y=log10(growth), group = trait, color = trait))+
    geom_line()+
    facet_wrap(~trait, ncol=2)
  
  return(list(plot_spectrum = plot_spectrum, plot_tcb = plot_tcb, plot_temp = plot_temp, plot_growth = plot_growth, biomass_time = biomass_time, temp = temp, biomass = biomass, growth = growth)) 
  
}


  # inputs same for both scenarios (no senescence and senescence)
  input_585<-readRDS(paste0("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/grid_",grids,"_IPSL-CM6A-LR_ssp585.rds"))
  input_126<-readRDS(paste0("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp126/grid_",grids,"_IPSL-CM6A-LR_ssp126.rds"))
  input_h<-readRDS(paste0("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_",grids,"_IPSL-CM6A-LR_historical.rds"))  

  # original NO temp effect on senescence but temp effect on Om:
  result_set_585<-readRDS(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_",grids,"_ssp585.rds"))
  result_set_126<-readRDS(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp126/dbpm_output_all_",grids,"_ssp126.rds"))
  result_set_h<-readRDS(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/historical/dbpm_output_all_",grids,"_historical.rds"))
 
  res_or<-plot_grid(result_set_h, result_set_585,result_set_126, input_h, input_585, input_126)

  # RE-RUN no scenenscence ----
  # repeat the above as for this test you are running the code from here
  library(zoo)
  esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
  scenario <- c("picontrol", "historical", "ssp126", "ssp585")

  # run 1 grid cell ----  
  source('runmodel_yearly.R')  
  # NOTE you have coommented source("./size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE) in runmodel_yearly.R first 
  # 3 options here - this is where the bug that made tcb increasing was: 
  source("./size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE) # no Tempeffect on senesence mortality (SM.v, SM.v) NOR on the component of detritus that comes from other mortality (OM - line 400)
                                                                                  # this is the version used in CMIP5 and in Julia's old models so we keep this for consistency
  # source("./size-based-models/dynamic_sizebased_model_functions_TempOnSenescence.R", chdir = TRUE) # temperature on senescence and the detritus component of OM
                                                                                  # first model runs used this version, the second model run used a version where 
                                                                                  # temp on senescence was off but on the detritus component of OM was on
  # source("./size-based-models/dynamic_sizebased_model_functions_CMIP52019.R", chdir = TRUE) # Ryan latest version with no temp effect on sen or OM - 
                                                                                              # this is the version we are using which has been merged with dynamic_sizebased_model_functions.R  

  i = 2 # ipsl 
  curr_esm <- esms[i]
  load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file

  # need to run  all scenarios historical, spp126 and spp585 as these are requiresd for the plotting function below 
  # run for all:
  j = 2 # historical  
  curr_scen <- scenario[j]
  input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen,"/", sep = "")
  output_loc_trial <- paste("/../../rd/gem/private/fishmip_outputs/temp_trials/", curr_esm, "/", curr_scen,"/", sep = "")

  # hist requires a rungridsep()
  rungridsep(igrid = grids,
           gcm = curr_esm,
           protocol = curr_scen,
           output = "partial",
           input_files_location = input_loc,
           output_files_location = output_loc_trial)

  # fut requires rungridsep_ssp() and inputs from hist runs above 
  for (j in 3:4){
  
    curr_scen <- scenario[j]
    input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen,"/", sep = "")
    output_loc_trial <- paste("/../../rd/gem/private/fishmip_outputs/temp_trials/", curr_esm, "/", curr_scen,"/", sep = "")
    output_loc_hist <- paste("/../../rd/gem/private/fishmip_outputs/temp_trials/", curr_esm, '/historical', sep = "")
    input_loc_hist <- paste("/../../rd/gem/private/fishmip_inputs/temp_trials/", curr_esm, '/historical', sep = "")
  
    # NOTES: this run the cell and save the outputs in defined here output_loc_trial (different from original runs) so you are not overwriting original files. 
    rungridsep_ssp(igrid = grids,
                 gcm = curr_esm,
                 protocol = curr_scen,
                 output = "partial",
                 input_files_location = input_loc,
                 output_files_location = output_loc_trial,
                 output_historical_location = output_loc_hist,
                 input_historical_location = input_loc_hist) # this last argument is not used in the end ... inputs that resulted in the historical outputs. 
  }

  # explore results 
  result_set_585<-readRDS(paste0("/../../rd/gem/private/fishmip_outputs/temp_trials/IPSL-CM6A-LR/ssp585/dbpm_output_all_",grids,"_ssp585.rds"))
  result_set_126<-readRDS(paste0("/../../rd/gem/private/fishmip_outputs/temp_trials/IPSL-CM6A-LR/ssp126/dbpm_output_all_",grids,"_ssp126.rds"))
  result_set_h<-readRDS(paste0("/../../rd/gem/private/fishmip_outputs/temp_trials/IPSL-CM6A-LR/historical/dbpm_output_all_",grids,"_historical.rds"))
  
  res_sc<-plot_grid(result_set_h, result_set_585,result_set_126, input_h, input_585, input_126)
  # res$plot_trend + res$plot_spectrum + res$plot_growth 

  # add info together - datasets 
  res_or$biomass_time$scenario<-"no_scenenscence"
  res_sc$biomass_time$scenario<-"scenenscence"
  df_bio<-res_or$biomass_time %>% 
    full_join(res_sc$biomass_time)
  
  res_or$temp$scenario<-"no_scenenscence"
  res_sc$temp$scenario<-"scenenscence"
  df_temp<-res_or$temp %>% 
    full_join(res_sc$temp)
  
  res_or$biomass$scenario<-"no_scenenscence"
  res_sc$biomass$scenario<-"scenenscence"
  df_spectra<-res_or$biomass %>% 
    full_join(res_sc$biomass)
  
  res_or$growth$scenario<-"no_scenenscence"
  res_sc$growth$scenario<-"scenenscence"
  df_growth<-res_or$growth %>% 
    full_join(res_sc$growth)
  
  # add info together - plots 
  
  # bio and temperature trends 
  layout <- "
  AB
  CD
  "
  res_or$plot_tcb<-res_or$plot_tcb + ggtitle("Original") + ylim(min(df_bio$bio),max(df_bio$bio))
  res_sc$plot_tcb<-res_sc$plot_tcb + ggtitle("New") + ylim(min(df_bio$bio),max(df_bio$bio))
  plot_bio <- res_or$plot_tcb + res_sc$plot_tcb + res_or$plot_temp + res_sc$plot_temp + plot_layout(design = layout)
  
  # size spectrum 
  layout <- "
  AA
  BB
  "
  
  df_spectra2<-filter(df_spectra, bio>0, step == max(as.numeric(df_spectra$step))-1) # consider 1 time step as you've done when producing the plot 
  res_or$plot_spectrum <-res_or$plot_spectrum + ggtitle("Original") + ylim(min(log10(df_spectra2$bio)),max(log10(df_spectra2$bio))) 
  res_sc$plot_spectrum <-res_sc$plot_spectrum + ggtitle("New") + ylim(min(log10(df_spectra2$bio)),max(log10(df_spectra2$bio)))
  plot_spectra <- res_or$plot_spectrum + res_sc$plot_spectrum + plot_layout(design = layout)
  
  # growth 
  df_growth2<-filter(df_growth, step == max(as.numeric(step))-1) # consider 1 time step as above
  res_or$plot_growth2 <-res_or$plot_growth + ggtitle("Original") + ylim(min(log10(df_growth2$growth)),max(log10(df_growth2$growth)))  
  res_sc$plot_growth2 <-res_sc$plot_growth + ggtitle("New") + ylim(min(log10(df_growth2$growth)),max(log10(df_growth2$growth)))
  plot_growth <- res_or$plot_growth2 + res_sc$plot_growth2 + plot_layout(design = layout)

  return(list(plot_bio = plot_bio, plot_spectra = plot_spectra, plot_growth = plot_growth, df_bio = df_bio, df_temp = df_temp))
  
}    

# depth file to select grid cells according to map 
load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", "IPSL-CM6A-LR", '/',  sep = ""), pattern = "*depth*", full.names = TRUE))

# cell grid where tcb is increasing (when using the TempOnSenescence): 
readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_22000_IPSL-CM6A-LR_historical.rds")$depth
# lon  lat    depth gridnum
# 92 -1.5 4687.581   31953

res<-compare_senarios(22000)
check3_bio<-res$plot_bio # same as above, greather biomass with senesence
check3_spectra<-res$plot_spectra # opposite than above - steeper sizespectrum for U (predators/pelagics) with senescence. Also inverted sise spectrum compared to above for both with and without senescence - i.e. U below V (detritivores/demersal)
check3_growth<-res$plot_growth # no difference between with and withot senescence - but U does not growh? 

# try another random grid 
readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_23000_IPSL-CM6A-LR_historical.rds")$depth
res<-compare_senarios(23000)
check3_bio<-res$plot_bio 
check3_spectra<-res$plot_spectra 
check3_growth<-res$plot_growth 

### explore historical and picontrol inputs and consideration for spin up ----
inputs_h <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_1_IPSL-CM6A-LR_historical.rds")
inputs_h<-inputs_h$ts 
inputs_p <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/picontrol/grid_1_IPSL-CM6A-LR_picontrol.rds")
inputs_p<-inputs_p$ts
    
### explore inputs/outputs and time steps difference ---- 
inputs_h <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_100_IPSL-CM6A-LR_historical.rds")
inputs_h <- inputs_h$ts 
dim(inputs_h)[1] # time dimention
7917/48 # historical 164.93 year meaning that 3 weeks are left out of 165 years (7920/48 = 165)
12045/48 # picontrol 250.9375 year meaning that 3 weeks are left out of 251 years (12048/48 = 251)
12045/4 # picontrol 3011.25 months (as I save monthy...) - 3 weeks are left out of 3012 months
4125/48 # ssp585 is 85.93 year - same as per historical - it should be 4128/48
result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_100_ssp585.rds")
output_h<-result_set$U
dim(output_h)[2] # time dimention
1980/12 # historical and saved monthly 
22318 - (300*48) # leave spin up out - one time step more than inputs  
3012 # picontrol saved months (spin up already left out) - here it's OK becasue when I save monthly I save the 1st week of each month (the 0.25 above becomes output for the last month)
1032/12 # ssp585 output dimentsion is ok but last time step for class 0-90 is NA 

# compate inputs with outputs 
output_h[,1031:1032]
inputs_h[dim(inputs_h)[1],]  # all inputs are ok for last time dimention (first week of last year)

### check time dimention of outputs in historical ----
# find grids for which result_set$notrun == TRUE in historical run, hence with problems when calculting initila abundance for ssp runs (last step of historical) and with problems with isave when creating output variables 
# other option: in terminal find files bigger than 124M (the size of a file with time dimention 22318): find . type- f -size +124M
# ./dbpm_output_all_21747_historical.rds is the only one with larger size 223162 (result_set$notrun = T, option 1)
# igrid = 21747
# to find smaller find . type- f -size -124M

#### plots ----

result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/GFDL-ESM4/ssp585/dbpm_output_all_38658_ssp585.rds")
result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/picontrol/dbpm_output_all_22430_picontrol.rds")
result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/historical/dbpm_output_all_22430_historical.rds")

str(result_set)
dim(result_set$GGU)
result_set$U[100:110, 1:10]
sum(result_set_h$V[,1980]) # this should be the starting abundance for the ssp126 run
dim(result_set$U)
sum(result_set$U[,1]) # this should be the second time step in V (the first being the the abundance above, but not saved)

# sum(result_set_h$U[,1978])
# sum(result_set$U[,1])

#### run model local ----
# where processed_forcing = private in gem48
# NOTE: input file in private/.../historical = random gridcell file that Ryan send you a while ago to test the model (pi_26_cesm.rds) just renamed.  
curr_esm <- "GFDL-ESM4"
load(list.files(path=paste("/Users/nov017/Dropbox/DBPM_fishing_extension/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) 
curr_scen <- "historical"
input_loc <- paste("/Users/nov017/Dropbox/DBPM_fishing_extension/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "")
output_loc <- paste("/Users/nov017/Dropbox/DBPM_fishing_extension/private/fishmip_outputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "")
  
numcores= 25 
cl <- makeForkCluster(getOption("cl.cores", numcores))
grids<-1
ptm=proc.time()
options(warn=-1)

# as a first trial, run it inside the function rungridsep in runmodel_yearly.r instead 
parallel::clusterApply(cl,x=grids,fun=rungridsep, gcm = curr_esm, protocol = curr_scen, output = "partial",  
                       input_files_location = input_loc, output_files_location = output_loc)
print((proc.time()-ptm)/60.0)
stopCluster(cl)
    