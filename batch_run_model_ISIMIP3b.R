
# CN this is an updated file from Ryan to run the model in parallel.
# it calls runmodel_yearly.R (or runmodel.R, depending on whether you need to run yearly or monthly). NOTE: You might need to load these files from Ryan's folder too. 
# in Julia's older version, this codes is included in runmodel_calls.r but it's very different!

#### STEP 3: RUN THE MODEL
# source("./DBPM/") # Set to the base folder for the DBPM runs
setwd("/data/home/camillan/dbpm")
#  setwd("/Users/nov017/R-projects/dbpm") # local 

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

# for(i in 1:length(esms)){ # Loop over esms
  
  i = 2
  curr_esm <- esms[i]
  
  load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  
  # for(j in 1:length(scenario)){ # Loop over scenario
  
    # historical saved weekly outputs + spin up = 4.9T, 4 days to run (but on 25 cores)
    # picontrol saved montly outputs = 340G, 2.5 days to run on 45 cores  
  
    # new runs without temp effect on senescenace:
    # historical: montly outputs 2.2 days; need to run separately as you are also saving growth - and picontrol is not necessary at this stage. 
    # note that you can either use the dynamics_sizebased_model_function.R or the dynamics_sizebased_model_function_TempOnSenescence.R in runmodel_yearly.R  
    j = 2
    
    curr_scen <- scenario[j]
    
    input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "")
    output_loc <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "") 
    
    # go in runmodel_yearly.r and run from there if you want to tri 1 gridcell only
    
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
#  }
#}
    
### projections protocols ssp ----
    
for(i in 1:length(esms)){ # Loop over esms
  
  i = 2 # ipsl 
  curr_esm <- esms[i]
  
  load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  
  for(j in 3:length(scenario)){ # Loop over scenario
    
    # ssp126 saved weekly outputs starting from last historical week = 13 h to run; 117G  
    # ssp585 saved weekly outputs starting from last historical week = 12 h to run; 117G 
    
    # runs without temperature effect on senescence - see above. both ssp/s
    # saved weekly outputs starting from last historical week = 25 h to run together
    j = 4 
    
    curr_scen <- scenario[j]
    
    input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen,"/", sep = "")
    output_loc <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, "/", curr_scen,"/", sep = "")
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

### explore effect of senescence ----
# run 1 grid call and  compare with/without senescence 
source('runmodel_yearly.R')  
i = 2 # ipsl 
curr_esm <- esms[i]
load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file

# need to run  all scenarios historical, spp126 and spp585 as these are requiresd for the plotting function below 
j = 4 # spp585
curr_scen <- scenario[j]
input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen,"/", sep = "")
output_loc <- paste("/../../rd/gem/private/fishmip_outputs/temp_trials/", curr_esm, "/", curr_scen,"/", sep = "")
output_loc_hist <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, '/historical', sep = "")
input_loc_hist <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/historical', sep = "")

grids<-1

# the  rungridsep_ssp() calls either functions below - but can call from here 
# source("./size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE) 
# source("./size-based-models/dynamic_sizebased_model_functions_TempOnSenescence.R", chdir = TRUE) 
result_set<-rungridsep_ssp(igrid = grids,
                           gcm = curr_esm,
                           protocol = curr_scen,
                           output = "partial",
                           input_files_location = input_loc, 
                           output_files_location = output_loc, 
                           output_historical_location = output_loc_hist,
                           input_historical_location = input_loc_hist)

# run plotting function - move somewhere? 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)

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
  
  plot_trend<-plot_tcb/plot_temp
  
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
  plot_growth<-ggplot(filter(growth, step == max(as.numeric(step))), aes(x=bin, y=log10(growth), group = trait, color = trait))+
    geom_line()+
    facet_wrap(~trait, ncol=2)
  
  # try  analyseoutput
  x = result_set_h$x
  dx = result_set_h$dx
  Nx = length(x)
  x1 = -7
  x1.det = -7
  xmin = -12
  ref = ((x1-xmin)/dx)+1
  ref.det = ((x1.det-xmin)/dx)+1 
  xmax = 6
  
  params = list (
    # isave = isave, 
    x = x, 
    dx = dx, 
    Nx = Nx, 
    x1 = x1, 
    x1.det = x1.det, 
    xmin = xmin, 
    ref = ref, 
    ref.det = ref.det,
    xmax = xmax)
  
  # res<-result_set_h
  
  # plot(params$x[params$ref:params$Nx],res$GGU[params$ref:params$Nx,1980],log="y", type = "l", col = "blue", ylab= "Relative growth rate",
  #      xlab = "Size") # ylim = c(0.001,1000), xlim=c(params$x1.det,params$xmax)
  
  # whty are they  different? becasue in plot() the data is in log scale but the values on the y  axis are not...  
  # trial<-growth %>% filter(step == max(as.numeric(step)), trait == "GU")
  # log10(trial$growth)
  # log(res$GGU[params$ref:params$Nx,1980])
  
  return(list(plot_spectrum = plot_spectrum, plot_trend = plot_trend, plot_growth = plot_growth)) 
  
}


# inputs 
input_585<-readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/grid_1_IPSL-CM6A-LR_ssp585.rds")
input_126<-readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp126/grid_1_IPSL-CM6A-LR_ssp126.rds")
input_h<-readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_1_IPSL-CM6A-LR_historical.rds")  

# original NO temp effect on senescence: 
# try grid cell: 22430
result_set_585<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_1_ssp585.rds")
result_set_126<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp126/dbpm_output_all_1_ssp126.rds")
result_set_h<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/historical/dbpm_output_all_1_historical.rds")
 
res<-plot_grid(result_set_h, result_set_585,result_set_126, input_h, input_585, input_126)
res$plot_trend + res$plot_spectrum + res$plot_growth 

# original WHITH temp effect on senescence: 
# try grid cell: 22430
result_set_585<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b_withTempOnSenescence/IPSL-CM6A-LR/ssp585/dbpm_output_all_1_ssp585.rds")
result_set_126<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b_withTempOnSenescence/IPSL-CM6A-LR/ssp126/dbpm_output_all_1_ssp126.rds")
result_set_h<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b_withTempOnSenescence/IPSL-CM6A-LR/historical/dbpm_output_all_1_historical.rds")

# lots of data was saved in history 
result_set_h$U<-result_set_h$U[,((300*48)+1):dim(result_set_h$U)[2]] # delete spin-up
vec<-seq(1,dim(result_set_h$U)[2], by =4) # consider monthly data 
result_set_h$U<-result_set_h$U[,vec]
result_set_h$V<-result_set_h$V[,((300*48)+1):dim(result_set_h$V)[2]] 
result_set_h$V<-result_set_h$V[,vec]
result_set_h$GGU<-result_set_h$GGU[,((300*48)+1):dim(result_set_h$GGU)[2]] 
result_set_h$GGU<-result_set_h$GGU[,vec]
result_set_h$GGV<-result_set_h$GGV[,((300*48)+1):dim(result_set_h$GGV)[2]] 
result_set_h$GGV<-result_set_h$GGV[,vec]

res<-plot_grid(result_set_h, result_set_585,result_set_126, input_h, input_585, input_126)
res$plot_trend + res$plot_spectrum + res$plot_growth

# last time step does not have PP (NA) as these are inputs and last time step is 'left overs' - corrected in new version. in plot we use the second last time step now 



# RE-RUN (using function with no senescence ...) 
result_set_585<-readRDS("/../../rd/gem/private/fishmip_outputs/temp_trials/IPSL-CM6A-LR/ssp585/dbpm_output_all_1_ssp585.rds")
result_set_126<-readRDS("/../../rd/gem/private/fishmip_outputs/temp_trials/IPSL-CM6A-LR/ssp126/dbpm_output_all_1_ssp126.rds")
result_set_h<-readRDS("/../../rd/gem/private/fishmip_outputs/temp_trials/IPSL-CM6A-LR/historical/dbpm_output_all_1_historical.rds")

















        
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

# result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_22430_ssp585.rds") 
# result_set_h<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/historical/dbpm_output_all_22430_historical.rds") 

# dim(result_set_h$V)
# sum(result_set_h$V[,1980]) # this should be the starting abundance for the ssp126 run 
# dim(result_set$U)
# sum(result_set$V[,1]) # this should be the second time step in V (the first being the the abundance above, but not saved)

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
    