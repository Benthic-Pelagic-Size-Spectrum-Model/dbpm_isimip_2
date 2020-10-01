
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
    # historical: need to run separately as you are also saving growth - and picontrol is not necessary at this stage. 
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
    grids<-grids[grids!=21747] # this is the only grid from the historical run with greater size dimentions, meaning that result_set$notrun == TRUE
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

    
### explore historical and picontrol inputs and consideration for spin up ----
inputs_h <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_1_IPSL-CM6A-LR_historical.rds")
inputs_h<-inputs_h$ts 
inputs_p <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/picontrol/grid_1_IPSL-CM6A-LR_picontrol.rds")
inputs_p<-inputs_p$ts
    
### explore inputs/outputs and time steps difference ---- 
inputs_h <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/grid_1_IPSL-CM6A-LR_ssp585.rds")
inputs_h <- inputs_h$ts 
dim(inputs_h)[1] # time dimention
7917/48 # historical 164.93 year meaning that 3 weeks are left out of 165 years (7920/48 = 165)
12045/48 # picontrol 250.9375 year meaning that 3 weeks are left out of 251 years (12048/48 = 251)
12045/4 # picontrol 3011.25 months (as I save monthy...) - 3 weeks are left out of 3012 months
4125/48 # ssp585 is 85.93 year - same as per historical - it should be 4128/48
result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_1_ssp585.rds")
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
library(dplyr)
library(tidyr)
library(ggplot2)

result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_1_ssp585.rds") 
result_set_h<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/historical/dbpm_output_all_1_historical.rds") 

dim(result_set_h$V)
sum(result_set_h$V[,1980]) # this should be the starting abundance for the ssp126 run 
dim(result_set$V)
sum(result_set$V[,1]) # this should be the second time step in V (the first being the the abundance above, but not saved)

sum(result_set_h$U[,1978])
sum(U.initial)
sum(result_set$U[,1])

# if you anly want to plot historical ....
# result_set<-result_set_h

addBin<-result_set$x # rows
addCol<-c(seq(1, ncol(result_set$U)),"bin")

result_set$V[,1031:1032]
result_set$U[,1031:1032] # this is strange - it's like the pp inputs are not available for this month .... ! now should be fixed 

result_partial <- as_data_frame(result_set$U)
result_partial$bin <-addBin
colnames(result_partial)<-addCol

# the is the slop part  
result_partial<- result_partial %>% 
  gather(step, bioU, -bin)
add <- as_data_frame(result_set$V) %>% 
  gather(step,bioV)
add2 <- as_data_frame(result_set$GGU) %>% 
  gather(step,growthU)
add3 <- as_data_frame(result_set$GGV) %>% 
  gather(step,growthV)
# add4 <- as_data_frame(result_set$W) # this has no size bins and cannot be plotted in the same way  

result_partial$bioV<-add$bioV
result_partial$growthU<-add2$growthU
result_partial$growthV<-add3$growthV
length(unique(result_partial$step)) # time dimention of historical outputs 

result_partial<-result_partial %>% 
  gather(trait, bio, -c(bin, step) )

length(unique(result_partial$step))
filter(result_partial, step == length(unique(result_partial$step)))

# spectrum 
ss<-filter(result_partial, bio>0, trait %in% c("bioU", "bioV"))
ggplot(filter(ss, step == length(unique(result_partial$step))-1), 
       aes(x=bin, y=log10(bio), group = trait, color = trait))+
  geom_line()

# biomass in time 
gw<-filter(result_partial, trait %in% c("bioU","bioV")) %>% 
  mutate(step = as.numeric(step)) %>% 
  group_by(trait, step) %>% 
  dplyr::summarise(bio = sum(bio))

ggplot(gw, aes(x=step, y=bio, group = trait, color = trait))+
  geom_line()+
  facet_wrap(~trait, scale = "free_y")

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
    