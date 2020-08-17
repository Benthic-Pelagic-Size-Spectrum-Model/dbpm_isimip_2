
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

# Don't think you need these commands, but I had them in prior script
temp.effect = T
eps = 1e-5
ERSEM.det.input = F
# list.files()

source('runmodel_yearly.R') # Load script to run model (YEARLY SCRIPT, CHANGE ) # CN to run this you need to change directories - discuss this with Julia and Ryan 
# source("runmodel.R") # CN this is set up to run the model adn save the results in the right directories - the _yearly version is Ryan's version and needs to be updated with new directories 
# NOTE - code not working now - you still need to change directories as per Ryan code as inputs now are in folders (historical etc...)

# igrid <-1
# readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/GFDL-ESM4/historical/grid_1_GFDL-ESM4_historical.rds") # try reading file 
# gcm = curr_esm 
# protocol = curr_scen
# output = "partial"
# input_files_location = input_loc 
# output_files_location = output_loc


for(i in 1:length(esms)){ # Loop over esms
  
  # CN try only 1 model and 1 scenario first 
  i = 2
  curr_esm <- esms[i]
  
  load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  
  for(j in 1:length(scenario)){ # Loop over scenario
    # CN try only 1 model adn 1 scenario first 
    j = 2
    
    curr_scen <- scenario[j]
    
    input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "")
    # list.files(input_loc)
    output_loc <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "") 
    # list.files(output_loc)
    
    # set up cluster
    numcores= 25 
    
    cl <- makeForkCluster(getOption("cl.cores", numcores))
    
    # grids to read in are sequential for the depth file
    grids<-1:dim(depth)[1]
    # length(1:dim(depth)[1]) #  41000 grids 
    
    # CN subsets of runs 
    grids<-501:5500 # 5000 grids should take ~12h (if 500 take 72 min i.e. 1.2 h) 
    
    # Running the model
    # CN other time cacualtion 
    # start.time <- Sys.time()
    ptm=proc.time()
    options(warn=-1)
    
    parallel::clusterApply(cl,x=grids,fun=rungridsep, gcm = curr_esm, protocol = curr_scen, output = "partial",  
                           input_files_location = input_loc, output_files_location = output_loc)
    
    print((proc.time()-ptm)/60.0)
    
    # CN other time calcualtion 
    # end.time <- Sys.time()
    # time.taken <- end.time - start.time
    # time.taken
    
    stopCluster(cl)
  }
}

# figure out why you got empty output files when running the picontrol scenario!! 
# try 1 model, 1 scenario, 1 grid cell first - CN - do this with historical as picontrol already has outputs
curr_esm <- esms[2]
load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
curr_scen <- scenario[2]
input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "")
output_loc <- paste("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "") 

# go in runmodel_yearly.r and run from there 

# check outputs - grid 1 has been run manually and the file loads
result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/historical/dbpm_output_all_5500_historical.rds")
ls(result_set)

# other  run - picontrol for gfdl - about 4600 grid calle 1.3 T 
result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/GFDL-ESM4/picontrol/dbpm_output_all_4600_picontrol.rds")
ls(result_set)

# plot spectrum 
# getAnywhere(plotsizespectrum()) # we don't have param saved into the partial one 
library(dplyr)
library(tidyr)

addBin<-result_set$x # rows
addCol<-c(seq(1, ncol(result_set$U)),"bin")

result_partial <-as_data_frame(result_set$U)
result_partial$bin <-addBin
colnames(result_partial)<-addCol

# the is the slop part  
result_partial<- result_partial %>% 
  gather(step, bioU, -bin)
# add other values 
add <- as_data_frame(result_set$V) %>% 
  gather(step,bioV)
add2 <- as_data_frame(result_set$GGU) %>% 
  gather(step,growthU)
add3 <- as_data_frame(result_set$GGV) %>% 
  gather(step,growthV)

add4 <- as_data_frame(result_set$W) # this has a differnt format.... no size bins... 
 
result_partial$bioV<-add$bioV
result_partial$growthU<-add2$growthU
result_partial$growthV<-add3$growthV

head(result_partial)

result_partial<-result_partial %>% 
  gather(trait, bio, -c(bin, step) )

head(result_partial)
length(unique(result_partial$step))
filter(result_partial, step == 22318) # there are NAs is bio for some grids 

# transformation values 

ss<-filter(result_partial, trait %in% c("bioU", "bioV"))
head(ss)
library(ggplot2)
ggplot(filter(ss, step == 22317, bio>0), aes(x=bin, y=log10(bio), group = trait, color = trait))+
  geom_line()

unique(result_partial$bin)
head(result_partial)
gw<-filter(result_partial, trait %in% c("bioU","bioV"), bio>0) %>% 
  mutate(step = as.numeric(step)) %>% 
  group_by(trait, step) %>% 
  dplyr::summarise(bio = sum(bio))

ggplot(gw, aes(x=step, y=bio, group = trait, color = trait))+
  geom_line()+
  facet_wrap(~trait, scale = "free_y")

yr<-seq(max(gw$step)-5,max(gw$step))
filter(gw, step %in% yr)


# or in local - where processed_forcing = private in gem48
# NOTE - the  input file  in private/.../historical is a random gridcell file taht Ryan  send you a while ago to test the model (pi_26_cesm.rds) just renamed.  
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




# Running the model
ptm=proc.time()
options(warn=-1)
  
prot = prots[i]
curr_gcm = "cesm"
input_path = "./Inputs/CESM1-BEC/processed_forcings/"
output_path = "./Outputs/CESM1-BEC/"
curr_output = "aggregated"  

protocol =prot
gcm = curr_gcm
output = curr_output
input_files_location = input_path
output_files_location = output_path
output = curr_output
igrid = 23015
temp.effect = T
eps = 1e-5
ERSEM.det.input = F

parallel::clusterApply(cl,x=grids,fun=rungridsep, gcm = curr_gcm, protocol = prot, output = curr_output,
                       input_files_location = input_path, output_files_location = output_path)

print((proc.time()-ptm)/60.0)



stopCluster(cl)
