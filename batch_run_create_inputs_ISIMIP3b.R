
# CN - this is un updated file from Ryan. It is based on getinputsISIMIP2b.R but it does not include step 1 (deal with netcdf raw inputs) as this step is now in batch_run_create_output_netcdf.R
# it also loops through the heart models inputs and saves outputs (i.e. final model inputs) in separate folders for each scenario (e.g. historical...).  
# it needs getgridin_ISIMIP3b.R, which needs input_funcs.R

#-------------------------------------STEP 2: DISAGGREGATE TIME SERIES INPUTS FOR MODEL TO WEEKLY (OR DAILY) TIME STEPS

rm(list=ls())
# If you need to install these packages, set your package library location here (if needed) 
# install.packages('zoo', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/') # 
# library(zoo, lib.loc = '/home/rhenegha/R_package_library')
library(zoo)
library(parallel)


setwd("/data/home/camillan/dbpm") # Set working directory
# list.files()
source("getgridin_ISIMIP3b.R") # Load getgridin function
numcores <- 12 # How many cores do you have to do the work? # CN 12 in Julia's older version

esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
scenario <- c("historical", "picontrol", "ssp126", "ssp585")

for(i in 1:length(esms)){
 curr_esm <- esms[i]

 # load(list.files(path=paste("./processed_forcings/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
 load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file # CN new location on gem48
 # list.files("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/")
 
  for(j in 1:length(scenario)){
    curr_scen <- scenario[j]
    
    # save_path=paste("./processed_forcings/", curr_esm, '/', curr_scen, '/',  sep = "") # Where do you want the grid files to be saved?
    save_path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/', curr_scen, '/',  sep = "") # Where do you want the grid files to be saved? # CN new path 
    
    # load(list.files(path=paste("./processed_forcings/", curr_esm, '/',  sep = ""), pattern = paste("*_", curr_scen,"*", sep = ""), full.names = TRUE)) # Load curr esm, curr scen forcings
    load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = paste("*_", curr_scen,"*", sep = ""), full.names = TRUE)) # Load curr esm, curr scen forcings # CN new location 
    
    # cl <- makeCluster(numcores,type="FORK",outfile='')
    cl <- makeForkCluster(getOption("cl.cores", numcores))
    
    # grids to read in are sequential for the depth file
    grids<-1:dim(depth)[1]
    
    # Running the model
    ptm=proc.time()
    options(warn=-1)
    
    clusterApply(cl,x=grids,fun=getgridin, curr_esm = curr_esm, curr_scen = curr_scen, save_path = save_path)
    
    print((proc.time()-ptm)/60.0)
    
    stopCluster(cl)
    
    
  }
   
}








