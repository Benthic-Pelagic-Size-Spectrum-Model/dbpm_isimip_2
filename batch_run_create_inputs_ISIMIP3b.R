
# CN - this is un updated file from Ryan. It is based on getinputsISIMIP2b.R but it does not include step 1 (deal with netcdf raw inputs) as this step is now in batch_run_create_output_netcdf.R
# it also loops through the earth models inputs and saves outputs (i.e. final model inputs) in separate folders for each scenario (e.g. historical...).  
# it needs getgridin_ISIMIP3b.R, which needs input_funcs.R

#-------------------------------------STEP 2: DISAGGREGATE TIME SERIES INPUTS FOR MODEL TO WEEKLY (OR DAILY) TIME STEPS

rm(list=ls())
# If you need to install these packages, set your package library location here (if needed) 
# install.packages('zoo', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/') # 
# library(zoo, lib.loc = '/home/rhenegha/R_package_library')

# CN 
# install.packages("zoo")

library(zoo)
library(parallel)


setwd("/data/home/camillan/dbpm") # Set working directory
# list.files()
source("getgridin_ISIMIP3b.R") # Load getgridin function
numcores <- 12 # How many cores do you have to do the work? # CN 12 in Julia's older version

# # CN CMIP3b runs 
# esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
# scenario <- c("historical", "picontrol", "ssp126", "ssp585")

# CN CMIP3a runs 
esms <- c("obsclim", "ctrlsclim")
scenario <- c("0.25deg", "1deg")

tic()
for(i in 1:length(esms)){
 
  # trial
  i = 1
  
  curr_esm <- esms[i]
 
  # load(list.files(path=paste("./processed_forcings/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  # CN CMIP63b
  # load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file # CN new location on gem48
  # list.files("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/")
  
  for(j in 1:length(scenario)){
    
    # trial 
    j = 1
    
    curr_scen <- scenario[j]
    
    # CN load depth file - one for each esm/scenario combination
    load(list.files(path=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", curr_esm, '/',  curr_scen ,sep = ""), pattern = "*deptho*", full.names = TRUE)) 
    
    # save_path=paste("./processed_forcings/", curr_esm, '/', curr_scen, '/',  sep = "") # Where do you want the grid files to be saved?
    # CN CMIP63b
    # save_path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/', curr_scen, '/',  sep = "") # Where do you want the grid files to be saved? # CN new path 
    # CN CMIP63a - WARNING should this be the same path to where inputs are saved from step 1? ADDED gridcell - need to create folder 
    save_path=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", curr_esm, '/', curr_scen, '/', 'gridcell/' ,sep = "") 
    
    # load(list.files(path=paste("./processed_forcings/", curr_esm, '/',  sep = ""), pattern = paste("*_", curr_scen,"*", sep = ""), full.names = TRUE)) # Load curr esm, curr scen forcings
    # CN CMIP63b
    # load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = paste("*_", curr_scen,"*", sep = ""), full.names = TRUE)) # Load curr esm, curr scen forcings # CN new location 
    # CN CMIP63a -  WARNING - this will load both depth and climate dataset...unless I specify obsclim but need to do this better. I could just use the above and do not specify any pattern.
    # but myght be risky if runnig 3b in the future?? 
    load(list.files(path=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", curr_esm, '/',  sep = ""), pattern = paste("*_", curr_scen,"*", sep = ""), full.names = TRUE))
    # CHECK 
    list.files(path=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", curr_esm, '/', curr_scen, sep = ""), pattern = "*obsclim*", full.names = TRUE)
    
    # cl <- makeCluster(numcores,type="FORK",outfile='')
    cl <- makeForkCluster(getOption("cl.cores", numcores))
    
    # # CN now commented not to overwrite stuff .... 
    # # grids to read in are sequential for the depth file
    # grids<-1:dim(depth)[1]
    # 
    # # Running the model
    # ptm=proc.time()
    # options(warn=-1)
    # 
    # clusterApply(cl,x=grids,fun=getgridin, curr_esm = curr_esm, curr_scen = curr_scen, save_path = save_path)
    # 
    # print((proc.time()-ptm)/60.0)
    # 
    # stopCluster(cl)
    
    
  }
   
}

toc()

#user   system  elapsed 
#3.5566   3.4890 226.9408 
#user     system    elapsed 
#5.426267   4.775600 311.945783 
#user     system    elapsed 
#2.157733   1.810933 110.382567 
#user     system    elapsed 
#2.201133   1.883000 110.372633 
#user     system    elapsed 
#3.109267   2.745400 171.720650 
#user     system    elapsed 
#4.274733   3.764200 249.398533 
#user    system   elapsed 
#1.708133  1.630200 86.747667 
#user    system   elapsed 
#1.668067  1.440400 88.715533 

# check there is data CMIP63b:  
getwd()
setwd("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/GFDL-ESM4/picontrol")
getwd()
list.files()
a<-readRDS("grid_108_GFDL-ESM4_picontrol.rds")
head(a)

