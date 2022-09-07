
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
library(tidyverse)
library(tictoc)
library(dtplyr)

setwd("/data/home/camillan/dbpm") # Set working directory
# list.files()
source("getgridin_ISIMIP3b.R") # Load getgridin function
# numcores <- 12 # How many cores do you have to do the work? # CN 12 in Julia's older version
numcores <- 40

# # CN CMIP3b runs
# esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
# scenario <- c("historical", "picontrol", "ssp126", "ssp585")

# CN CMIPab runs
esms <- c("obsclim", "ctrlsclim", "spinup")
scenario <- c("1deg") #, "0.25deg") # WARNING 0.25 still too big ... 

tic()
for(i in 1:length(esms)){

  # trial 
  # i = 3 # spinup
  
  curr_esm <- esms[i]

  # load(list.files(path=paste("./processed_forcings/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  # CN CMIP63b
  # load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file # CN new location on gem48
  
  for(j in 1:length(scenario)){

    # trial 
    # j = 2 # 1deg
    
    curr_scen <- scenario[j]
    
    # CN CMIP3a load depth file - one for all scenarios (obsclim and ctrlclim and therefore spinup)
    load(list.files(path=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/",
                               "obsclim", '/',
                               curr_scen ,sep = ""),
                    pattern = "*deptho*", full.names = TRUE))
    
    # nrow(depth) # 41934 gridcell 1 deg # 670589 gridcell 0.25deg
    

    # save_path=paste("./processed_forcings/", curr_esm, '/', curr_scen, '/',  sep = "") # Where do you want the grid files to be saved?
    # CN CMIP63b
    # save_path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/', curr_scen, '/',  sep = "") # Where do you want the grid files to be saved? # CN new path
    # CN CMIP3a
    save_path=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/",
                    curr_esm, '/',
                    curr_scen, '/',
                    'gridcell/',
                    sep = "")
    
    # load(list.files(path=paste("./processed_forcings/", curr_esm, '/',  sep = ""), pattern = paste("*_", curr_scen,"*", sep = ""), full.names = TRUE)) # Load curr esm, curr scen forcings
    # CN CMIP63b
    # load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = paste("*_", curr_scen,"*", sep = ""), full.names = TRUE)) # Load curr esm, curr scen forcings # CN new location
    # CN CMIP63a
    if(curr_esm != "spinup"){
    load(file.path(paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
                         curr_esm, '/',
                         curr_scen,
                         sep = ""),
                   paste(curr_esm, '_',curr_scen,".RData",  sep = "")))
    }else{
      # CN not working ... need to igure out why... 
      # load(file.path(paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/",
      #                      curr_esm, '/', # spinup is always using ctrlclim
      #                      curr_scen,
      #                      sep = ""),
      #                paste("ctrlclim", '_',curr_scen,"_SpinUP.RData",  sep = "")))
      # /rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/spinup/1deg/ctrlclim_1deg_SpinUP.RData
      # /rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/spinup/1deg/ctrlclim_1deg_SpinUp.RData

      if(curr_scen == "1deg"){
        load("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/spinup/1deg/ctrlclim_1deg_SpinUp.RData")  
      }else{
        load("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/spinup/0.25deg/ctrlclim_0.25deg_SpinUp.RData")
      }
      
      pp<-spinup
      # head(pp)

      }
    
    # nrow(pp) # 25160400 obsclim 1 deg
    
    # cl <- makeCluster(numcores,type="FORK",outfile='')
    cl <- makeForkCluster(getOption("cl.cores", numcores))

    # grids to read in are sequential for the depth file
    # grids<-1:dim(depth)[1]

    # trial 
    grids<-1
    tic()
    # Running the model
    ptm=proc.time()
    options(warn=-1)

    clusterApply(cl,x=grids,fun=getgridin, curr_esm = curr_esm, curr_scen = curr_scen, save_path = save_path)

    print((proc.time()-ptm)/60.0)

    stopCluster(cl)
    toc() # 1 sec per gridcell... 

  }

}

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

# # check there is data CMIP63b:  
# getwd()
# setwd("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/GFDL-ESM4/picontrol")
# getwd()
# list.files()
# a<-readRDS("grid_108_GFDL-ESM4_picontrol.rds")
# head(a)

# ##### CN CMIP3a runs ----
# # esms <- c("obsclim", "ctrlsclim")
# scenario <- c("1deg", "0.25deg")
# 
# for(i in 1:length(scenario)){
#   
#   # trial - test on time 
#   # i = 1
#    
#   curr_scen <- scenario[i]
#     
#   # CN load depth file - one for both scenarios (obsclim adn ctrlclim)
#   load(list.files(path=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
#                              "obsclim", '/',  
#                              curr_scen ,sep = ""), 
#                   pattern = "*deptho*", full.names = TRUE)) 
#     
#   # nrow(depth) # 41934 gridcell 1 deg # 670589 gridcell 0.25deg
#   
#   # CN CMIP63a
#   save_path_obsclim=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
#                           "obsclim", '/', 
#                           curr_scen, '/', 
#                           'gridcell/' ,
#                           sep = "") 
#   save_path_ctrlclim=paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
#                           "ctrlclim", '/', 
#                           curr_scen, '/', 
#                           'gridcell/' ,
#                           sep = "") 
#     
#   # CN CMIP63a 
#   load(file.path(paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
#                        "ctrlclim", '/',
#                        curr_scen ,  
#                        sep = ""), 
#                  paste("ctrlclim", '_',curr_scen ,".RData",  sep = "")))
#   ctrlclim<-pp
#   
#   load(file.path(paste("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/", 
#                        "obsclim", '/',
#                        curr_scen ,  
#                        sep = ""), 
#                  paste("obsclim", '_',curr_scen ,".RData",  sep = "")))
#   obsclim<-pp
#     
#   cl <- makeForkCluster(getOption("cl.cores", numcores))
#     
#   tic()
#   # grids<-1:dim(depth)[1]
#   # grids = 1:100
#   # Running the model
#   ptm=proc.time()
#   options(warn=-1)
# 
#   clusterApply(cl,x=grids,fun=getgridin_CMIP63a,
#                curr_scen = curr_scen,
#                save_path_ctrlclim = save_path_ctrlclim,
#                save_path_obsclim = save_path_obsclim,
#                ctrlclim = ctrlclim, 
#                obsclim = obsclim)
# 
#   print((proc.time()-ptm)/60.0)
#   stopCluster(cl)
#   toc() 
#   # TOO SLOW - go back to previous method. but add spinup to esm
#   # with plyr: 12 min per 100 cell (estimates: 2h per 1000 cell, 3.8 days for 1deg and 60 days (3.8*16) for 0.25deg)
#   # with dtplyer: 13.59595 min ... 
#   
# }
# 
# # check there is data CMIP63a: OK - WARNING not sre how weekly time steps are calculated (see line 189 in getgridin_ISMIP3b.r) 
# a<-readRDS("/rd/gem/private/fishmip_inputs/ISIMIP3a/processed_forcings/obsclim/1deg/gridcell/grid_1_obsclim_1deg.rds")
# a<-a$ts
# head(a)


