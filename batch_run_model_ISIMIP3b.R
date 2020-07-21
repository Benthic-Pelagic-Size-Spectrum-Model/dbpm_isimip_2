
# CN this is an updated file from Ryan to run the model in parallel.
# it calls runmodel_yearly.R (or runmodel.R, depending on whether you need to run yearly or monthly). NOTE: You might need to load these files from Ryan's folder too. 
# in Julia's older version, this codes is included in runmodel_calls.r but it's very different!


#### STEP 3: RUN THE MODEL
# source("./DBPM/") # Set to the base folder for the DBPM runs
source("/data/home/camillan/dbpm") # CN change directory 

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

source('runmodel_yearly.R') # Load script to run model (YEARLY SCRIPT, CHANGE )

for(i in 1:length(esms)){ # Loop over esms
  curr_esm <- esms[i]
  
  load(list.files(path=paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, '/',  sep = ""), pattern = "*depth*", full.names = TRUE)) # Load esm depth file
  
  for(j in 1:length(scenario)){ # Loop over scenario
    curr_scen <- scenario[j]
    
    input_loc <- paste("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/", curr_scen, "/", sep = "")
    output_loc <- paste("/../../rd/gem/private/fishmip_outputs/", curr_esm, "/", curr_scen, "/", sep = "") # CN might need to organise this folder better 
    
    # set up cluster
    numcores= 25 
    
    cl <- makeForkCluster(getOption("cl.cores", numcores))
    
    # grids to read in are sequential for the depth file
    grids<-1:dim(depth)[1]
    
    # Running the model
    ptm=proc.time()
    options(warn=-1)
    
    parallel::clusterApply(cl,x=grids,fun=rungridsep, gcm = curr_esm, protocol = curr_scen, output = "aggregates",
                           input_files_location = input_loc, output_files_location = output_loc)
    
    print((proc.time()-ptm)/60.0)
    
  stopCluster(cl)
  }
}





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

}


stopCluster(cl)
