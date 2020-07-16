#### STEP 3: RUN THE MODEL

rm(list=ls())

install.packages('zoo', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
library(zoo, lib.loc = '/home/rhenegha/R_package_library')
library(parallel)

setwd('./DBPM_runs/')

load(file="./Inputs/CESM1-BEC/processed_forcings/cesm_depth_v2.RData") # load depth data

source('./dbpm-ryan/runmodel_yearly.R')

# set up cluster 
numcores= 25 # Check if this is available on Ocean before running

# cl <- makeCluster(numcores,type="FORK",outfile='')
cl <- makeForkCluster(getOption("cl.cores", numcores))

# grids to read in are sequential for the depth file
grids<-1:dim(depth)[1]


prots = c("clim", "pi")

for(i in 1:length(prots)){

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
