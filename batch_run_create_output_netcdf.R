#----------------PUT FISH_MIP MODEL OUTPUTS INTO netcdf 4
rm(list=ls())
install.packages('RNetCDF', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
library('RNetCDF', lib.loc = '/home/rhenegha/R_package_library')

setwd('./DBPM_runs/')
source('./dbpm-ryan/makenetcdfs_func.R')

load(file="./Inputs/CESM1-BEC/processed_forcings/cesm_depth_v2.RData") # load depth data
# grids to read in are sequential for the depth file
grids<-1:dim(depth)[1]

input_loc <- './Inputs/CESM1-BEC/processed_forcings/'
output_loc <- './Outputs/CESM1-BEC/'
save_loc <- './Processed_Outputs/CESM1-BEC/'

vars2make <- c('tcb', 'tsb', 'b10cm', 'b30cm')
prots <- c('pi','npp-control', 'temperature-control', 'clim')
prot_full_names <- c('pre-industrial', 'npp-control', 'temperature-control', 'clim')

for(i in 1:length(vars2make)){
  for(j in 1:length(prots)){
  print(paste('Now working on ', vars2make[i], ' for protocol ', prots[j], sep = ''))
  mknetcdf(vars2make[i], prots[j], prot_full_names[j], input_loc, output_loc, save_loc, grids)
  }
}
