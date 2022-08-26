
rm(list=ls())
#install.packages('RNetCDF', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
#install.packages('reshape2', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
#install.packages('abind', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')

library(RNetCDF) #library(RNetCDF, lib.loc = '/home/rhenegha/R_package_library') # For reading/manipulating netCDFs
library(reshape2) #library(reshape2, lib.loc = '/home/rhenegha/R_package_library')
library(abind) #library(abind, lib.loc = '/home/rhenegha/R_package_library')

# ---------------------------------- STEP 1: GET GCM INPUTS FOR DYNAMIC BENTHIC-PELAGIC SIZE SPECTRUM MODEL

# CN - CN has not run this step yet. this step for CMIP6 similulations was run by Ryan who then provided the inputs 
# explore LME inputs provided by Ryan 

library(raster)
library(stringr)

file_path<-"/rd/gem/private/fishmip_inputs/ISIMIP3a/lme_inputs/obsclim/0.25deg"
file_name<-"gfdl-mom6-cobalt2_obsclim_tos_degC_15arcmin_LME_42_monthly_1961_2010.csv"
lme<-read.csv(file.path(file_path, file_name))
# class(lme)
# colnames(lme)
# lme[1:3, 1:6]
# nrow(lme)

# this seems ok - raster is created so that dim 1 is long, dim 2 is lat and dim 3 is time...  
lmeRaster<-lme[,c(2,1,4:ncol(lme))]
lmeRaster <- rasterFromXYZ(lmeRaster)
# dim(lmeRaster)
# ncol(lme) # with lat, lon and area 

# CHECK 
# trial<-lme[,c(2,1,6)]
# trial <- rasterFromXYZ(trial)
# dim(trial)

# pdf("/data/home/camillan/dbpm/Output/plot.pdf")
# plot(lmeRaster[[3]])
# dev.off()

# pdf("/data/home/camillan/dbpm/Output/plot1.pdf")
# plot(trial)
# dev.off()

# TO 
# do the weighted average of climate input (for all inputs including depth)
# mean values - weighted by grid cell latitude 
# https://stackoverflow.com/questions/55230510/calculating-weighted-spatial-global-annual-averages-across-grid-cells-using-netc

# raster with latitude cell values 
w <- init(lmeRaster, 'y')
# cosine after transforming to radians
w <- cos(w  * (pi/180))
# multiply weights with values
x <- lmeRaster * w

# # need to maks the weights too otherwise denominator too high
# pdf("Output/plot1.pdf", height = 8, width = 6)
# plot(x[[1]]) # MULTIPLE DIMENTION
# dev.off()
# 
# pdf("Output/plot2.pdf", height = 8, width = 6)
# plot(w) # ONE DIMENTION 
# dev.off()

w2<-mask(w, lmeRaster, updatevalue=NA) # MULTIPLE DIMENTIONS 
# w2<-mask(w, lmeRaster[[1]], updatevalue=NA) # ONE DIMENTION - it does not seem to matter but need to check 

# pdf("Output/plot3.pdf", height = 8, width = 6)
# plot(w2) 
# dev.off()

# compute weighted average - WARNING is na.rm = TRUE ok??  
weighted_mean<-cellStats(x, sum, na.rm = TRUE) / cellStats(w2, sum, na.rm = TRUE)
# class(weighted_mean)
# weighted_mean[1:10]

weighted_mean<-data.frame(Year = colnames(lme[,-c(1,2,3)]), weighted_mean = weighted_mean)
weighted_mean$LME <- str_extract(file_name,"(?<=LME_).+(?=_monthly)")# extract number after LME_
rownames(weighted_mean)<-NULL

# Calculate the spin up 


# calculate the total area of LME (save this as part of the inputs file - one file per inputs)
lme[1:2,1:4]
weighted_mean$area_m2<-sum(lme$area_m2)

# do the sum of effort by LME and / by total area of LME - LOAD FASER READING FUNCION... 
effort<-read.csv("/rd/gem/private/users/yannickr/DKRZ_EffortFiles/effort_histsoc_1841_2010.csv")
# do the above for 67 LME so that you can add area in effort here and for all LMEs 

# end result: 1 estimate by month of all inputs - one value only for depth 

# go to step 2 - batch create inputs. 








# get gridded GCM inputs for ISIMIP3b phase 1 protocol - from GFDL-ESM4, IPSL-CM6A-LR
# monthly time steps, so need to set up time-varying plankton input into code (as in Q_F, start with climatology, then apply dynamical forcing) 
# gridded values 1 by 1 degree lat/lon
# use parallel to do runs for a bunch of grids cell at the same time

# ------------------------------------------------------ 
# What inputs are needed from GCMS?
# Depends on method used to get the plankton size spectrum:

#  use Woodworth-Jefcoats 2013 GCB paper method:
# get small and large phytoplankton densities (if diazotroph density provided, add to large phyto),
# to get slope and intercept, also get median size of consumer and minimum size of phytoplankton everything else same as above

# --------------------------------------------------------

setwd('/Users/ryanheneghan 1/Desktop/Papers/FishMIP_CMIP6/')

### FishMIP Phase 1 protocols for ISIMIP3b,
# 1. picontrol, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')
# 2. historical, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')
# 3. ssp126, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')
# 4. ssp585, gcm = c('IPSL-CM6A-LR', 'GFDL-ESM4')

getGCM<-function(gcmPath = './inputs/', protocol, gcm = 'IPSL-CM6A-LR', savepath, getdepth = T, vers = 2){
  

    phydiat_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*phydiat-vint*', full.names = TRUE)
    phyc_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*phyc-vint*', full.names = TRUE)
    to_zb_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*thetao-bot*', full.names = TRUE)
    to_zs_file = list.files(path = paste(gcmPath, gcm, '/', protocol, '/', sep = ''), pattern = '*_tos_*', full.names = TRUE)

  
  #get large phy
  lphy <- var.get.nc(open.nc(phydiat_file), 'phydiat-vint')
  
  t <- var.get.nc(open.nc(phydiat_file), 'time')
  lon <- var.get.nc(open.nc(phydiat_file), 'lon')
  lat <- var.get.nc(open.nc(phydiat_file), 'lat')
  
  # Format lphy
  dimnames(lphy) <- list(lon=lon,lat=lat,t=t)
  pp <- melt(lphy)
  names(pp) <-  c("lon","lat","t","lphy")
  pp <- pp[!is.na(pp[,"lphy"]),]
  rm(list = ('lphy'))
  
  # sphy
  sphy <- var.get.nc(open.nc(phyc_file), 'phyc-vint')- var.get.nc(open.nc(phydiat_file), 'phydiat-vint')
  
  sphy <- as.vector(sphy)
  sphy <- sphy[!is.na(sphy)]
  pp$sphy <- sphy
  rm(list = ('sphy'))
  
  # bottom temperature
  to_zb <- var.get.nc(open.nc(to_zb_file), 'tob')
  
  to_zb <- as.vector(to_zb)
  to_zb <- to_zb[!is.na(to_zb)]
  pp$sbt <- to_zb
  rm(list = ('to_zb'))
  
  # Surface temperature
  to_zs <- var.get.nc(open.nc(to_zs_file), 'tos')

  to_zs <- as.vector(to_zs)
  to_zs <- to_zs[!is.na(to_zs)]
  pp$sst <- to_zs
  rm(list = ('to_zs'))
  
  
  # Standardise colnames
  names(pp) <- c("lon", "lat", "t", "lphy", "sphy", "sbt", "sst")

  
  if(getdepth == T){
  # get depth
  depth_nc <- open.nc(list.files(path = paste(gcmPath, gcm, '/', sep = ''), pattern = 'depth', full.names = TRUE))
  depth <- var.get.nc(depth_nc, 'depth') # Depth in metres
  dimnames(depth) <- list(lon=var.get.nc(depth_nc, 'lon'), lat=var.get.nc(depth_nc, 'lat'))
  depth <- melt(depth)
  names(depth) <- c("lon", "lat", "depth")
  depth$gridnum <- 1:length(depth[,1])
  # Remove land values (na and 0 depth)
  depth <- depth[!is.na(depth[,"depth"]),]
  depth <- depth[depth[,'depth'] != 0,]
  
  ## Save depth
  depth_save_name <- paste(savepath, '/', gcm, '/', gcm, "_depth.RData", sep = '')
  save(depth, file = depth_save_name, version = vers)
  }
  

  ## Save processed forcings
  print(paste('Now saving forcings for protocol ', protocol, sep = ''))
  pp_save_name <- paste(savepath, '/', gcm, '/',protocol, '/', gcm, "_", protocol, ".RData", sep = '')
  save(pp, file = pp_save_name, version = vers)
  
  #remove any objects no longer needed 
  if(getdepth == T){
  rm(pp, depth)
  }else{rm(pp)}
}

getGCM(gcmPath = './inputs/', protocol = 'picontrol', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = T, vers = 3)
getGCM(gcmPath = './inputs/', protocol = 'historical', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
getGCM(gcmPath = './inputs/', protocol = 'ssp126', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
getGCM(gcmPath = './inputs/', protocol = 'ssp585', gcm = 'IPSL-CM6A-LR', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)

getGCM(gcmPath = './inputs/', protocol = 'picontrol', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = T, vers = 3)
getGCM(gcmPath = './inputs/', protocol = 'historical', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
getGCM(gcmPath = './inputs/', protocol = 'ssp126', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)
getGCM(gcmPath = './inputs/', protocol = 'ssp585', gcm = 'GFDL-ESM4', savepath = "./DBPM/processed_forcings/", getdepth = F, vers = 3)



#-------------------------------------STEP 2: DISAGGREGATE TIME SERIES INPUTS FOR MODEL TO WEEKLY (OR DAILY) TIME STEPS


# CN Step 2 is now part of batch_run_create_inputs_ISIMIP3b.R. Ste p 1 above could also be moved to this file 

# rm(list=ls())
# #install.packages(pkgs = 'http://zoo.r-forge.r-project.org/', lib = './R_package_library')
# library(zoo)
# library(parallel)
# setwd('/Users/ryanheneghan 1/Desktop/Papers/FishMIP_CMIP6/DBPM')
# 
# esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
# scens <- c("picontrol", "historical", "ssp126", "ssp585")
# num_cells <- c(44564, 41328)
# 
# for(i in 1:length(esms)){
#   curr_esm <- esms[i]
#   for(j in 1:length(scens)){
#     curr_scen <- scens[j]
#     load(file=paste("./processed_forcings/", curr_esm, "/", curr_scen, "/", curr_esm, "_", curr_scen, ".RData", sep ="")) # load forcings
#     load(file=paste("./processed_forcings/", curr_esm, "/",  curr_esm, "_depth.RData", sep ="")) # load depth data
#     
#     source("./dbpm_CMIP6/getgridin.R")
#     
#     # set up cluster 
#     numcores= detectCores()-2
#     
#     # cl <- makeCluster(numcores,type="FORK",outfile='')
#     cl <- makeForkCluster(getOption("cl.cores", numcores))
#     
#     # grids to read in are sequential for the depth file
#     grids<-1:num_cells[i]
#     
#     # Running the model
#     ptm=proc.time()
#     options(warn=-1)
#     
#     clusterApply(cl,x=grids,fun=getgridin,curr_esm = curr_esm, curr_scen = curr_scen)
#     
#     print((proc.time()-ptm)/60.0)
#     
#     stopCluster(cl)
#     
#   }
#   
# }




