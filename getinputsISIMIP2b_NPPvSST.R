rm(list=ls())
install.packages('RNetCDF', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
install.packages('reshape2', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')
install.packages('abind', lib = '/home/rhenegha/R_package_library', repos = 'https://cran.rstudio.com/')

library(RNetCDF, lib.loc = '/home/rhenegha/R_package_library') # For reading/manipulating netCDFs
library(reshape2, lib.loc = '/home/rhenegha/R_package_library')
library(abind, lib.loc = '/home/rhenegha/R_package_library')

# ---------------------------------- STEP 1: GET GCM INPUTS FOR DYNAMIC BENTHIC-PELAGIC SIZE SPECTRUM MODEL

# get gridded GCM inputs - from CESM1-BEC
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

setwd('/Users/ryanheneghan 1/Desktop/Papers/FishMIP_NPPvSST/DBPM_runs/')

### FishMIP nPP v SST protocol experiments:
# 1. All preindustrial (phyto_run = 'pi', temp_run = 'pi', protocol = 'pi')
# 2. nPP control (phyto_run = 'pi', temp_run = 'clim', protocol = 'npp-control')
# 3. SST control (phyto_run = 'clim', temp_run = 'pi', protocol = 'temperature-control')
# 4. All climate change (phyto_run = 'clim', temp_run = 'clim', protocol = 'clim')

getGCM<-function(gcmPath = './Inputs/CESM1-BEC', protocol, gcm = 'cesm', getdepth = T, vers = 2){
  
  ## Get file names
  if(protocol == 'pi'){
    lphy_files = list.files(path = paste(gcmPath, '/pi/lphy_zint', sep = ''), pattern = '*lphy_zint*', full.names = TRUE)
    sphy_files = list.files(path = paste(gcmPath, '/pi/sphy_zint', sep = ''), pattern = '*sphy_zint*', full.names = TRUE)
    dphy_files = list.files(path = paste(gcmPath, '/pi/dphy_zint', sep = ''), pattern = '*dphy_zint*', full.names = TRUE)
    to_zb_files = list.files(path = paste(gcmPath, '/pi/to_zb', sep = ''), pattern = '*to_zb*', full.names = TRUE)
    to_zs_files = list.files(path = paste(gcmPath, '/pi/to_zs', sep = ''), pattern = '*to_zs*', full.names = TRUE)
  }
  
  if(protocol == 'npp-control'){
    lphy_files = list.files(path = paste(gcmPath, '/pi/lphy_zint', sep = ''), pattern = '*lphy_zint*', full.names = TRUE)
    sphy_files = list.files(path = paste(gcmPath, '/pi/sphy_zint', sep = ''), pattern = '*sphy_zint*', full.names = TRUE)
    dphy_files = list.files(path = paste(gcmPath, '/pi/dphy_zint', sep = ''), pattern = '*dphy_zint*', full.names = TRUE)
    to_zb_files = c(list.files(path = paste(gcmPath, '/hist/to_zb', sep = ''), pattern = '*to_zb*', full.names = TRUE),
                    list.files(path = paste(gcmPath, '/rcp85/to_zb', sep = ''), pattern = '*to_zb*', full.names = TRUE))
    to_zs_files = c(list.files(path = paste(gcmPath, '/hist/to_zs', sep = ''), pattern = '*to_zs*', full.names = TRUE),
                    list.files(path = paste(gcmPath, '/rcp85/to_zs', sep = ''), pattern = '*to_zs*', full.names = TRUE))
  }
  
  if(protocol == 'temperature-control'){
    lphy_files = c(list.files(path = paste(gcmPath, '/hist/lphy_zint', sep = ''), pattern = '*lphy_zint*', full.names = TRUE),
                   list.files(path = paste(gcmPath, '/rcp85/lphy_zint', sep = ''), pattern = '*lphy_zint*', full.names = TRUE))
    sphy_files = c(list.files(path = paste(gcmPath, '/hist/sphy_zint', sep = ''), pattern = '*sphy_zint*', full.names = TRUE),
                   list.files(path = paste(gcmPath, '/rcp85/sphy_zint', sep = ''), pattern = '*sphy_zint*', full.names = TRUE))
    dphy_files = c(list.files(path = paste(gcmPath, '/hist/dphy_zint', sep = ''), pattern = '*dphy_zint*', full.names = TRUE),
                   list.files(path = paste(gcmPath, '/rcp85/dphy_zint', sep = ''), pattern = '*dphy_zint*', full.names = TRUE))
    to_zb_files = list.files(path = paste(gcmPath, '/pi/to_zb', sep = ''), pattern = '*to_zb*', full.names = TRUE)
    to_zs_files = list.files(path = paste(gcmPath, '/pi/to_zs', sep = ''), pattern = '*to_zs*', full.names = TRUE)
  }
  
  if(protocol == 'clim'){
    lphy_files = c(list.files(path = paste(gcmPath, '/hist/lphy_zint', sep = ''), pattern = '*lphy_zint*', full.names = TRUE),
                   list.files(path = paste(gcmPath, '/rcp85/lphy_zint', sep = ''), pattern = '*lphy_zint*', full.names = TRUE))
    sphy_files = c(list.files(path = paste(gcmPath, '/hist/sphy_zint', sep = ''), pattern = '*sphy_zint*', full.names = TRUE),
                   list.files(path = paste(gcmPath, '/rcp85/sphy_zint', sep = ''), pattern = '*sphy_zint*', full.names = TRUE))
    dphy_files = c(list.files(path = paste(gcmPath, '/hist/dphy_zint', sep = ''), pattern = '*dphy_zint*', full.names = TRUE),
                   list.files(path = paste(gcmPath, '/rcp85/dphy_zint', sep = ''), pattern = '*dphy_zint*', full.names = TRUE))
    to_zb_files = c(list.files(path = paste(gcmPath, '/hist/to_zb', sep = ''), pattern = '*to_zb*', full.names = TRUE),
                    list.files(path = paste(gcmPath, '/rcp85/to_zb', sep = ''), pattern = '*to_zb*', full.names = TRUE))
    to_zs_files = c(list.files(path = paste(gcmPath, '/hist/to_zs', sep = ''), pattern = '*to_zs*', full.names = TRUE),
                    list.files(path = paste(gcmPath, '/rcp85/to_zs', sep = ''), pattern = '*to_zs*', full.names = TRUE))
  }
  
  #get large phy (lphy + dphy)
  lphy <- var.get.nc(open.nc(lphy_files[1]), 'lphy') + var.get.nc(open.nc(dphy_files[1]), 'dphy')
  
  t <- var.get.nc(open.nc(lphy_files[1]), 'time')
  lon <- var.get.nc(open.nc(lphy_files[1]), 'lon')
  lat <- var.get.nc(open.nc(lphy_files[1]), 'lat')
  
  pb = txtProgressBar(min = 0, max = length(lphy_files), initial = 1, style = 3) # Initial progress bar
  
  print('Now adding lphy to dataframe')
  for(i in 2:length(lphy_files)){
  lphy_curr <- var.get.nc(open.nc(lphy_files[i]), 'lphy') + var.get.nc(open.nc(dphy_files[i]), 'dphy')
  lphy <- abind(lphy, lphy_curr)
  t <- c(t, var.get.nc(open.nc(lphy_files[i]), 'time'))
  
  setTxtProgressBar(pb, i) # Update progress bar
  }
  
  # Format lphy
  dimnames(lphy) <- list(lon=lon,lat=lat,t=t)
  pp <- melt(lphy)
  names(pp) <-  c("lon","lat","t","lphy")
  pp <- pp[!is.na(pp[,"lphy"]),]
  rm(list = ('lphy'))
  
  # sphy
  sphy <- var.get.nc(open.nc(sphy_files[1]), 'sphy')
  lon <- var.get.nc(open.nc(sphy_files[1]), 'lon')
  lat <- var.get.nc(open.nc(sphy_files[1]), 'lat')
  
  pb = txtProgressBar(min = 0, max = length(sphy_files), initial = 1, style = 3) # Initial progress bar
  
  print('Now adding sphy to dataframe')
  for(i in 2:length(sphy_files)){
    sphy <- abind(sphy, var.get.nc(open.nc(sphy_files[i]), 'sphy'))
    
    setTxtProgressBar(pb, i) # Update progress bar
  }
  

  sphy <- as.vector(sphy)
  sphy <- sphy[!is.na(sphy)]
  pp$sphy <- sphy
  rm(list = ('sphy'))
  
  # bottom temperature
  to_zb <- var.get.nc(open.nc(to_zb_files[1]), 'to')
  lon <- var.get.nc(open.nc(to_zb_files[1]), 'lon')
  lat <- var.get.nc(open.nc(to_zb_files[1]), 'lat')
  
  pb = txtProgressBar(min = 0, max = length(to_zb_files), initial = 1, style = 3) # Initial progress bar
  
  print('Now adding to_zb to dataframe')
  for(i in 2:length(to_zb_files)){
    to_zb <- abind(to_zb, var.get.nc(open.nc(to_zb_files[i]), 'to'))
    
    setTxtProgressBar(pb, i) # Update progress bar
  }
  
  to_zb <- as.vector(to_zb)
  to_zb <- to_zb[!is.na(to_zb)]
  pp$sbt <- to_zb
  rm(list = ('to_zb'))
  
  # Surface temperature
  to_zs <- var.get.nc(open.nc(to_zs_files[1]), 'to')
  lon <- var.get.nc(open.nc(to_zs_files[1]), 'lon')
  lat <- var.get.nc(open.nc(to_zs_files[1]), 'lat')
  
  print('Now adding to_zs to dataframe')
  pb = txtProgressBar(min = 0, max = length(to_zs_files), initial = 1, style = 3) # Initial progress bar
  
  for(i in 2:length(to_zs_files)){
    to_zs <- abind(to_zs, var.get.nc(open.nc(to_zs_files[i]), 'to'))
    
    setTxtProgressBar(pb, i) # Update progress bar
  }
  
  to_zs <- as.vector(to_zs)
  to_zs <- to_zs[!is.na(to_zs)]
  pp$sst <- to_zs
  rm(list = ('to_zs'))
  
  
  # Standardise colnames
  names(pp) <- c("lon", "lat", "t", "lphy", "sphy", "sbt", "sst")
  
  # Convert lphy and sphy units from mmol C/ m^2 to mol C/ m^2
  pp$lphy <- pp$lphy/1000
  pp$sphy <- pp$sphy/1000
  
  if(getdepth == T){
  # get depth
  depth_nc <- open.nc(paste(gcmPath, '/cesm_depth.nc4', sep = ''))
  depth <- var.get.nc(depth_nc, 'HT')/100 # Divide by 100 to go from cm to m
  dimnames(depth) <- list(lon=var.get.nc(depth_nc, 'lon'), lat=var.get.nc(depth_nc, 'lat'))
  depth <- melt(depth)
  names(depth) <- c("lon", "lat", "depth")
  depth$gridnum <- 1:length(depth[,1])
  # Remove land values (na and 0 depth)
  depth <- depth[!is.na(depth[,"depth"]),]
  depth <- depth[depth[,'depth'] != 0,]
  
  ## Save depth
  depth_save_name <- paste(gcmPath, '/processed_forcings/', gcm, "_depth", ".RData", sep = '')
  save(depth, file = depth_save_name, version = vers)
  }
  

  ## Save processed forcings
  print(paste('Now saving forcings for protocol ', protocol, sep = ''))
  pp_save_name <- paste(gcmPath, '/processed_forcings/', gcm, "_", protocol, ".RData", sep = '')
  save(pp, file = pp_save_name, version = vers)
  
  #remove any objects no longer needed 
  if(getdepth == T){
  rm(pp, depth)
  }else{rm(pp)}
}

getGCM(gcmPath = './Inputs/CESM1-BEC', protocol = 'pi', gcm = 'cesm', getdepth = T, vers = 3)
getGCM(gcmPath = './Inputs/CESM1-BEC', protocol = 'npp-control', gcm = 'cesm', getdepth = F, vers = 3)
getGCM(gcmPath = './Inputs/CESM1-BEC', protocol = 'temperature-control', gcm = 'cesm', getdepth = F, vers = 3)
getGCM(gcmPath = './Inputs/CESM1-BEC', protocol = 'clim', gcm = 'cesm', getdepth = F, vers = 3)





#-------------------------------------STEP 2: DISAGGREGATE TIME SERIES INPUTS FOR MODEL TO WEEKLY (OR DAILY) TIME STEPS

rm(list=ls())
install.packages(pkgs = 'http://zoo.r-forge.r-project.org/', lib = './R_package_library')
library(zoo)
library(parallel)
setwd('./DBPM_runs/')

load(file="./Inputs/CESM1-BEC/processed_forcings/cesm_depth.RData") # load depth data

prot = 'pi'
load(file=paste("./Inputs/CESM1-BEC/processed_forcings/cesm_", prot,".RData",sep=""))


source("./dbpm-ryan/getgridin.R")

# set up cluster 
numcores= detectCores()-1

# cl <- makeCluster(numcores,type="FORK",outfile='')
cl <- makeForkCluster(getOption("cl.cores", numcores))

# grids to read in are sequential for the depth file
grids<-1:200

# Running the model
ptm=proc.time()
options(warn=-1)

clusterApply(cl,x=grids,fun=getgridin)

print((proc.time()-ptm)/60.0)

stopCluster(cl)




# test

#run="rcp60"
#getgridin(igrid=1)


# read inputs for each grid cell and for each rcp scenario (only using gcm="ipsl-cm5a-lr")

gcm="ipsl-cm5a-lr"
run="rcp85"


load(file="/../../rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_historical.RData",sep=""))
pp1<-pp
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_",run,".RData",sep=""))


# set up cluster 
numcores= 12
# cl <- makeCluster(numcores,type="FORK",outfile='')
cl <- makeForkCluster(getOption("cl.cores", numcores))
#clusterExport(cl, as.list(ls()))
# clusterEvalQ(cl, library(zoo))

# grids to read in are sequential for the depth file

# grids<-2:1000

grids<-1:44037


# Running the model
ptm=proc.time()
options(warn=-1)


clusterApply(cl,x=grids,fun=getgridin)

print((proc.time()-ptm)/60.0)

stopCluster(cl)



gcm="ipsl-cm5a-lr"
run="rcp60"


load(file="/../../rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_historical.RData",sep=""))
pp1<-pp
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_",run,".RData",sep=""))


# set up cluster 
numcores= 12
# cl <- makeCluster(numcores,type="FORK",outfile='')
cl <- makeForkCluster(getOption("cl.cores", numcores))
#clusterExport(cl, as.list(ls()))
# clusterEvalQ(cl, library(zoo))

# grids to read in are sequential for the depth file

# grids<-2:1000

grids<-1:39567


# Running the model
ptm=proc.time()
options(warn=-1)


clusterApply(cl,x=grids,fun=getgridin)

print((proc.time()-ptm)/60.0)

stopCluster(cl)


gcm="ipsl-cm5a-lr"
run="rcp26"


load(file="/../../rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_historical.RData",sep=""))
pp1<-pp
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_",run,".RData",sep=""))


# set up cluster 
numcores= 12
# cl <- makeCluster(numcores,type="FORK",outfile='')
cl <- makeForkCluster(getOption("cl.cores", numcores))
#clusterExport(cl, as.list(ls()))
# clusterEvalQ(cl, library(zoo))

# grids to read in are sequential for the depth file

# grids<-2:1000

grids<-1:39567


# Running the model
ptm=proc.time()
options(warn=-1)


clusterApply(cl,x=grids,fun=getgridin)

print((proc.time()-ptm)/60.0)

stopCluster(cl)


gcm="ipsl-cm5a-lr"
run="rcp45"


load(file="/../../rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_historical.RData",sep=""))
pp1<-pp
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_",run,".RData",sep=""))


# set up cluster 
numcores= 12
# cl <- makeCluster(numcores,type="FORK",outfile='')
cl <- makeForkCluster(getOption("cl.cores", numcores))
#clusterExport(cl, as.list(ls()))
# clusterEvalQ(cl, library(zoo))

# grids to read in are sequential for the depth file

# grids<-2:1000

grids<-1:39567


# Running the model
ptm=proc.time()
options(warn=-1)


clusterApply(cl,x=grids,fun=getgridin)

print((proc.time()-ptm)/60.0)

stopCluster(cl)