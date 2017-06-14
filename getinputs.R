rm(list=ls())
library(ncdf)
library(reshape2)

# ---------------------------------- STEP 1: GET GCM INPUTS FOR DYNAMIC BENTHIC-PELAGIC SIZE SPECTRUM MODEL

# get gridded GCM inputs - from GFDL-ESM2M and IPSL (actually only IPSL at the moment because each GCM seems to have different outputs, see getinputs_GFDL-R.r for other model)
# monthly time steps, so need to set up time-varying plankton input into code (as in Q_F, start with climatology, then apply dynamical forcing) 
# gridded values 1 by 1 degree lat/lon
# use parallel to do runs for a bunch of grids cell at the same time


# think about- using annual means instead? how to get daily values for small dt

# ------------------------------------------------------ 
# What inputs are needed from GCMS?
# Depends on method used to get the plankton size spectrum:

# 1) use Barnes et al method: pp, sst (to get size spectrum slopes), surface and bottom temperatures, depth (to get er) 
# intpp = spp + lpp 
# where is depth?

# 2) use Woodworth-Jefcoats 2013 GCB paper method:
# get small and large phytoplankton densities, to get slope and intercept, also get median size of consumer and minimum size of phytoplankton everything else same as above
# add option to inlcude small and large zooplankton desnities ( as in Quest-fish work) ?

# THIS CODE DOES METHOD 2 - see getinputs_intpp.R for METHOD 1 


# --------------------------------------------------------

getGCM<-function(gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/',run = 'historical',gcm = 'ipsl-cm5a-lr',tme = "_monthly_195001_200512.nc4",getdepth="F"){
  
  
  # gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/'
  # run = 'historical'
  # gcm = 'ipsl-cm5a-lr'
  # tme = "_monthly_195001_200512.nc4"
  # getdepth = "F"   
  
  #get large phy 
  
  # inFile <-c(paste(gcmPath,run,"/","lphy_",gcm,"_",run,"_zint",tme,sep=""))
  inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_lphy_zint",tme,sep=""))
  nc = open.ncdf(inFile, write=FALSE)
  # lphy = get.var.ncdf(nc,'intpp')
  if (run=="historical"){
    lphy = get.var.ncdf(nc,'LPHY_ZINT')
    lon <- get.var.ncdf(nc,'LONGITUDE')
    lat <- get.var.ncdf(nc,'LATITUDE')
    t <- get.var.ncdf(nc,'TIME')
  } 
  
  if (run!="historical"){
    lphy = get.var.ncdf(nc,'lphy')
    lon <- get.var.ncdf(nc,'longitude')
    lat <- get.var.ncdf(nc,'latitude')
    t <- get.var.ncdf(nc,'time')
  } 
  
  dimnames(lphy)<-list(lon=lon,lat=lat,t=t)
  lphy<-melt(lphy)
  names(lphy)<-c("lon","lat","t","lphy")
  lphy<-lphy[!is.na(lphy[,"lphy"]),]
  
  
  #get small phy density
  # inFile <-c(paste(gcmPath,run,"/","sphy_",gcm,"_",run,tme,sep=""))
  
  inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_sphy_zint",tme,sep=""))
  sphy = get.var.ncdf(open.ncdf(inFile, write=FALSE),'sphy')
  # sphy = get.var.ncdf(open.ncdf(inFile, write=FALSE),'intpp')
  dimnames(sphy)<-list(lon=lon,lat=lat,t=t)
  sphy<-melt(sphy)
  names(sphy)<-c("lon","lat","t","sphy")
  sphy<-sphy[!is.na(sphy[,"sphy"]),]
  
  
  # 
  # #get small zooplankton density
  # 
  # inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_szoo_zint",tme,sep=""))
  # szoo = get.var.ncdf(open.ncdf(inFile, write=FALSE),'szoo')
  # dimnames(szoo)<-list(lon=lon,lat=lat,t=t)
  # szoo<-melt(szoo)
  # names(szoo)<-c("lon","lat","t","szoo")
  # szoo<-szoo[!is.na(szoo[,"szoo"]),]
  # 
  # #get large zooplankton density
  # inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_lzoo_zint",tme,sep=""))
  # lzoo = get.var.ncdf(open.ncdf(inFile, write=FALSE),'lzoo')
  # dimnames(lzoo)<-list(lon=lon,lat=lat,t=t)
  # lzoo<-melt(lzoo)
  # names(lzoo)<-c("lon","lat","t","lzoo")
  # lzoo<-lzoo[!is.na(lzoo[,"lzoo"]),] 
  
  # if GFDL get diazphy 
  
  if (gcm=="gfdl") {
    inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_diaz_zint",tme,sep=""))
    diaz = get.var.ncdf(open.ncdf(inFile, write=FALSE),'intpp')
    dimnames(diaz)<-list(lon=lon,lat=lat,t=t)
    diaz<-melt(diaz)
    names(diaz)<-c("lon","lat","t","diaz")
    diaz<-diaz[!is.na(diaz[,"diaz"]),]
    
  }
  
  # Standardized forcing (Mandatory):
  #   1) Total primary production (and therefore intpp) should include all primary producers (e.g. diazotrophs, large phytoplankton and small phytoplankton in the GFDL model; large and small phytoplankton in the IPSL model).
  # Thus, total intpp = intpp_lphy + intpp_sphy + intpp_diaz
  # 2) Large phytoplankton biomass (lphy) should be the sum of the diazotroph (diaz) biomass and the large phytoplankton biomass in the GFDL model.
  # Thus, lphy = lphy + diaz
  # 3) Small phytoplankton biomass (sphy) should be just the small phytoplankton biomass
  # 
  
  
  
  #get sst
  
  inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_to_zs",tme,sep=""))
  
  sst = get.var.ncdf(open.ncdf(inFile, write=FALSE),'to')
  
  dimnames(sst)<-list(lon=lon,lat=lat,t=t)
  sst<-melt(sst)
  names(sst)<-c("lon","lat","t","sst")
  sst<-sst[!is.na(sst[,"sst"]),]
  # convert to celsius
  sst <- sst - 273.15
  
  # get sbt
  
  inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_to_zb",tme,sep=""))
  sbt = get.var.ncdf(open.ncdf(inFile, write=FALSE),'to')
  dimnames(sbt)<-list(lon=lon,lat=lat,t=t)
  sbt<-melt(sbt)
  names(sbt)<-c("lon","lat","t","sbt")
  sbt<-sbt[!is.na(sbt[,"sbt"]),]
  # convert to celsius
  sbt <- sbt - 273.15
  
  
  
  # get depth
  # is this the correct file for the mean depth for each grid cell?
  if (getdepth=="T"){
    inFile <-c(paste(gcmPath,"misc/deptho_fx_IPSL-CM5A-LR_1.0deg.nc4",sep=""))
    depth = get.var.ncdf(open.ncdf(inFile, write=FALSE),'deptho')
    dimnames(depth)<-list(lon=lon,lat=lat)
    depth<-melt(depth)
    names(depth)<-c("lon","lat","depth")
    depth$gridnum<-1:length(depth[,1])
    depth<-depth[!is.na(depth[,"depth"]),]
  }
  
  if (getdepth=="F"){
    load(file="/../../rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")
  }
  
  
  # combine inputs
  
  
  temp<-cbind(sst,sbt[,4])
  pp<-cbind(sphy,lphy[,4],temp[,c(4,5)])
  names(pp)<-c("lon","lat","t","sphy","lphy","sst","sbt")
  
  # the next two steps call functions that convert the above to inputs needed for the size-based model
  
  pp$slope<-pp$intercept<-pp$er <- pp[,7]
  
  #calculate intercept and slope and export ratio based on these inputs
  
  source('~/GlobalModel/size-based-models/Input/export_ratio.R', chdir = TRUE)
  source('~/GlobalModel/size-based-models/R/dynamic_sizebased_model_functions.R', chdir = TRUE)
  
  pp[,"er"]<-mapply(getExportRatio,sphy=sphy[,"sphy"],lphy=lphy[,"lphy"],sst=sst[,"sst"],depth=depth[,"depth"])
  # truncate er values to be [0,1]
  pp[which(pp[,"er"]<0),"er"]<-0
  pp[which(pp[,"er"]>1),"er"]<-1
  
  pp[,"intercept"]<-mapply(GetPPIntSlope,sphy=pp[,"sphy"],lphy=pp[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[,"depth"],output="intercept")
  
  pp[,"slope"]<-mapply(GetPPIntSlope,sphy=pp[,"sphy"],lphy=pp[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[,"depth"],output="slope")
  
  
  save(pp,file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_",run,".RData",sep=""))
  
  if (getdepth=="T"){
    save(depth,file=paste("/../../rd/gem/private/fishmip_inputs/depth_",gcm,run,".RData",sep=""))
  }
  
  #remove any objects no longer needed 
  
  rm(sbt,sst,lpp,spp,nc,temp,lat,lon,t,sphy,lphy)
  
}

# redo above for all GCMs and CMIP5/ renalysis needed (see Fishmip simulation protocal) model inputs required 

# Climate scenarios: 
# - Historical runs: 1 re-analysis product & IPSL hindcast; Next: GDFL & CESM hindcasts
# -	Future runs: Priority IPSL 2.6 & 8.5; Next GFDL & CESM 8.5; Next IPSL 4.5 & 6.0
# Fishing scenarios: 
# -	Historical runs: Priority (default): use time-varying effort; Next (unfished): zero fishing effort/mortality
# -	Future runs: Priority (default): keep fishing constant at 2005 levels; Next (unfished): continue historical unfished (zero fishing effort/mortality) run into future


#IPSL:


getGCM(gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/',run = 'historical',gcm = 'ipsl-cm5a-lr',tme = "_monthly_195001_200512.nc4",getdepth="T")
getGCM(gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/',run = 'rcp26',gcm = 'ipsl-cm5a-lr',tme = "_monthly_200601_21001231.nc4",getdepth="F")
getGCM(gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/',run = 'rcp85',gcm = 'ipsl-cm5a-lr',tme = "_monthly_200601_21001231.nc4",getdepth="F")
getGCM(gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/',run = 'rcp45',gcm = 'ipsl-cm5a-lr',tme = "_monthly_200601_21001231.nc4",getdepth="F")
getGCM(gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/',run = 'rcp60',gcm = 'ipsl-cm5a-lr',tme = "_monthly_200601_21001231.nc4",getdepth="F")



#-------------------------------------STEP 2: DISAGGREGATE TIME SERIES INPUTS FOR MODEL TO WEEKLY (OR DAILY) TIME STEPS

rm(list=ls())

library(zoo)
library(parallel)

# read inputs for each grid cell 

gcm="ipsl-cm5a-lr"
# run="historical"
run="rcp85"
getdepth="F"

if (getdepth=="F") load(file="/../../rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")
if (getdepth=="T") load(file=paste("/../../rd/gem/private/fishmip_inputs/depth_",gcm,"_",run,".RData",sep=""))

# load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinput_",gcm,"_",run,".RData",sep=""))
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_historical.RData",sep=""))
pp1<-pp
load(file=paste("/../../rd/gem/private/fishmip_inputs/ppinputdepth100_",gcm,"_",run,".RData",sep=""))


# function to extract monthly time series and convert to weekly (or daily) then save the inputs, for each grid cell separately

getgridin<-function(igrid){
  
  gridinputs1<-pp1[which(pp1[,c("lat")]==depth[igrid,"lat"] & pp1[,c("lon")]==depth[igrid,"lon"]),c("t","sst","sbt","er","intercept","slope")]
  gridinputs<-pp[which(pp[,c("lat")]==depth[igrid,"lat"] & pp[,c("lon")]==depth[igrid,"lon"]),c("t","sst","sbt","er","intercept","slope")]
  
  # merge historical and rcp run
  gridinputs<-rbind(gridinputs1,gridinputs)
  
  # write over t
  gridinputs$t<-seq(0.0, (dim(gridinputs)[1]-1),by=1)
  
  dep<-depth[igrid,]
  
  # in gridinputs there are 5 inputs that will change thru time:sst,sft,er,intercept and slope 
  
  #weekly timesteps
  wts=data.frame(t=seq(0.0, (dim(gridinputs)[1]-1),by=0.25))
  
  wts<-merge(gridinputs,wts,by="t",all=T)
  
  #use na.approx in zoo package to fill in NAs (linear interpolation)
  
  fwts<-data.frame(na.approx(wts))
  
  # if (run=="historical"|run=="reanalysis") {
  
  # add spin up - change to 300 yrs
  
  #spinup<-data.frame(sst=rep(mean(fwts$sst),each=300*48),sbt=rep(mean(fwts$sbt),each=300*48),er=rep(mean(fwts$er),each=300*48),intercept=rep(mean(fwts$intercept),each=300*48),slope=rep(mean(fwts$slope),each=300*48))
  
  spinup<-data.frame(sst=rep(mean(gridinputs1$sst),each=300*48),sbt=rep(mean(gridinputs1$sbt),each=300*48),er=rep(mean(gridinputs1$er),each=300*48),intercept=rep(mean(gridinputs1$intercept),each=300*48),slope=rep(mean(gridinputs1$slope),each=300*48))
  
  
  fwts <- rbind(spinup,fwts[,-1])
  
  
  # }
  
  inputs=list(depth=dep,ts=fwts)
  
  save(inputs,file=paste("/../../rd/gem/private/fishmip_inputs/grid_",igrid,"_inputs2_",gcm,"_",run,".RData",sep=""))
  
  rm(inputs,fwts,wts,gridinputs1,gridinputs,spinup)
  
}

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

grids<-1:39567


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