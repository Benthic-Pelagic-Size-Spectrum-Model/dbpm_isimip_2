rm(list=ls())

library(parallel)

source('runmodel.R')
source('runmodel_helpers.R')

#30/7/17 VEN-482 fishmip rcp85 run, 
#whole world: all 1:39567 cells
#fishmip_ouputs/20170726_rcp85/
ptm=proc.time()
options(warn=-1) #?

numcores=47
cl <- makeForkCluster(getOption("cl.cores", numcores))
clusterApplyLB(cl
               ,x=12486:39567
               ,fun=rungridsep
               ,gcm="ipsl-cm5a-lr"
               ,run="rcp85"
               ,output="aggregated"
               ,output_files_location = "/rd/gem/private/fishmip_outputs/20170730_rcp85/")
stopCluster(cl)

print((proc.time()-ptm)/60.0)

rm(cl, numcores, ptm)


#26/7/17 VEN-466 fishmip rcp60 run, 
#whole world: all 1:39567 cells
#fishmip_ouputs/20170726_rcp60/
ptm=proc.time()
options(warn=-1) #?

numcores=47
cl <- makeForkCluster(getOption("cl.cores", numcores))
clusterApplyLB(cl
               ,x=1698:39567
               ,fun=rungridsep
               ,gcm="ipsl-cm5a-lr"
               ,run="rcp60"
               ,output="aggregated"
               ,output_files_location = "/rd/gem/private/fishmip_outputs/20170726_rcp60/")
stopCluster(cl)

print((proc.time()-ptm)/60.0)

rm(cl, numcores, ptm)





## Test run the model
ptm=proc.time()
options(warn=-1)
rungridsep(igrid=1,gcm="ipsl-cm5a-lr",run="rcp85",output="aggregated", output_files_location = "/rd/gem/private/justtest_fishmip_ouputs/")
print((proc.time()-ptm)/60.0)
# igrid=1
# gcm="ipsl-cm5a-lr"
# run="rcp45"
# load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",igrid,"_",gcm,"_",run,".RData",sep=""))
# print((proc.time()-ptm)/60.0)
# user   system  elapsed 
# 3.471900 0.006850 3.477017 
# > print((proc.time()-ptm)/60.0) without slopes estimated
# user      system     elapsed 
# 3.117400000 0.005783333 3.123183333 
# not run for 
#  igrid=318 - need to check if have run model and inputs for this gridcell
#  igrid=29115 - need to check if have run model and inputs for this gridcell
#  igrid=32779 - need to check if have run model and inputs for this gridcell
#  igrid=32780 - need to check if have run model and inputs for this gridcell
#  igrid=32902 - need to check if have run model and inputs for this gridcell

#————————— need to do above using parallel computing to do for all grid locations 

#set up runs 
# set up cluster

# numcores <- detectCores() - 1

#testing just

ptm=proc.time()
options(warn=-1) #?

numcores=47
cl <- makeForkCluster(getOption("cl.cores", numcores))
clusterApplyLB(cl, x=1:50, fun=rungridsep, gcm="ipsl-cm5a-lr", run="rcp60"
               ,output="aggregated"
               ,output_files_location = "/rd/gem/private/justtest_fishmip_ouputs/")
stopCluster(cl)

print((proc.time()-ptm)/60.0)

rm(cl, numcores, ptm)

rungridsep(igrid = 24, gcm="ipsl-cm5a-lr", run="rcp60"
           ,output="aggregated"
           ,output_files_location = "/rd/gem/private/justtest_fishmip_ouputs/")

#cl <- makeCluster(numcores,type="FORK")
# ------------------------------
# 
#
# setup the grid cells to run model over - split into chunks to save memory
run="rcp85"
gcm="ipsl-cm5a-lr"
grids <- 1:39567
clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp85")



run="rcp60"
gcm="ipsl-cm5a-lr"
grids <- 1:39567
clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp60")

stopCluster(cl)




# 
# clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp45")

# run for missed gird cells
run="rcp85"
gcm="ipsl-cm5a-lr"
missed<-NA
for(i in 1:39567)
{
  cc <- try(load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",i,"_",gcm,"_",run,".RData",sep="")), silent=T) 
  if(is(cc,"try-error")|!is(cc,"try-error") && dim(agg)[1] < 5000 )
  {
    missed<-append(missed,i)
    next
  } 
}


grids <- missed[-1]
# grids <- 9867:9873
# 
clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp26")


run="rcp85"
missed<-NA
for(i in 1:39567)
{
  cc <- try(load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",i,"_",gcm,"_",run,".RData",sep="")), silent=T) 
  if(is(cc,"try-error")|!is(cc,"try-error") && dim(agg)[1] < 5000 )
  {
    missed<-append(missed,i)
    next
  } 
}


grids <- missed[-1]
# grids <- 9867:9873
# 
clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp85")

run="rcp60"
missed<-NA
for(i in 1:39567)
{
  cc <- try(load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",i,"_",gcm,"_",run,".RData",sep="")), silent=T) 
  if(is(cc,"try-error")|!is(cc,"try-error") && dim(agg)[1] < 5000 )
  {
    missed<-append(missed,i)
    next
  } 
}

grids <- missed[-1]
# grids <- 9867:9873
# 
clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp60")




# grids <- 5001:10000
# 
# clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp26")
# 
# 
# grids <- 10001:39567
# 
# clusterApplyLB(cl,x=grids,fun=rungridsep,gcm="ipsl-cm5a-lr",run="rcp26")
# 


stopCluster(cl)

# load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",1,"_",gcm,"_",run,".RData",sep=""))

# save(resout,file=paste("/../../rd/gem/private/fishmip_outputs/resout_",gcm,"_",run,".RData",sep=""))

# 

# 

# older stuff


# 
# # # set up big list of arrays to store model outputs, check gcm is the right one.
# # 
# # # gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/'
# # # gcmPath = '~/GCM_INPUT/IPSL_CM5A_LR/'
# # # gcm = 'ipsl-cm5a-lr_'
# # # 
# # # 
# # # inFile <-c(paste(gcmPath,'rcp85',"/",gcm,'rcp85',"_lpp_zint","_monthly_200601_21001231.nc4",sep=""))
# # # nc = open.ncdf(inFile, write=FALSE)
# # # 
# # # lon <- get.var.ncdf(nc,'longitude')
# # # lat <- get.var.ncdf(nc,'latitude')
# # # t2 <- get.var.ncdf(nc,'time')
# # # 
# # # 
# # # rm(nc)
# # # 
# # # inFile <-c(paste(gcmPath,"historical","/",gcm,"historical","_lpp_zint","_monthly_195001_200512.nc4",sep=""))
# # # nc = open.ncdf(inFile, write=FALSE)
# # # 
# # # t1 <- get.var.ncdf(nc,'TIME')
# # # 
# # # t <- c(t1,t2+671)
# # # 
# # # rm(nc,t1,t2)
# # # 
# # # # subset time to be months from  01- 1971 to 12-2100 
# # # 
# # # # Gridded outputs for each run 
# # # resout<-list( 
# # #   # TOTAL system biomass density (tsb),g C m-2,all primary producers and consumers
# # #   tsc=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # #   # TOTAL consumer biomass density (tbc),g C m-2, all consumers (trophic level >1, vertebrates and invertebrates)           
# # #   tbc=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # # #   # Biomass density of consumers >10cm  (10 grams) 
# # # #   tbc10=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # # #   # Biomass density of consumers >30cm   (270 grams)                             
# # # #   tbc30=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),            
# # #   # Total catch of all commerical groups, g m-2 wet weight, all consumers
# # #   tc=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # #   # Biomass density (by functional group / size class) (Bi),g C m-2,Provide name of each size class (<class>) and functional group (<group>) used, and provide a  definition of each class/group 
# # #   b_10_pelpred=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)), 
# # #   b_30_pelpred=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # #   b_10_bendet=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # #   b_30_bendet=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),  
# # #   b_det=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),  
# # #   # Catch (by functional group / size class) (Ci) ,g  m-2 wet weight
# # #   c_10_pelpred=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)), 
# # #   c_30_pelpred=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)), 
# # #   c_10_bendet=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # #   c_30_bendet=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)),
# # #   # Size spectrum slopes 
# # #   bss_pelpred=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)), 
# # #   bss_bendet=array(NA,dim=c(length(lon),length(lat),length(t)),dimnames=list(lon,lat,t)) 
# # # )
# # # 
# # # 
# # 
# # 
# # # ----------------------------------SIMULATE DYNAMIC BENTHIC-PELAGIC SIZE SPECTRUM MODEL
# # 
# # #function to run model one grid cell at a time, later on call this function to do a massive parallel job
# # 
# # rungrid<-function(igrid=1,gcm="ipsl-cm5a-lr",run="rcp85",output="aggregated"){
# #   
# # # we need to run the model with time-varying inputs   
# # # the largest dt of the model is a monthly time step (Q-F was daily...which may still be needed) 
# # # once we have all of the inputs, we need to set the model up on an appropriate time step. the output may be too large a time step
# # # checked this: model can run on a weekly timestep, but not monthly
# # 
# #   
# # # how long does it take to set up and run model on one grid cell for whole (disaggregated) time series?
# #   
# # # ptm=proc.time()
# # # options(warn=-1)
# # 
# # if (gcm != "reanalysis") {
# #   load(paste("/../../rd/gem/private/fishmip_inputs/grid_",igrid,"_inputs_",gcm,"_historical.RData",sep=""))
# #   hist_inputs<-inputs
# #   load(paste("/../../rd/gem/private/fishmip_inputs/grid_",igrid,"_inputs_",gcm,"_",run,".RData",sep=""))
# #   inputs$ts <- rbind(hist_inputs$ts,inputs$ts[,-1])
# # #   inputs$ts <- rbind(hist_inputs$ts,inputs$ts[4801:dim(inputs$ts)[1],])
# #   rm(hist_inputs)  
# # }
# #   
# # if (gcm =="reanalysis"){  
# #   load(paste("/../../rd/gem/private/fishmip_inputs/grid_",igrid,"_inputs_",gcm,"_",run,".RData",sep=""))
# #   
# # }
# # 
# # 
# # #get params 
# # params<-sizeparam(equilibrium=T,dx=0.1,xmin.consumer.u=-3,xmin.consumer.v=-3,tmax=dim(inputs$ts)[1]/48,tstepspryr = 48,fmort.u=0.0, fminx.u=0,fmort.v=0.0, fminx.v=-1,depth=inputs$depth$depth, er=inputs$ts$er,pp = inputs$ts$intercept,slope=inputs$ts$slope,sst=inputs$ts$sst,sft=inputs$ts$sbt,lat=inputs$depth$lat,lon=inputs$depth$lon)     
# # 
# # res<-sizemodel(params=params,ERSEM.det.input=F,U_mat=NULL,V_mat=NULL,W_mat=NULL,temp.effect=T) 
# # 
# # # print((proc.time()-ptm)/60.0)
# # 
# # # it takes 0.5637 minutes to run the model, although it takes a long time to do the time series disaggregation, may wnat to work those up first?
# # 
# # #  get fish-mip outputs - these are aggregated by time and size classes
# # 
# # if (output=="aggregated") {
# #   
# #   
# #   #  extract these into monthly time series
# #   
# #   isave<-seq(4801,((dim(inputs$ts)[1])+1),4)
# #   
# #   #sum biomass (need to convert to g ww per m^3, across size classes) 
# #   
# #   # then need to convert to g C per m^2 (multiply by depth at end)
# #   
# #   # conversion used is:  0.0352 g C = 1 g wet weight
# #   
# #   #total biomass in functional groups
# #   
# # 
# # # TOTAL system biomass density (tsb),g C m-2,all primary producers and consumers
# # resout$tsc[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- 0.0352*apply(res$U[1:params$Nx,isave]*params$dx*10^params$x[1:params$Nx],2,sum) +
# #                                                           0.0352*apply(res$V[params$ref.det:params$Nx,isave]*params$dx*10^params$x[params$ref.det:params$Nx],2,sum) +
# #                                                           0.0352*res$W[isave]
# #   
# # 
# # # TOTAL consumer biomass density (tbc),g C m-2, all consumers (trophic level >1, vertebrates and invertebrates)           
# # resout$tsc[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- 0.0352*apply(res$U[params$ref:params$Nx,isave]*params$dx*10^params$x[params$ref:params$Nx],2,sum) +
# #                                                           0.0352*apply(res$V[params$ref.det:params$Nx,isave]*params$dx*10^params$x[params$ref.det:params$Nx],2,sum) 
# # 
# # # Biomass in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
# #   
# #   wcut<-c(10,270,1000,10000)
# #   
# #   xcutref<-wcut
# #   
# #   for (i in 1:length(wcut)) xcutref[i] = (min(which(params$x >=log10(wcut[i]))))
# #     
# # 
# # # Biomass density (by functional group / size class) (Bi),g C m-2,Provide name of each size class (<class>) and functional group (<group>) used, and provide a  definition of each class/group 
# # 
# # resout$b_10_pelpred[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- 0.0352*apply(res$U[xcutref[1]:params$Nx,isave]*params$dx*10^params$x[xcutref[1]:params$Nx],2,sum) 
# # resout$b_30_pelpred[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- 0.0352*apply(res$U[xcutref[2]:params$Nx,isave]*params$dx*10^params$x[xcutref[2]:params$Nx],2,sum) 
# # resout$b_10_bendet[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- 0.0352*apply(res$V[xcutref[1]:params$Nx,isave]*params$dx*10^params$x[xcutref[1]:params$Nx],2,sum)
# # resout$b_30_bendet[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- 0.0352*apply(res$V[xcutref[2]:params$Nx,isave]*params$dx*10^params$x[xcutref[2]:params$Nx],2,sum) 
# # resout$b_det[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- 0.0352*res$W[isave]
# # 
# #   #total catches in functional groups
# #   
# #   #sum catches ( already in grams per yr, across size classes) 
# #   #and then they need to be converted to g ww per m^2 
# #   
# # # Total catch of all commerical groups, g m-2 wet weight, all consumers
# # resout$tc[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- apply(res$Y.u[params$ref:params$Nx,isave]*params$dx,2,sum) +
# #                                                          apply(res$Y.v[params$ref.det:params$Nx,isave]*params$dx,2,sum) 
# # 
# #   #catches in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
# #   
# # 
# # # Catch (by functional group / size class) (Ci) ,g  m-2 wet weight
# # resout$c_10_pelpred[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- apply(res$Y.u[xcutref[1]:params$Nx,isave]*params$dx,2,sum) 
# # resout$c_30_pelpred[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- apply(res$Y.u[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
# # resout$c_10_bendet[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- apply(res$Y.v[xcutref[1]:params$Nx,isave]*params$dx,2,sum)
# # resout$c_30_bendet[paste(res$param$lon),paste(res$param$lat),1:length(isave)] <- apply(res$Y.v[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
# #   
# # # Size spectrum slopes 
# # 
# # # normalised  - plot of log (biomass density/arithmetic width of size bin) vs log size class
# # # pareto - use method as in Rogers et al 2014
# # 
# # 
# # # resout$bss_pelpred[paste(res$param$lon),paste(res$param$lat),]
# # # resout$bss_bendet[paste(res$param$lon),paste(res$param$lat),]
# #   
# # 
# # }
# # 
# # if (output!="aggregated") {
# # save(res,file=paste("/../../rd/gem/private/fishmip_outputs/res_wts_igrid_",igrid,"_",gcm,"_",run,".RData",sep=""))
# # }
# # 
# # # return(res)
# # #  end rungrid function
# # rm(params,res,inputs)
# # 
# # }
# 
# 
