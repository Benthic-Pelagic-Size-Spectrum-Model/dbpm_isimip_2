
source("./dbpm_CMIP6/input_funcs.R") # Set to where you have put the input_funcs folder

getgridin <- function(igrid, curr_esm, curr_scen, save_path){
  curr_grid <- which(pp[,c("lat")]==depth[igrid,"lat"] & pp[,c("lon")]==depth[igrid,"lon"])
  
  if(length(curr_grid) > 0){
    ## Identify curr grid inputs
    gridinputs <- pp[curr_grid,]
    
    ## For current grid, calculate slope, intercept and export ratio (er)
    gridinputs$slope<-gridinputs$intercept<-gridinputs$er <- gridinputs[,7]
    
    gridinputs[,"er"] <- mapply(getExportRatio,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],sst=gridinputs[,"sst"],depth=depth[igrid,"depth"])
    gridinputs[which(gridinputs[,"er"]<0),"er"]<-0
    gridinputs[which(gridinputs[,"er"]>1),"er"]<-1
    
    gridinputs[,"intercept"]<-mapply(GetPPIntSlope,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="intercept")
    
    gridinputs[,"slope"]<-mapply(GetPPIntSlope,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="slope")
    
    gridinputs <- gridinputs[,c("t","sst","sbt","er","intercept","slope", "sphy", "lphy")]
    
    # write over t
    gridinputs$t<-seq(0.0, (dim(gridinputs)[1]-1),by=1)
    
    dep<-depth[igrid,]
    
    # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
    
    #weekly timesteps
    wts=data.frame(t=seq(0.0, (dim(gridinputs)[1]-1),by=0.25))
    
    wts<-merge(gridinputs,wts,by="t",all=T)
    
    #use na.approx in zoo package to fill in NAs (linear interpolation)
    fwts<-data.frame(na.approx(wts))
    
    # add spin up - change to 300 yrs
    #  spinup<-data.frame(sst=rep(mean(fwts$sst[1:12]),each=300*48),sbt=rep(mean(fwts$sbt[1:12]),each=300*48),
    #                    er=rep(mean(fwts$er[1:12]),each=300*48),intercept=rep(mean(fwts$intercept[1:12]),each=300*48),
    #                    slope=rep(mean(fwts$slope[1:12]),each=300*48))
    
    #  fwts <- rbind(spinup,fwts[,-1])
    
    
    inputs=list(depth=dep,ts=fwts)
    
  #  thepath <- paste('./processed_forcings/', curr_esm, '/', curr_scen, '/grid_forcings', sep = '')
    filename <- paste(save_path, "/",'grid_', igrid, '_', curr_esm,'_' ,curr_scen, '.rds', sep = '')
    saveRDS(inputs, file=filename, compress = FALSE)
    
    rm(inputs,fwts,wts,gridinputs,spinup)
  }
}

