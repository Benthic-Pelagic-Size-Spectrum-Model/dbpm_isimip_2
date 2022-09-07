
source("input_funcs.R") # Set to where you have put the input_funcs folder

# # function ued for CMIP63b
# getgridin <- function(igrid, curr_esm, curr_scen, save_path){
#   curr_grid <- which(pp[,c("lat")]==depth[igrid,"lat"] & pp[,c("lon")]==depth[igrid,"lon"])
#   
#   if(length(curr_grid) > 0){
#     ## Identify curr grid inputs
#     gridinputs <- pp[curr_grid,]
#     
#     ## For current grid, calculate slope, intercept and export ratio (er)
#     gridinputs$slope<-gridinputs$intercept<-gridinputs$er <- gridinputs[,7]
#     
#     gridinputs[,"er"] <- mapply(getExportRatio,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],sst=gridinputs[,"sst"],depth=depth[igrid,"depth"])
#     gridinputs[which(gridinputs[,"er"]<0),"er"]<-0
#     gridinputs[which(gridinputs[,"er"]>1),"er"]<-1
#     
#     gridinputs[,"intercept"]<-mapply(GetPPIntSlope,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="intercept")
#     
#     gridinputs[,"slope"]<-mapply(GetPPIntSlope,sphy=gridinputs[,"sphy"],lphy=gridinputs[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="slope")
#     
#     gridinputs <- gridinputs[,c("t","sst","sbt","er","intercept","slope", "sphy", "lphy")]
#     
#     # write over t
#     gridinputs$t<-seq(0.0, (dim(gridinputs)[1]-1),by=1)
#     
#     dep<-depth[igrid,]
#     
#     # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
#     
#     #weekly timesteps
#     wts=data.frame(t=seq(0.0, (dim(gridinputs)[1]-1),by=0.25))
#     
#     wts<-merge(gridinputs,wts,by="t",all=T)
#     
#     #use na.approx in zoo package to fill in NAs (linear interpolation)
#     fwts<-data.frame(na.approx(wts))
#     
#     # add spin up - change to 300 yrs
#     #  spinup<-data.frame(sst=rep(mean(fwts$sst[1:12]),each=300*48),sbt=rep(mean(fwts$sbt[1:12]),each=300*48),
#     #                    er=rep(mean(fwts$er[1:12]),each=300*48),intercept=rep(mean(fwts$intercept[1:12]),each=300*48),
#     #                    slope=rep(mean(fwts$slope[1:12]),each=300*48))
#     
#     #  fwts <- rbind(spinup,fwts[,-1])
#     
#     
#     inputs=list(depth=dep,ts=fwts)
#     
#   #  thepath <- paste('./processed_forcings/', curr_esm, '/', curr_scen, '/grid_forcings', sep = '')
#     filename <- paste(save_path, "/",'grid_', igrid, '_', curr_esm,'_' ,curr_scen, '.rds', sep = '')
#     saveRDS(inputs, file=filename, compress = FALSE)
#     
#     rm(inputs,fwts,wts,gridinputs,spinup)
#   }
# }

# CN function used for CMIP63a where need to calcualte spin-up for both obsclim and ctrlclim, from ctrlclim. 
getgridin_CMIP63a <- function(igrid, curr_scen, save_path_obsclim, save_path_ctrlclim, ctrlclim, obsclim){
  
  # trial 
  # igrid = 2
  
  curr_grid_obsclim <- which(obsclim[,c("lat")]==depth[igrid,"lat"] & obsclim[,c("lon")]==depth[igrid,"lon"])
  curr_grid_ctrlclim <- which(ctrlclim[,c("lat")]==depth[igrid,"lat"] & ctrlclim[,c("lon")]==depth[igrid,"lon"])

  if(length(curr_grid_obsclim) > 0){
    
    ## Identify curr grid inputs
    gridinputs_obsclim <- obsclim[curr_grid_obsclim,]
    gridinputs_ctrlclim <- ctrlclim[curr_grid_ctrlclim,]
    
    ## calculate spin up for both grid cells 
    # calculate timesteps, as t is confusing (inputs: monthly_1961_2010.nc) 
    Date<-seq(as.Date("1961-01-01"), as.Date("2010-12-01"), by="month")
    t<-unique(ctrlclim$t)
    time<-data.frame(t = t, Date = Date)
    rm(Date)
    
    # # check - OK
    # unique(time$t)
    # unique(gridinputs_obsclim$t)
    # nrow(time)
    
    # CONTROL 
    gridinputs_ctrlclim<-gridinputs_ctrlclim %>% 
      full_join(time) %>%  
      arrange(Date)
    
    # calcualte spin-up
    spinup<-gridinputs_ctrlclim %>%
      filter(Date >= "1961-01-01", Date <="1980-12-01") %>%
      slice(rep(1:n(), times = 6)) #%>% 
    
    # calc new date and t for spin-up 
    new_date = seq(as.Date("1841-01-01"), as.Date("1960-12-01"), by="months")
    new_t<-seq((t[1]-length(new_date)),t[1]-1)
    new_date_df<-data.frame(Date = new_date, t = new_t)
    rm(new_date, t, new_t)
    
    # repplace spinup date 
    spinup<-spinup %>% 
      select(-Date, -t) %>% 
      mutate(Date = new_date_df$Date, 
             t = new_date_df$t) 
    
    # # check - OK 
    # length(new_date_df$Date)
    # nrow(spinup)
    
    # add spinup to ctrlclim  
    gridinputs_ctrlclim<-gridinputs_ctrlclim %>% 
      full_join(spinup) %>% 
      arrange(Date)
    
    # check - OK 
    # nrow(gridinputs_ctrlclim) # 1440+600 = 2040
    
    # OBSERVED 
    # add spinup to obsclim  
    gridinputs_obsclim<-gridinputs_obsclim %>% 
      full_join(time) %>% # this is not necessary
      full_join(spinup) %>% 
      arrange(Date) # %>% 
    
    # # CHECK - plot - working OK 
    # head(gridinputs_obsclim)
    # trial<-gridinputs_ctrlclim %>% 
    #   mutate(Year = as.numeric(format(as.Date(Date), "%Y")))  %>%
    #   group_by(Year) %>% 
    #   summarise(value = mean(lphy)) %>% 
    #   ungroup()
    # 
    # plot<-ggplot(trial, aes(x = Year, y = value))+
    #   geom_line()+
    #   geom_point()+
    #   annotate("rect", xmin=1961, xmax=1980, ymin=-Inf, ymax=Inf, fill = "#b2e2e2", alpha = 0.4)+
    #   theme_bw()
    # 
    # pdf("Output/plot1.pdf", height = 4, width = 6)
    # plot
    # dev.off()
    
    # I should loop here ... 
    ## For current grid (obsclim), calculate slope, intercept and export ratio (er)
    gridinputs_obsclim$slope<-gridinputs_obsclim$intercept<-gridinputs_obsclim$er <- gridinputs_obsclim[,ncol(gridinputs_obsclim)-1]
    
    gridinputs_obsclim[,"er"] <- mapply(getExportRatio,sphy=gridinputs_obsclim[,"sphy"],lphy=gridinputs_obsclim[,"lphy"],sst=gridinputs_obsclim[,"sst"],depth=depth[igrid,"depth"])
    gridinputs_obsclim[which(gridinputs_obsclim[,"er"]<0),"er"]<-0
    gridinputs_obsclim[which(gridinputs_obsclim[,"er"]>1),"er"]<-1
    
    gridinputs_obsclim[,"intercept"]<-mapply(GetPPIntSlope,sphy=gridinputs_obsclim[,"sphy"],lphy=gridinputs_obsclim[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="intercept")
    
    gridinputs_obsclim[,"slope"]<-mapply(GetPPIntSlope,sphy=gridinputs_obsclim[,"sphy"],lphy=gridinputs_obsclim[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="slope")
    
    gridinputs_obsclim <- gridinputs_obsclim[,c("t","sst","sbt","er","intercept","slope", "sphy", "lphy")]
    
    # write over t
    gridinputs_obsclim$t<-seq(0.0, (dim(gridinputs_obsclim)[1]-1),by=1)
    
    # check time - OK
    # nrow(gridinputs_obsclim) # 2040
    
    ## For current grid (ctrlclim), calculate slope, intercept and export ratio (er)
    # could loop ... 
    gridinputs_ctrlclim$slope<-gridinputs_ctrlclim$intercept<-gridinputs_ctrlclim$er <- gridinputs_ctrlclim[,ncol(gridinputs_ctrlclim)-1]
    
    gridinputs_ctrlclim[,"er"] <- mapply(getExportRatio,sphy=gridinputs_ctrlclim[,"sphy"],lphy=gridinputs_ctrlclim[,"lphy"],sst=gridinputs_ctrlclim[,"sst"],depth=depth[igrid,"depth"])
    gridinputs_ctrlclim[which(gridinputs_ctrlclim[,"er"]<0),"er"]<-0
    gridinputs_ctrlclim[which(gridinputs_ctrlclim[,"er"]>1),"er"]<-1
    
    gridinputs_ctrlclim[,"intercept"]<-mapply(GetPPIntSlope,sphy=gridinputs_ctrlclim[,"sphy"],lphy=gridinputs_ctrlclim[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="intercept")
    
    gridinputs_ctrlclim[,"slope"]<-mapply(GetPPIntSlope,sphy=gridinputs_ctrlclim[,"sphy"],lphy=gridinputs_ctrlclim[,"lphy"],mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth=depth[igrid,"depth"],output="slope")
    
    gridinputs_ctrlclim <- gridinputs_ctrlclim[,c("t","sst","sbt","er","intercept","slope", "sphy", "lphy")]
    
    # write over t
    gridinputs_ctrlclim$t<-seq(0.0, (dim(gridinputs_ctrlclim)[1]-1),by=1)
    
    # add depth an consider weekly time-steps 
    dep<-depth[igrid,]
    
    # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
    
    # weekly timesteps
    wts=data.frame(t=seq(0.0, (dim(gridinputs_obsclim)[1]-1),by=0.25))
    
    # check time # WARNING 
    # nrow(wts) # 8157
    # length(seq(as.Date("1842-01-01"), as.Date("2010-11-30"), by="week")) # 8814
    # (2010-1841)*12*4 # 8112
    # (2010-1840)*12*4 # 8160
    
    # merge with gridinputs_obsclim and gridinputs_ctrlclim
    wts_obsclim<-merge(gridinputs_obsclim,wts,by="t",all=T)
    wts_ctrlclim<-merge(gridinputs_ctrlclim,wts,by="t",all=T)
    
    #use na.approx in zoo package to fill in NAs (linear interpolation)
    fwts_obsclim<-data.frame(na.approx(wts_obsclim))
    fwts_ctrlclim<-data.frame(na.approx(wts_ctrlclim))
    
    # CN in previous code but not used in CMIP63b and CMIP63a
    # add spin up - change to 300 yrs
    #  spinup<-data.frame(sst=rep(mean(fwts$sst[1:12]),each=300*48),sbt=rep(mean(fwts$sbt[1:12]),each=300*48),
    #                    er=rep(mean(fwts$er[1:12]),each=300*48),intercept=rep(mean(fwts$intercept[1:12]),each=300*48),
    #                    slope=rep(mean(fwts$slope[1:12]),each=300*48))
    
    #  fwts <- rbind(spinup,fwts[,-1])
    
    inputs_obsclim=list(depth=dep,ts=fwts_obsclim)
    inputs_ctrlclim=list(depth=dep,ts=fwts_ctrlclim)
    
    # save files 
    filename_obsclim <- paste(save_path_obsclim,'grid_', igrid, '_', "obsclim",'_' ,curr_scen, '.rds', sep = '')
    filename_ctrlclim <- paste(save_path_ctrlclim,'grid_', igrid, '_', "ctrlclim",'_' ,curr_scen, '.rds', sep = '')
    
    saveRDS(inputs_obsclim, file=filename_obsclim, compress = FALSE)
    saveRDS(inputs_ctrlclim, file=filename_ctrlclim, compress = FALSE)
    
    rm(inputs_obsclim,inputs_ctrlclim,fwts_obsclim, fwts_ctrlclim,wts_obsclim,wts_ctrlclim,gridinputs_obsclim,gridinputs_ctrlclim,spinup)
  }
}