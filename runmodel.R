source("size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE)
source("helpers.R")


rungridsep <- function(igrid=1
                       ,gcm="ipsl-cm5a-lr"
                       ,run="rcp45"
                       ,output="aggregated"
                       ,input_files_location = "/rd/gem/private/fishmip_inputs/"
                       ,output_files_location = "/rd/gem/private/fishmip_outputs/") {
  
  # we need to run the model with time-varying inputs   
  # the largest dt of the model is a monthly time step (Q-F was daily...which may still be needed) 
  # once we have all of the inputs, we need to set the model up on an appropriate time step. 
  # the output may be too large a time step
  # checked this: model can run on a weekly timestep, but not monthly
  
  # how long does it take to set up and run model on one grid cell for whole (disaggregated) time series?
  
  # ptm=proc.time()
  # options(warn=-1)
  
  if (gcm =="reanalysis"){
    
    input_filename <- sprintf("%sgrid_%i_inputs_%s_%s.RData", input_files_location, igrid, gcm, run)
    load(input_filename)
    
  } else {
    
    input_filename <- sprintf("%sgrid_%i_inputs2_%s_%s.rds", input_files_location, igrid, gcm, run)
    inputs <- readRDS(input_filename)

    #plot(inputs$ts$sst) 
    
    #historical_means <- colMeans(inputs$ts[(301*48):(300*48+55*48),])
    historical_means <- colMeans(inputs$ts[start_of_history_weeks:end_of_history_weeks,])
    
    #replace spinup if different from historical_means
    inputs$ts[start_of_spinup_weeks:end_of_spinup_weeks,"sst"] <- historical_means["sst"]
    inputs$ts[start_of_spinup_weeks:end_of_spinup_weeks,"sbt"] <- historical_means["sbt"]
    inputs$ts[start_of_spinup_weeks:end_of_spinup_weeks,"er"] <- historical_means["er"]
    inputs$ts[start_of_spinup_weeks:end_of_spinup_weeks,"intercept"] <- historical_means["intercept"]
    inputs$ts[start_of_spinup_weeks:end_of_spinup_weeks,"slope"] <- historical_means["slope"]
    rm(historical_means)
  }
  
  #get params 
  params <- sizeparam(equilibrium = TRUE
                      ,dx = 0.1
                      ,xmin.consumer.u = -3
                      ,xmin.consumer.v = -3
                      ,tmax = dim(inputs$ts)[1]/48
                      ,tstepspryr  =  48
                      ,fmort.u = 0.0
                      ,fminx.u = 0
                      ,fmort.v = 0.0
                      ,fminx.v = -1
                      ,depth = inputs$depth$depth
                      ,er = inputs$ts$er
                      ,pp = inputs$ts$intercept
                      ,slope = inputs$ts$slope
                      ,sst = inputs$ts$sst
                      ,sft = inputs$ts$sbt
                      ,lat = inputs$depth$lat
                      ,lon = inputs$depth$lon)      
  
  result_set <- sizemodel(params  =params
                          ,ERSEM.det.input = FALSE
                          ,U_mat = NULL
                          ,V_mat = NULL
                          ,W_mat = NULL
                          ,temp.effect = TRUE) 
  
  result_set$notrun <- ifelse(any(result_set$U[150,]=="NaN")==FALSE, FALSE, TRUE)
  
  # print((proc.time()-ptm)/60.0)
  
  # it takes 0.5637 minutes to run the model, although it takes a long time to do the time series disaggregation, may wnat to work those up first?
  
  # extract these into monthly time series - cut off first 100 yrs
  
  isave <- seq(from=2, to=((dim(inputs$ts)[1])+1),4)
  
  #  get fish-mip outputs - these are aggregated by time and size classes
  
  if (result_set$notrun==F) {  
    
    if (output=="aggregated") {
      
      
      #sum biomass (need to convert to g ww per m^3, across size classes) 
      
      # then need to convert to g C per m^2 (multiply by depth at end)
      
      # conversion used is:  0.0352 g C = 1 g wet weight
      
      #total biomass in functional groups
      
      TotalUbiomass <- 0.0352*apply(result_set$U[params$ref:params$Nx,isave]*params$dx*10^params$x[params$ref:params$Nx],2,sum) 
      TotalVbiomass <- 0.0352*apply(result_set$V[params$ref.det:params$Nx,isave]*params$dx*10^params$x[params$ref.det:params$Nx],2,sum) 
      TotalW <- 0.0352*result_set$W[isave]
      
      
      # Biomass in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
      
      wcut <- c(10, 270, 1000, 10000)
      
      xcutref <- wcut
      
      for (i in 1:length(wcut)) xcutref[i] = (min(which(params$x >=log10(wcut[i]))))
      
      Ubiomass10plus <- 0.0352*apply(result_set$U[xcutref[1]:params$Nx,isave]*params$dx*10^params$x[xcutref[1]:params$Nx],2,sum) 
      Ubiomass270plus <- 0.0352*apply(result_set$U[xcutref[2]:params$Nx,isave]*params$dx*10^params$x[xcutref[2]:params$Nx],2,sum) 
      
      Vbiomass10plus <- 0.0352*apply(result_set$V[xcutref[1]:params$Nx,isave]*params$dx*10^params$x[xcutref[1]:params$Nx],2,sum) 
      Vbiomass270plus <- 0.0352*apply(result_set$V[xcutref[2]:params$Nx,isave]*params$dx*10^params$x[xcutref[2]:params$Nx],2,sum) 
      
      #total catches in functional groups
      
      #sum catches ( already in grams per yr, across size classes) 
      #and then they need to be converted to g ww per m^2 
      
      TotalUcatch <- apply(result_set$Y.u[,isave]*params$dx,2,sum)
      TotalVcatch <- apply(result_set$Y.v[,isave]*params$dx,2,sum) 
      
      #catches in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
      
      Ucatch10plus <- apply(result_set$Y.u[xcutref[1]:params$Nx,isave]*params$dx,2,sum) 
      Ucatch270plus <- apply(result_set$Y.u[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
      Vcatch10plus <- apply(result_set$Y.v[xcutref[1]:params$Nx,isave]*params$dx,2,sum) 
      Vcatch270plus <- apply(result_set$Y.v[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
      
      
      # Size spectrum slopes 
      # log 10 abundance vs. log 10 body mass, across size range beginnig of consumer spectra up to 10^4 g (where senescence kicks in)
      # could do: normalised  - plot of log (biomass density/arithmetic width of size bin) vs log size class
      # pareto - use method as in Rogers et al 2014.
      #Uslope <- Vslope <- rep(0, length(isave))
      #for (i in 1:length(isave)){
      #  Uslope[i]<-lm(log10(result_set$U[which(params$x==-3):which(params$x==3) ,isave[i]]) ~params$x[which(params$x==-3):which(params$x==3)])$coef[2] 
      #  Vslope[i]<-lm(log10(result_set$V[which(params$x==-3):which(params$x==3),isave[i]]) ~params$x[which(params$x==-3):which(params$x==3)])$coef[2] 
      #}
      
      agg <- data.frame(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus,TotalW) #,Uslope,Vslope)
      
      #        agg <- data.frame(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalW,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVcatch,Vcatch10plus,Vcatch270plus)
      #        rm(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, TotalW)
      
      rm(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, TotalW) #, Uslope,Vslope)
      
      agg$lat <- rep(params$lat,each=length(agg[,1]))
      agg$lon <- rep(params$lon,each=length(agg[,1]))
      agg$depth <- rep(params$depth,each=length(agg[,1]))
      
      # convert all biomasses  and catches from g C or g WW per m^3 to per m^2   
      agg[1,1:13] <- agg[,1:13] * min(agg$depth[],100)
      
      
      output_filename <- sprintf("%sres_mts_agg_igrid_%i_%s_%s.rds", output_files_location, igrid, gcm, run)
      saveRDS(agg, file=output_filename, compress = FALSE)
    }
    
    
    if (output!="aggregated") {
      output_filename <- sprintf("%sres_wts_igrid_%i_%s_%s.rds",output_files_location, igrid, gcm, run)
      saveRDS(result_set, file = output_filename, compress = FALSE)
    }
    
  }
  
  if (result_set$notrun==T) {
    
    TotalUbiomass <- Ubiomass10plus <- Ubiomass270plus <- rep(NA,length=length(isave))
    
    TotalVbiomass <- Vbiomass10plus <- Vbiomass270plus <- TotalW <- rep(NA,length=length(isave))
    
    TotalUcatch <- Ucatch10plus <- Ucatch270plus <- TotalVcatch <- Vcatch10plus <- Vcatch270plus <- rep(NA,length=length(isave))
    
    agg <- data.frame(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalW,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVcatch,Vcatch10plus,Vcatch270plus)
    
    agg$lat <- rep(params$lat,each=length(agg[,1]))
    
    agg$lon <- rep(params$lon,each=length(agg[,1]))
    
    agg$depth <- rep(params$depth,each=length(agg[,1]))
    
    output_filename <- sprintf("%sres_mts_agg_igrid_%i_%s_%s.rds", output_files_location, igrid, gcm, run)
    saveRDS(agg, file=output_filename, compress = FALSE)
    
    rm(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, TotalW)
    
    
  }
  
  # return(result_set)
  #  end rungrid function
  rm(params,result_set,inputs)
  
}
# end function 