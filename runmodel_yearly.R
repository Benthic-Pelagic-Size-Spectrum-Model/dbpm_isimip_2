# source("./dbpm-ryan/size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE)
source("./size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE) # CN change dir 

## This script runs the DBPM input with inputs updating yearly. Script processes weekly inputs
## to be yearly inputs, interpolating between yearly input in the same way as weekly inputs
## interpolated between monthly inputs. Ryan Heneghan.

##### CN function for picontrol and historical protocols -----
# including spin up 

rungridsep <- function(igrid
                       ,gcm
                       ,protocol
                       ,output
                       ,input_files_location
                       ,output_files_location) {
  
  # CN trial 
  # where is igrid specified? in the loop that calls this function 
  # igrid <- 1
  # gcm = curr_esm 
  # protocol = curr_scen
  # output = "partial"
  # input_files_location = input_loc 
  # output_files_location = output_loc

  
  # we need to run the model with time-varying inputs   
  # the largest dt of the model is a monthly time step (Q-F was daily...which may still be needed) 
  # once we have all of the inputs, we need to set the model up on an appropriate time step. 
  # the output may be too large a time step
  # checked this: model can run on a weekly timestep, but not monthly
  
  # curr_grid_input <- list.files(path = paste(input_files_location, protocol, sep = ""), pattern = paste(protocol, "_", igrid, "_" , gcm, ".rds", sep = ""), full.names = TRUE)
  # curr_grid_output <- list.files(path = paste(output_files_location, protocol, sep = ""), pattern = paste("dbpm_output_all_", igrid, '_', protocol, '.rds', sep = ""), full.names = TRUE)
  
  # CN 
  curr_grid_input <- list.files(path = input_files_location, pattern = paste("grid", "_", igrid, "_" , gcm, "_",  protocol, ".rds", sep = ""), full.names = TRUE)
  curr_grid_output <- list.files(path = output_files_location, pattern = paste("dbpm_output_all_", igrid, '_', protocol, '.rds', sep = ""), full.names = TRUE)
  
  if(length(curr_grid_input) > 0 & length(curr_grid_output) == 0){ # If current grid has not been run, and inputs exist for it
    
    inputs <- readRDS(curr_grid_input) 

    ## Extract single input for each year (first week of each year), then fill all other inputs as NA's
    num_years <- ceiling(dim(inputs$ts)[1]/48)
    tss <- as.matrix(inputs$ts)
    
    for(i in 1:(num_years-1)){ # First week of year is the yearly average, all other weeks are NA
    tss[((i-1)*48+1),-1]  <- colMeans(tss[c(((i-1)*48+1):(i*48)),-1])
    tss[c(((i-1)*48+2):(i*48)),-1] <- NA
    }
    
    # For last 'year' - final week of year is yearly average
    tss[dim(tss)[1],-1] <- colMeans(tss[((num_years-1)*48+1):dim(tss)[1],-1])
    tss[c(((num_years-1)*48+1):((dim(tss)[1]-1))),-1] <- NA
    
    # use na.approx in zoo package to fill in NAs (linear interpolation)
    fwts<-data.frame(na.approx(tss))
    
    # Add 300 year spinup to top of file (48 weeks in each year)
    spinup<-data.frame(sst=rep(mean(inputs$ts$sst[1:480]),each=300*48),sbt=rep(mean(inputs$ts$sbt[1:480]),each=300*48),er=rep(mean(inputs$ts$er[1:480]),each=300*48),
                       intercept=rep(mean(inputs$ts$intercept[1:480]),each=300*48),slope=rep(mean(inputs$ts$slope[1:480]),each=300*48),
                       sphy=rep(mean(inputs$ts$sphy[1:480]),each=300*48), lphy=rep(mean(inputs$ts$lphy[1:480]),each=300*48))
    
    fwts <- rbind(spinup, fwts[,-1])
    
    inputs=list(depth=inputs$depth,ts=fwts)
    
    rm(spinup)

    # get params 
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
                        ,lon = inputs$depth$lon
                        ,use.init = FALSE
                        ,U.initial = NA 
                        ,V.initial = NA  
                        ,W.initial = NA)       
    
    result_set <- sizemodel(params  =params
                            ,ERSEM.det.input = FALSE
                            ,U_mat = NULL
                            ,V_mat = NULL
                            ,W_mat = NULL
                            ,temp.effect = TRUE
                            ,use.init = FALSE
                            ,burnin.len = 300*48) 
  
    result_set$notrun <- ifelse(any(result_set$U[150,]=="NaN")==FALSE, FALSE, TRUE)

    # save yearly - as per Ryan def. 
    # isave <- seq(from=300*48, to=((dim(inputs$ts)[1])+1), by = 48) #  CN NOTE: cut the spinup (300years*48 weeks in each year) and save outputs from then forwards every 48 weeks (i.e. one year)
    # length(isave) # save yearly outputs # e.g. 251 if picontrol
    # save monlhy
    isave <- seq(from=300*48, to=((dim(inputs$ts)[1])+1), by = 4) # save outputs every 4 weeks (montly)
    # length(isave) # save montly outputs # e.g. 3012 if picontrol
    # isave <- seq(from=300*48, to=((dim(inputs$ts)[1])+1), by = 1) # save outputs weekly (but cut spinup)
    
    # CN: make sure you are not saving the last time dimention of the U, V and W matrices 
    # these matrices in project() are built as U[,Neq+1] and hence have 1 time step more than the inputs 
    # also you should start after spinup and finish when inputs finish (including start and end) 
    isave <- seq(from=(300*48)+1, to=((dim(inputs$ts)[1])), by = 4) # not sure why this was set up that way 
    
    ## CHECK IF MODEL HAS CRASHED, IF IT HAS, 10X MORE STEPS AND RUN AGAIN
    # CN in this case, we do the same as for the function above but we increase the time step resolution (run more in-between input values)
  
    if(result_set$notrun == TRUE){
    
      # GET LAST STEP OF SPINUP, FOR FIRST STEP OF NEW RUN (DON'T NEED TO RUN BURN IN AGAIN)
      U.start = as.numeric(result_set$U[,14400]) # CN 300*48
      V.start = as.numeric(result_set$V[,14400])
      W.start = as.numeric(result_set$W[14400])
    
      # OPTION 1: If model crashed in spin up, need to re run entire spinup
      if(sum(U.start) == 0){ 
        
        inputs <- readRDS(curr_grid_input)
        num_years <- ceiling(dim(inputs$ts)[1]/48)
        tss <- as.matrix(inputs$ts)
      
        for(i in 1:(num_years-1)){ # First week of year is the yearly average, all other weeks are NA
          tss[((i-1)*48+1),-1]  <- colMeans(tss[c(((i-1)*48+1):(i*48)),-1])
          tss[c(((i-1)*48+2):(i*48)),-1] <- NA
          }

        # For last 'year' - final week of year is yearly average
        tss[dim(tss)[1],-1] <- colMeans(tss[((num_years-1)*48+1):dim(tss)[1],-1])
        tss[c(((num_years-1)*48+1):((dim(tss)[1]-1))),-1] <- NA
    
        # use na.approx in zoo package to fill in NAs (linear interpolation)
        fwts<-data.frame(na.approx(tss))
        
        # CN: here is where we increase the time steps of the inputs from 0, 0.25, 0.5 to 0, 0.1, 0.2 etc
        inputs$ts <- fwts
      
        # write over t
        inputs$ts$t <-seq(0.0, (dim(inputs$ts)[1]-1),by=1)
    
        # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
        # weekly timesteps
        wts=data.frame(t=seq(0.0, (dim(inputs$ts)[1]-1),by=0.1))
        wts<-merge(inputs$ts,wts,by="t",all=T)
      
        # use na.approx in zoo package to fill in NAs (linear interpolation)
        fwts<-data.frame(na.approx(wts))
      
        # Add 300 year spinup to top of file (48 weeks in each year)
        spinup<-data.frame(sst=rep(mean(inputs$ts$sst[1:4800]),each=300*480),sbt=rep(mean(inputs$ts$sbt[1:4800]),each=300*480),er=rep(mean(inputs$ts$er[1:4800]),each=300*480),
                           intercept=rep(mean(inputs$ts$intercept[1:4800]),each=300*480),slope=rep(mean(inputs$ts$slope[1:4800]),each=300*480),
                           sphy=rep(mean(inputs$ts$sphy[1:4800]),each=300*480), lphy=rep(mean(inputs$ts$lphy[1:4800]),each=300*480)) # 4800 means X10 more steps 
      
        fwts <- rbind(spinup, fwts[,-1]) 
        inputs=list(depth=inputs$depth,ts=fwts)
        rm(spinup)
      
        # get params 
        params <- sizeparam(equilibrium = TRUE
                            ,dx = 0.1
                            ,xmin.consumer.u = -3
                            ,xmin.consumer.v = -3
                            ,tmax = dim(inputs$ts)[1]/480
                            ,tstepspryr  =  480
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
                            ,lon = inputs$depth$lon
                            ,use.init = FALSE
                            ,U.initial = U.start # CN these could be NA as well as you are not using initial abundance 
                            ,V.initial = V.start
                            ,W.initial = W.start)  
      
        result_set <- sizemodel(params  =params
                                ,ERSEM.det.input = FALSE
                                ,U_mat = NULL
                                ,V_mat = NULL
                                ,W_mat = NULL
                                ,temp.effect = TRUE
                                ,use.init = FALSE
                                ,burnin.len = 300*480) 
      
        result_set$notrun <- ifelse(any(result_set$U[150,]=="NaN")==FALSE, FALSE, TRUE)
      
        # save yearly - as per Ryan def
        # isave <- seq(from=300*480+2, to=((dim(inputs$ts)[1])+1), by = 480)
        # CN save monthly 
        isave <- seq(from=300*480+2, to=((dim(inputs$ts)[1])+1), by = 40)
        isave <- seq(from=(300*480)+1, to=((dim(inputs$ts)[1])), by = 40) # CN  see above 
        
        }
    
        # OPTION 2: If model did not crash in spinup, we do not need to rerun the spinup, this saves about 20 minutes per run
        if(sum(U.start) != 0){ 
          
          inputs <- readRDS(curr_grid_input)
          num_years <- ceiling(dim(inputs$ts)[1]/48)   
          tss <- as.matrix(inputs$ts)
        
         for(i in 1:(num_years-1)){ # First week of year is the yearly average, all other weeks are NA
            tss[((i-1)*48+1),-1]  <- colMeans(tss[c(((i-1)*48+1):(i*48)),-1])
            tss[c(((i-1)*48+2):(i*48)),-1] <- NA
          }
        
          # For last 'year' - final week of year is yearly average
          tss[dim(tss)[1],-1] <- colMeans(tss[((num_years-1)*48+1):dim(tss)[1],-1])
          tss[c(((num_years-1)*48+1):((dim(tss)[1]-1))),-1] <- NA
          
          # use na.approx in zoo package to fill in NAs (linear interpolation)
          fwts<-data.frame(na.approx(tss))
          inputs$ts <- fwts
          
          # write over t
          inputs$ts$t <-seq(0.0, (dim(inputs$ts)[1]-1),by=1)
          
          # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
          # weekly timesteps
          wts=data.frame(t=seq(0.0, (dim(inputs$ts)[1]-1),by=0.1))
          wts<-merge(inputs$ts,wts,by="t",all=T)
        
          # use na.approx in zoo package to fill in NAs (linear interpolation)
          fwts<-data.frame(na.approx(wts))
          inputs=list(depth=inputs$depth,ts=fwts[,-1])
          
          # CN spin up here is only for the first time step, which results in U.start, V.start and W.start:
          spinup<-data.frame(sst=mean(inputs$ts$sst[1:4800]),sbt=mean(inputs$ts$sbt[1:4800]),er=mean(inputs$ts$er[1:4800]),
                             intercept=mean(inputs$ts$intercept[1:4800]),slope=mean(inputs$ts$slope[1:4800]),
                             sphy=mean(inputs$ts$sphy[1:4800]), lphy=mean(inputs$ts$lphy[1:4800]))
        
          inputs$ts[1,] <- spinup # GLUE IN FIRST TIME STEP AS SPINUP
          rm(spinup)
        
          # get params 
          params <- sizeparam(equilibrium = TRUE
                              ,dx = 0.1
                              ,xmin.consumer.u = -3
                              ,xmin.consumer.v = -3
                              ,tmax = dim(inputs$ts)[1]/480
                              ,tstepspryr  =  480
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
                              ,lon = inputs$depth$lon
                              ,use.init = TRUE
                              ,U.initial = U.start
                              ,V.initial = V.start
                              ,W.initial = W.start)  
    
                result_set <- sizemodel(params  =params
                                  ,ERSEM.det.input = FALSE
                                  ,U_mat = NULL
                                  ,V_mat = NULL
                                  ,W_mat = NULL
                                  ,temp.effect = TRUE
                                  ,use.init = TRUE
                                  ,burnin.len = 1) 
        
          result_set$notrun <- ifelse(any(result_set$U[150,]=="NaN")==FALSE, FALSE, TRUE)
        
          # save yearly - as per Ryan def.
          # isave <- seq(from=2, to=((dim(inputs$ts)[1])+1), by = 480)
          # CN save monthly 
          isave <- seq(from=2, to=((dim(inputs$ts)[1])+1), by = 40)
          isave <- seq(from=1, to=((dim(inputs$ts)[1])), by = 40) # CN see above - here you are saving the first time step which is spin up and have overwritten the first time step  
        }
      }
  
  if (result_set$notrun==FALSE) {  
    
    # CN options:
    # output == "aggregated" old - moved to makenetcdf_func.R and considered isismip3b requirements  
    # output == "partial" saves disaggreageted outputs but only biomass (and growth) 
    # output == "non-aggregated" saves all disaggreageted outputs
    
    if (output=="aggregated") { 
      
      # sum biomass (need to convert to g ww per m^3, across size classes) 
      # then need to convert to g C per m^2 (multiply by depth at end)
      # conversion used is: 0.0352 g C = 1 g wet weight
      # total biomass in functional groups
      
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
      
      # total catches in functional groups
      
      # sum catches (already in grams per yr, across size classes) 
      # and then they need to be converted to g ww per m^2 
      
      TotalUcatch <- apply(result_set$Y.u[,isave]*params$dx,2,sum)
      TotalVcatch <- apply(result_set$Y.v[,isave]*params$dx,2,sum) 
      
      # catches in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
      
      Ucatch10plus <- apply(result_set$Y.u[xcutref[1]:params$Nx,isave]*params$dx,2,sum) 
      Ucatch270plus <- apply(result_set$Y.u[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
      Vcatch10plus <- apply(result_set$Y.v[xcutref[1]:params$Nx,isave]*params$dx,2,sum) 
      Vcatch270plus <- apply(result_set$Y.v[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
      
      # Size spectrum slopes 
      # log 10 abundance vs. log 10 body mass, across size range beginnig of consumer spectra up to 10^4 g (where senescence kicks in)
      # could do: normalised  - plot of log (biomass density/arithmetic width of size bin) vs log size class
      # pareto - use method as in Rogers et al 2014.
      # Uslope <- Vslope <- rep(0, length(isave))
      # for (i in 1:length(isave)){
      #  Uslope[i]<-lm(log10(result_set$U[which(params$x==-3):which(params$x==3) ,isave[i]]) ~params$x[which(params$x==-3):which(params$x==3)])$coef[2] 
      #  Vslope[i]<-lm(log10(result_set$V[which(params$x==-3):which(params$x==3),isave[i]]) ~params$x[which(params$x==-3):which(params$x==3)])$coef[2] 
      # }
      
      agg <- data.frame(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus,TotalW) #,Uslope,Vslope)
      rm(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, TotalW) #, Uslope,Vslope)
      
      agg$lat <- rep(params$lat,each=length(agg[,1]))
      agg$lon <- rep(params$lon,each=length(agg[,1]))
      agg$depth <- rep(params$depth,each=length(agg[,1]))
      
      # convert all biomasses and catches from g C or g WW per m^3 to per m^2   
      agg[,1:13] <- agg[,1:13] * min(agg$depth,100)
      
      output_filename <- paste(output_files_location, "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
      saveRDS(agg, file=output_filename, compress=FALSE)
      
    }
    
    # biomass is in ww g m^3 according to the above 
    if (output == "partial") {
    
      result_partial<-list(U = result_set$U[,isave], # size class X time
                            V = result_set$V[,isave], 
                            GGU = result_set$GG.u[,isave], # only if historical 
                            GGV = result_set$GG.v[,isave], # only if historical 
                            W = result_set$W[isave], # time 
                            x = result_set$params$x, # size class in log10  
                            lat = result_set$params$lat, 
                            lon = result_set$params$lon,
                            depth = result_set$params$depth, 
                            dx = result_set$params$dx) 
  
      # save 
      output_filename <- paste(output_files_location, "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
      saveRDS(result_partial, file = output_filename, compress = FALSE)
      rm(result_partial)
      }
    
    if (output=="not_aggregated") {
      
      # CN see above for saving options - i.e. add isave to save only yearly instead of weakly 
      output_filename <- paste(output_files_location, "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
      saveRDS(result_set, file = output_filename, compress = FALSE)
      
    }
    
  }
  
#  if (result_set$notrun==T) { # CN this sequence of 'if' might not be needed 
#                              # as 'aggregated' is not an option anymore
#                              # hence when the model does not run, we can save matrix of NAs (see framework below)
#    
#    # CN do not need if(output=="aggregated") and so on becasue if the model has not run after going through all options above, we are saving NAs anyway 
#    # although  this might become problematic when building the  netcdf becasue objects (e.g. U) for this grid won't be found - need an ifelse there? 
#    TotalUbiomass <- Ubiomass10plus <- Ubiomass270plus <- rep(NA,length=length(isave))
#    TotalVbiomass <- Vbiomass10plus <- Vbiomass270plus <- TotalW <- rep(NA,length=length(isave))
#    TotalUcatch <- Ucatch10plus <- Ucatch270plus <- TotalVcatch <- Vcatch10plus <- Vcatch270plus <- rep(NA,length=length(isave))
#    agg <- data.frame(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalW,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVcatch,Vcatch10plus,Vcatch270plus)
#    agg$lat <- rep(params$lat,each=length(agg[,1]))
#    agg$lon <- rep(params$lon,each=length(agg[,1]))
#    agg$depth <- rep(params$depth,each=length(agg[,1]))
#    
#    output_filename <- paste(output_files_location, "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
#    saveRDS(agg, file=output_filename, compress = FALSE)
#    rm(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, TotalW)
    
#  }
  
  # end rungrid function
  rm(params,result_set,inputs)
  
    }
    
}

##### CN function for ssp protocols -----
# starting from last week of historical runs and not including spin up 
rungridsep_ssp <- function(igrid
                       ,gcm
                       ,protocol
                       ,output
                       ,input_files_location
                       ,output_files_location
                       ,input_historical_location
                       ,output_historical_location) {
  
  # CN trial 
  # igrid <- 22430
  # gcm = curr_esm 
  # protocol = curr_scen
  # output = "partial"
  # input_files_location = input_loc 
  # output_files_location = output_loc
  # output_historical_location = output_loc_hist 
  # input_historical_location = input_loc_hist 
  
  # we need to run the model with time-varying inputs   
  # the largest dt of the model is a monthly time step (Q-F was daily...which may still be needed) 
  # once we have all of the inputs, we need to set the model up on an appropriate time step. 
  # the output may be too large a time step
  # checked this: model can run on a weekly timestep, but not monthly
  
  # original are the following 2 lines 
  # curr_grid_input <- list.files(path = paste(input_files_location, protocol, sep = ""), pattern = paste(protocol, "_", igrid, "_" , gcm, ".rds", sep = ""), full.names = TRUE)
  # curr_grid_output <- list.files(path = paste(output_files_location, protocol, sep = ""), pattern = paste("dbpm_output_all_", igrid, '_', protocol, '.rds', sep = ""), full.names = TRUE)
  
  curr_grid_input <- list.files(path = input_files_location, pattern = paste("grid_", igrid, "_" , gcm, "_",  protocol, ".rds", sep = ""), full.names = TRUE)
  curr_grid_output <- list.files(path = output_files_location, pattern = paste("dbpm_output_all_", igrid, '_', protocol, '.rds', sep = ""), full.names = TRUE)
  
  # CN upload historical run for the grid cell and extrapolate last time step sizespectra for U,V anf W
  # CN NOTE: do I also need historical input that resulted in these outputs
  # and to glue them on the inputs as per spin up below? NO - COMMENTED 
  # curr_grid_input_h <- list.files(path = input_historical_location, pattern = paste("grid_", igrid, '_', gcm , '_historical', '.rds', sep = ""), full.names = TRUE)
  curr_grid_output_h <- list.files(path = output_historical_location, pattern = paste("dbpm_output_all_", igrid, '_', 'historical', '.rds', sep = ""), full.names = TRUE)
  
  if(length(curr_grid_input) > 0 & length(curr_grid_output) == 0){ # If current grid has not been run, and inputs exist for it
    
    inputs <- readRDS(curr_grid_input) 
    
    # CN start from last time step of historical runs 
    # this step is similar to result_set$notrun == TRUE in the functions above, where we start from last time step of spin up 
    
    # CN upload historical run for the grid cell and extrapolate: 
    # 1) last time step inputs - NO COMMENTED see above  
    # inputs_hist <- readRDS(curr_grid_input_h) 
    # compare historical and projection values - need to do interpolation below before gluing these values to the projections ones 
    # inputs$ts[1,]
    # inputs_hist$ts[dim(inputs_hist$ts)[1],] 

    # 2) last time step sizespectra for U, V and W
    # specify these values in param() below
    outputs_hist <- readRDS(curr_grid_output_h) 
    # dim(outputs_hist$V)
    U.initial = outputs_hist$U[,dim(outputs_hist$U)[2]] # use second last time step spectra for U if using historical weekly saved outputs as per ISIMIP3b_withTempOnSenecsence - outputs_hist$U[,dim(outputs_hist$U)[2]-1]
                                                        # because historical outputs have 1 time step more than inputs
                                                        # should be adjusted in model runs or historical outputs 
    V.initial = outputs_hist$V[,dim(outputs_hist$V)[2]]
    W.initial = outputs_hist$W[dim(outputs_hist$W)]
    
    ## Extract single input for each year (first week of each year), then fill all other inputs as NA's
    num_years <- ceiling(dim(inputs$ts)[1]/48)
    tss <- as.matrix(inputs$ts)
    
    for(i in 1:(num_years-1)){ # First week of year is the yearly average, all other weeks are NA
      tss[((i-1)*48+1),-1]  <- colMeans(tss[c(((i-1)*48+1):(i*48)),-1])
      tss[c(((i-1)*48+2):(i*48)),-1] <- NA
    }
    
    # For last 'year' - final week of year is yearly average
    tss[dim(tss)[1],-1] <- colMeans(tss[((num_years-1)*48+1):dim(tss)[1],-1])
    tss[c(((num_years-1)*48+1):((dim(tss)[1]-1))),-1] <- NA
    
    # use na.approx in zoo package to fill in NAs (linear interpolation)
    fwts<-data.frame(na.approx(tss))
    
    # No spin up if running ssp
    fwts <- fwts[,-1] # CN -1 is the t dimentions column    
    
    ### CN: NOT SURE about the need of this step ----
    # NO - COMMENTED see above 
    # adding last time step of historical inputs
    # the reasoning is that we are following Ryan's approach in result_set$notrun == TRUE in function above
    # if we consider this step, we need to do the interpolation above for the historical inputs 
    ## Extract single input for each year (first week of each year), then fill all other inputs as NA's
    # num_years_h <- ceiling(dim(inputs_hist$ts)[1]/48)
    #tss_h <- as.matrix(inputs_hist$ts)
    
    # for(i in 1:(num_years_h-1)){ # First week of year is the yearly average, all other weeks are NA
    #   tss_h[((i-1)*48+1),-1]  <- colMeans(tss_h[c(((i-1)*48+1):(i*48)),-1])
    #   tss_h[c(((i-1)*48+2):(i*48)),-1] <- NA
    # }
    
    # For last 'year' - final week of year is yearly average
    # tss_h[dim(tss_h)[1],-1] <- colMeans(tss_h[((num_years_h-1)*48+1):dim(tss_h)[1],-1])
    # tss_h[c(((num_years_h-1)*48+1):((dim(tss_h)[1]-1))),-1] <- NA
    
    # use na.approx in zoo package to fill in NAs (linear interpolation)
    # fwts_h<-data.frame(na.approx(tss_h))
    
    # compare historical and projection values after interpolation 
    # fwts[1,]
    # fwts_h[dim(fwts_h)[1],]
    
    # glue last year of historical inputs and first year of projection inputs after interpolation above 
    # CN NOTE Ryan overwrites the first time step of inputs with this one (in result_set$notrun == TRUE above), 
    # we add it on top of the  input file instead  
    # fwts<-rbind(fwts_h[dim(fwts_h)[1],-1], fwts) # there is a big jump in values from historical to projections 
 
    ### end of NOT SURE (can skip) ----
    
    inputs=list(depth=inputs$depth,ts=fwts)
    
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
                        ,lon = inputs$depth$lon
                        ,use.init = TRUE # CN update initial values
                        ,U.initial = U.initial # CN update initial values 
                        ,V.initial = V.initial # CN update initial values
                        ,W.initial = W.initial) # CN update initial values       
    
    result_set <- sizemodel(params  =params
                            ,ERSEM.det.input = FALSE
                            ,U_mat = NULL
                            ,V_mat = NULL
                            ,W_mat = NULL
                            ,temp.effect = TRUE
                            ,use.init = TRUE # CN update initial values
                            ,burnin.len = 1) # CN no burn in but this param is not used in function ...    
    
    result_set$notrun <- ifelse(any(result_set$U[150,]=="NaN")==FALSE, FALSE, TRUE)
    
    # fix ssp585 cell 22430 as outputs are given as characters...
    # U<-result_set$U
    # dim(result_set$U)
    # U<-sapply(U, FUN=as.numeric)
    # U <- matrix(data=U, ncol=dim(result_set$U)[2], nrow= dim(result_set$U)[1])
    # result_set$U<-U
    
    # check if initial abundance have been considered 
    # sum(result_set$U[,1])
    # sum(U.initial)
    # sum(result_set$V[,1])
    # sum(V.initial)
    
    # save results (no spin up this time) 
    # CN check inputs and outputs time dimention 
    # nrow(inputs$ts) # 4125 input values 
    # dim(result_set$U) # 4126 output values 
    # the first time step has initial values (abundance matrices have time setps of inputs+1)
    # (either calucated given input params or from last step of historical runs 
    # - i.e. 2014 values - so we save from 2 to dim()+1
    # length(seq(from=2, to=((dim(inputs$ts)[1])+1), by = 1)) # weekly 
    # length(seq(from=2, to=((dim(inputs$ts)[1])+1), by = 48)) # yearly
    isave <- seq(from=2, to=((dim(inputs$ts)[1])+1), by = 4) # montly 
    isave <- seq(from=1, to=((dim(inputs$ts)[1])), by = 4) # montly, but considering the fistrst time step (which is not the initial values) 
    # and excluding last time step of outputs, which have one time step more than inputs - see above 
    # length(isave) # 1032 if monthly
    # chack saved variable: 
    # U = result_set$U[,isave]
    # dim(U)
    
    ## CHECK IF MODEL HAS CRASHED, IF IT HAS, 10X MORE STEPS AND RUN AGAIN
    
    if(result_set$notrun == TRUE){
      
      # OPTION 1: If model crashed in spin up, need to re run entire spinup
      # CN - not an option here as we don't have spin up 
 
      # OPTION 2: If model did not crash in spinup, we do not need to rerun the spinup
      # CN in this case, we do the same as for the function above but we increase the time step resolution (run more in-between input values)
      
      # CN: tss is calcualted above, but now fwts changes 
      # becasue we increase the time steps of the inputs from 0, 0.25, 0.5 to 0, 0.1, 0.2 etc 
      # use na.approx in zoo package to fill in NAs (linear interpolation)
      fwts<-data.frame(na.approx(tss))
      inputs$ts <- fwts  
        
      # write over t
      inputs$ts$t <-seq(0.0, (dim(inputs$ts)[1]-1),by=1)
        
      # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
      # weekly timesteps
      wts=data.frame(t=seq(0.0, (dim(inputs$ts)[1]-1),by=0.1))
      wts<-merge(inputs$ts,wts,by="t",all=T)
        
      # use na.approx in zoo package to fill in NAs (linear interpolation)
      fwts<-data.frame(na.approx(wts))
        
      # CN no spin up if running ssp
      # inputs=list(depth=inputs$depth,ts=fwts[,-1])
      # spinup<-data.frame(sst=mean(inputs$ts$sst[1:4800]),sbt=mean(inputs$ts$sbt[1:4800]),er=mean(inputs$ts$er[1:4800]),
      #                    intercept=mean(inputs$ts$intercept[1:4800]),slope=mean(inputs$ts$slope[1:4800]),
      #                    sphy=mean(inputs$ts$sphy[1:4800]), lphy=mean(inputs$ts$lphy[1:4800]))
      # inputs$ts[1,] <- spinup # GLUE IN FIRST TIME STEP AS SPINUP
      # rm(spinup)
      
      # NO - COMMENTED see above
      # but instead, we should include inputs from last time step of historical runs that resulted in initial values (as above)
      # this step in option result_set$notrun == TRUE function above is the reason why we do this 
      # glue last year of historical inputs and first year of projection inputs after interpolation above 
      # CN added on top of file instead of replacing first time step (as per Ryan's code)
      # fwts<-rbind(fwts_h[dim(fwts_h)[1],], fwts) # there is a big jump in values from historical to projections 
      
      inputs=list(depth=inputs$depth,ts=fwts[,-1])
        
      # get params 
      params <- sizeparam(equilibrium = TRUE
                          ,dx = 0.1
                          ,xmin.consumer.u = -3
                          ,xmin.consumer.v = -3
                          ,tmax = dim(inputs$ts)[1]/480
                          ,tstepspryr  =  480
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
                          ,lon = inputs$depth$lon
                          ,use.init = TRUE
                          ,U.initial = U.initial # CN keep initial spectra as per last historical run 
                          ,V.initial = V.initial
                          ,W.initial = W.initial)  
        
      result_set <- sizemodel(params  =params
                              ,ERSEM.det.input = FALSE
                              ,U_mat = NULL
                              ,V_mat = NULL
                              ,W_mat = NULL
                              ,temp.effect = TRUE
                              ,use.init = TRUE
                              ,burnin.len = 1) 
        
      result_set$notrun <- ifelse(any(result_set$U[150,]=="NaN")==FALSE, FALSE, TRUE)
        
      # save results (no spin up this time) 
      # CN 40 instead of 4 (480 if yearly and 10 if weekly) beacsue we expanded the time component of the inputs
      isave <- seq(from=2, to=((dim(inputs$ts)[1])+1), by = 40)  
      isave <- seq(from=1, to=((dim(inputs$ts)[1])), by = 40) #  see above   
      # length(isave) # 1032 
      
    }
    
    #  get fish-mip outputs
    
    # CN options:
    # output == "aggregated" old and moved to makenetcdf_func.R as per above  
    # output == "partial" saves disaggreageted outputs but only biomass (and growth) 
    # output == "non-aggregated" saves all disaggreageted outputs
      
    if (output == "partial") {
        
      # [,isave] makes sure that you are cutting spinup and saving monthy (or yearly) results
      result_partial<-list(U = result_set$U[,isave], # size class x time 
                           V = result_set$V[,isave], # size class x time 
                           W = result_set$W[isave], # time 
                           x = result_set$params$x, # size class in log10 
                           lat = result_set$params$lat, 
                           lon = result_set$params$lon,
                           depth = result_set$params$depth, 
                           dx = result_set$params$dx) 
      
      
      result_partial$U[,1]
      
      # save 
      output_filename <- paste(output_files_location, "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
      saveRDS(result_partial, file = output_filename, compress = FALSE)
      rm(result_partial)
      }
      
    if (output=="not_aggregated") {
      
      # CN see above for saving options - i.e. add isave to save only yearly instead of weakly 
      output_filename <- paste(output_files_location, "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
      saveRDS(result_set, file = output_filename, compress = FALSE)
      }

    # end rungrid function
    rm(params,result_set,inputs)
    
  }
  
}

# explore outpuys and inputs time dimention 
# inputs <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp126/grid_1_IPSL-CM6A-LR_ssp126.rds")
# inputs <-inputs$ts 
# dim(inputs)[1] # time dimention
# 4125/4 # year

# output<-result_partial$U
# dim(output)[2] # time dimention






