# source("./dbpm-ryan/size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE)
source("./size-based-models/dynamic_sizebased_model_functions.R", chdir = TRUE) # CN change dir 

## This script runs the DBPM input with inputs updating yearly. Script processes weekly inputs
## to be yearly inputs, interpolating between yearly input in the same way as weekly inputs
## interpolated between monthly inputs. Ryan Heneghan.

rungridsep <- function(igrid
                       ,gcm
                       ,protocol
                       ,output
                       ,input_files_location
                       ,output_files_location) {
  
  # we need to run the model with time-varying inputs   
  # the largest dt of the model is a monthly time step (Q-F was daily...which may still be needed) 
  # once we have all of the inputs, we need to set the model up on an appropriate time step. 
  # the output may be too large a time step
  # checked this: model can run on a weekly timestep, but not monthly
  
  # how long does it take to set up and run model on one grid cell for whole (disaggregated) time series?
  
  # ptm=proc.time()
  # options(warn=-1)
    curr_grid_input <- list.files(path = paste(input_files_location, protocol, sep = ""), pattern = paste(protocol, "_", igrid, "_" , gcm, ".rds", sep = ""), full.names = TRUE)
    curr_grid_output <- list.files(path = paste(output_files_location, protocol, sep = ""), pattern = paste("dbpm_output_all_", igrid, '_', protocol, '.rds', sep = ""), full.names = TRUE)

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
    
    #use na.approx in zoo package to fill in NAs (linear interpolation)
    fwts<-data.frame(na.approx(tss))
    
    # Add 300 year spinup to top of file (48 weeks in each year)
    spinup<-data.frame(sst=rep(mean(inputs$ts$sst[1:480]),each=300*48),sbt=rep(mean(inputs$ts$sbt[1:480]),each=300*48),er=rep(mean(inputs$ts$er[1:480]),each=300*48),
                       intercept=rep(mean(inputs$ts$intercept[1:480]),each=300*48),slope=rep(mean(inputs$ts$slope[1:480]),each=300*48),
                       sphy=rep(mean(inputs$ts$sphy[1:480]),each=300*48), lphy=rep(mean(inputs$ts$lphy[1:480]),each=300*48))
    
    fwts <- rbind(spinup, fwts[,-1])
    
    inputs=list(depth=inputs$depth,ts=fwts)
    
    rm(spinup)

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

  isave <- seq(from=300*48, to=((dim(inputs$ts)[1])+1), by = 48)
  
 # a <- Sys.time()
  ## CHECK IF MODEL HAS CRASHED, IF IT HAS, 10X MORE STEPS AND RUN AGAIN
  if(result_set$notrun == TRUE){
    
    
    # GET LAST STEP OF SPINUP, FOR FIRST STEP OF NEW RUN (DON'T NEED TO RUN BURN IN AGAIN)
    U.start = as.numeric(result_set$U[,14400])
    V.start = as.numeric(result_set$V[,14400])
    W.start = as.numeric(result_set$W[14400])
    
    if(sum(U.start) == 0){ # If model crashed in spin up, need to re run entire spinup
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
    
    #use na.approx in zoo package to fill in NAs (linear interpolation)
    fwts<-data.frame(na.approx(tss))
    
    inputs$ts <- fwts
    
    # write over t
    inputs$ts$t <-seq(0.0, (dim(inputs$ts)[1]-1),by=1)
    
    # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
    #weekly timesteps
    wts=data.frame(t=seq(0.0, (dim(inputs$ts)[1]-1),by=0.1))
    
    wts<-merge(inputs$ts,wts,by="t",all=T)
    
    #use na.approx in zoo package to fill in NAs (linear interpolation)
    fwts<-data.frame(na.approx(wts))
    
    # Add 300 year spinup to top of file (48 weeks in each year)
    spinup<-data.frame(sst=rep(mean(inputs$ts$sst[1:4800]),each=300*480),sbt=rep(mean(inputs$ts$sbt[1:4800]),each=300*480),er=rep(mean(inputs$ts$er[1:4800]),each=300*480),
                       intercept=rep(mean(inputs$ts$intercept[1:4800]),each=300*480),slope=rep(mean(inputs$ts$slope[1:4800]),each=300*480),
                       sphy=rep(mean(inputs$ts$sphy[1:4800]),each=300*480), lphy=rep(mean(inputs$ts$lphy[1:4800]),each=300*480))
    
    fwts <- rbind(spinup, fwts[,-1])
    
    inputs=list(depth=inputs$depth,ts=fwts)

    rm(spinup)
    
    #get params 
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
                        ,U.initial = U.start
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
    
    isave <- seq(from=300*480+2, to=((dim(inputs$ts)[1])+1), by = 480)
    }
    
    if(sum(U.start) != 0){ # If model did not crash in spinup, we do not need to rerun the spinup, this saves about 20 minutes per run
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
      
      #use na.approx in zoo package to fill in NAs (linear interpolation)
      fwts<-data.frame(na.approx(tss))
      
      inputs$ts <- fwts
      
      # write over t
      inputs$ts$t <-seq(0.0, (dim(inputs$ts)[1]-1),by=1)
      
      # in gridinputs there are 5 inputs that will change thru time:sst,sbt,er,intercept and slope 
      #weekly timesteps
      wts=data.frame(t=seq(0.0, (dim(inputs$ts)[1]-1),by=0.1))
      
      wts<-merge(inputs$ts,wts,by="t",all=T)
      
      #use na.approx in zoo package to fill in NAs (linear interpolation)
      fwts<-data.frame(na.approx(wts))
      
      inputs=list(depth=inputs$depth,ts=fwts[,-1])
      
      spinup<-data.frame(sst=mean(inputs$ts$sst[1:4800]),sbt=mean(inputs$ts$sbt[1:4800]),er=mean(inputs$ts$er[1:4800]),
                         intercept=mean(inputs$ts$intercept[1:4800]),slope=mean(inputs$ts$slope[1:4800]),
                         sphy=mean(inputs$ts$sphy[1:4800]), lphy=mean(inputs$ts$lphy[1:4800]))
      
      inputs$ts[1,] <- spinup # GLUE IN FIRST TIME STEP AS SPINUP
      
      rm(spinup)
      
      #get params 
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
      
      isave <- seq(from=2, to=((dim(inputs$ts)[1])+1), by = 480)
    }
    
  }
  #b <- Sys.time()
  #b-a
  # print((proc.time()-ptm)/60.0)
  
  # it takes 0.5637 minutes to run the model, although it takes a long time to do the time series disaggregation, may wnat to work those up first?
  
  # extract these into monthly time series - cut off first 300 yr spinup
  
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
      agg[,1:13] <- agg[,1:13] * min(agg$depth,100)
      
      
      output_filename <- paste(output_files_location, protocol, "/", "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
      saveRDS(agg, file=output_filename, compress = FALSE)
    }
    
    
    if (output!="aggregated") {
      output_filename <-paste(output_files_location, protocol, "/", "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
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
    
    output_filename <- paste(output_files_location, protocol, "/", "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
    saveRDS(agg, file=output_filename, compress = FALSE)
    
    rm(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, TotalW)
    
    
  }
  
  # return(result_set)
  #  end rungrid function
  rm(params,result_set,inputs)
  
    }
    
}
# end function 