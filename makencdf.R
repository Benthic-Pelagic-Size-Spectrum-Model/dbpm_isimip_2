source("./helpers.R")

#----------------PUT FISH_MIP MODEL OUTPUTS INTO netcdf 4
library(ncdf4)

# If you want to WRITE data to a new netCDF file, the procedure is to first define the dimensions
# your data array has, then define the variable, then create the file. So, first call
# ncdim_def
# to define
# the dimensions that your data exists along (for example, latitude, longitude, and time).  Then call
# ncvar_def
# to  define  a  variable  that  uses  those  dimensions,  and  will  hold  your  data.   Then  call
# nc_create
# to  create  the  netCDF  file.   Finally,  call
# ncvar_put
# to  write  your  data  to  the  newly
# created netCDF file, and
# nc_close
# when you are done.

mknetcdf<-function(varname = "tcb"
                   ,gcm = 'ipsl-cm5a-lr'
                   ,run = "rcp85"
                   ,gcmPath = '/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/'
                   ,savetopath="/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf2_trial/"
                   ,grids = 1:39567
                   ,data_path = '/rd/gem/private/fishmip_outputs/aug_2017_2/'){
  
  #initialisations
  if (savetopath=="") savetopath <- data_path
  description <- dbpm.variables$description[dbpm.variables$name==varname]
  units <-  dbpm.variables$units[dbpm.variables$name==varname]
  
  
  #get lon, lat and time from inputfile
  tme  =  "_monthly_200601_21001231.nc4"
  
  inFile <-c(paste(gcmPath,run,"/",gcm,"_",run,"_lpp_zint",tme,sep = ""))
  nc <- nc_open(inFile, write = FALSE)
  lon <- ncvar_get(nc,'longitude')
  lat <- ncvar_get(nc,'latitude')
  t2 <- ncvar_get(nc,'time')
  nc_close(nc)
  rm(nc)
  
  #get additional time from historical file
  inFile <-c(paste(gcmPath,"historical","/",gcm,"_","historical","_lpp_zint","_monthly_195001_200512.nc4",sep = ""))
  nc  =  nc_open(inFile, write = FALSE)
  nc <- nc_open(inFile)  
  
  t1 <- ncvar_get(nc,'TIME')
  
  #construct time from inputfile time and historical time
  t <- c(t1,t2+length(t1))
  nc_close(nc)
  rm(nc,t1,t2)
  
  # Define some straightforward dimensions
  x <- ncdim_def( "lon", "degreesE", lon)
  y <- ncdim_def( "lat", "degreesN", lat)
  ti <- ncdim_def( "time", "months since 1950-01-01", t, unlim = TRUE)
  
  
  #------------------------------------------------------------- 
  # # Make a variable with those dimensions.  Note order: time is LAST
  
  var.nc <- ncvar_def(varname, units, longname = description,  list(x,y,ti), 1.e20 )
  
  # 
  #savetopath <- "~/"
  #full_file_name <- paste(savetopath, "dbpm_", gcm, "_",run,"_","no-fishing","_","no-oa","_",varname,".nc",sep = "")
  full_file_name <- sprintf("%sdbpm_%s_%s_no-fishing_no-oa_%s.nc", savetopath, gcm, run, varname)
  
  ncnew <- nc_create(full_file_name, var.nc)
  
  
  # get the gridded outputs for that variable from each model run 
  var <- array(NA, dim = c(length(lon), length(lat), length(t)), dimnames = list(lon,lat,t))
  
  for (igrid in grids) {
    
    data_filename <- sprintf("%s/%s/res_mts_agg_igrid_%i_%s_%s.rds", data_path, run, igrid, gcm, run)
    agg <- readRDS(data_filename)
      
    # load the inputs to get lat , lon positions
    input_filename <- sprintf("/rd/gem/private/fishmip_inputs/rds/%s/grid_%i_inputs2_%s_%s.rds", run, igrid, gcm, run)
    readRDS(input_filename)
    
    # check indexing
    
    idx <- seq(from=1, to=(dim(inputs$ts)[1]),4)
    # 1950 onwards
    cut <- seq(from=(300*12+1),to=length(idx))
    # 1970 onwards
    # cut2 <- seq(from = (300*12+241),to = length(idx))
    
    # TOTAL system biomass density (tsb),g C m-2,all primary producers and consumers
    if (varname=="tsb")           var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- (agg$TotalUbiomass[cut] + agg$TotalVbiomass[cut] + agg$TotalW[cut])
    
    # # TOTAL consumer biomass density (tbc),g C m-2, all consumers (trophic level >1, vertebrates and invertebrates)           
    
    if (varname=="tcb")           var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- (agg$TotalUbiomass[cut] + agg$TotalVbiomass[cut])
    
    # # Biomass density (by functional group / size class) (Bi),g C m-2,Provide name of each size class (<class>) and functional group (<group>) used, and provide a  definition of each class/group 
    # 
    if (varname=="b")             var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$TotalUbiomass[cut] 
    
    if (varname=="b10cm")         var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Ubiomass10plus[cut] 
    
    if (varname=="b30cm")         var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Ubiomass270plus[cut] 
    # 
    if (varname=="b-bendet")      var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$TotalVbiomass[cut] 
    
    if (varname=="b10cm-bendet")  var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Vbiomass10plus[cut] 
    # 
    if (varname=="b30cm-bendet")  var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Vbiomass270plus[cut] 
    #  
    if (varname=="bdet")          var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$TotalW[cut]
    # 
    # #total catches in functional groups
    # 
    # #sum catches ( already in grams per yr, across size classes) 
    # #and then they need to be converted to g ww per m^2 
    # 
    # # Total catch of all commerical groups, g m-2 wet weight, all consumers
    if (varname=="tc")            var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$TotalUcatch[cut] + agg$TotalVcatch[cut]
    # 
    # #catches in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
    # 
    # 
    # # Catch (by functional group / size class) (Ci) ,g  m-2 wet weight
    # 
    if (varname=="c10cm")         var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Ucatch10plus[cut]
    #   
    if (varname=="c30cm")         var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Ucatch270plus[cut]
    #  
    if (varname=="c10cm-bendet")  var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Vcatch10plus[cut]
    # 
    if (varname=="c30cm-bendet")  var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:length(cut)] <- agg$Vcatch270plus[cut]
    #  
    # 
    # # Size spectrum slopes 
    # 
    # # normalised  - plot of log (biomass density/arithmetic width of size bin) vs log size class
    # 
    #   if (varname=="bss-pelpred") var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:dim(agg)[1]]
    #   #
    #   if (varname=="bss-bendet")  var[paste(inputs$depth$lon),paste(inputs$depth$lat),1:dim(agg)[1]]
    #
    #print every 100th (and the very last) igrid as progress indicator 
    if (igrid%%100 == 0 | igrid == length(grids))
      print(paste(run, varname, igrid))    
    
  }
  # end for loop
  
  # save var as R file 
  #save(var,file = paste(savetopath,"dbpm_",gcm,"_",run,"_","no-fishing","_","no-oa","_",varname,".RData",sep = ""))
  
  # # write full array (tsc from above) to netcdf 
  ncvar_put(ncnew, var.nc, var)
  # 
  # # close netcdf file 
  nc_close(ncnew) 
  
}

# end mknetcfd function
