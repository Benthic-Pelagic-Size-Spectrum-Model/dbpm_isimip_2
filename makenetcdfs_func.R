
# dbpm.variables <- read.csv("./dbpm-ryan/variables.csv", header = TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
# CN version - this is important when you create the netcdf file at the end of the function - 
###### disaggregated outputs ----

# this si not used - see lines ~227
#dbpm.variables<-data.frame(name = vars2make, 
#                           description = c("abundance density of fish",
#                                           "abundance density of detritivores", 
#                                           "growth fish",
#                                           "growth detritivores"), # 'detritus pool',"dx","bins"), 
#                           units = c("g/m^2","g/m^2", "unitless","unitless")) # "unitless","unitless"))

mknetcdf<-function(varname, protocol, inputpath, datapath, savetopath, grids, isave, yearRange){

  # CN  trial 
  # varname = vars2make[1]
  # protocol = prots[1]
  # prot_full_name = prot_full_names[1]
  # inputpath = input_loc
  # datapath = output_loc
  # savetopath = save_loc
  # igrid = 1
  
  ## Cut values and lat-lons have been checked for CESM
  # cut1 <- 1:156 # years from 1850-01 to 2005-12
  # cut2 <- 157:251 # years from 2006-01 to 2100-12
  # lon <- 0:359 # CN lon and lat have different format 
  # lat <- -89.5:89.5
  # t1 <- (0:155)*12 # months since 1850-1-1
  # t2 <- (156:250)*12 # months since 1850-1-1 # CN should this be as cut2 above
    
  # CN version: # both model use this 
  lon<- -180:179
  lat<- -89.5:89.5
  
  # CN time dimention: 
  # we don't need cut1 and cut2 and t1 ans t2 as we are not splitting outputs in 2 files
  # "t" above is the n of months in "cut" years 
  # we save results yearly (or however time is defined), but the t1 and t2 above define months and the description of this variable below is 'months since...'
  # should we have a "cut", and a "t" = cut*12? 
  t<-1:length(isave) # Ryan suggested: t<-0:(dim(result_set$U)[2]-1) or t<-0:length(isave)-1 but dimentions do not add up
  
  # CN size dimention: 
  # we need a size dimention if we are saving abundaces by size
  # this dimention needs to be considered in all the below descriptions 
  size<-1:dim(result_set$U)[1] 
  
  # storage for the gridded outputs for current variable 
  # var1 <- array(NA, dim = c(length(lon), length(lat), length(cut1)), dimnames = list(lon,lat,t1))
  # var2 <- array(NA, dim = c(length(lon), length(lat), length(cut2)), dimnames = list(lon,lat,t2))
  
  # CN we only need one var matrix
  var <- array(NA, dim = c(length(lon), length(lat), length(size), length(t)), dimnames = list(lon,lat,size,t))
  
  # get netcdf names
  # if(prot_full_name != 'clim'){
  # name1 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_", prot_full_name, "_nosoc_co2_", varname, "_global_annual_1850-2005.nc4", sep = "")
  # name2 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_", prot_full_name, "_nosoc_co2_", varname, "_global_annual_2006-2100.nc4", sep = "")
  #}
  
  # if(prot_full_name == 'clim'){
  # name1 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_historical_nosoc_co2_", varname, "_global_annual_1850-2005.nc4", sep = "")
  # name2 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_rcp85_nosoc_co2_", varname, "_global_annual_2006-2100.nc4", sep = "")  
  # }
  
  # CN we need 1 name for the only file we are saving
  # we don't need to differentiate between clim and others
  # we changed name and acronisms
  # name <- paste(savetopath, protocol, "/dbpm_ipsl_", varname, "_global_annual.nc4", sep = "")
  name <- paste(savetopath, "/dbpm_ipsl_cm6a_lr_", "nobc_", protocol, "_nosoc_default_", "disaggregated_", varname, "_global_montly_", yearRange, ".nc4", sep = "") # according to protocol see below 
  
  # nc_names <- c(name1, name2)
  
  # CN version
  nc_names<-name
  
  # pb = txtProgressBar(min = 0, max = length(grids), initial = 1, style = 3) # Initial progress bar
  
  for (igrid in grids) {
    
    # CN  trial 
    # igrid = 1
    
    data_filename <- paste(datapath, protocol, "/", "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
    
    if(file.exists(data_filename) == TRUE){
      
      # agg <- readRDS(data_filename) 
      # CN version
      result_set <- readRDS(data_filename) 
    
      # load the inputs to get lat , lon positions
      # input_filename <- paste(inputpath, protocol, "/",protocol,'_' ,igrid, "_cesm", '.rds', sep = "")
      # inputs <- readRDS(input_filename)
    
      # CN version of the above  
      input_filename <- paste(inputpath, protocol, "/", "grid" ,'_' ,igrid, "_IPSL-CM6A-LR_", protocol,'.rds', sep = "")
      inputs <- readRDS(input_filename)
    
      # CN see next function for aggregated outputs: 
      # TOTAL system biomass density (tsb),g C m-2,all primary producers and consumers and detritus. 
      # CN: the first bit also adds phyto (lphy and sphy) and converts it to something (e.g. from wet weight to carbon?) 
      # if (varname=="tsb"){
        # var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut1] + agg$TotalVbiomass[cut1] + agg$TotalW[cut1] + (inputs$ts$lphy[cut1] + inputs$ts$sphy[cut1])*12.0107)
        # var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut2] + agg$TotalVbiomass[cut2] + agg$TotalW[cut2] + (inputs$ts$lphy[cut2] + inputs$ts$sphy[cut2])*12.0107)
        # }
      # TOTAL consumer biomass density (tbc),g C m-2, all consumers (trophic level >1, vertebrates and invertebrates)            
      # if (varname=="tcb"){
        # var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut1] + agg$TotalVbiomass[cut1])
        # var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut2] + agg$TotalVbiomass[cut2])
      # }
      # Biomass density (by functional group / size class) (Bi),g C m-2,Provide name of each size class (<class>) and functional group (<group>) used, and provide a  definition of each class/group 
      # if (varname=="b10cm"){
        # var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass10plus[cut1] + agg$Vbiomass10plus[cut1] 
        # var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass10plus[cut2] + agg$Vbiomass10plus[cut2] 
      # }
      # if (varname=="b30cm"){
        # var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass270plus[cut1] + agg$Vbiomass270plus[cut1]
        # var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass270plus[cut2] + agg$Vbiomass270plus[cut2] 
      # }

      # CN version of the above with disaggregated data 
      # Abundance density of fish (U), g/m-2 - CN: does this apply? all consumers (trophic level >1, vertebrates and invertebrates)           
      if (varname=="U"){
        # CN: what happens if a grid includes NAs as per aggregated data?  
        if("U" %in% names(result_set) == TRUE){
          result_set$U<-result_set$U[,isave]
          var[paste(inputs$depth$lon),paste(inputs$depth$lat),,] <- result_set$U # you only need t if you've selected a t range above  
        } else {
          var[paste(inputs$depth$lon),paste(inputs$depth$lat),,] <- NA
        }
      }
      
      if (varname=="GGU"){
        # need to do the same as above 
      }
      
      if (varname=="V"){
      }
      
      if (varname=="GGV"){
      }
    
      # CN - need to think about this as it has a different dimention (no size) and it's thus similar to the aggragated outputs 
      # CN move below? 
      # if (varname=="W"){
        # var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- result_set$V[,t]
      # }
    
      }
    
    # setTxtProgressBar(pb, igrid) # Update progress bar
  }
  
  ## WRITE NETCDFS
  # var_list = list(var1, var2)
  var_list = var
  
  #for(i in 1:length(nc_names)){
    
  curr_array = var_list #[[i]]
  # curr_array[curr_array > 10000] = NA # CN: don't know what this is and was relevant to aggregated data 
  
  #setwd("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/")
  new_nc <- create.nc(nc_names)#[i])
  
  dim.def.nc(new_nc, 'lon', dimlength = length(lon)) 
  var.def.nc(new_nc, 'lon', 'NC_FLOAT', 'lon')
  var.put.nc(new_nc, 'lon', lon) # c(-179:179)) # CN 
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'long_name', value = 'longitude')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'units', value = 'degrees')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR',  name = 'axis', value = 'X')
  
  dim.def.nc(new_nc, 'lat', dimlength = length(lat))
  var.def.nc(new_nc, 'lat', 'NC_FLOAT', 'lat')
  var.put.nc(new_nc, 'lat', lat) # c(-89.5:89.5))
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'long_name', value = 'latitude')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'units', value = 'degrees_north')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'axis', value = 'Y')
  
  # CN define all about size as per lat and lon above 
  dim.def.nc(new_nc, 'size', dimlength = length(size))
  var.def.nc(new_nc, 'size', 'NC_FLOAT', 'size')
  var.put.nc(new_nc, 'size', size) # c(-89.5:89.5))
  att.put.nc(new_nc, variable = 'size', type = 'NC_CHAR', name = 'long_name', value = 'size')
  att.put.nc(new_nc, variable = 'size', type = 'NC_CHAR', name = 'units', value = 'log10g')
  att.put.nc(new_nc, variable = 'size', type = 'NC_CHAR', name = 'axis', value = 'Z')
  
  dim.def.nc(new_nc, 'time', unlim = TRUE)
  var.def.nc(new_nc, 'time', 'NC_DOUBLE', 'time')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'long_name', value = 'time')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'units', value = 'months since 1850-1-1 00:00:00')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'calendar', value = 'standard')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'axis', value = 'T')
  
  var.def.nc(new_nc, varname, "NC_FLOAT", c('lon', 'lat', 'size','time')) # CN added size 
  # att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'long_name', value = dbpm.variables$description[dbpm.variables$name==varname])
  att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'short_name', value = varname)
  # att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'units', value = dbpm.variables$units[dbpm.variables$name==varname])
  att.put.nc(new_nc, variable = varname, type = 'NC_FLOAT', name = 'missing_value', value = 1e20)
  var.put.nc(new_nc, varname, curr_array)
  
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Created by Camilla Novaglio, with help from Julia Blanchard, Ryan Heneghan, and Just Berkhout <camilla.novaglio@gmail.com>")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "institution", type = "NC_CHAR", value = "Institute for Marine and Antarctic Studies")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "date_created", type = "NC_CHAR", value = date())
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "comments", type = "NC_CHAR", value = "Model output for ISIMIP3b")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "length-weight_conversion", type = "NC_CHAR", value = 'wet weight = 0.01*(length^3)') # Cn ok with  this bot but only used if saving aggregated data
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "ph_input_used", type = "NC_CHAR", value = "no") # CN not sure about this - need to ask 
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "diazotroph_input_used", type = "NC_CHAR", value = "yes, diaz carbon biomass added to large phyto carbon biomass") # CN not sure about this - need to ask
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "wet-weight to carbon conversion", type = "NC_CHAR", value = "0.0352") # CN not sure about this, current outputs should be in wet weight - need to ask

  close.nc(new_nc)
  
  # }
  
}

# check  
# nc <- open.nc("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/dbpm_ipsl_U_global_annual.nc4")
# print.nc(nc)
# u <- var.get.nc(nc, "U")
# dim(u)
# which(is.na(u), arr.ind = TRUE)
# u[243,30,,1]  
# lon <- var.get.nc(nc, "lon")

# plot(u[15,12,1,])
# plot(result_set$U[180,])

# which is NA print results:
# 700991080 #  rows with NA - how many  grid cells? 
#  700991080/(181*164) #  23615.12 are NAs and these should be land 
# 360*180  # 64800 total cell in file; ocean celles for which we have data are ~41000 - check 

#dim1 dim2 dim3 dim4
#[1,]    1    1    1    1
#[2,]    2    1    1    1
#[3,]    3    1    1    1
#[4,]    4    1    1    1
#[ reached getOption("max.print") -- omitted 700991030 rows ]

###### aggregated outputs ----
mknetcdf_agg<-function(varname, protocol, inputpath, datapath, savetopath, grids, other_param, yearRange){
  
  # CN  trial 
  # varname = vars2make[1]
  # protocol = prots[4] 
  # inputpath = input_loc
  # datapath = output_loc
  # savetopath = save_loc
  # igrid = 1
  # yearRange = yearRange[4]
  
  ## Cut values and lat-lons for earth models 
  lon<- -180:179
  lat<- -89.5:89.5
  t<-1:length(isave)
  
  # # storage for the gridded outputs for current variable 
  # var <- array(NA, dim = c(length(lon), length(lat), length(t)), dimnames = list(lon,lat,t))
  # CN NOTE!!!!!!! : should this be 1e20 instead?
  var <- array(1e20, dim = c(length(lon), length(lat), length(t)), dimnames = list(lon,lat,t))
  
  # get netcdf names
  # nc_names <- paste(savetopath, protocol, "/dbpm_ipsl_", "agg_",varname, "_global_annual.nc4", sep = "") # /dbpm_cesm1-bgc_nobc_rcp85_nosoc_co2_
  nc_names <- paste(savetopath, protocol,"/dbpm_ipsl_cm6a_lr_", "nobc_", protocol, "_nosoc_default_",varname, "_global_montly_", yearRange, ".nc4", sep = "") 
  # according to protocol - need to change year for each protocol 
  # bias-adjustment = nobc 
  # soc-scenario = nosoc # CN not sure about this! just using Ryan def (COULD BE 'nat' as in no fishing)
  # sens-scenario = default
  
  # pb = txtProgressBar(min = 0, max = length(grids), initial = 1, style = 3) # Initial progress bar
  
  for (igrid in grids) {
    
    # CN  trial 
    # igrid = 1
    
    data_filename <- paste(datapath, protocol, "/", "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
    
    if(file.exists(data_filename) == TRUE){
      
      # load model outputs 
      result_set <- readRDS(data_filename) 
      
      # load grid inputs   
      input_filename <- paste(inputpath, protocol, "/", "grid" ,'_' ,igrid, "_IPSL-CM6A-LR_", protocol,'.rds', sep = "")
      inputs <- readRDS(input_filename)
      
      # CN calculate aggregated outputs here (copied/moved from inside models - see comments in batch_run_create_output_netcdf.r) 
      # U = pelagic
      # V = demeral 
      # W = detritus 
      TotalUbiomass <- apply(result_set$U[other_param$ref:other_param$Nx,other_param$isave]*other_param$dx*10^other_param$x[other_param$ref:other_param$Nx],2,sum) 
      TotalVbiomass <- apply(result_set$V[other_param$ref.det:other_param$Nx,other_param$isave]*other_param$dx*10^other_param$x[other_param$ref.det:other_param$Nx],2,sum) 
      # TotalW <- result_set$W[other_param$isave]
      # consumer_spec <- result_set$U[other_param$ref:other_param$Nx,other_param$isave] + result_set$V[other_param$ref.det:other_param$Nx,other_param$isave]
    
      # CN wcut for new isimip requiremetns (30 and 90 cm as tresholds)
      wcut<-round(c(0.01*30^3,0.01*90^3)) # from cm to weight
      xcutref <- wcut # weight in log scale 
      for (i in 1:length(wcut)) xcutref[i] = (min(which(other_param$x >=log10(wcut[i]))))
      
      # TOTAL consumer biomass density ,g C m-2, all consumers (trophic level >1, vertebrates and invertebrates)  
      # CN note this should be g m-2 now as have not been converted in g C - CHECK!
      # NOTE!!! missing following step (see line 307 and 364 in runmodel_yearly.R):
      # convert all biomasses from g WW per m^3 to per m^2   
      # agg[,1:13] <- agg[,1:13] * min(agg$depth,100)
      if (varname=="tcb"){
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (TotalUbiomass + TotalVbiomass)
      }
      
      # if (varname=="tcblog10"){ #  total consumers biomass density in log10 weight bins
        #  TO DO - is this consumer_spec ??? but size bins need to change? it also requires another netcdf structure.... with size 
      # }
      
      if (varname=="tpb"){ # total pelagic biomass density, all pelagic consumers (trophic level >1, vertebrates and invertebrates)   
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- TotalUbiomass 
      }
      
      if (varname=="bp30cm"){ # biomass density of small pelagics <30 cm    
        bp30cm <- apply(result_set$U[1:xcutref[1]-1,isave]*other_param$dx*10^other_param$x[1:xcutref[1]-1],2,sum) # <30
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- bp30cm
      }
      
      if (varname=="bp30to90cm"){ # biomass density of medium pelagics <=30 to 90 cm   
        bp30to90cm <- apply(result_set$U[xcutref[1]:xcutref[2]-1,isave]*other_param$dx*10^other_param$x[xcutref[1]:xcutref[2]-1],2,sum) # >=30, <90 
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- bp30to90cm
      }
      
      if (varname=="bp90cm"){ # biomass density of large pelagics >=90 cm    
        bp90cm <- apply(result_set$U[xcutref[2]:other_param$Nx,isave]*other_param$dx*10^other_param$x[xcutref[2]:other_param$Nx],2,sum) # >=90
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- bp90cm
      }
      
      if (varname=="tdb"){ # total demersal biomass density, all demersal consumers (trophic level >1, vertebrates and invertebrates)   
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- TotalVbiomass 
      }
      
      if (varname=="bd30cm"){ # biomass density of small demersal <30 cm    
        bd30cm <- apply(result_set$V[1:xcutref[1]-1,isave]*other_param$dx*10^other_param$x[1:xcutref[1]-1],2,sum) # <30
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- bd30cm
      }
      
      if (varname=="bd30to90cm"){ # biomass density of medium demersal <=30 to 90 cm   
        bd30to90cm <- apply(result_set$V[xcutref[1]:xcutref[2]-1,isave]*other_param$dx*10^other_param$x[xcutref[1]:xcutref[2]-1],2,sum) # >=30, <90
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- bd30to90cm
      }
      
      if (varname=="bd90cm"){ # biomass density of large demersal >=90 cm    
        bd90cm <- apply(result_set$V[xcutref[2]:other_param$Nx,isave]*other_param$dx*10^other_param$x[xcutref[2]:other_param$Nx],2,sum) # >=90
        var[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- bd90cm
      }
      
    }
    
    # setTxtProgressBar(pb, igrid) # Update progress bar
  }
  
  ## WRITE NETCDFS

  curr_array = var 
  # curr_array[curr_array > 10000] = NA # CN: don't know what this is 

  new_nc <- create.nc(nc_names)
  
  dim.def.nc(new_nc, 'lon', dimlength = length(lon)) 
  var.def.nc(new_nc, 'lon', 'NC_FLOAT', 'lon')
  var.put.nc(new_nc, 'lon', lon) # c(-179:179)) # CN 
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'long_name', value = 'longitude')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'units', value = 'degrees')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR',  name = 'axis', value = 'X')
  
  dim.def.nc(new_nc, 'lat', dimlength = length(lat))
  var.def.nc(new_nc, 'lat', 'NC_FLOAT', 'lat')
  var.put.nc(new_nc, 'lat', lat) # c(-89.5:89.5))
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'long_name', value = 'latitude')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'units', value = 'degrees_north')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'axis', value = 'Y')
  
  dim.def.nc(new_nc, 'time', unlim = TRUE)
  var.def.nc(new_nc, 'time', 'NC_DOUBLE', 'time')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'long_name', value = 'time')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'units', value = 'months since 2015-1-1 00:00:00')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'calendar', value = 'standard')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'axis', value = 'T')
  
  var.def.nc(new_nc, varname, "NC_FLOAT", c('lon', 'lat','time'))
  # att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'long_name', value = dbpm.variables.agg$description[dbpm.variables.agg$name==varname])
  att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'short_name', value = varname)
  # att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'units', value = dbpm.variables.agg$units[dbpm.variables.agg$name==varname]) # need to figure this out!
  att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'units', value = "g/m^2") 
  att.put.nc(new_nc, variable = varname, type = 'NC_FLOAT', name = 'missing_value', value = 1e20)
  var.put.nc(new_nc, varname, curr_array)
  
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Created by Camilla Novaglio, with help from Julia Blanchard, Ryan Heneghan, and Just Berkhout <camilla.novaglio@gmail.com>")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "institution", type = "NC_CHAR", value = "Institute for Marine and Antarctic Studies")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "date_created", type = "NC_CHAR", value = date())
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "comments", type = "NC_CHAR", value = "Model output for ISIMIP3b")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "length-weight_conversion", type = "NC_CHAR", value = 'wet weight = 0.01*(length^3)') 
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "ph_input_used", type = "NC_CHAR", value = "no") # CN not sure about this - need to ask 
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "diazotroph_input_used", type = "NC_CHAR", value = "yes, diaz carbon biomass added to large phyto carbon biomass") # CN not sure about this - need to ask
  # att.put.nc(new_nc, variable = "NC_GLOBAL", name = "wet-weight to carbon conversion", type = "NC_CHAR", value = "0.0352") # CN not sure about this, current outputs should be in wet weight - need to ask
  
  close.nc(new_nc)
  
}

