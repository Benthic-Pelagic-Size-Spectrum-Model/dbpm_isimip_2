dbpm.variables <- read.csv("./dbpm-ryan/variables.csv", header = TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

mknetcdf<-function(varname, protocol, prot_full_name, inputpath, datapath, savetopath, grids){

  ## Cut values and lat-lons have been checked for CESM
  cut1 <- 1:156 # years from 1850-01 to 2005-12
  cut2 <- 157:251 # years from 2006-01 to 2100-12
  
  lon <- 0:359
  lat <- -89.5:89.5
  t1 <- (0:155)*12 # months since 1850-1-1
  t2 <- (156:250)*12 # months since 1850-1-1
    
  # storage for the gridded outputs for current variable 
  var1 <- array(NA, dim = c(length(lon), length(lat), length(cut1)), dimnames = list(lon,lat,t1))
  var2 <- array(NA, dim = c(length(lon), length(lat), length(cut2)), dimnames = list(lon,lat,t2))
  
  # get netcdf names
  if(prot_full_name != 'clim'){
  name1 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_", prot_full_name, "_nosoc_co2_", varname, "_global_annual_1850-2005.nc4", sep = "")
  name2 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_", prot_full_name, "_nosoc_co2_", varname, "_global_annual_2006-2100.nc4", sep = "")
  }
  
  if(prot_full_name == 'clim'){
  name1 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_historical_nosoc_co2_", varname, "_global_annual_1850-2005.nc4", sep = "")
  name2 <- paste(savetopath, protocol, "/dbpm_cesm1-bgc_nobc_rcp85_nosoc_co2_", varname, "_global_annual_2006-2100.nc4", sep = "")  
  }
  
  nc_names <- c(name1, name2)
  
  pb = txtProgressBar(min = 0, max = length(grids), initial = 1, style = 3) # Initial progress bar
  
  for (igrid in grids) {
    
    data_filename <- paste(datapath, protocol, "/", "dbpm_output_all_", igrid, '_', protocol, '.rds', sep = "") 
    
    if(file.exists(data_filename) == TRUE){
    agg <- readRDS(data_filename)
    
    # load the inputs to get lat , lon positions
    input_filename <- paste(inputpath, protocol, "/",protocol,'_' ,igrid, "_cesm", '.rds', sep = "")
    inputs <- readRDS(input_filename)
    
    # TOTAL system biomass density (tsb),g C m-2,all primary producers and consumers and detritus
    if (varname=="tsb"){
      var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut1] + agg$TotalVbiomass[cut1] + agg$TotalW[cut1] + (inputs$ts$lphy[cut1] + inputs$ts$sphy[cut1])*12.0107)
      var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut2] + agg$TotalVbiomass[cut2] + agg$TotalW[cut2] + (inputs$ts$lphy[cut2] + inputs$ts$sphy[cut2])*12.0107)
      }
    # # TOTAL consumer biomass density (tbc),g C m-2, all consumers (trophic level >1, vertebrates and invertebrates)           
    
    if (varname=="tcb"){
      var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut1] + agg$TotalVbiomass[cut1])
      var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- (agg$TotalUbiomass[cut2] + agg$TotalVbiomass[cut2])
    }
    
    # # Biomass density (by functional group / size class) (Bi),g C m-2,Provide name of each size class (<class>) and functional group (<group>) used, and provide a  definition of each class/group 
    # 
    
    if (varname=="b10cm"){
      var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass10plus[cut1] + agg$Vbiomass10plus[cut1] 
      var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass10plus[cut2] + agg$Vbiomass10plus[cut2] 
    }
    
    if (varname=="b30cm"){
      var1[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass270plus[cut1] + agg$Vbiomass270plus[cut1]
      var2[paste(inputs$depth$lon),paste(inputs$depth$lat),] <- agg$Ubiomass270plus[cut2] + agg$Vbiomass270plus[cut2] 
    }

    }
    
    setTxtProgressBar(pb, igrid) # Update progress bar
  }
  
  
  ## WRITE NETCDFS
  var_list = list(var1, var2)
  
  for(i in 1:length(nc_names)){
    
  curr_array = var_list[[i]]
  curr_array[curr_array > 10000] = NA
  
  new_nc <- create.nc(nc_names[i])
  
  dim.def.nc(new_nc, 'lon', dimlength = 360)
  var.def.nc(new_nc, 'lon', 'NC_FLOAT', 'lon')
  var.put.nc(new_nc, 'lon', c(0:359))
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'long_name', value = 'longitude')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR', name = 'units', value = 'degrees')
  att.put.nc(new_nc, variable = 'lon', type = 'NC_CHAR',  name = 'axis', value = 'X')
  
  dim.def.nc(new_nc, 'lat', dimlength = 180)
  var.def.nc(new_nc, 'lat', 'NC_FLOAT', 'lat')
  var.put.nc(new_nc, 'lat', c(-89.5:89.5))
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'long_name', value = 'latitude')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'units', value = 'degrees_north')
  att.put.nc(new_nc, variable = 'lat', type = 'NC_CHAR', name = 'axis', value = 'Y')
  
  dim.def.nc(new_nc, 'time', unlim = TRUE)
  var.def.nc(new_nc, 'time', 'NC_DOUBLE', 'time')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'long_name', value = 'time')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'units', value = 'months since 1850-1-1 00:00:00')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'calendar', value = 'standard')
  att.put.nc(new_nc, variable = 'time', type = 'NC_CHAR', name = 'axis', value = 'T')
  
  var.def.nc(new_nc, varname, "NC_FLOAT", c('lon', 'lat', 'time'))
  att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'long_name', value = dbpm.variables$description[dbpm.variables$name==varname])
  att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'short_name', value = varname)
  att.put.nc(new_nc, variable = varname, type = 'NC_CHAR', name = 'units', value = dbpm.variables$units[dbpm.variables$name==varname])
  att.put.nc(new_nc, variable = varname, type = 'NC_FLOAT', name = 'missing_value', value = 1e20)
  var.put.nc(new_nc, varname, curr_array)
  
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "author", type = "NC_CHAR", value = "Created by Ryan Heneghan, with help from Julia Blanchard and Just Berkhout <ryan.heneghan@gmail.com>")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "institution", type = "NC_CHAR", value = "Universitat Autonoma de Barcelona")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "date_created", type = "NC_CHAR", value = date())
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "comments", type = "NC_CHAR", value = "Impact model output for ISIMIP2b and FishMIP NPPvSST experimental protocol")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "length-weight_conversion", type = "NC_CHAR", value = 'wet weight = 0.01*(length^3)')
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "ph_input_used", type = "NC_CHAR", value = "no")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "diazotroph_input_used", type = "NC_CHAR", value = "yes, diaz carbon biomass added to large phyto carbon biomass")
  att.put.nc(new_nc, variable = "NC_GLOBAL", name = "wet-weight to carbon conversion", type = "NC_CHAR", value = "0.0352")

  close.nc(new_nc)
  }
  
  
}

