
rm(list=ls())

library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(patchwork)
library(lubridate)

var <- c("tpb", "tcb", "bp30cm", "bp30to90cm", "bp90cm")

mollCRS <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
mollCRS_no <- 54009

robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
robCRS_no <- 54030

lonlatCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
lonlatCRS_no <- 4326

plotGlobalChange <- function(all, tit, w_sf, clim){

  # CN trial
  # all = all 
  # tit 
  # w_sf = world_sf
  # clim
  
  if(length(all)>1){
    x <- all$hist 
    y = all$fut 
  }else{ # this should be right - CHECK!
    x <- all$hist
  }

  if(length(all)>1){
    # avarage by month first ? 
    # avarage 1990-1999
    refFirstYear<-(1990-1950)*12
    # ((refFirstYear+10*12)-refFirstYear)/12
    # x 1950-2014
    # y 2015-2100
    # y from 2090-2099
    refLastYear<-(2091-2015)*12
    # (dim(y)[3]-refLastYear)/12 # 10 years avarage
    out <- calc(x[[refFirstYear:(refFirstYear+10*12)]], mean) # Average 1990-1999 *12 if model resolution is months 
    out <- addLayer(out, calc(y[[refLastYear:dim(y)[3]]], mean)) #  Average last decade
    x_change <- ((out[[2]] - out[[1]])/out[[1]]) * 100
  }else{ # this should be right - CHECK!
    refFirstYear<-(1990-1950)*12
    refLastYear<-(2091-1950)*12
    out <- calc(x[[refFirstYear:(refFirstYear+10*12)]], mean) # as above 
    out <- addLayer(out, calc(x[[refLastYear:dim(x)[3]]], mean)) 
    # previous: first vs last week 
    # out <- calc(x[[1:10*12]], mean) # Average first decade *12 if model resolution is months 
    # out <- addLayer(out, calc(x[[(dim(x)[3]-9*12):dim(x)[3]]], mean)) #  Average last decade
    x_change <- ((out[[2]] - out[[1]])/out[[1]]) * 100
  }

  # OR as per function below - but should be the same ! 
  # df2$BiomassChange = (df2$Biomass - mean(df2$Biomass[1:10], na.rm = TRUE))/mean(df2$Biomass[1:10], na.rm = TRUE) * 100
  
  dat <- st_as_sf(rasterToPolygons(x_change))
  dat <- st_transform(dat, crs = st_crs(robCRS)) # Convert to Robinson Projection

  gg <- ggplot() +
    geom_sf(data = dat, aes(fill = layer), colour = NA) +
    geom_sf(data = w_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient2(name = "Total Biomass Change (%)",
                         limits = clim,
                         midpoint = 0,
                         low = "red",
                         mid = "white",
                         high = "blue",
                         position = "right",
                         na.value = "grey80",
                         guide = "colourbar",
                         oob = scales::squish) +
    ggtitle(tit) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.title = element_text(angle = -90),
          panel.background = element_blank(),
          title = element_text(size = 8),
          legend.key.height = unit(2, "cm"),
          legend.title.align = 0.5) +
    guides(fill = guide_colourbar(title.position = "right"))

  # print(gg)
  
  return(gg)
}

plotGlobalYear <- function(dat, tit, w_sf){

  # CN trial 
  #dat<-fut[[(86*12)-2]]
  #w_sf = world_sf
  
  names(dat) <- "layer"
  dat <- st_as_sf(rasterToPolygons(dat)) %>%
    st_transform(crs = st_crs(robCRS)) # %>% # Convert to Robinson Projection
    # mutate(layer = log10(layer/1e3)) # Convert to kg

  gg <- ggplot() +
    geom_sf(data = dat, aes(fill = layer), colour = NA) +
    geom_sf(data = w_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient(name = expression("Total Biomass (g m"^-2*")"), # (log"[10]*"(kg m"^-2*"))"),
                        limits = c(quantile(dat$layer, .10), quantile(dat$layer, .90)),
                        low = "yellow",
                        high = "red",
                        position = "right",
                        na.value = "grey80",
                        guide = "colourbar",
                        oob = scales::squish) +
    ggtitle(tit) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.title = element_text(angle = -90),
          panel.background = element_blank(),
          title = element_text(size = 8),
          legend.key.height = unit(1, "cm"),
          legend.title.align = 0.5) +
    guides(fill = guide_colourbar(title.position = "right"))

  return(gg)
}

plotTimeseries <- function(all, tit){
  
  #x = hist 
  #y = fut
  
  #  ssp126
  # hist<-stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/dbpm_ipsl_cm6a_lr_nobc_historical_nat_default_tcb_global_montly_1850_2014.nc4"))
  # fut <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4"))
  # tit <- paste0("IPSL SSP126 ",var[v]," (1850-2100 Change)")
  
  #  ssp585 - doe not seem right....
  # hist <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/dbpm_ipsl_cm6a_lr_nobc_historical_nat_default_tcb_global_montly_1850_2014.nc4"))
  # fut <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp585/dbpm_ipsl_cm6a_lr_nobc_ssp585_nat_default_tcb_global_montly_2015_2100.nc4"))
  # tit <- paste0("IPSL SSP585",var[v]," (1850-2100 Change)")
  
  if(length(all)>1){
    out <- stack(all$hist, all$fut) 
  }else{
    out <- all$hist
  }
  
  # out <- stack(hist, fut) 
  
  # dim(hist) # X9.96920996838687e.36.1
  # dim(fut) # X9.96920996838687e.36.1 # I don't understand these names ..... are we sure that fut comes after hist? 
  # dim(out)
  df <- as.data.frame(out, xy = TRUE) #%>%
  # dim(df)
  
  # colnames as dates... fix problem
  date = seq(as.Date("1950-1-1"), as.Date("2100-11-1"), by = "months")
  if(length(all)==1){
    date = seq(as.Date("1950-1-1"), as.Date("2100-12-1"), by = "months")
  } # theis is for the picontrol case....  
  length(date) # lost with date..... need to check 
  date = as.character(date)
  colnames(df)<-c("x","y",date)
  
  df<-pivot_longer(df,!c(x,y), names_to = "Date", values_to = "Biomass") 
  
  df<-mutate(df, Date = ymd(Date),
             Year = year(Date),
             Month = month(Date))
  
  # explore last dot point - why so high biomass?????? 
  # a<-filter(df, Year == 2100, Month %in% c(10,11,12))
  # a<-filter(a, y == 89.5)
  # head(a)
  # a$Biomass
  # df<-filter(df, Date < "2100-12-01") # done already outside function
  # explore NA or 1e20 values 
  # trial <- filter(df, Year == 2011, 
  #                 Month  == 10, 
  #                 is.na(Biomass))
  # nrow(trial) # should be 23473 (land - OK)
  # filter years - already done outside function
  # df2<-filter(df2, Year>=1950) # for consistency with Zoom 
    
  df2 <- df %>%
    group_by(Year) %>%
    summarise(Biomass = median(Biomass, na.rm = TRUE)) #,
              # .groups = "keep") #  not sure what this is 
  
  # CN adding  - make it as Lotze et al 
  # consider only 1970 owards and calcualte changes from 1990-1999 decade 
  df2<- filter(df2, Year>=1970)
  refDecade <- df2 %>% 
    filter(Year >= 1990, Year <=2000)
  refDecade<-mean(refDecade$Biomass, na.rm = TRUE)
  
  # This doesn't seem to work in mutate. It just returns 0
  df2$BiomassChange = (df2$Biomass - refDecade)/refDecade * 100
  
  # CN stop adding - use line below if you consider all years and do the mean over first decade 
  # This doesn't seem to work in mutate. It just returns 0
  # df2$BiomassChange = (df2$Biomass - mean(df2$Biomass[1:10], na.rm = TRUE))/mean(df2$Biomass[1:10], na.rm = TRUE) * 100
  
  gg <- ggplot(data = df2, aes(x = Year, y = BiomassChange)) +
    geom_line() +
    ylim(-5,3)+
    geom_smooth(method = "lm") +
    ggtitle(tit) +
    theme_bw() +
    theme(title = element_text(size = 8)) +
    ylab("Total Biomass Change (%)")
  
  # original CODE - see file in dropbox  
  # CN check data  
  # nc <- open.nc("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4")
  # temp <- var.get.nc(nc, "tcb")
  # temp1<-temp[,,1]
  # temp1[1,180] # lat -180 (fist) long 89.5 (last) month 1.1.1850 (first)
  # fut <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4"))
  # df[1,1:3] # same as above - OK
  
  rm(out)
  return(gg)
}

# Download and process world outline
world <- ne_countries(scale = "medium", returnclass = "sf")
world_sf <- st_transform(world, crs = st_crs(robCRS)) # Convert to different CRS

#for(v in 1:length(var)){
  
# CN trial   
v = 2  

  gg_map <- list()
  gg_map1950 <- list()
  gg_map2100 <- list()
  gg_ts <- list()
  clim <- c(-50, 50)
  
  ##### CMIP6 ----
  
  # ssp126
  hist<-stack("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/dbpm_ipsl_cm6a_lr_nobc_historical_nat_default_tcb_global_montly_1850_2014.nc4")
  fut <- stack("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4")
  tit <- paste0("IPSL SSP585 ",var[v]," (1950-2100 Change CMIP6)")
  
  ### CN trial 
  # explore the file before plotting and get some values 
  # nc <- open.nc("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4")
  # class(nc)
  # print.nc(nc)
  # nc_data <- read.nc(nc)
  # time_units <- att.get.nc(nc, "time", "units")
  # lat_units <- att.get.nc(nc, "lat", "units")
  # mytcb <- nc_data$tcb
  # mylat <- nc_data$lat
  # mylon <- nc_data$lon # something wrong here but length is 360 so should be good? ... 
  # length(mylon)
  # mytime<-nc_data$time # something wrong here but this should be sequential (0 to 1031?) ... 
  # length(mytime)

  # cut 1850-1950 for hist data  
  cut<-((2015-1850)*12) - ((2015-1950)*12) # from beginning of 1850 to end of 2014
  hist<-dropLayer(hist, seq(1:cut))
  # fut last month of projections (high values need to check why)
  fut<-dropLayer(fut, dim(fut)[3])
  # check 1e20 and replace with NA 
  # trial<-dropLayer(fut, seq(1,1030))
  # trial<-getValues(trial)
  # max(trial, na.rm=TRUE)
  # length(trial[is.na(trial)]) # it looks like they have been already replaced by the stack() function
  # library('RNetCDF')
  # nc <- open.nc("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4")
  # nc <- var.get.nc(nc, "tcb")
  # nc<-nc[,,1030]
  # length(nc[nc == max(nc)]) # yes it has 
  # nc[nc == max(nc)]<-NA
  # image(nc)

  all<-list(hist = hist, fut = fut)
  
  gg_map[[1]] <- plotGlobalChange(all, tit, world_sf, clim)
  gg_ts[[1]] <- plotTimeseries(all, tit)

  # CN in your case the model resultion is month so 86 does not mean anything - FIX OPPOSITE!!!!!
  # this gives the same map as image above - but the above is much more detailed, is the color scale important? 
  gg_map2100[[1]] <- plotGlobalYear(fut[[(86*12)-2]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut, all)
  
  #  ssp585
  hist<-stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/dbpm_ipsl_cm6a_lr_nobc_historical_nat_default_tcb_global_montly_1850_2014.nc4"))
  fut <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp585/dbpm_ipsl_cm6a_lr_nobc_ssp585_nat_default_tcb_global_montly_2015_2100.nc4"))
  tit <- paste0("IPSL SSP585 ",var[v]," (1950-2100 Change CMIP6)")
  
  cut<-((2015-1850)*12) - ((2015-1950)*12) # from beginning of 1850 to end of 2014
  hist<-dropLayer(hist, seq(1:cut))
  fut<-dropLayer(fut, dim(fut)[3])
  all<-list(hist = hist, fut = fut)
  
  gg_map[[2]] <- plotGlobalChange(all, tit, world_sf, clim)
  gg_ts[[2]] <- plotTimeseries(all, tit)
  gg_map2100[[2]] <- plotGlobalYear(fut[[(86*12)-2]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut, all)
  
  #  picontrol  
  hist <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/picontrol/dbpm_ipsl_cm6a_lr_nobc_picontrol_nat_default_tcb_global_montly_1850_2100.nc4"))
  tit <- paste0("IPSL PRE-INDUSTRIAL ",var[v]," (1950-2100 Change CMIP6)")
  cut<-((2015-1850)*12) - ((2015-1950)*12) # from beginning of 1850 to end of 2014
  hist<-dropLayer(hist, seq(1:cut))
  all<-list(hist = hist)
  gg_map[[3]] <- plotGlobalChange(all, tit, world_sf, clim)
  gg_ts[[3]] <- plotTimeseries(all, tit) 
  gg_map2100[[3]] <- plotGlobalYear(hist[[(150*12)-2]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist,all)
  
  # print here in dbpm in gem48 and then get it through github? 
  # Jase method does not work here - use patchwork 
  # adjust legend first 
  # gg_map2100[[3]]<-gg_map2100[[3]]+theme(legend.position = "none")
  # gg_map2100[[2]]<-gg_map2100[[2]]+theme(legend.position = "none")
  # gg_map[[3]]<-gg_map[[3]]+theme(legend.position = "none")
  # gg_map[[2]]<-gg_map[[2]]+theme(legend.position = "none")
  
  library(patchwork)
  # getwd()
  # ?pdf()
  # setwd("/Users/nov017/Dropbox/Mizer-fleet_extension/plot/FD/Final")
  pdf("DBPM_IPSLmaps_CMIP6.pdf", height=18, width=20) # units ='in', res=300)
  (gg_map2100[[3]]+gg_map[[3]]+gg_ts[[3]])/
  (gg_map2100[[1]]+gg_map[[1]]+gg_ts[[1]])/
  (gg_map2100[[2]]+gg_map[[2]]+gg_ts[[2]])
  dev.off()
  
  ##### CMIP5 ----
  
  gg_map_cmip5 <- list()
  gg_map1950_cmip5 <- list()
  gg_map2100_cmip5 <- list()
  gg_ts_cmip5 <- list()
  
  # ssp126
  # explore the file before plotting and get some values 
  # nc <- open.nc("/../../rd/gem/private/fishmip_outputs/aug_2017/netcdf/dbpm_ipsl-cm5a-lr_rcp26_no-fishing_no-oa_tcb.nc")
  # nc_stack <- stack("/../../rd/gem/private/fishmip_outputs/aug_2017/netcdf/dbpm_ipsl-cm5a-lr_rcp26_no-fishing_no-oa_tcb.nc")
  # class(nc)
  # print.nc(nc)
  # nc_data <- read.nc(nc)
  # time_units <- att.get.nc(nc, "time", "units")
  # lat_units <- att.get.nc(nc, "lat", "units")
  # mytcb <- nc_data$tcb
  # mylat <- nc_data$lat
  # mylon <- nc_data$lon 
  # length(mylon)
  # mytime<-nc_data$time  
  
  hist <- stack("/../../rd/gem/private/fishmip_outputs/aug_2017/netcdf/dbpm_ipsl-cm5a-lr_rcp26_no-fishing_no-oa_tcb.nc")
  tit <- paste0("IPSL SSP126 ",var[v]," (1950-2100 Change CMIP5)")
  # convert long 0:360 to -180:180 
  hist_rotate<-raster::rotate(hist)
  all<-list(hist = hist_rotate)
  gg_map_cmip5[[1]] <- plotGlobalChange(all, tit, world_sf, clim)
  gg_ts_cmip5[[1]] <- plotTimeseries(all, tit)
  gg_map2100_cmip5[[1]] <- plotGlobalYear(hist_rotate[[(150*12)-2]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, all,hist_rotate)
  
  ## ssp585 # change file! 
  hist <- stack("/../../rd/gem/private/fishmip_outputs/aug_2017/netcdf/dbpm_ipsl-cm5a-lr_rcp85_no-fishing_no-oa_tcb.nc")
  tit <- paste0("IPSL SSP585 ",var[v]," (1950-2100 Change CMIP5)")
  # convert long 0:360 to -180:180 
  hist_rotate<-raster::rotate(hist)
  all<-list(hist = hist_rotate)
  gg_map_cmip5[[2]] <- plotGlobalChange(all, tit, world_sf, clim)
  gg_ts_cmip5[[2]] <- plotTimeseries(all, tit)
  gg_map2100_cmip5[[2]] <- plotGlobalYear(hist_rotate[[(150*12)-2]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, all,hist_rotate)
  
  library(patchwork)
  # getwd()
  # ?pdf()
  # setwd("/Users/nov017/Dropbox/Mizer-fleet_extension/plot/FD/Final")
  pdf("DBPM_IPSLmaps_CMIP5.pdf", height=12, width=20) # units ='in', res=300)
  (gg_map2100_cmip5[[1]]+gg_map_cmip5[[1]]+gg_ts_cmip5[[1]])/
  (gg_map2100_cmip5[[2]]+gg_map_cmip5[[2]]+gg_ts_cmip5[[2]])
  dev.off()
  
  
  
  
  
  
  
  ##### CMIP5 Derek code ----
  library('ncdf4')
  nc <- nc_open("/../../rd/gem/private/fishmip_outputs/aug_2017/netcdf/dbpm_ipsl-cm5a-lr_rcp26_no-fishing_no-oa_tcb.nc")
  print(nc)
  # Look at the attributes and dimensions
  year1_historical = 1971
  year1_present = 2006
  
  # Extract important information
  data_attributes <-ncatt_get(nc,"tcb")
  missing_value <- data_attributes$'_FillValue'
  main_title <- data_attributes$long_name
  data_units <- data_attributes$units
  
  if (data_units != "g C / m^2"){ # original is gC m-2
    print("Unrecognized units")
    data_units = "unknown"
  }else {
    data_units = "kg km-2" 
  }

  # get variable of interest 
  lon1<-ncvar_get(nc, "lon")
  lat1<-ncvar_get(nc, "lat")
  time_months1<-ncvar_get(nc, "time")
  temp_array1 <-ncvar_get(nc,"tcb")
  dim(temp_array1)
  1812/12 # 151 years (1950-2100)
  
  # for present as well ... 
  
  # Extract yearly averages
  historical_catches = array(rep(0,length(time_months1)/12* length(lon1)* length(lat1)),c(length(time_months1)/12, length(lon1), length(lat1)))
  present_catches = array(rep(0,length(time_months2)/12* length(lon2)* length(lat2)),c(length(time_months2)/12, length(lon2), length(lat2)))
  temp_value = 0
  location = 1
  
  # historical
  for (ii in 1:length(lon1))
  {
    for (jj in 1:length(lat1))
    {
      # Extract values per grid cell
      temp_vec1 = vector(length = length(time_months1))
      for (hh in 1:length(time_months1))
      {
        if (!is.na(temp_array1[ii,jj,hh]))
          temp_vec1[hh] = temp_array1[ii,jj,hh]
      }
      
      # Extract yearly averages
      year_values1 = vector(length = length(time_months1)/12)
      year_vector1 = vector(length = 12)
      year_position = 1
      for (hh in 1:length(time_months1))
      {
        year_vector1[hh %% 12] = temp_vec1[hh]
        if ((hh %% 12) == 0)
        {
          year_values1[year_position] = mean(year_vector1)
          year_position = year_position + 1
          year_vector1 = vector(length = 12)
        }
        
      }
      
      # Fill in 3D matrix of annual averages
      historical_catches[,ii,jj] = year_values1
    }
  }
  
  # for present as well ....
  
  # Extract the 1971:1979 averages
  mat_1971_1979 = matrix(nrow = length(lon1), ncol = length(lat1))
  first_position_to_extract = 1971 - year1_historical + 1
  for (ii in 1:length(lon1))
  {
    for (jj in 1:length(lat1))
    {
      mat_1971_1979[ii,jj] = mean(historical_catches[first_position_to_extract:(first_position_to_extract + 8),ii,jj])
      # Convert top kg / km2
      mat_1971_1979[ii,jj] = mat_1971_1979[ii,jj] * 1000
    }
  }
  
  # Extract the 1990:1999 averages
  mat_1990_1999 = matrix(nrow = length(lon1), ncol = length(lat1))
  first_position_to_extract = 1990 - year1_historical + 1
  for (ii in 1:length(lon1))
  {
    for (jj in 1:length(lat1))
    {
      mat_1990_1999[ii,jj] = mean(historical_catches[first_position_to_extract:(first_position_to_extract + 9),ii,jj])
      # Convert to kg / km2
      mat_1990_1999[ii,jj] = mat_1990_1999[ii,jj] * 1000
    }
  }
  
  
  # Extract the 2090:2099 averages
  # original was about present - so lat2 and lon2 and present_catches
  mat_2090_2099 = matrix(nrow = length(lon1), ncol = length(lat1))
  first_position_to_extract = 2090 - year1_present + 1
  for (ii in 1:length(lon1))
  {
    for (jj in 1:length(lat1))
    {
      mat_2090_2099[ii,jj] = mean(historical_catches[first_position_to_extract:(first_position_to_extract + 9),ii,jj])
      # Convert top kg / km2
      mat_2090_2099[ii,jj] = mat_2090_2099[ii,jj] * 1000
      
    }
    
  }
  
  #Extract 2090-2099 relative to 1990-1999 (%)
  mat_delta26<- mat_2090_2099/mat_1990_1999*100 - 100
  mat_delta85<- mat_2090_2099/mat_1990_1999*100 - 100
  
  
  ### trial merge Jase adn Derek codes ?? not working try otherwise 
  all<-list(all = mat_delta26)
  tit = "trial"
  # gg_map_trial <- plotGlobalChange(all, tit, world_sf, clim)
  
  # from plotFishMip -
  # I don't have  the coastline data ... 
  library(raster)
  # install.packages("maptools")
  library(maptools)
  library(RColorBrewer)

  

  
  
  
  
  
  
  
  
  
  ##### original code Jase - looping through variables ----
  
  gg_map <- list()
  gg_map1950 <- list()
  gg_map2100 <- list()
  gg_ts <- list()
  clim <- c(-50, 50)

  ### PI
  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_gfdl-esm4_nobc_picontrol_nat_default_",var[v],"_global_annual_1950-2014.nc"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_gfdl-esm4_nobc_picontrol_nat_default_",var[v],"_global_annual_2015-2100.nc"))
  tit <- paste0("GFDL PRE-INDUSTRIAL ",var[v]," (1950-2100 Change)")
  gg_map[[1]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[1]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[1]] <- plotGlobalYear(hist[[1]], str_remove(tit,"-2100 Change"), world_sf)
  gg_map2100[[3]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_ipsl-cm6a-lr_nobc_picontrol_nat_default_",var[v],"_global_annual_1950-2014.nc"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_ipsl-cm6a-lr_nobc_picontrol_nat_default_",var[v],"_global_annual_2015-2100.nc"))
  tit <- paste0("IPSL PRE-INDUSTRIAL ",var[v]," (1950-2100 Change)")
  gg_map[[2]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[2]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[2]] <- plotGlobalYear(hist[[1]], str_remove(tit,"-2100 Change"), world_sf)
  gg_map2100[[4]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  ### SSP126
  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_gfdl-esm4_nobc_historical_nat_default_",var[v],"_global_annual_1950-2014.nc"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_gfdl-esm4_nobc_ssp126_nat_default_",var[v],"_global_annual_2015-2100.nc"))
  tit <- paste0("GFDL SSP126 ",var[v]," (1950-2100 Change)")
  gg_map[[3]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[3]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[5]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_ipsl-cm6a-lr_nobc_historical_nat_default_",var[v],"_global_annual_1950-2014.nc"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_ipsl-cm6a-lr_nobc_ssp126_nat_default_",var[v],"_global_annual_2015-2100.nc"))
  tit <- paste0("IPSL SSP126 ",var[v]," (1950-2100 Change)")
  gg_map[[4]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[4]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[6]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  ### SSP585
  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_gfdl-esm4_nobc_historical_nat_default_",var[v],"_global_annual_1950-2014.nc"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_gfdl-esm4_nobc_ssp585_nat_default_",var[v],"_global_annual_2015-2100.nc"))
  tit <- paste0("GFDL SSP585 ",var[v]," (1950-2100 Change)")
  gg_map[[5]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[5]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[7]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)

  hist <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_ipsl-cm6a-lr_nobc_historical_nat_default_",var[v],"_global_annual_1950-2014.nc"))
  fut <- stack(paste0("/Users/jason/Nextcloud/MME2Work/FishMIP/Phase1/Output/ZooMSS_ipsl-cm6a-lr_nobc_ssp585_nat_default_",var[v],"_global_annual_2015-2100.nc"))
  tit <- paste0("IPSL SSP585 ",var[v]," (1950-2100 Change)")
  gg_map[[6]] <- plotGlobalChange(hist, fut, tit, world_sf, clim)
  gg_ts[[6]] <- plotTimeseries(hist, fut, tit)
  gg_map2100[[8]] <- plotGlobalYear(fut[[86]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut)



  ### 1950-2100 Change Maps ###
  graphics.off()
  x11(width = 12, height = 8)
  wrap_plots(gg_map, ncol = 2, guides = "collect")
  ggsave(paste0("Figures/ZooMSS_MapDiff_",var[v],".pdf"))

  ### Time Series ###
  graphics.off()
  x11(width = 12, height = 6)
  wrap_plots(gg_ts, ncol = 2)
  ggsave(paste0("Figures/ZooMSS_TimeSeriesDiff_",var[v],".pdf"))


  ### 2100 Map ###
  graphics.off()
  x11(width = 12, height = 12)
  wrap_plots(gg_map2100, ncol = 2)
  ggsave(paste0("Figures/ZooMSS_Map2100_",var[v],".pdf"))
#}

  
  
  