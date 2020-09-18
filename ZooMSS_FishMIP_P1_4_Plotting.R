
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
    out <- calc(x[[1:10*12]], mean) # Average first decade *12 if model resolution is months 
    out <- addLayer(out, calc(y[[(dim(y)[3]-9*12):dim(y)[3]]], mean)) #  Average last decade
    x_change <- ((out[[2]] - out[[1]])/out[[1]]) * 100
  }else{ # this should be right - CHECK!
    out <- calc(x[[1:10*12]], mean) # Average first decade *12 if model resolution is months 
    out <- addLayer(out, calc(x[[(dim(x)[3]-9*12):dim(x)[3]]], mean)) #  Average last decade
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
    st_transform(crs = st_crs(robCRS)) #%>% # Convert to Robinson Projection
    # mutate(layer = log10(layer/1e3)) # Convert to kg

  gg <- ggplot() +
    geom_sf(data = dat, aes(fill = layer), colour = NA) +
    geom_sf(data = w_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient(name = expression("Total Biomass (g m"^-2*")"), #(log"[10]*"(kg m"^-2*"))"),
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
    summarise(Biomass = median(Biomass, na.rm = TRUE),
              .groups = "keep") #  not sure what this is 
  
  # This doesn't seem to work in mutate. It just returns 0
  df2$BiomassChange = (df2$Biomass - mean(df2$Biomass[1:10], na.rm = TRUE))/mean(df2$Biomass[1:10], na.rm = TRUE) * 100
  
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
  
  ### CN trial 
  #  ssp126
  hist <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/dbpm_ipsl_cm6a_lr_nobc_historical_nat_default_tcb_global_montly_1850_2014.nc4"))
  fut <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4"))

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

  tit <- paste0("IPSL SSP126 ",var[v]," (1950-2100 Change)")
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
  tit <- paste0("IPSL SSP585 ",var[v]," (1950-2100 Change)")
  
  cut<-((2015-1850)*12) - ((2015-1950)*12) # from beginning of 1850 to end of 2014
  hist<-dropLayer(hist, seq(1:cut))
  fut<-dropLayer(fut, dim(fut)[3])
  all<-list(hist = hist, fut = fut)
  
  gg_map[[2]] <- plotGlobalChange(all, tit, world_sf, clim)
  gg_ts[[2]] <- plotTimeseries(all, tit)
  gg_map2100[[2]] <- plotGlobalYear(fut[[(86*12)-2]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist, fut, all)
  
  #  pi - error to figure out 
  hist <- stack(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/picontrol/dbpm_ipsl_cm6a_lr_nobc_picontrol_nat_default_tcb_global_montly_1850_2100.nc4"))
  tit <- paste0("IPSL PRE-INDUSTRIAL ",var[v]," (1950-2100 Change)")
  cut<-((2015-1850)*12) - ((2015-1950)*12) # from beginning of 1850 to end of 2014
  hist<-dropLayer(hist, seq(1:cut))
  all<-list(hist = hist)
  gg_map[[3]] <- plotGlobalChange(all, tit, world_sf, clim)
  gg_ts[[3]] <- plotTimeseries(all, tit) 
  gg_map2100[[3]] <- plotGlobalYear(hist[[(150*12)-2]], str_replace(tit,"1950-2100 Change", "2100"), world_sf)
  rm(hist,all)
  
  # print here in dbpm in gem48 and then get it through github? 
  # Jase method does not work here - use patchwork 
  library(patchwork)
  getwd()
  # setwd("/Users/nov017/Dropbox/Mizer-fleet_extension/plot/FD/Final")
  tiff("DBPM_IPSLmaps.tiff", height=12, width=18, units ='in', res=300)
  (gg_map2100[[3]]+gg_map[[3]]+gg_ts[[3]])/
  (gg_map2100[[1]]+gg_map[[1]]+gg_ts[[1]])/
  (gg_map2100[[2]]+gg_map[[2]]+gg_ts[[2]])
  dev.off()
  
  
  
  
  ## original code 
  
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
