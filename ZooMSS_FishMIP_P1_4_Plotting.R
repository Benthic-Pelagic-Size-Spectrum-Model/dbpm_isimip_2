library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(patchwork)
library(lubridate)


var <- "tcb"
var <- "bp90cm"
var <- "bp30cm"

var <- c("tpb", "tcb", "bp30cm", "bp30to90cm", "bp90cm")

mollCRS <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
mollCRS_no <- 54009

robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
robCRS_no <- 54030

lonlatCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
lonlatCRS_no <- 4326


plotGlobalChange <- function(x, y, tit, w_sf, clim){

  out <- calc(x[[1:10]], mean) # Average first decade
  out <- addLayer(out, calc(y[[(dim(y)[3]-9):dim(y)[3]]], mean)) #  Average last decade
  x_change <- ((out[[2]] - out[[1]])/out[[1]]) * 100

  dat <- st_as_sf(rasterToPolygons(x_change))
  dat <- st_transform(dat, crs = st_crs(robCRS)) # Convert to Robinson Projection

  gg <- ggplot() +
    geom_sf(data = dat, aes(fill = layer), colour = NA) +
    geom_sf(data = w_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient2(name = "Total Biomass Change (%)",
                         limits = clim,
                         midpoint = 0,
                         low = "blue",
                         mid = "white",
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
          legend.key.height = unit(2, "cm"),
          legend.title.align = 0.5) +
    guides(fill = guide_colourbar(title.position = "right"))

  return(gg)
}


plotGlobalYear <- function(dat, tit, w_sf){

  names(dat) <- "layer"
  dat <- st_as_sf(rasterToPolygons(dat)) %>%
    st_transform(crs = st_crs(robCRS)) %>% # Convert to Robinson Projection
    mutate(layer = log10(layer/1e3)) # Convert to kg

  gg <- ggplot() +
    geom_sf(data = dat, aes(fill = layer), colour = NA) +
    geom_sf(data = w_sf, size = 0.05, fill = "grey20") +
    scale_fill_gradient(name = expression("Total Biomass (log"[10]*"(kg m"^-2*"))"),
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

plotTimeseries <- function(x, y, tit){
  out <- stack(hist, fut)

  df <- as.data.frame(out, xy = TRUE) %>%
    pivot_longer(cols = X1950.01.01:X2100.01.01, names_to = "Date", values_to = "Biomass") %>%
    mutate(Date = str_remove(Date, "X"),
           Date = ymd(Date),
           Year = year(Date))

  df2 <- df %>%
    group_by(Year) %>%
    summarise(Biomass = median(Biomass, na.rm = TRUE),
              .groups = "keep")

  # This doesn't seem to work in mutate. It just returns 0
  df2$BiomassChange = (df2$Biomass - mean(df2$Biomass[1:10], na.rm = TRUE))/mean(df2$Biomass[1:10], na.rm = TRUE) * 100

  gg <- ggplot(data = df2, aes(x = Year, y = BiomassChange)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggtitle(tit) +
    theme_bw() +
    theme(title = element_text(size = 8)) +
    ylab("Total Biomass Change (%)")
  rm(out)
  return(gg)
}

# Download and process world outline
world <- ne_countries(scale = "medium", returnclass = "sf")
world_sf <- st_transform(world, crs = st_crs(robCRS)) # Convert to different CRS


for(v in 1:length(var)){
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
}
