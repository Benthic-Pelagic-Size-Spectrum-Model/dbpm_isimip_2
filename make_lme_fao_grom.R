


get_lme_fao_grid <- function(){
  #read LME _FAO Areas
  lme_fao <- read.csv("/rd/gem/private/fishmip_inputs/misc/LMEFAO_Areas/Areas.csv")
  
  #rescale to 1°
  
  lme_fao_grid <- rasterize(
    lme_fao[, c('LonCentre', 'LatCentre')], 
    raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=1, crs="+proj=longlat +datum=WGS84"), 
    lme_fao[, 'AreaCode'])
  
}


get_grom <- function(){
  #read GROM +spdf?
  grom <- readRDS("grom.rds")
  
  #Grom uses a positive 0-360° extent for longitudes. 
  #to align with the -180°-180° extent of the lme_fao data I math transform lon
  # should this be done with extent()?
  grom$lon <- ifelse(grom$lon > 180, grom$lon -360, grom$lon)  
  
  coordinates(grom) <- ~lon+lat
  
  return(grom)
  
}


lme_fao_grid <- get_lme_fao_grid()
grom <- get_grom()

#extract 
#use raster extract to get a vector of raster values for each point in grom
lme_fao_code <- extract(lme_fao_grid, grom)

#bind lme_fao_code vector to grom
grom@data <- cbind(grom@data, lme_fao_code)
grom <-as.data.frame(grom)

#write grom_lme_fao
#move extent back from -180° - 180° to 0° - 360°, to align with original inputs (grom)
grom$lon <- ifelse(grom$lon<0, grom$lon + 360, grom$lon)

#saveRDS(grom, file="grom_lme_fao.rds")



#(read Grom_lme_fao)
grom <- readRDS("grom_lme_fao.rds")



rm(lme_fao_code, lme_fao_grid)


# loop through grom lme_fao

#read weekly gridcell inputs 2

#write weekly lme inputs 2



