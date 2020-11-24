
rm(list=ls())
source("/data/home/camillan/dbpm/plotMaps.R")
library('ncdf4')
clim <- c(-50, 50)
var <- c('tcb', 
         'tpb', 'bp30cm', 'bp30to90cm',"bp90cm",
         'tdb','bd30cm', 'bd30to90cm',"bd90cm") 
# NOTE 'bd30cm', 'bd30to90cm',"bd90cm" missing from new outputs and 'tdb' missing from ssp585 new outputs 
runs<-c("ISIMIP3b","ISIMIP3b_withTempOnSenescence")

# senescence: tcb (increases), 

map_ssp585 <- list()
var <- c('tcb', 
         'tpb')

map_ssp585[[2]]

# for(i in 1:length(runs)){
  i = 2
  
    for(j in 1:length(var)){
      # j = 1
      # locations for new files 
      new_h<-paste0("/../../rd/gem/private/fishmip_outputs/",runs[i],"/IPSL-CM6A-LR/netcdf/historical/")
      new_ssp126<-paste0("/../../rd/gem/private/fishmip_outputs/",runs[i],"/IPSL-CM6A-LR/netcdf/ssp126/")
      new_ssp585<-paste0("/../../rd/gem/private/fishmip_outputs/",runs[i],"/IPSL-CM6A-LR/netcdf/ssp585/")
    
      # new tcb
      hist_tcb<-stack(paste0(new_h,"/dbpm_ipsl_cm6a_lr_nobc_historical_nat_default_",var[j],"_global_montly_1850_2014.nc4"))
      # fut126_tcb <- stack(paste0(new_ssp126, "/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_",var[j],"_global_montly_2015_2100.nc4"))
      fut585_tcb <- stack(paste0(new_ssp585, "/dbpm_ipsl_cm6a_lr_nobc_ssp585_nat_default_",var[j],"_global_montly_2015_2100.nc4")) 
      # IF SENESCENCE!!!! you need to drop a level!
      # fut126_tcb<-dropLayer(fut126_tcb, dim(fut126_tcb)[3])
      if(runs[[i]] == "ISIMIP3b_withTempOnSenescence"){
        fut585_tcb<-dropLayer(fut585_tcb, dim(fut585_tcb)[3])  
      }
    
      # plot change ssp126 
      # tit<-"new tcb change ssp126"
      # all<-list(hist = hist_tcb, fut = fut126_tcb)
      refFirstYear<-(1990-1850)*12
      refLastYear<-(2090-2015)*12
      output<-"montly"
      # map_new_tcb_ssp126 <- plotGlobalChange(all, tit, world_sf, clim)
    
      # plot change ssp585 
      tit<-paste0(runs[i]," ",var[j], "change ssp585")
      all<-list(hist = hist_tcb, fut = fut585_tcb)
      map_ssp585[[j]] <- plotGlobalChange(all, tit, world_sf, clim)
                
      # plot trend
      # date = seq(as.Date("1850-1-1"), as.Date("2100-12-1"), by = "months")
      # tr_new_tcb_ssp585 <- plotTimeseries(all, tit)
                
      rm(hist_tcb, fut585_tcb, all, map_ssp585)
                
  }
  
# }

library(patchwork)

layout <- "
A###
BCDE
FGHI"

pt<-map_ssp585[[1]]+
  map_ssp585[[2]]+map_ssp585[[3]]+map_ssp585[[4]]+map_ssp585[[5]]+
  map_ssp585[[6]]+map_ssp585[[7]]+map_ssp585[[8]]+map_ssp585[[9]]
  plot_layout(design = layout)

pdf("DBPM_CMIP6_allVariables_senescence.pdf", height=15, width=20) 
pt 
dev.off()


