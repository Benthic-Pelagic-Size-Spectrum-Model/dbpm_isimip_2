
#----------------PUT FISH_MIP MODEL OUTPUTS INTO netcdf 4

rm(list=ls())

library('RNetCDF')
setwd("/data/home/camillan/dbpm")

# CN work with one earth model at the time: 
# load(file ="/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/IPSL-CM6A-LR_depth.RData") # load depth data
# load(file ="/../../rd/gem/private/fishmip_inputs/ISIMIP3b/GFDL-ESM4/GFDL-ESM4_depth.RData") 

# CN or chose your model here # NOTE HERE YOU DECIDE THE ESM 
esms <- c("GFDL-ESM4", "IPSL-CM6A-LR")
curr_esm <- esms[1]
file = paste0("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm, "/" , curr_esm, "_depth.RData")
load(file)

# # locate Noth Sea and dowload for 1 grid sise-spectrum analysis
# trial<-depth
# trial$trial<-seq(1:nrow(trial))
# trial[which(trial$lat == 60.5 & trial$lon == -4.5),] # GFDL 54176 is the gridnum which correspond to the 37014 igrid (following row numbers)
# result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/GFDL-ESM4/ssp585/dbpm_output_all_37014_ssp585.rds")
# input<-readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/GFDL-ESM4/ssp585/grid_37014_GFDL-ESM4_ssp585.rds")
# # trial[which(trial$lat == 60.5 & trial$lon == -4),] # IPSL 54177 is the gridnum which correspond to the 34856 igrid (following row numbers)
# result_set<-readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_34856_ssp585.rds")
# input<-readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/grid_34856_IPSL-CM6A-LR_ssp585.rds")

# grids to read in are sequential for the depth file
grids<-1:dim(depth)[1] 

# locations of inputs, outputs from models and where to save netcdf 
input_loc <- paste0("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/", curr_esm,"/")
output_loc <- paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/",curr_esm,"/")
save_loc <- paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/",curr_esm,"/netcdf/")

# CN variables to make 
# vars2make <- c('U', 'V', 'GGU','GGV') # 'W','dx','x') # if not aggregated   
vars2make <- c('tcb', 'tpb', 'bp30cm', 'bp30to90cm',"bp90cm",'tdb','bd30cm', 'bd30to90cm',"bd90cm") # if aggregated # NOTE this order is very important!! 
                                                                                                    # if you change it, you need to change dbpm.variables at the beginning of the
                                                                                                    # makenetcdfs_func.R
# added tcblog10 below, as it needs a size dimention
prots <- c('picontrol','historical', 'ssp126', 'ssp585')
yearRange<- c('1850_2100', '1850_2014', '2015_2100', '2015_2100')

# ### option 1  OBSOLETE to DELETE ----
# # CN: define additional parameters outsirde the function (isave if considering historical protocol where weekly outputs have been saved)
# # use a random grid input/output
# # historical 
# result_set <- readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/historical/dbpm_output_all_1_historical.rds")
# inputs <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/historical/grid_1_IPSL-CM6A-LR_historical.rds")
# # ssp126 
# # result_set <- readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp126/dbpm_output_all_1_ssp126.rds")
# # inputs <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp126/grid_1_IPSL-CM6A-LR_ssp126.rds")
# # ssp585 
# # result_set <- readRDS("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/dbpm_output_all_1_ssp585.rds")
# # inputs <- readRDS("/../../rd/gem/private/fishmip_inputs/ISIMIP3b/IPSL-CM6A-LR/ssp585/grid_1_IPSL-CM6A-LR_ssp585.rds")
# 
# library("zoo")
# num_years <- ceiling(dim(inputs$ts)[1]/48)
# tss <- as.matrix(inputs$ts)
# for(i in 1:(num_years-1)){ # First week of year is the yearly average, all other weeks are NA
#   tss[((i-1)*48+1),-1]  <- colMeans(tss[c(((i-1)*48+1):(i*48)),-1])
#   tss[c(((i-1)*48+2):(i*48)),-1] <- NA
# }
# tss[dim(tss)[1],-1] <- colMeans(tss[((num_years-1)*48+1):dim(tss)[1],-1])
# tss[c(((num_years-1)*48+1):((dim(tss)[1]-1))),-1] <- NA
# fwts<-data.frame(na.approx(tss))
# spinup<-data.frame(sst=rep(mean(inputs$ts$sst[1:480]),each=300*48),sbt=rep(mean(inputs$ts$sbt[1:480]),each=300*48),er=rep(mean(inputs$ts$er[1:480]),each=300*48),
#                    intercept=rep(mean(inputs$ts$intercept[1:480]),each=300*48),slope=rep(mean(inputs$ts$slope[1:480]),each=300*48),
#                    sphy=rep(mean(inputs$ts$sphy[1:480]),each=300*48), lphy=rep(mean(inputs$ts$lphy[1:480]),each=300*48))
# nrow(spinup)
# fwts <- rbind(spinup, fwts[,-1])
# inputs=list(depth=inputs$depth,ts=fwts)
# rm(spinup)
# isave <- seq(from=300*48, to=((dim(inputs$ts)[1])+1), by = 4) # 48 is weeks in a year; 4 is months in a year 
# # isave <- seq(from=(300*48)+48, to=((dim(inputs$ts)[1])), by = 4)
# length(isave) # 165 years for historical (or 1980 months); 85 for others (NOTE: the first year (or 12 month) are the last step of spinup)
# grids<-grids[grids!=21747] # only when running historical - do not consider this grid becasue it's problematic and run on a more detailed time scale (X10 time steps)

#### option 3 ----

result_set <- readRDS(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/",curr_esm,"/historical/dbpm_output_all_1_historical.rds"))
isave_h<-1:dim(result_set$U)[2] 
result_set <- readRDS(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/",curr_esm,"/ssp126/dbpm_output_all_1_ssp126.rds"))
isave_126<-1:dim(result_set$U)[2] 
result_set <- readRDS(paste0("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/",curr_esm,"/ssp585/dbpm_output_all_1_ssp585.rds"))
isave_585<-1:dim(result_set$U)[2] 

isave<- list(isave_h, isave_h, isave_126, isave_585) # picontrol needs to be added instead of historical if picontrol is run 

# other parameters to cut and aggregate outputs 
# these were originally used (and can still be used) inside the model to aggregate outputs (runmodel_yearly ~ line 316)
# these can be change according to fishmip requirements
# ask Julia or investigate the code on the  meaning of these parameters
x = result_set$x
dx = result_set$dx
Nx = length(x)
x1 = -7
x1.det = -7
xmin = -12
ref = ((x1-xmin)/dx)+1
ref.det = ((x1.det-xmin)/dx)+1 

other_param = list (
  # isave = isave, 
              x = x, 
              dx = dx, 
              Nx = Nx, 
              x1 = x1, 
              x1.det = x1.det, 
              xmin = xmin, 
              ref = ref, 
              ref.det = ref.det)

# CN: dimnames for size (in not aggregated outputs) should be also defined as 'x'

# CN call the function
source("makenetcdfs_func.R")

# CN: loop through variables and scenarios and create a netcdf file for each combination. 

#### for not aggregated ("partial") outputs ----

# USE TO SAVE THE RAW SIZE_SPECTRUM!? need to adjust according to functions below as you did not update this one I think

# for(i in 1:length(vars2make)){
  # i = 1
  
  # for(j in 1:length(prots)){

    # j = 1 # for not aggregated outputs - one variable for one protocol (all grids, yearly outputs) takes 18h to run    
    
    # ptm=proc.time()
    # options(warn=-1)
    # print(paste('Now working on ', vars2make[i], ' for protocol ', prots[j], sep = ''))
    # mknetcdf(vars2make[i], prots[j], input_loc, output_loc, save_loc, grids, isave, yearRange[j])
    # print((proc.time()-ptm)/60.0)
    
  # }
# }

#### for aggregated outputs ---- 

dbpm.variables<-data.frame(name = vars2make, 
                           description = c("Total Consumer Biomass Density",
                                           "Total Pelagic Biomass Density", 
                                           "Biomass Density of Small Pelagics <30cm",
                                           "Biomass Density of Medium Pelagics >=30cm and <90cm",
                                           "Biomass Density of Large Pelagics >=90cm",
                                           "Total Demersal Biomass Density",
                                           "Biomass Density of Small Demersals <30cm",
                                           "Biomass Density of Medium Demersals >=30cm and <90cm",
                                           "Biomass Density of Large Demersals >=90cm"))


vars2make <- c('tcb', 'tpb', 'bp30cm', 'bp30to90cm',"bp90cm",'tdb','bd30cm', 'bd30to90cm',"bd90cm")

for(i in 2:length(vars2make)){ 

  for(j in 1:length(prots)){
    
    # j = 3 # ssp126: for aggregated outputs (all grids, monthly outputs) all variables takes 10 h to run  
    # j = 4 # ssp585: for aggregated outputs (all grids, monthly outputs) all variables takes 10 h  
    # j = 2 # historical: for aggregated outputs (all grids, monthly outputs) all variables takes ~2.2 days 
    # j = 1 # picontrol for aggregated outputs (all grids, monthly outputs) all variables takes 17 h 
    
    # new round as above but only for histo and spps and given model outputs with no temp effect on senescence 
    # run all together 
     
     # fix this for ssp585 due to 22430 cell where U is saved as character - fixed in runmodel_yearly line ~570 but temp solution (now deleted and moved to line ~330 makenetcdf_fumc.R; see below)
     # Error in result_set$U[other_param$ref:other_param$Nx, isave] * other_param$dx : 
       # non-numeric argument to binary operator
    # tcb OK for the 3 protocols  
    
    # Fix errors as per Matthias 
    # j = 3
    
    # new runs given no temp effect on senescence and detritus otehr mortality 
    # run both IPSL and GFDL
    # the erron on cell 22430 for IPSl did not come up when creating outputs - but it did for another cell for GFDL... how to identify the grid cell? explore which file is bigger than a regular one: cell 38658
    # to fix this error I transform outputs to numeric (if they have been saved as characters) in line ~ 330 makenetcdf_fumc.R
    
    # grids<-36 # for GFDL this grid cell gives an error, it did not like paste(input$depth$lon) so now replaced with input$depth$lon 
    
    # i = 1
    # j = 4
    
    ptm=proc.time()
    options(warn=-1)
    print(paste('Now working on ', vars2make[i], ' for protocol ', prots[j], sep = ''))
    mknetcdf_agg(vars2make[i], prots[j], input_loc, output_loc, save_loc, grids, other_param, isave[[j]] ,yearRange[j], curr_esm)
    print((proc.time()-ptm)/60.0)
    
 }
    
}

#### for aggregated size-spectrum outputs ----

for(j in 1:length(prots)){
  
  # picontrol error then function stopped  
  # Error in `[<-`(`*tmp*`, inputs$depth$lon, inputs$depth$lat, size[2], ,  : 
  #                  subscript out of bounds
  
  ptm=proc.time()
  options(warn=-1)
  print(paste('Now working on ', "tcblog10", ' for protocol ', prots[j], sep = ''))
  mknetcdf_agg_sp("tcblog10", prots[j], input_loc, output_loc, save_loc, grids, other_param, isave[[j]] ,yearRange[j], curr_esm)
  print((proc.time()-ptm)/60.0)
  
}

#### check ----

library(ncdf4)
nc <- nc_open("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/historical/dbpm_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_monthly_1850_2014.nc")
print(nc)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time_months <- ncvar_get(nc, "time")
size <- ncvar_get(nc, "size")
yearmonth = as.yearmon("1601-01") + time_months[1] / 12

tcbh <- ncvar_get(nc, "tcblog10")
dim(tcbh)
tcbh[1:2,1:2,1:2,1:2]

nc <- open.nc("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp126/dbpm_ipsl_cm6a_lr_nobc_ssp126_nat_default_tcb_global_montly_2015_2100.nc4")
tcb126 <- var.get.nc(nc, "tcb")
max(tcb126, na.rm =TRUE)
min(tcb126, na.rm =TRUE)
tcb126[which(tcb126 == max(tcb126, na.rm =TRUE))]<-NA # not sure what the cut off should be here for the different variables 
image(tcb126[,,1031])
dim(tcb126)
# problem with last time step.... need to CHECK why...
tcb126<-tcb126[,,1:dim(tcb126)[3]-1]

nc <- open.nc("/../../rd/gem/private/fishmip_outputs/ISIMIP3b/IPSL-CM6A-LR/netcdf/ssp585/dbpm_ipsl_cm6a_lr_nobc_ssp585_nat_default_tcb_global_montly_2015_2100.nc4")
tcb585 <- var.get.nc(nc, "tcb")
tcb585[which(tcb585 == max(tcb585, na.rm =TRUE))]<-NA # this is 1e20
image(tcb585[,,1031])
# problem with last time step.... need to CHECK why... could be similar to historical runs? 
tcb585<-tcb585[,,1:dim(tcb585)[3]-1]

#### biomass thorugh time ----
bio_h<-rowSums(aperm(tcbh, c(3,1,2)), dims = 1, na.rm = TRUE)
plot(bio_h) # yearly values for now  
bio_126<-rowSums(aperm(tcb126, c(3,1,2)), dims = 1, na.rm = TRUE)
plot(bio_126) # monty - decreases 
bio_585<-rowSums(aperm(tcb585, c(3,1,2)), dims = 1, na.rm = TRUE)
plot(bio_585)

# install.packages("vctrs")
library("tidyr")
library("dplyr")
library("ggplot2")
df<-data.frame(bio_126 = bio_126, bio_585 = bio_585) %>% 
  mutate(time= seq(1:length(bio_585))) %>% # 2015-20100 in months and including last year (12 months) of spinup
  gather(key, value, -time) 

ggplot(df, aes(x=time, y=value, group = key, color = key))+
  geom_point()

