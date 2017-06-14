
library(RColorBrewer)
library(maps)
library(maptools)
library(ggplot2)
library(ggmap)
library(mapproj)
library(animation)
library(dplyr)
library(plyr)
library(rworldmap)
data(wrld_simpl)

#----------------get saved R file aggregated results and plot them

#------checking differences in hist period acorss RCPs (picked up by Matthias)

gcm="ipsl-cm5a-lr"
run="rcp60"
varname="tcb"
load(file=paste("~/netcdfs/","dbpm_",gcm,"_",run,"_","no-fishing","_","no-oa","_",varname,".RData",sep=""))
var60 <- var
image(var[,,2])

run="rcp85"
varname="tcb"
load(file=paste("~/netcdfs/","dbpm_",gcm,"_",run,"_","no-fishing","_","no-oa","_",varname,".RData",sep=""))
var85 <- var

run="rcp26"
varname="tcb"
load(file=paste("~/netcdfs/","dbpm_",gcm,"_",run,"_","no-fishing","_","no-oa","_",varname,".RData",sep=""))
var26<- var

run="rcp45"
varname="tcb"
load(file=paste("~/netcdfs/","dbpm_",gcm,"_",run,"_","no-fishing","_","no-oa","_",varname,".RData",sep=""))


gm26<-apply(var26,c(3), mean, na.rm=T)
gm45<-apply(var45,c(3), mean, na.rm=T)
gm60<-apply(var60,c(3), mean, na.rm=T)
gm85<-apply(var85,c(3), mean, na.rm=T)

mth<-seq(as.Date("1950/1/1"),as.Date("2100/12/31"), by = "month")
which(mth =="2005/12/1")
#[1] 672
which(mth =="1970/1/1")
which(mth =="1980/1/1")


plot(gm85[which(mth =="1990/1/1"):which(mth =="2006/1/1")],typ="l")
points(gm60[which(mth =="1990/1/1"):which(mth =="2006/1/1")],typ="l",col="red")
points(gm45[which(mth =="1990/1/1"):which(mth =="2006/1/1")],typ="l",col="blue")
points(gm26[which(mth =="1990/1/1"):which(mth =="2006/1/1")],typ="l",col="green")

# differences in the beginning - why is this?

#pick a grid cell

plot(var60["0.5","64.5",1:100],type="l",col="red")
points(var45["0.5","64.5",1:100],type="l",col="blue")
points(var26["0.5","64.5",1:100],type="l",col="green")
points(var85["0.5","64.5",1:100],type="l",col="black")

#issue still there


# check output for a grid cell with spin up period still in - model not reached equlilibrium

run="rcp26"
load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",1,"_",gcm,"_",run,".RData",sep=""))

agg26 <- agg

run="rcp85"
load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",1,"_",gcm,"_",run,".RData",sep=""))

agg85 <- agg

run="rcp45"
load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",1,"_",gcm,"_",run,".RData",sep=""))

agg45 <- agg

run="rcp60"
load(file=paste("/../../rd/gem/private/fishmip_outputs/res_mts_agg_igrid_",1,"_",gcm,"_",run,".RData",sep=""))

agg60 <- agg

# results are most different at the beginning of the time series...

# is this an aggregation effect?  are numerical chatter?

plot((((agg85$TotalUbiomass-agg26$TotalUbiomass)/agg26$TotalUbiomass)*100)[3600:4200],typ="l",ylab="% diff rcp85 to rcp26")

# which index corresponds to 1-12-2005 ( end of hist period)
# 1:3600 (300 yr*12)  spinup + 672 (56 year) historical run + 151 year (1812)
# 4273 (1/1/2006)

mth<-seq(as.Date("1650/1/1"),as.Date("2100/12/31"), by = "month")
which(mth =="2005/12/1")
#[1] 4272
which(mth =="1950/1/1")
which(mth =="1970/1/1")
which(mth =="1980/1/1")

plot((((agg85$TotalUbiomass-agg26$TotalUbiomass)/agg26$TotalUbiomass)*100)[which(mth =="1980/1/1"):which(mth =="2005/12/1")],typ="l",ylab="% diff fish biomass rcp85 to rcp26")
abline(h=0)

plot(agg85$TotalUbiomass[3600: 4272],typ="l",ylab="Ubiomass")
points(agg45$TotalUbiomass[3600: 4272],typ="l",ylab="Ubiomass",col="blue")
points(agg26$TotalUbiomass[3600: 4272],typ="l",ylab="Ubiomass",col="green")
points(agg60$TotalUbiomass[3600: 4272],typ="l",ylab="Ubiomass",col="red")

plot(agg85$TotalVbiomass[3600: 4272],typ="l",ylab="Vbiomass")
points(agg45$TotalVbiomass[3600: 4272],typ="l",ylab="Vbiomass",col="blue")
points(agg26$TotalVbiomass[3600: 4272],typ="l",ylab="Vbiomass",col="green")
points(agg60$TotalVbiomass[3600: 4272],typ="l",ylab="Vbiomass",col="red")

## are the inputs different too? note: these are weekly inputs not monthly outputs

run="rcp85"
igrid=39566
load(paste("/../../rd/gem/private/fishmip_inputs/grid_",igrid,"_inputs2_",gcm,"_",run,".RData",sep=""))
in85 <- inputs

run="rcp26"
igrid=39566
load(paste("/../../rd/gem/private/fishmip_inputs/grid_",igrid,"_inputs2_",gcm,"_",run,".RData",sep=""))
in26 <- inputs

run="rcp60"
igrid=39566
load(paste("/../../rd/gem/private/fishmip_inputs/grid_",igrid,"_inputs2_",gcm,"_",run,".RData",sep=""))
in60 <- inputs


in85$ts$slope[1]
in60$ts$slope[1]
#slightly different spin-up period values- why?

plot(in85$ts$intercep[1:(300*48+55*48)],in60$ts$intercep[1:(300*48+55*48)],typ="l")
abline(0,1,col="red")
#exact same values during rest of hist period, so it is a spin up period error
in85$ts$intercep[(300*48):(300*48+55*48)] %in% in26$ts$intercep[(300*48):(300*48+55*48)]


plot((((in85$ts$intercept-in26$ts$intercept)/in26$ts$intercept)*100)[(300*48 + 1):(300*48+55*48)],typ="l",ylab="% diff intercept")

plot((((in85$ts$intercept-in26$ts$intercept)/in26$ts$intercept)*100)[1:(300*48)],typ="l",ylab="% diff intercept")

# slight difference in the inputs exist - but only during the spin-up period
# looked like indexing in spinup input was wrong as it took mean across whole time series inlcuding the rcps not only the historical - now it's fixed.

run="rcp60"
load(paste("/../../rd/gem/private/fishmip_inputs/grid_",1,"_inputs2_",gcm,"_",run,".RData",sep=""))
head(inputs$ts)
run="rcp85"
load(paste("/../../rd/gem/private/fishmip_inputs/grid_",1,"_inputs2_",gcm,"_",run,".RData",sep=""))
head(inputs$ts)


#----------------get saved ncdf aggregated results relative change: average 2050-2059/ averages 1980-2009

inFile <- "~/netcdfs/dbpm_ipsl-cm5a-lr_rcp60_no-fishing_no-oa_tcb.nc"


nc = open.ncdf(inFile, write=FALSE)
print(nc)


ncArray = get.var.ncdf(nc,'tcb')

lat <- nc$dim$lat$vals
lon <- nc$dim$lon$vals 

lon <- ifelse(lon > 180,-(360-lon),lon)

ncbaseline <- ncArray[,,which(mth =="1980/1/1"):which(mth =="2009/12/1")]
ncbaseline <- apply(ncbaseline,c(1,2),mean)
image(ncbaseline)

ncproject <- ncArray[,,which(mth =="2050/1/1"):which(mth =="2059/12/1")]
ncproject <- apply(ncproject,c(1,2),mean)
image(ncproject)

ncchange <- ncproject/ncbaseline


row.names(ncchange) <- lon
colnames(ncchange) <- lat

image(ncchange)

head(ncchange)

lonchange<-melt(ncchange)
#lonbase<-melt(ncbaseline)
#lonproj<-melt(ncproject)


names(lonchange) <-c("lon","lat","dpbm_relchange")

write.table(lonchange,file="~/summaries/dpbm_rcp60.txt")

#read in Derek's results for BOATS

load(file="~/summaries/BOATS_rcp60")

rownames(boats_ipsl_tcb_nofish_80_09_mean)<-boats_ipsl_tcb_nofish_50_59$lon
colnames(boats_ipsl_tcb_nofish_80_09_mean)<-boats_ipsl_tcb_nofish_50_59$lat

rownames(boats_ipsl_tcb_nofish_50_59_mean)<-boats_ipsl_tcb_nofish_50_59$lon
colnames(boats_ipsl_tcb_nofish_50_59_mean)<-boats_ipsl_tcb_nofish_50_59$lat


boats_relchange<-boats_ipsl_tcb_nofish_50_59_mean/boats_ipsl_tcb_nofish_80_09_mean

boats_relchange<-melt(boats_relchange)

names(boats_relchange) <- c("lon","lat","boats_relchange")



#read in Derek's results for APECOSM

load(file="~/summaries/apecosm_rcp60")

rownames(apecosm_ipsl_tcb_nofish_80_09_mean)<-apecosm_ipsl_tcb_nofish_50_59$lon
colnames(apecosm_ipsl_tcb_nofish_80_09_mean)<-apecosm_ipsl_tcb_nofish_50_59$lat

rownames(apecosm_ipsl_tcb_nofish_50_59_mean)<-apecosm_ipsl_tcb_nofish_50_59$lon
colnames(apecosm_ipsl_tcb_nofish_50_59_mean)<-apecosm_ipsl_tcb_nofish_50_59$lat


apecosm_relchange<-apecosm_ipsl_tcb_nofish_50_59_mean/apecosm_ipsl_tcb_nofish_80_09_mean

apecosm_relchange<-melt(apecosm_relchange)

names(apecosm_relchange) <- c("lon","lat","apecosm_relchange")



##### read in Simon's


macro<-read.csv(file="~/summaries/JB_tcb_means_31102016.csv")

macro_relchange <- macro[,c("lon","lat")]

macro_relchange$lon <- ifelse(macro_relchange$lon > 180,-(360-macro_relchange$lon),macro_relchange$lon)

macro_relchange$macro_relchange <- macro$rcp60/macro$hist

#### merge

fishmip_rcp60<-merge(lonchange,boats_relchange,by=c("lon","lat"))

fishmip_rcp60<-merge(fishmip_rcp60,apecosm_relchange,by=c("lon","lat"))

fishmip_rcp60<-merge(fishmip_rcp60,macro_relchange,by=c("lon","lat"))

fishmip_rcp60$multi_relchange <- rowMeans(fishmip_rcp60[,3:6],na.rm=T)

fishmip_rcp60$multi_relchange_noapecosm <- rowMeans(fishmip_rcp60[,c(3,4,6)],na.rm=T)


fishmip_rcp60$negatives<- rowSums(ifelse(fishmip_rcp60[,3:6]<1,1,0),na.rm=T)

fishmip_rcp60$positives <- rowSums(ifelse(fishmip_rcp60[,3:6]>=1,1,0),na.rm=T)

fishmip_rcp60$agreement <- ifelse(fishmip_rcp60$multi_relchange < 1 & fishmip_rcp60$negatives > 2,1,0)

fishmip_rcp60$agreement <- ifelse(fishmip_rcp60$multi_relchange >= 1 & fishmip_rcp60$positives > 2,1,fishmip_rcp60$agreement)




write.table(fishmip_rcp60,file="~/summaries/fish-mip-rcp60.txt")





#### read in crop data

m <- read.csv("~/summaries/maize_2050_noco2.csv")

w <- read.csv("~/summaries/wheat_2050_noco2.csv")

s <- read.csv("~/summaries/soy_2050_noco2.csv")

r <- read.csv("~/summaries/rice_2050_noco2.csv")

mm <-rowMeans(m[,colnames(m)[grep("rcp6p0",colnames(m))]])
mw <-rowMeans(w[,colnames(w)[grep("rcp6p0",colnames(w))]])
ms <-rowMeans(s[,colnames(s)[grep("rcp6p0",colnames(s))]])
mr <-rowMeans(r[,colnames(r)[grep("rcp6p0",colnames(r))]])

crops_relchange <- rowMeans(cbind(mm,mw,ms,mr))

negatives_m <- rowSums(ifelse(m[,colnames(m)[grep("rcp6p0",colnames(m))]]<1,1,0),na.rm=T)

positives_m <- rowSums(ifelse(m[,colnames(m)[grep("rcp6p0",colnames(m))]]>=1,1,0),na.rm=T)

agree_m <- ifelse( < 1 & fishmip_rcp60$negatives > 2,1,0)


agreement <- rowMeans(cbind(mm,mw,ms,mr))

head(w[,colnames(w)[grep("rcp6p0",colnames(w))]])

dim(m[,colnames(m)[grep("rcp6p0",colnames(m))]])

dim(s[,colnames(s)[grep("rcp6p0",colnames(s))]])

dim(r[,colnames(r)[grep("rcp6p0",colnames(r))]])








#mean across whole time series, based on monthly values ( from most 1976-2005 )

meanNc<-apply(ncArray,3,mean)


ncAdjust <- ncArray[c(which(lon==180.5):which(lon==359.5),0.5:(which(lon==180.5)-1)),,]
lon2 <- lon - 180
mth<-seq(as.Date("1950/1/1"),as.Date("2100/12/31"), by = "month")


# breaks for the colour scale
brks<-quantile(ncAdjust,seq(0,1,0.1),na.rm=T)
#creating a colourPalette for all plots
# colourPalette <-brewer.pal(10,"PiYG")
colourPalette=colorRampPalette(c("beige", "wheat2", "lightskyblue1", "cornflowerblue", "darkblue"))
par(mfrow=c(1,1),mai=rep(1,4))

pdf("map-b-10cm-pelpred.pdf")

for ( i in 1:length(mth)) {
image(lon2,lat,ncAdjust[,,i],add=F,col=colourPalette(length(brks)),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
# plot(wrld_simpl,add=T,col="white",border="grey")
mtext(mth[i])
do.call(addMapLegend
        ,list(
          cutVector=brks  
          ,colourVector=colourPalette(length(brks))
          ,labelFontSize = 0.5 
          ,legendWidth = 1
          ,legendShrink = 0.8
          ,legendMar = 3 
          ,horizontal = T
          ,tcl = -0.2
          ,mgp=c(3,0.5,0)
          ,legendLabels="all" 
          ,legendArgs=list(text="",cex=0.8)))

}

dev.off()


var<-ifelse(var>=1e20,NA,var)

# constrain gradient to desnities that occur between min and max
density <- seq(min(var,na.rm=T), max(var,na.rm=T),length=11)
mth<-seq(as.Date("1950/1/1"),as.Date("2100/12/31"), by = "month")
x<-as.numeric(rownames(var))
y<-as.numeric(colnames(var))
z<-as.matrix(var)

pdf("b10cmpelpred.pdf")

image(x,y,var[,,1],col=terrain.colors(10),breaks=density,ylab="",xlab="")
mtext(mth[i])

for (i in 2:1812) {
image(x,y,var[,,i],col=terrain.colors(10),ylab="",xlab="")
mtext(mth[i])
}

dev.off()


levelplot(z,col.regions=terrain.colors(20))


# # -----------------examine some of the mored detailed saved outputs within a grid cell
# 
# # load detailed data for one grid 
# 
# 
# 
# load(file=paste("/../../rd/gem/private/fishmip_outputs/res_wts_igrid_",igrid,"_",gcm,"_",run,".RData",sep=""))
# 
# # check names of output to maniplate results, plot etc:
# names(res)   
# params<-res$params
# Neq<-params$Neq
# 
# par(mfrow=c(3,1),mai=c(0.5,1,0.5,1))
# # size spectra at end of simulation period
# 
# 
# plot(10^params$x[params$ref:params$Nx],res$U[params$ref:params$Nx,Neq], log="xy",typ="l",col="blue", ylab="Density",xlab="Weight") 
# points(10^params$x[params$ref:params$Nx],res$V[params$ref:params$Nx,Neq], log="xy",typ="l",col="red") 
# plotsizespectrum(modelout=res,params=params,itime=Neq)
mtext(paste("Location ", itime=Neq,sep=""),outer=F,side=3)            
# 
# #Total biomass
# 
# TotalUbiomass<-apply(res$U[params$ref:params$Nx,]*params$dx*10^params$x[params$ref:params$Nx],2,sum) 
# TotalVbiomass<-apply(res$V[params$ref.det:params$Nx,]*params$dx*10^params$x[params$ref.det:params$Nx],2,sum) 
# # par(mfrow=c(1,1))
# plot((1:(Neq+1)/48),TotalUbiomass,typ="l",col="blue",ylim=c(min(TotalUbiomass,TotalVbiomass,res$W), max(TotalUbiomass,TotalVbiomass,res$W)),xlab="Years",ylab="Total Biomass Density",log="y") 
# points((1:(Neq+1)/48),TotalVbiomass,typ="l",col="red",xlab="Years") 
# points((1:(Neq+1)/48),res$W,typ="l",col="brown") 
# 
# #Relative growth rates
# 
# plot(params$x[params$ref:params$Nx],res$GG.u[params$ref:params$Nx,params$Neq],log="y", type = "l", col = "blue", ylab= "Relative growth rate", xlim=c(params$x1.det,params$xmax),
#      xlab = "Size", ylim = c(0.001,1000))
# mean(res$GG.u[params$ref:params$Nx])
# 
# points(params$x[params$ref.det:params$Nx],res$GG.v[params$ref.det:params$Nx,params$Neq],log="y", type = "l", col = "red")
# mean(res$GG.v)
# 
# 
# #  get fish-mip outputs 
# 
# 
# #  extract these into monthly time series
# 
# isave<-seq(1201,dim(res$U)[2],4)
# 
# 
# #sum biomass (need to convert to g ww per m^3, across size classes) 
# # then need to convert to g C per m^2 
# 
# #total biomass in functional groups
# 
# TotalUbiomass<-apply(res$U[params$ref:params$Nx,isave]*params$dx*10^params$x[params$ref:params$Nx],2,sum) 
# TotalVbiomass<-apply(res$V[params$ref.det:params$Nx,isave]*params$dx*10^params$x[params$ref.det:params$Nx],2,sum) 
# 
# #biomass in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
# 
# wcut<-c(10,270,1000,10000)
# 
# xcutref<-wcut
# 
# for (i in 1:length(wcut)) xcutref[i] = (min(which(params$x >=log10(wcut[i]))))
# 
# Ubiomass10plus<-apply(res$U[xcutref[1]:params$Nx,isave]*params$dx*10^params$x[xcutref[1]:params$Nx],2,sum) 
# Ubiomass270plus<-apply(res$U[xcutref[2]:params$Nx,isave]*params$dx*10^params$x[xcutref[2]:params$Nx],2,sum) 
# # Ubiomass1000plus<-apply(res$U[xcutref[3]:params$Nx,isave]*params$dx*10^params$x[xcutref[3]:params$Nx],2,sum) 
# # Ubiomass10000plus<-apply(res$U[xcutref[4]:params$Nx,isave]*params$dx*10^params$x[xcutref[4]:params$Nx],2,sum) 
# Vbiomass10plus<-apply(res$V[xcutref[1]:params$Nx,isave]*params$dx*10^params$x[xcutref[1]:params$Nx],2,sum) 
# Vbiomass270plus<-apply(res$V[xcutref[2]:params$Nx,isave]*params$dx*10^params$x[xcutref[2]:params$Nx],2,sum) 
# 
# #total catches in functional groups
# 
# #sum catches ( already in grams per yr, across size classes) 
# #and then they need to be converted to g ww per m^2 
# 
# TotalUcatch<-apply(res$Y.u[params$ref:params$Nx,isave]*params$dx,2,sum) 
# TotalVcatch<-apply(res$Y.v[params$ref.det:params$Nx,isave]*params$dx,2,sum) 
# 
# #catches in functional groups and size classes - these weight classes correspond to 10, 30 , 46 and 100 cm thresholds (e.g. biomass in these sizes and up)
# 
# Ucatch10plus<-apply(res$Y.u[xcutref[1]:params$Nx,isave]*params$dx,2,sum) 
# Ucatch270plus<-apply(res$Y.u[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
# # Ucatch1000plus<-apply(res$Y.u[xcutref[3]:params$Nx,isave]*params$dx,2,sum) 
# # Ucatch10000plus<-apply(res$Y.u[xcutref[4]:params$Nx,isave]*params$dx,2,sum) 
# Vcatch10plus<-apply(res$Y.v[xcutref[1]:params$Nx,isave]*params$dx,2,sum) 
# Vcatch270plus<-apply(res$Y.v[xcutref[2]:params$Nx,isave]*params$dx,2,sum) 
# 
# TotalW<-res$W[isave]
# 
# # biomass size spectrum slope 
# 
# # normalised  - plot of log (biomass density/arithmetic width of size bin) vs log size class
# # pareto - use method as in Rogers et al 2014
# 
# # Uslope<-apply(res$U[params$ref:params$Nx,isave]*params$dx*10^params$x[params$ref:params$Nx],2,sum) 
# # Vslope<-apply(res$V[params$ref.det:params$Nx,isave]*params$dx*10^params$x[params$ref.det:params$Nx],2,sum) 
# 
# # agg<-data.frame(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, Uslope,Vslope, TotalW)
# agg<-data.frame(TotalUbiomass,Ubiomass10plus,Ubiomass270plus,TotalUcatch,Ucatch10plus,Ucatch270plus,TotalVbiomass,Vbiomass10plus,Vbiomass270plus,TotalVcatch,Vcatch10plus,Vcatch270plus, TotalW)
# 
# agg$lat<-rep(params$lat,each=length(agg[,1]))
# 
# agg$lon<-rep(params$lon,each=length(agg[,1]))
# 
# agg$depth<-rep(params$depth,each=length(agg[,1]))
# 
# save(agg,file=paste("outputs/res_mts_agg_igrid_",igrid,"_",gcm,"_",run,".RData",sep=""))
# 
# 
# load("outputs/res_mts_agg_igrid_1_ipsl-cm5a-lr_rcp26.RData")
# 
# plot(agg$TotalUbiomass)
# plot(agg$TotalVbiomass)
# plot(agg$TotalW,typ="l")
# 
# 
# 
# # (585*39567)/1000000
# 
# # --------------- NEED TO UPDATE THIS SECTION 
# # Getting sum of pelagic catch and demersal catch
# dealing<-function(params,res)  
# {
#   Yu<-(res$Y.u[,params$Neq]*10^-6) 
#   Yv<-(res$Y.v[,params$Neq]*10^-6)    
#   params<-res$params
#   Yuhat <- sum(Yu[which(params$x > params$min.fishing.size.u)]*params$dx)
#   Yvhat <- sum(Yv[which(params$x > params$min.fishing.size.v)]*params$dx)
#   error<- c((log(Yuhat)),(log(Yvhat)))
#   return(error)
# }
# 
# # Checking other outputs
# qualitative<-function(com)
# {
#   params<-com$param
#   # get modelled relative growth rates (yr^-1)
#   
#   t = seq(0,3,by=0.1)
#   ref.max<-length(params$x)
#   idx<-params$ref:ref.max
#   wt<-10^params$x[idx]
#   ts<-params$Neq
#   #browser()
#   gg = com$GG.u[idx,ts] ## Final growth rate
#   pel = com$U[idx,ts] ## final density 
#   
#   outts<-c(max(gg),gg[which(wt==10)] ,gg[which(wt==100)],pel[which(wt==0.01)],pel[which(wt==100)])
#   return(outts)	
# }
# 
# 
# 
# # Load catch data
# catches<-read.csv("~/Dropbox/Global F/Data/INPUT Feb 2014/catches_fao_lme_areas_JB20140902.csv")
# 
# 
# 
# # sum into functional groups
# # predators = lifetypes 1,2,4,5 
# # detritivore = lifetype 3
# 
# catch.pred<-catches[catches$lifetype!=3,]
# catch.pred<-aggregate(catch.pred$catch,list(catch.pred$loc1,catch.pred$loc2,catch.pred$logcat,catch.pred$area),sum)
# names(catch.pred)<-c("loc1","loc2","logcat","area","catch")
# 
# catch.pred <-cbind(catch.pred,0)
# colnames(catch.pred)[6]<-"location"
# 
# # match locations
# 
# for (i in 1:85)
# {
#   catch.pred[which(catch.pred$loc1==pp.sat[i,"loc1"] & catch.pred$loc2 ==pp.sat[i,"loc2"]),6]<-i
# }
# 
# catch.pred<-aggregate(catch.pred$catch,list(catch.pred$location,catch.pred$area),sum)
# 
# catch.pred<-catch.pred[order(catch.pred[,1]),-1]
# 
# obs<-matrix(0,nrow=2,ncol=85)
# 
# #sort out units
# 
# obs[1,]<-log(catch.pred[,2])-log(10e6)-log(catch.pred[,1])-log(depth)
# 
# 
# ######## Now for detritivors
# 
# catch.det<-catches[catches$lifetype==3,]
# catch.det<-aggregate(catch.det$catch,list(catch.det$loc1,catch.det$loc2,catch.det$logcat,catch.det$area),sum)
# names(catch.det)<-c("loc1","loc2","logcat","area","catch")
# 
# catch.det <-cbind(catch.det,0)
# colnames(catch.det)[6]<-"location"
# 
# for (i in 1:85)
# {
#   catch.det[which(catch.det$loc1==pp.sat[i,"loc1"] & catch.det$loc2 ==pp.sat[i,"loc2"]),6]<-i
# }
# 
# catch.det<-aggregate(catch.det$catch,list(catch.det$location,catch.det$area),sum)
# missing.det<-(1:85)[-(unique(catch.det[,1]))]
# add.det<-cbind(missing.det,1,99)
# colnames(add.det)<-colnames(catch.det)
# catch.det<-rbind(catch.det,add.det)
# catch.det<-catch.det[order(catch.det[,1]),-1]
# 
# obs[2,]<-log(catch.det[,2])-log(10e6)-log(catch.det[,1])-log(depth)
# obs[2,missing.det]<-99
