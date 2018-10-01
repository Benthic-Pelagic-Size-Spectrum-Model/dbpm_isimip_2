library(ggplot2)
library(raster)
library(rgdal)
#install.packages("gridExtra")

library(gridExtra)

.area_means <- function(run, varname, nc_path, area="Global"){

  nc_filename <- sprintf("%s/dbpm_ipsl-cm5a-lr_%s_no-fishing_no-oa_%s.nc", nc_path, run, varname)
  
  the_brick <- brick(nc_filename)

  if (area=="AusEEZ") {
    
    eez_dsn <- "/rd/gem/private/fishmip_inputs/misc/Seas and Submerged Lands Act 1973/Seas and Submerged Lands Act 1973.gdb"
    eez <- readOGR(dsn=eez_dsn, layer="Exclusive_Economic_Zone_Amended_By_Perth_Treaty_1997_AMB2014a_Area")
    
    eez <- spTransform(eez, "+init=epsg:4326")
    
    #plot(eez, col="firebrick")
    #TODO decide how to  deal with Antarctic component of EEZ
    
    #optimisation:
    #crop away all of the brick outside of the MBR of the eez
    the_brick <- crop(the_brick, eez) 
    
    the_brick <- mask(the_brick, eez)
  }
  
  my_means <- data.frame(cellStats(the_brick, mean))
  names(my_means) <- run
  my_means$timestep <- the_brick@z[[1]]
  
  return (my_means)
}

.plot_area_means <- function(runs, varname, nc_path, area="Global", smooth=FALSE){

  source("helpers.R", local = TRUE)
  numcores <- length(runs)
  the_cluster <- parallel::makeForkCluster(getOption("cl.cores", numcores))
  
  output <- parallel::clusterApplyLB(the_cluster
                                     ,x=runs
                                     ,fun=.area_means
                                     ,varname=varname
                                     ,nc_path=nc_path
                                     ,area=area)
  
  parallel::stopCluster(the_cluster)
  
  df_wide <- data.frame(timestep=0:1811)
  for(i in 1:length (output)) {
    df_wide <- plyr::join(df_wide, output[[i]], by="timestep", type="left")
  }
  
  
  df <- reshape2::melt(df_wide, id="timestep", variable.name = "scenario", value.name = "value", na.rm=TRUE)
  
  months <- seq(as.Date("1950/1/1"),as.Date("2100/12/31"), by = "month")  
  #months[start_of_projections - end_of_spinup]
  
  the_title <- area
  if (smooth) the_title <- paste(the_title, "with smoothing")

  the_plot <- ggplot(df) 
  the_plot <-  the_plot + ggtitle(the_title)
  if(smooth==TRUE)
    the_plot <- the_plot + geom_smooth(aes(x=months[timestep+1], y=value, colour=scenario),  method = 'loess')
  else
    the_plot <- the_plot + geom_line(aes(x=months[timestep+1], y=value, colour=scenario))
  
  
  the_plot <- the_plot + scale_colour_manual(values=c("grey", "red","green","black", "blue")) + 
    geom_vline(xintercept = as.numeric(months[start_of_projections - end_of_spinup]), col = "red", linetype="dotted") + 
    xlab('time') +
    ylab(varname)

  return(the_plot)
}


.four_area_means_plots <- function(runs, varname, nc_path){
  
  p1 <- .plot_area_means(runs, varname, smooth = TRUE, nc_path = nc_path)
  p2 <- .plot_area_means(runs, varname, nc_path = nc_path)
  p3 <- .plot_area_means(runs, varname, area="AusEEZ", smooth = TRUE, nc_path = nc_path)
  p4 <- .plot_area_means(runs, varname, area="AusEEZ", nc_path = nc_path)
  
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)
  
  
}

myvar <- "tsb"
myruns <- c("history", "rcp26", "rcp45", "rcp60", "rcp85")
mync_path <- "/rd/gem/public/fishmip/netcdf"

.four_area_means_plots(myruns, myvar, mync_path)
