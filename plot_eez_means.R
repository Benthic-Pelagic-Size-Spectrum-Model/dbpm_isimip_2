library(ggplot2)
library(raster)

eez_mean <- function(run, varname, nc_path="/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf"){

#  nc_path="/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf"
#  run <- "rcp26"
#  varname <- "tcb"
  
  nc_filename <- sprintf("%s/dbpm_ipsl-cm5a-lr_%s_no-fishing_no-oa_%s.nc", nc_path, run, varname)
  
  the_brick <- brick(nc_filename)
  
  eez_dsn <- "/data/home/justb/Seas and Submerged Lands Act 1973/Seas and Submerged Lands Act 1973.gdb"
  eez <- readOGR(dsn=eez_dsn, layer="Exclusive_Economic_Zone_Amended_By_Perth_Treaty_1997_AMB2014a_Area")
  
  eez <- spTransform(eez, "+init=epsg:4326")
  
  #TODO decide how to  deal with Antarctic component of EEZ
  
  #optimisation:
  #crop away all of the brick outside of the MBR of the eez
  the_brick <- crop(the_brick, eez) 
  
  masked_brick <- mask(the_brick, eez)
  
  plot(the_brick[[1]])
  plot(masked_brick[[1]])  # gives viz of extent
  plot(masked_brick[[6]])  # gives viz of extent
  
  my_means <- data.frame(cellStats(masked_brick, mean))
  names(my_means) <- run

  return (my_means)
}

plot_eez_means <- function(runs, varname, nc_path="/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf"){
  
#  varname <- "tcb"
#  runs <- c("rcp26","rcp45","rcp60","rcp85")
#  nc_path="/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf"
  
  source("helpers.R", local = TRUE)
  numcores <- length(runs)
  the_cluster <- parallel::makeForkCluster(getOption("cl.cores", numcores))
  
  output <- parallel::clusterApplyLB(the_cluster
                                     ,x=runs
                                     ,fun=eez_mean
                                     ,varname=varname
                                     ,nc_path=nc_path)
  
  parallel::stopCluster(the_cluster)
  
  
  df <- do.call("cbind", output)
  df$timestep <- seq.int(nrow(df))
  
  df <- reshape2::melt(df, id="timestep", variable.name = "scenario", value.name = "value")

  months <- seq(as.Date("1950/1/1"),as.Date("2100/12/31"), by = "month")  
  #months[start_of_projections - end_of_spinup]
  the_plot <- ggplot(df) + 
    geom_line(aes(x=months[timestep], y=value, colour=scenario)) +
    scale_colour_manual(values=c("red","green","black", "blue")) + 
    geom_vline(xintercept = as.numeric(months[start_of_projections - end_of_spinup]), col = "red", linetype="dotted") + 
    xlab('time') +
    ylab(varname)

  return(the_plot)
}

myvar <- "tcb"
myruns <- c("rcp26","rcp45","rcp60","rcp85")

plot_eez_means(myruns, myvar)
plot_eez_means(myruns, "tsb")
