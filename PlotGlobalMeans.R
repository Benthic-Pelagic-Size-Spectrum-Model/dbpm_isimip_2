library(ggplot2)

global_mean <- function(run, varname){

  nc_filename <- sprintf("/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf/dbpm_ipsl-cm5a-lr_%s_no-fishing_no-oa_%s.nc", run, varname)
  
  nc <- ncdf4::nc_open(nc_filename, write = FALSE)
  
  # get longitude and latitude
  lon <- ncdf4::ncvar_get(nc,"lon")
  lat <- ncdf4::ncvar_get(nc,"lat")

  var_array <- ncdf4::ncvar_get(nc,varname)
  fillvalue <- ncdf4::ncatt_get(nc,varname,"_FillValue")
  
  var_array[var_array==fillvalue$value] <- NA
  
  my_mean <- data.frame(apply(var_array, 3, mean, na.rm = TRUE))
  names(my_mean) <- run
  
  ncdf4::nc_close(nc)
  
  return (my_mean)
}

plot_global_means <- function(runs, varname){
  source("helpers.R", local = TRUE)
  numcores <- length(runs)
  the_cluster <- parallel::makeForkCluster(getOption("cl.cores", numcores))
  
  output <- parallel::clusterApplyLB(the_cluster
                                     ,x=runs
                                     ,fun=global_mean
                                     ,varname=varname)
  
  parallel::stopCluster(the_cluster)
  
  
  df <- do.call("cbind", output)
  df$timestep <- as.numeric(rownames(df))
  
  df <- reshape2::melt(df, id="timestep", variable.name = "scenario", value.name = "value")
  
  
  the_plot <- ggplot(df) + 
    geom_line(aes(x=timestep, y=value, colour=scenario)) +
    scale_colour_manual(values=c("red","green","black", "blue")) + 
    geom_vline(xintercept = start_of_projections - end_of_spinup, col = "red", linetype="dotted") + 
    xlab('time') +
    ylab(varname)

  return(the_plot)
}

myvar <- "tcb"
myruns <- c("rcp26","rcp45","rcp60","rcp85")

plot_global_means(myruns, myvar)
plot_global_means(myruns, "tsb")

