source("helpers.R")
#decadal averages
#using nco
input_path <- "/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf2"
output_path <- "/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf2/decadal"

make_decadal_averages <- function(run, varname, input_path, output_path){

  filename <- sprintf("dbpm_ipsl-cm5a-lr_%s_no-fishing_no-oa_%s.nc", run, varname)
  
  input_filename <- sprintf("%s/%s", input_path, filename)
  output_filename <- sprintf("%s/%s", output_path, filename)
  
  steps <- seq(0,1811)
  steps_per_period <- 120
  periods <- 1:(length(steps)%/%steps_per_period)
  
  for (period in periods){
    # Start = $((120*($k-1)))
    start <- (period - 1) * steps_per_period
    # End = $((120$k-1))  
    end <- (period * steps_per_period) - 1
    
    
    
    #print(paste(start, end))
    # ncrcat -d time,$start,$end big_file.nc time_slice_$k.nc
    slice_filename <- sprintf("%s/time_slice_%03d_%s_%s_%i_%i.nc", output_path, period, run, varname, start, end)
    cmd <- sprintf("ncrcat -O -d time,%i,%i %s %s", start, end, input_filename, slice_filename)
    system(cmd)
    #print(cmd)
    
    # ncra time_slice_$k.nc slive_av_$k.nc
    slice_ave_filename <- sprintf("%s/time_slice_ave_%03d_%s_%s_%i_%i.nc", output_path, period, run, varname, start, end)
    cmd <- sprintf("ncra -O %s %s", slice_filename, slice_ave_filename)
    system(cmd)
    #print(cmd)
  }
  
  #ncrcat time_slice_ave*.nc joined_averages.nc
  cmd <- sprintf("ncrcat -O %s/time_slice_ave*.nc %s", output_path, output_filename)
  system(cmd)
  
  cmd <-sprintf("rm %s/time_slice*.nc", output_path)
  system(cmd)

}

g <- expand.grid(runs=dbpm.runs, vars=dbpm.variables$name, stringsAsFactors = FALSE)
mapply(make_decadal_averages, g$runs, g$vars, input_path, output_path)
