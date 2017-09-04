source("./makencdf.R")

my_netcdf_path <- "/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf2/"
my_data_path <- "/rd/gem/private/fishmip_outputs/aug_2017_2/"
runs <- c("rcp26", "rcp45", "rcp60", "rcp85")
vars <- dbpm.variables$name

do_run <- function(run, vars, savetopath, data_path){
 
  discard_output <- sapply(vars
                           ,FUN = mknetcdf
                           ,run = run
                           ,savetopath = savetopath
                           ,data_path = data_path)
  
}

do_runs <- function(runs, vars, savetopath, data_path){

  discard_output <- sapply(runs
                           ,FUN = do_run
                           ,vars = vars
                           ,savetopath = savetopath
                           ,data_path = data_path)
}

do_runs(runs, vars, my_netcdf_path, my_data_path)


