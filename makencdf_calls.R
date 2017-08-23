
source("./makencdf.R")

my_netcdf_path <- "/rd/gem/private/fishmip_outputs/aug_2017_2/netcdf/"
my_data_path <- "/rd/gem/private/fishmip_outputs/aug_2017_2/"
runs_to_do <- c("rcp26","rcp45","rcp60","rcp80")

for (run in runs_to_do) {
  
  mknetcdf(varname="tcb"
           ,description="Total consumer biomass density"
           ,run=run
           ,savetopath = my_netcdf_path
           ,data_path = my_data_path)
  
  mknetcdf(varname="tsb"
           ,description="Total system biomass density"
           ,run=run
           ,savetopath = my_netcdf_path
           ,data_path = my_data_path)
  
  mknetcdf(varname="b10cm"
           ,description="Biomass density of pelagic predators > 10 cm"
           ,run=run
           ,savetopath = my_netcdf_path
           ,data_path = my_data_path)
  
  mknetcdf(varname="b30cm"
           ,description="Biomass density of pelagic predators > 30 cm"
           ,run=run
           ,savetopath = my_netcdf_path
           ,data_path = my_data_path)
  
  mknetcdf(varname="b10cm-bendet"
           ,description="Biomass density of benthic detritivores > 10 cm"
           ,run=run
           ,savetopath = my_netcdf_path
           ,data_path = my_data_path)
  
  mknetcdf(varname="b30cm-bendet"
           ,description="Biomass density of benthic detritivores > 30 cm"
           ,run=run
           ,savetopath = my_netcdf_path
           ,data_path = my_data_path)
}
