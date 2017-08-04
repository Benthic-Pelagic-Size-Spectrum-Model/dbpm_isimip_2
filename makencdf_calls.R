
source("./makencdf.R")

mknetcdf(varname="tcb"
         ,description="Total consumer biomass density"
         ,units="g C / m^2"
         ,gcm='ipsl-cm5a-lr'
         ,run="rcp45"
         ,gcmPath = '/rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/'
         ,savetopath="~/netcdfs/"
         ,grids=1:39567
         ,data_files_input_path="/rd/gem/private/fishmip_outputs/20170730_rcp85/")

# gcm ipsl, run rcp 85

# mknetcdf(varname="tcb",description="Total consumer biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# # mknetcdf(varname="b",description="Total pelagic predator biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# # mknetcdf(varname="b-bendet",description="Total benthic detritivore biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# mknetcdf(varname="tsb",description="Total system biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# mknetcdf(varname="b10cm",description="Biomass density of pelagic predators > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# mknetcdf(varname="b30cm",description="Biomass density of pelagic predators > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# # mknetcdf(varname="b10cm-bendet",description="Biomass density of benthic detritivores > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# # mknetcdf(varname="b30cm-bendet",description="Biomass density of benthic detritivores > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
# # mknetcdf(varname="bdet",description="Biomass density of detritus",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)

# gcm ipsl, run rcp 45

mknetcdf(varname="tcb",description="Total consumer biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp45",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="tsb",description="Total system biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp45",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b10cm",description="Biomass density of pelagic predators > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp45",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm",description="Biomass density of pelagic predators > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp45",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)

mknetcdf(varname="b10cm-bendet",description="Biomass density of benthic detritivores > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp45",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm-bendet",description="Biomass density of benthic detritivores > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp45",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)


# gcm ipsl, run rcp 60

mknetcdf(varname="tcb",description="Total consumer biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp60",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="tsb",description="Total system biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp60",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b10cm",description="Biomass density of pelagic predators > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp60",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm",description="Biomass density of pelagic predators > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp60",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)

mknetcdf(varname="b10cm-bendet",description="Biomass density of benthic detritivores > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp60",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm-bendet",description="Biomass density of benthic detritivores > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp60",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)

# gcm ipsl, run rcp 26

mknetcdf(varname="tcb",description="Total consumer biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp26",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="tsb",description="Total system biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp26",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b10cm",description="Biomass density of pelagic predators > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp26",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm",description="Biomass density of pelagic predators > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp26",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)

mknetcdf(varname="b10cm-bendet",description="Biomass density of benthic detritivores > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp26",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm-bendet",description="Biomass density of benthic detritivores > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp26",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)

# gcm ipsl, run rcp 85

mknetcdf(varname="tcb",description="Total consumer biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="tsb",description="Total system biomass density",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b10cm",description="Biomass density of pelagic predators > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm",description="Biomass density of pelagic predators > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)

mknetcdf(varname="b10cm-bendet",description="Biomass density of benthic detritivores > 10 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
mknetcdf(varname="b30cm-bendet",description="Biomass density of benthic detritivores > 30 cm",units="g C / m^2",gcm='ipsl-cm5a-lr',run="rcp85",gcmPath = '/../../rd/gem/private/GCM_INPUT/IPSL_CM5A_LR/',savetopath="~/netcdfs/",grids=1:39567)
