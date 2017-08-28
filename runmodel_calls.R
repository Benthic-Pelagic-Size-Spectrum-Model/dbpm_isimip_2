options(warn=-1) 

#if (!require("GetoptLong")) install.packages("GetoptLong", repos ="https://cran.csiro.au/")

rm(list=ls())

source('runmodel.R')
source('helpers.R')

#set defaults 
startCell <- 1
endCell <- 1
run <- "rcp26"
maxCores <- 1
VERSION <- "0.01 (25-Aug-2017)"


#handle commandline parameters
GetoptLong::GetoptLong(
  "startCell=i", "Cell to start processing (1:39567), optional, default (1)",
  "endCell=i", "Cell to end processing (1:39567), optional, default (1). Must be >= startCell",
  "maxCores=i", "Maximum of cores to be used",
  "run=s", "Emissions scenario (rcp26, rcp45, rcp60, rcp85), optional, default (rcp26)",
  "inputPath=s", "Path where well-known input files are available, mandatory",
  "outputPath=s", "Path where output files can and will be written, mandatory"
)

#validations
if (startCell < 1 | startCell > 39567 ) stop(sprintf("value out of range for 'startCell' '%s'", startCell))
if (endCell < 1 | endCell > 39567 ) stop(sprintf("value out of range for 'endCell' '%s'", endCell))
if (startCell > endCell)  stop(sprintf("'endCell' ('%s') must be greater than 'startCell ('%s')", endCell, startCell))

cells <- seq(startCell, endCell, by = 1)
cells <- cells[!cells %in% grid_ids_found(outputPath)]

numCores <- ifelse(length(cells) > maxCores, maxCores, length(cells))

the_cluster <- parallel::makeForkCluster(getOption("cl.cores", numCores))

discard_output <- parallel::clusterApplyLB(the_cluster
               ,x=cells
               ,fun=rungridsep
               ,gcm="ipsl-cm5a-lr"
               ,run=run
               ,output="aggregated"
               ,input_files_location = inputPath
               ,output_files_location = outputPath)

parallel::stopCluster(the_cluster)

