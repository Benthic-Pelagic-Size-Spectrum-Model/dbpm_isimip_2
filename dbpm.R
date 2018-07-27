#!/usr/bin/env Rscript
#options(warn=-1) 

#if (!require("GetoptLong")) install.packages("GetoptLong", repos ="https://cran.csiro.au/")
rm(list=ls())

#set defaults 
startId <- 1
endId <- 1
run <- "rcp26"
maxCores <- 1
VERSION <- "0.02 (26-Jul-2018)"
dryRun <- FALSE
scale <- "degree" #degree, lmefao, eez
gcm="ipsl-cm5a-lr"
output="aggregated"

#handle commandline parameters
GetoptLong::GetoptLong(
  "startId=i", "ID to start processing, optional, default (1)",
  "endId=i", "ID to end processing, optional, default (1). Must be >= startId",
  "ids=s", "CSV string of IDs to process. If this argument is given, startId and endID are ignored.",
  "maxCores=i", "Maximum of cores to be used",
  "run=s", "Emissions scenario (rcp26, rcp45, rcp60, rcp85), optional, default (rcp26)",
  "inputPath=s", "Path where well-known input files are available, mandatory",
  "outputPath=s", "Path where output files can and will be written, mandatory",
  "dryRun!", "if set, the long running model code will not be run; all other code will run (default = FALSE, i.e. will run all)",
  "scale=s", "Scale (degree, lmefao, eez) (default = degree)"
)

#validations

#TODO validate ids csv
#TODO consider other range formats, e.g. 1-3,11-13
if (scale=="degree") {
  #TODO if ids is given, then must be 1 or more unque integers, bewteen 1 and 39567
  #else us startId and endID
  
  if (startId < 1 | startId > 39567 ) stop(sprintf("value out of range for 'startId' '%s'", startId))
  if (endId < 1 | endId > 39567 ) stop(sprintf("value out of range for 'endId' '%s'", endId))
  if (startId > endId)  stop(sprintf("'endId' ('%s') must be greater than 'startId ('%s')", endId, startId))
}

if (scale=="lmefao") {
  #TODO if ids is given, then must be 1 or more unque integers, in the bespoke range
  #of lmefao Ids
  #else us startId and endID  
  #when stratID and endID, then mustbe defaulting to valid choice within range
  stop("lmefao scale not implemented")
  
}
if (scale=="eez") {
  stop("eez scale not implemented")
}

if (!exists("inputPath")) stop
if (!exists("outputPath")) stop


# start execution
source('runmodel.R')
source('helpers.R')

# if ids is not given, then 
# use this ids logic. This is dependent on scale. code should likely move into 
# validation/initialisation above
cells <- seq(startId, endId, by = 1)
cells <- cells[!cells %in% grid_ids_found(outputPath)]

# if the length of ids is still 0 after this, then stop
if (length(cells) > 0) {
  
  numCores <- ifelse(length(cells) > maxCores, maxCores, length(cells))
  
  the_cluster <- parallel::makeForkCluster(getOption("cl.cores", numCores))
  
  if(!dryRun) {
    discard_output <- parallel::clusterApplyLB(the_cluster
                                               ,x=cells
                                               ,fun=rungridsep
                                               ,gcm=gcm
                                               ,run=run
                                               ,output=output
                                               ,input_files_location = inputPath
                                               ,output_files_location = outputPath)
  }
  
  parallel::stopCluster(the_cluster)
  
}

