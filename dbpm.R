#!/usr/bin/env Rscript
#options(warn=-1) 

#if (!require("GetoptLong")) install.packages("GetoptLong", repos ="https://cran.csiro.au/")
rm(list=ls())

#set defaults 
startId <- 1
endId <- 1
ids <- ""
run <- "rcp26"
maxCores <- 1
VERSION <- "0.02 (26-Jul-2018)"
dryRun <- FALSE
scale <- "degree" #degree, lmefao, eez
gcm="ipsl-cm5a-lr" #reanalysis, ipsl-cm5a-lr
output="aggregated" #aggregated

#handle commandline parameters
GetoptLong::GetoptLong(
  "startId=i", "ID to start processing, optional, default (1)",
  "endId=i", "ID to end processing, optional, default (1). Must be >= startId",
  "ids=s", "CSV string of IDs to process. If this argument is given, startId and endID are ignored",
  "maxCores=i", "Maximum of CPU cores to be used, optional, default (1)",
  "run=s", "Emissions scenario (rcp26, rcp45, rcp60, rcp85), optional, default (rcp26)",
  "inputPath=s", "Path where well-known input files are available, mandatory",
  "outputPath=s", "Path where output files can and will be written, mandatory",
  "dryRun!", "if set, the long running model code will not be run; all other code will run (default = FALSE, i.e. will run all)",
  "scale=s", "Scale (degree, lmefao, eez) (default = degree)"
)

#validations
#implement assert_that
if (ids == "") {
  if (startId > endId)  stop("'endId' must be greater than 'startId'")
  ids <- seq(startId, endId, by = 1)
  
} else if (grepl("^([0-9]{1,5},)*[0-9]{1,5}$", ids)) {
  ids <- unique(as.integer(strsplit(ids,",")[[1]]))
  
} else {
  stop ("'ids' must be strictly comma separated integers")  
  }


if (scale=="degree") {
  #TODO if ids is given, then must be 1 or more unique integers, bewteen 1 and 39567
  if (startId < 1 | startId > 39567 ) stop(sprintf("value out of range for 'startId' '%s'", startId))
  if (endId < 1 | endId > 39567 ) stop(sprintf("value out of range for 'endId' '%s'", endId))
  
}

if (scale=="lmefao") {
  #TODO if ids is given, then must be 1 or more unique integers, in the bespoke range
  #of lmefao Ids 
  
  lmefao_range <- readRDS("lme_scale/grom_lme_fao.rds") %>%
    select(lme_fao_code) %>%
    unique()%>%
    arrange(lme_fao_code) %>%
    pull()
  
  if (!all(ids %in% lmefao_range)) stop ("ids aren't all lme_fao ids")

}


if (scale=="eez") {
  stop("eez scale not implemented")
}

if (!exists("inputPath")) stop ("inputPath not found")
if (!exists("outputPath")) stop("outputPath not found")


# start execution
source('runmodel.R')
source('helpers.R')

#any id already found in outputpath, excluded from ids (must be made scale dependent)
ids <- ids[!ids %in% grid_ids_found(outputPath)]

# if the length of ids is 0 after this, then stop
numJobs <- length(ids)
if (numJobs > 0) {
  
  numCores <- min(numJobs, maxCores)
  
  theCluster <- parallel::makeForkCluster(getOption("cl.cores", numCores))
  
  if(!dryRun) {
    discard_output <- parallel::clusterApplyLB(theCluster
                                               ,x=ids
                                               ,fun=rungridsep
                                               ,gcm=gcm
                                               ,run=run
                                               ,output=output
                                               ,input_files_location = inputPath
                                               ,output_files_location = outputPath
                                               ,scale = scale)
  }
  
  parallel::stopCluster(theCluster)
  
}

