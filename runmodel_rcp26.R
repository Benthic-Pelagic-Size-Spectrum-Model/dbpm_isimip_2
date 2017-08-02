rm(list=ls())

library(parallel)

source('runmodel.R')
source('runmodel_helpers.R')

the_input_path <- "/rd/gem/private/fishmip_inputs/"
the_output_path <- "/rd/gem/private/fishmip_outputs/20170802_rcp26/"

grid_ids <- 1:39567
grid_ids <- grid_ids[!grid_ids %in% grid_ids_found(the_output_path)]

the_run <- "rcp26"
the_gcm <- "ipsl-cm5a-lr"


#2/8/17 trials

ptm=proc.time()
options(warn=-1) #?

maxcores <-  detectCores()-1

numcores <- ifelse(length(grid_ids) > maxcores, maxcores, length(grid_ids))

the_cluster <- makeForkCluster(getOption("cl.cores", numcores))

discard_output <- clusterApplyLB(the_cluster
               ,x = grid_ids
               ,fun = rungridsep
               ,gcm = the_gcm
               ,run = the_run
               ,output = "aggregated"
               ,output_files_location = the_output_path
               ,input_files_location = the_input_path)


stopCluster(the_cluster)

print((proc.time()-ptm)/60.0)

rm(the_cluster, numcores, ptm, discard_output)