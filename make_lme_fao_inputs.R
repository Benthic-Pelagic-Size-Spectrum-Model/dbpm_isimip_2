library(dplyr)



#(read Grom_lme_fao)
grom_lme_fao <- readRDS("grom_lme_fao.rds") %>% 
  select(gridid, lme_fao_code, gridnum) %>%
  na.omit() #TODO find out why there are NAs?

# grom_lme_fao %>%
#   group_by(lme_fao_code) %>%
#   summarise(numcells = n()) %>%
#   arrange(numcells)

# loop through grom lme_fao

input_file_pattern <- "/rd/gem/private/fishmip_inputs/rds/rcp%s/grid_%s_inputs2_ipsl-cm5a-lr_rcp%s.rds"
output_file_pattern <- "/rd/gem/private/fishmip_inputs/lmefao/lmefao_inputs2_ipsl-cm5a-lr_rcp%s.rds"

#loop through rcp scenarios
runs <- c(26, 45, 60, 85)

for(run in runs) {
  
     #loop through LME/FAO codes from grom_lme_fao
    lme_fao_codes <-sort(unique(grom_lme_fao$lme_fao_code))
    # lme_fao_codes <-1:4 # for testing
    
    list_out <- list()
    
    for (lme_fao_code in lme_fao_codes) {
      
         #Loop through gridcells filtered by LME/FAO code
        for (gridid in grom_lme_fao[grom_lme_fao$lme_fao_code==lme_fao_code, 1]) {
          
          #read weekly gridcell "inputs2"
          input_file <- sprintf(input_file_pattern, run, gridid, run)
          rds <- readRDS(input_file)
          
          rds <- cbind(ts = seq.int(nrow(rds$ts)), rds$ts, lme_fao_code) #add explicit timestep (ts) as column
          
          list_out[[paste(gridid)]] <- rds #add to list of dataframes, 1 df for each cell in an LME/FAO
          
          rm(rds)
        }
    }
    
    # append all dataframes in list
    # remove gridid
    # average (by mean), grouped by LME/FAO code and timestep
    df_out <- plyr::rbind.fill(list_out) %>%
      select(-gridid) %>%
      group_by(ts, lme_fao_code) %>%
      summarise_all(mean) %>%
      ungroup()
    
    #save as single rds (dataframe) per rcp scenario
    output_file <- sprintf(output_file_pattern, run)
    saveRDS(df_out, file=output_file)   
    rm(list_out)
}

#write weekly lme inputs 2

test_file_pattern <- "/rd/gem/private/fishmip_inputs/lmefao/lmefao_inputs2_ipsl-cm5a-lr_rcp%s.rds"
test_run <- 26

test <- readRDS (sprintf(test_file_pattern, test_run))
