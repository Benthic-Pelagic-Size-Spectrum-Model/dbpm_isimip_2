



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

#TODO loop runs
runs <- c(26, 45, 60, 85)
runs <- c(26)


for(run in runs) {

    #TODO loop areas
    lme_fao_codes <-sort(unique(grom_lme_fao$lme_fao_code))
    # lme_fao_codes <-1:4
    
    list_out <- list()
    
    for (lme_fao_code in lme_fao_codes) {
        for (gridid in grom_lme_fao[grom_lme_fao$lme_fao_code==lme_fao_code, 1]) {
          
          #read weekly gridcell inputs 2  
          input_file <- sprintf(input_file_pattern, run, gridid, run)
          rds <- readRDS(input_file)
          
          rds <- cbind(ts = seq.int(nrow(rds$ts)), rds$ts, lme_fao_code)
          
          list_out[[paste(gridid)]] <- rds
          
          rm(rds)
        }
    }
    
    
    
    df_out <- plyr::rbind.fill(list_out) %>%
      select(-gridid) %>%
      group_by(ts, lme_fao_code) %>%
      summarise_all(mean) %>%
      ungroup()
    
    output_file <- sprintf(output_file_pattern, run)
    saveRDS(df_out, file=output_file)    
}

#write weekly lme inputs 2

test_file_pattern <- "/rd/gem/private/fishmip_inputs/lmefao/lmefao_inputs2_ipsl-cm5a-lr_rcp%s.rds"
test_run <- 26

test <- readRDS (sprintf(test_file_pattern, test_run))
