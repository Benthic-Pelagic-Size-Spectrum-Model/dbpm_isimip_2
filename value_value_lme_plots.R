library(tidyverse)
source("helpers.R")

.vv_lme_chart <- function(run, vars) {
  #value-value lme chart
  #charts value pairs (at equal timestep)
  a_filename <- sprintf("/rd/gem/private/fishmip_outputs/20180805/lmefao/res_mts_agg_ipsl-cm5a-lr_%s.rds", run)
  b_filename <- sprintf("/rd/gem/private/fishmip_outputs/20180805/lmefao_fromgridout/res_mts_agg_ipsl-cm5a-lr_%s.rds", run)
  a <- readRDS(a_filename) %>% 
    select (lmefao, ts, TotalUbiomass, Ubiomass10plus, Ubiomass270plus, TotalVbiomass, Vbiomass10plus, Vbiomass270plus, TotalW) %>% 
    gather(key = "variable", value = "value", -lmefao, -ts) %>% 
    mutate(type = "avg_on_input")
  
  b <- readRDS(b_filename)%>% 
    select (lmefao, ts, TotalUbiomass, Ubiomass10plus, Ubiomass270plus, TotalVbiomass, Vbiomass10plus, Vbiomass270plus, TotalW) %>% 
    gather(key = "variable", value = "value", -lmefao, -ts) %>% 
    mutate(type = "avg_on_output")
  
  possible_vars <- c("TotalUbiomass", "Ubiomass10plus", "Ubiomass270plus", "TotalVbiomass", "Vbiomass10plus", "Vbiomass270plus", "TotalW")
  
  if(missing(vars)){
    selected_variables <- possible_vars 
  } else   {
    selected_variables <- intersect(possible_vars, vars)
    assertthat::assert_that(length(selected_variables) > 0, msg = "No variables specified to chart")
  }
  # selected_variables <- c("Ubiomass10plus", "Ubiomass270plus", "Vbiomass10plus", "Vbiomass270plus")
  
  
  d <- a %>% 
    bind_rows(b) %>% 
    spread(type, value) %>% 
    filter(variable %in% selected_variables)
  
  num <-  d %>% summarise(n()) %>% pull()
  num_na <- d %>% filter(is.na(avg_on_input) | is.na(avg_on_output) ) %>% summarise(n()) %>% pull()
  
  
  p <- ggplot(d) +
    geom_abline(slope = 1 , intercept = 0, size = 0.1) +
    geom_point(aes(x = avg_on_input, y = avg_on_output, colour = factor(lmefao)), shape=".") +
    facet_wrap(~ variable, ncol = 3) +
    ggtitle(
      paste("LME-FAO aggregation comparison, values@timestep", run), 
      subtitle = paste(num, "values,", num_na, "NAs removed")
    ) +
    xlab("LME-scale inputs, LME-scale outputs") +
    ylab("Grid-scale inputs, ouputs averaged to LME-scale") +
    theme(
     legend.position="none",
     plot.subtitle=element_text(size=10, face="plain")
    )
  
  return(p)
}


.vv_lme_chart("rcp26")

.vv_lme_chart("rcp26",c("Ubiomass270plus"))

