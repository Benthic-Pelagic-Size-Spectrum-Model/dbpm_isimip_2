filename_template <- "/rd/gem/private/fishmip_inputs/rds/rcp26/grid_%s_inputs2_ipsl-cm5a-lr_rcp26.rds"

mylist = list()

gridids <- 1:39567

for (gridid in gridids) {
  dat <- readRDS(sprintf(filename_template, gridid))

  mylist[[gridid]] <- cbind(dat$depth[1,], gridid)
  
  rm(dat)
  
  }

grom <- dplyr::bind_rows(mylist)

saveRDS(grom, file = "grom.rds")
rm(mylist, grom, gridids, gridid, filename_template)

grom2 <- readRDS("grom.rds")
