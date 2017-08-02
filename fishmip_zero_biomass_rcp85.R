#rm(list=ls())


findzeros <- function(filename, path = "./") {
  load(paste(path, "/", filename, sep=""))
  return(any(agg$TotalUbiomass[3600:4200] == 0) )
  
}


#rcp85 run
run <- "rcp85"
outputfiles_path_rcp85 <- "/rd/gem/private/fishmip_outputs/20170730_rcp85/"

#retrieve filenames in a vector
vec_filenames_rcp85 <-  unlist(
  list.files(path = outputfiles_path_rcp85, 
             pattern = "RData$",
             full.names = FALSE)
)

df_filenames_rcp85 <- data.frame(vec_filenames_rcp85,stringsAsFactors = FALSE)
names(df_filenames_rcp85) <- "filename"



if (exists("df_all_files_rcp85")) {
  df_new_files_rcp85 <- data.frame(df_filenames_rcp85[!(df_filenames_rcp85$filename %in% df_all_files_rcp85$filename), ],stringsAsFactors = FALSE)
  names(df_new_files_rcp85) <- "filename"
  
} else {
  df_new_files_rcp85 <- df_filenames_rcp85
  
}


df_new_files_rcp85$zeros <- sapply(df_new_files_rcp85[,1]
                                   ,findzeros
                                   ,path=outputfiles_path_rcp85
          )

df_new_files_rcp85$gridid <- as.numeric(gsub("(res_mts_agg_igrid_|_ipsl-cm5a-lr_rcp85.RData)", "", df_new_files_rcp85[,1]))

max(df_new_files_rcp85$gridid)


if (exists("df_all_files_rcp85")) {
  df_all_files_rcp85 <- rbind(df_all_files_rcp85, df_new_files_rcp85[,c("filename","gridid","zeros")])
  
} else {
  df_all_files_rcp85 <- df_new_files_rcp85 
  
}



rm(df_new_files_rcp85,df_filenames_rcp85, vec_filenames_rcp85)
rm(outputfiles_path_rcp85, findzeros, run)


#show number and list all files with zeros found
total <- nrow(df_all_files_rcp85)
zeros <- nrow(df_all_files_rcp85[df_all_files_rcp85$zeros==TRUE,])
sprintf("Total cells: %s", total)
sprintf("With zeros in 'agg$TotalUbiomass[3600:4200]': %s", zeros)

sprintf("That's %s %%", round(100*zeros/total, 1))


head(df_all_files_rcp85[df_all_files_rcp85$zeros==TRUE,])

rm( total, zeros)

load("/rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")

#it looks like depth$gridnum is not a strict sequence of cells from 1:39567
#rather it looks like the 39567 cells that remain after removing all land cells from a larger set
#presumably 360 * 180 = 64800
# im adding an attribute gridid as a ordered sequence to depth
#assuming that that sequnece corresponds with the sequnece in the rcp60 run

depth$gridid[order(depth$gridnum)] <- 1:nrow(depth)


#mergin into 'all_files_with_depth' now results in corresponding correct numbers
#(i.e. the number of rows in df_all_files, corresponds with the resultant number 
#of rows  in all_files_with_depth)
all_files_rcp85_with_depth <- merge(df_all_files_rcp85, depth, by = "gridid")

plot(all_files_rcp85_with_depth$lon,all_files_rcp85_with_depth$lat, col=c("blue", "red")[all_files_rcp85_with_depth$zeros+1], cex=.5, pch=".")




library(ggplot2)

#ggplot(all_files_rcp85_with_depth, aes(x=lon, y=lat, colour=zeros)) + geom_point()

ggplot(all_files_rcp85_with_depth, aes(x=lon, y=lat, fill=zeros)) + geom_raster() 


max(all_files_rcp85_with_depth$gridid)


df_all_files_rcp85run <- all_files_rcp85_with_depth

head (df_all_files_rcp85run)

save(df_all_files_rcp85run, file="/rd/gem/private/fishmip_zero_problem/df_all_files_rcp85run.RData")


