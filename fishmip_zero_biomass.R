#rm(list=ls())

source("helpers.R")

findzeros <- function(filename, path = "./") {
  load(paste(path, "/", filename, sep=""))
  return(any(agg$TotalUbiomass[start_of_history:end_of_history] == 0) )
  
}


#test files
#outputfiles_path <- "/rd/gem/private/justtest_fishmip_ouputs"

#rcp run
outputfiles_path <- "/rd/gem/private/fishmip_outputs/aug_2017/rcp45/"

#retrieve filenames in a vector
vec_filenames <-  unlist(
  list.files(path = outputfiles_path, 
             pattern = "RData$",
             full.names = FALSE)
)

df_filenames <- data.frame(vec_filenames,stringsAsFactors = FALSE)
names(df_filenames) <- "filename"



if (exists("df_all_files")) {
  df_new_files <- data.frame(df_filenames[!(df_filenames$filename %in% df_all_files$filename), ],stringsAsFactors = FALSE)
  names(df_new_files) <- "filename"
  
} else {
  df_new_files <- data.frame(df_filenames,stringsAsFactors = FALSE)
  names(df_new_files) <- "filename"
  
}


df_new_files$zeros <- sapply(df_new_files[,1]
                             ,findzeros
                             ,path=outputfiles_path
          )

df_new_files$gridid <- as.numeric(gsub("(res_mts_agg_igrid_|_ipsl-cm5a-lr_rcp45.RData)", "", df_new_files[,1]))

if (exists("df_all_files")) {
df_all_files <- rbind(df_all_files, df_new_files[,c("filename","gridid","zeros")])
} else
{
  df_all_files <- df_new_files[,c("filename","gridid","zeros")]
  
}
rm(df_new_files,df_filenames, vec_filenames)
rm(outputfiles_path, findzeros)


#show number and list all files with zeros found
total <- nrow(df_all_files)
zeros <- nrow(df_all_files[df_all_files$zeros==TRUE,])
sprintf("Total cells: %s", total)
sprintf("With zeros in 'agg$TotalUbiomass[3600:4200]': %s", zeros)

sprintf("That's %s %%", round(100*zeros/total, 1))


head(df_all_files[df_all_files$zeros==TRUE,])

rm( total, zeros)


load("/rd/gem/private/fishmip_inputs/depth_ipsl-cm5a-lr_historical.RData")

#it looks like depth$gridnum is not a strict sequence of cells from 1:39567
#rather it looks like the 39567 cells that remain after removing all land cells from a larger set
#presumably 360 * 180 = 64800
# im adding an attribute gridid as a ordered sequence to depth
#assuming that that sequnece corresponds with the sequnece in the rcp60 run


head(depth)

depth$gridid[order(depth$gridnum)] <- 1:nrow(depth)


#mergin into 'all_files_with_depth' now results in corresponding correct numbers
#(i.e. the number of rows in df_all_files, corresponds with the resultant number 
#of rows  in all_files_with_depth)
all_files_with_depth <- merge(df_all_files, depth, by = "gridid")


plot(all_files_with_depth$lon[all_files_with_depth$zeros==FALSE],all_files_with_depth$lat[all_files_with_depth$zeros==FALSE], col="blue", cex=.5, pch=".")
points(all_files_with_depth$lon[all_files_with_depth$zeros==TRUE],all_files_with_depth$lat[all_files_with_depth$zeros==TRUE], col="red", cex=.5, pch=".")


plot(all_files_with_depth$lon,all_files_with_depth$lat, col=c("blue", "red")[all_files_with_depth$zeros+1], cex=.5, pch=".")




library(ggplot2)

ggplot(all_files_with_depth, aes(x=lon, y=lat, colour=zeros)) + geom_point()

ggplot(all_files_with_depth, aes(x=lon, y=lat, fill=zeros)) + geom_raster() 


df_all_files_rcp45run <- all_files_with_depth
save(df_all_files_rcp45run, file="/rd/gem/private/fishmip_zero_problem/df_all_files_rcp45run.RData")


