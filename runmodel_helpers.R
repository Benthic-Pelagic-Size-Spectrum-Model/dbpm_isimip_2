grid_ids_found <- function (thepath) {
  
  #TODO return NA  for path not exists
  
  #retrieve filenames in a vector
  
  #thepath <- "/rd/gem/private/fishmip_outputs/20170802_trial/"
  filenames <-  data.frame(unlist(
    list.files(path = thepath, 
               pattern = "RData$",
               full.names = FALSE)
  ),stringsAsFactors = FALSE)
  
  names(filenames) <- "filename"
  
  if (nrow(filenames)==0){
    retval <-  0
  } else {
  
    filenames$gridid <- as.numeric(stringr::str_match(filenames$filename, "igrid_(.+?)_")[,2])
    retval <- filenames$gridid
  }
  return(retval)
}