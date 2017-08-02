grid_ids_found <- function (thepath) {
  
  #TODO return NA  for path not exists
  #TODO return 0 for no files found
  
  #retrieve filenames in a vector
  filenames <-  data.frame(unlist(
    list.files(path = thepath, 
               pattern = "RData$",
               full.names = FALSE)
  ),stringsAsFactors = FALSE)
  
  names(filenames) <- "filename"
  
  filenames$gridid <- as.numeric(stringr::str_match(filenames$filename, "igrid_(.+?)_")[,2])
  
  
  return(filenames$gridid)
}