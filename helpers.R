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


get_in_file_name <- function(run, grid_id, in_path="/rd/gem/private/fishmip_inputs"){
  
  in_file_name <- sprintf("%s/grid_%i_inputs2_ipsl-cm5a-lr_%s.RData", in_path, grid_id, run)
  return(in_file_name )
    
}


get_out_file_name <- function(run, grid_id, out_path="/rd/gem/private/fishmip_outputs/aug_2017"){

  out_file_name <- sprintf("%s/%s/res_mts_agg_igrid_%i_ipsl-cm5a-lr_%s.RData", out_path, run, grid_id, run)  

  return( out_file_name )
  
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# from : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#cutoffs
start_of_spinup <- 1
end_of_spinup <- 300*12
start_of_history <- end_of_spinup + 1
end_of_history <- end_of_spinup + 55*12
start_of_projections <- end_of_history + 1
end_of_projections <- 451*12

start_of_spinup_weeks <- 1
end_of_spinup_weeks <- 300*48
start_of_history_weeks <- end_of_spinup_weeks + 1
end_of_history_weeks <- end_of_spinup_weeks + 55*48
start_of_projections_weeks <- end_of_history_weeks + 1
end_of_projections_weeks <- 21645 #451*48 - 3

dbpm.variables <- read.csv("variables.csv", header = TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
