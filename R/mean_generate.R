#'mean_generate
#'
#'Add global means for each time series
#'
#'@param dt  A matrix, json or data frame where each row is a time series, or a list where each element is a time series
#'@param explanatory_variable list of covariates (optional),
#'@param print_time computational time
#'@param print_graph print clusters and controid c("all", "centroid","cluster","none")
#'
#'@return An object of class \code{\link{TSClusters-class}}
#'
#'@examples
#'mean_generate(dt = data.train)
#'@import tictoc
#'@export
mean_generate <- function(dt, learn_size = (nrow(dt)) , explanatory_variable = colnames(dt), print_time = FALSE ){

  #Optional sub selection
  dt <- dt[1:learn_size,]
  dt <- dt[,explanatory_variable ]

  ## add global means for each time series

  for(i in 1:length(explanatory_variable)) dt[[paste("mean",explanatory_variable[i],sep = "")]] <- 0

  if( print_time != FALSE ) tic()

  for(i in 1:nrow(dt)){
     for(j in explanatory_variable)  dt[[paste("mean",j,sep = "")]][i] <-  mean( unlist(dt[[j]][i] ))
  }

  #if computational time is require
  if( print_time != FALSE ) print(toc())

  return(data = dt)
}
