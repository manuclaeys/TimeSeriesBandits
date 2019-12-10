#'dba_clust_associate
#'
#'Associate numerical times series to the closest centroid for each freatures with the \code{\link{dtw}} function.
#'Based o DTW distance.
#'
#'@param dt  A matrix, json or data frame where each row is a time series, or a list where each element is a time series
#'@param explanatory_variable list of covariates (optional),
#'@param print_time computational time
#'@param pc.dba An object of class \code{\link{TSClusters-class}
#'@param list_cluster_k Number of desired clusters. It can be a numeric vector with different values.
#'
#'@return An dataframe
#'
#'@examples
#'dba_clust_associate(dt = data.train[1:1000,listInteger],  list_cluster_k = list_cluster_k)
#'@import dtwclust
#'@import tictoc
#'@import doParallel
#'@import dplyr
#'@export
dba_clust_associate <- function(dt, pc.dba, explanatory_variable = colnames(dt), list_cluster_k, print_time = FALSE ){


  #Optional sub selection
  dt <- dt[,explanatory_variable ]

  #arm for learn To DO


  #if computational time is require
  if( print_time != FALSE ) print(toc())

  ## choose the closest centroid
  k=0
  temp_i=0
  for(i in explanatory_variable){
    print(i)
    temp_i = temp_i + 1
    k <- k + 1
    for(j in 1: nrow(dt)){
      #print(j)
      temp_clust = 1
      temp_clust_dist  = dtw2(unlist(dt[[i]][j]), unlist(pc.dba[temp_i][[1]]@centroids[1]))$distance
      for(l in 2:list_cluster_k[k]){

        #init
        if(temp_clust_dist > dtw2(unlist(data.train[[i]][j]), unlist(pc.dba[temp_i][[1]]@centroids[l]))$distance){
          temp_clust = l
          temp_clust_dist  =  dtw2(unlist(dt[[i]][j]), unlist(pc.dba[temp_i][[1]]@centroids[1]))$distance
        }

      }

      dt[[paste("cluster",listInteger[k],sep = "")]][j] <- temp_clust
    }


  }


  return(dt)


}

