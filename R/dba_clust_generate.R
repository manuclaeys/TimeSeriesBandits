#'dba_clust_generate
#'
#'Generate a clustering of freatures with \code{\link{dba}} and \code{\link{tsclust}} functions.
#'Apply a global averaging method for time series under DTW (Petitjean, Ketterlin and Gancarski 2011) of numerical times series features.
#'
#'@param dt  A matrix, json or data frame where each row is a time series, or a list where each element is a time series
#'@param type What type of clustering method to use: "partitional", "hierarchical", "tadpole" or "fuzzy" (optional),
#'@param list_cluster_k Number of desired clusters. It can be a numeric vector with different values.
#'@param learn_size number of items dedicated to the learnset (step 1) ,
#'@param arm_for_learn arm dedicated to the learnset (step 1) (optional),
#'@param explanatory_variable list of covariates (optional),
#'@param print_time computational time
#'@param print_graph print clusters and controid c("all", "centroid","cluster","none")
#'
#'@return An object of class \code{\link{TSClusters-class}}
#'
#'@examples
#'dba_clust_generate(dt = data.train[1:1000,listInteger],  list_cluster_k = list_cluster_k , print_graph = "none")
#'@import dtwclust
#'@import tictoc
#'@import jsonlite
#'@import doParallel
#'@import dplyr
#'@export
dba_clust_generate <- function(dt, type = "partitional" , list_cluster_k = list_cluster_k, learn_size = (nrow(dt)) , explanatory_variable = colnames(dt), print_time = FALSE , arm_for_learn = "all" , print_graph = "all"){


  #Optional sub selection
  dt <- dt[1:learn_size,]
  dt <- dt[,explanatory_variable ]

  #arm for learn To DO

  #object to keep clustering informations
  pc.dba <- c()

  #init
  j=1
  ############Clustering
  cl <- makeCluster(detectCores())
  invisible(clusterEvalQ(cl, library(dtwclust)))
  registerDoParallel(cl)
  require(cluster)

  if( print_time != FALSE ) tic()

  for(i in explanatory_variable){
    print(i)

    pc.dba[j]

    pc.dba <-c(pc.dba, tsclust(

      #HCLUST
      #  data.train[[i]], k = list_cluster_k[j], type = "h",
      # seed = 3251,
      # distance = "dtw",  trace = TRUE


      # DBA
      data.train[[i]], k = list_cluster_k[j],
      centroid = "dba",
      seed = 3251, trace = TRUE
      ,control = partitional_control(nrep = 1L,iter.max = 10L )
    ))

    j=j+1


  }

  # Stop parallel workers
  stopCluster(cl)

  # Return to sequential computations. This MUST be done after stopCluster()
  registerDoSEQ()

  #if computational time is require
  if( print_time != FALSE ) print(toc())

  # TO CORRECT
  #  case_when(
  #    print_graph == "all"  ~ for(i in 1:length(explanatory_variable)) plot(pc.dba[i][[1]]) ,
  #    print_graph == "centroid" ~  for(i in 1:length(explanatory_variable)) plot(pc.dba[i][[1]], type = "centroids")
  #  )

  #Add clusters
  for(i in 1:length(explanatory_variable)) data.train[[paste("cluster",explanatory_variable[i],sep = "")]] <- pc.dba[i][[1]]@cluster

  return(list(data = data.train , pc.dba=  pc.dba ))


  }

