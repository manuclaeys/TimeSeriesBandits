#Importe data
remove(list = ls())
setwd("~/manip/R/serveur_cluster_these/cluster/dorcel")

#load bandit4abtest package
library(devtools)
#install_github("manuclaeys/bandit4abtest")
library(bandit4abtest)
#https://github.com/manuclaeys/bandit4abtest

#Importe data
library(jsonlite)


data.train  <- jsonlite::fromJSON("parcoursuserDatabaseFinalModz1.JSON", simplifyDataFrame = TRUE)
visitorReward <- jsonlite::fromJSON("parcours_reward_modz.JSON", simplifyDataFrame = TRUE)

#global summary of data
summary(as.factor(visitorReward$A))
summary(as.factor(visitorReward$B))

#which covariate will be observe?
#here covariates are time series
listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )

#don't use time series with lenght smaller than 2
data.train <-data.train[data.train$size_time_serie>1 , ]
#how many transaction do we have now?
summary(as.factor(data.train$transactions))

#define time series as numerical time series
data.train$presence_time_serie <- lapply(data.train$presence_time_serie, as.numeric)
data.train$connexion_time_time_serie <- lapply(data.train$connexion_time_time_serie, as.numeric)
data.train$time_spend_time_serie <- lapply(data.train$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
for(i in 1:nrow(data.train)) data.train$time_spend_time_serie[i] <- lapply(data.train$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))

################################################

list_cluster_k <- c(3,3,3)

library(dplyr)


library(dplyr)

library(TimeSeriesBandits)

my_obj <-   dba_clust_generate(dt = data.train, learn_size = (0.3*nrow(data.train)), explanatory_variable =  listInteger, list_cluster_k = list_cluster_k , print_graph = "none")

myobj2 <-    mean_generate(dt = data.train , learn_size = (0.3*nrow(data.train)), explanatory_variable =  listInteger)

#############################################
### clustering on the rest of the dataset ###

### Will be done directly on dbactreeucb ###
myobj3 <- dba_clust_associate(dt = data.train[(0.3*nrow(data.train) + 1 )  :  nrow(data.train) ,]  ,  pc.dba = my_obj$pc.dba ,
                              explanatory_variable =  listInteger, list_cluster_k = list_cluster_k      )

### add means ###
myobj4 <-    mean_generate(dt =  data.train[(0.3*nrow(data.train) + 1 )  :  nrow(data.train) ,]  , explanatory_variable =  listInteger)


data_for_learn <- merge(my_obj$data ,myobj2  )


data_for_test<- merge(  merge(data.train[(0.3*nrow(data.train) + 1 )  :  nrow(data.train) ,] , myobj3) ,myobj4  )

##############################################
############  dynamic allocation  ############



