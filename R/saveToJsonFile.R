#'saveToJsonFile
#'
#'Save a dataframe with time series as colomn on a .json file
#'
#'@param data_table Table containing the data
#'@param name
#'@param fileLoc
#'@return a message
#'
#'@examples
#'data_table = data.frame(replicate(10,sample(0:1,1000,rep=TRUE)))
#'name = "my_file"
#'fileLoc = "C:/Users/Manue/Documents"
#'saveToJsonFile(data_table = data_table,name = name, fileLoc = fileLoc )
#'@import jsonlite
#'@export

saveToJsonFile <- function(data_table,name,fileLoc){
  ## Save the JSON to file
  library(jsonlite)
  exportJson <- toJSON(data_table)


  write(exportJson, paste(fileLoc,"/",name,".JSON",sep=""))

  return(paste("file saved in ",fileLoc, sep = ""))
}
