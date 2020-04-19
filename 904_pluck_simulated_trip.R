#function that selects travels near a bufferzone from a point
#TODO add time constraints to function
pluck_simulated_trip <- function(spatialPoint, spatialDatabase, bufferSize = 1000, time = F){
  require(raster)
  require(dplyr)
  require(sp)
  
  return(spatialDatabase[which(is.na(over(x = spatialDatabase,y = buffer(spatialPoint, width = bufferSize))) ==FALSE),])
  
}
