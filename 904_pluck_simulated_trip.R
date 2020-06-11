#function that selects travels near a bufferzone from a point
pluck_simulated_trip <- function(spatialPoint, spatialDatabase, bufferSize = 1000, time = F){
  require(raster)
  require(dplyr)
  require(sp)
  
  if(time == T){
    return(spatialDatabase[which(is.na(over(x = spatialDatabase,y = buffer(spatialPoint, width = bufferSize))) == FALSE & 
                            ((spatialDatabase@data$Hour_cut >= spatialPoint@data$H_SAIDA-2) & (spatialDatabase@data$Hour_cut <= spatialPoint@data$H_SAIDA+2))),])
  }
  return(spatialDatabase[which(is.na(over(x = spatialDatabase,y = buffer(spatialPoint, width = bufferSize))) == FALSE),])
}
