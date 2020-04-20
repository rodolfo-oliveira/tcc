#diagnostic function - how many simulated trips fitted for each OD travel

count_buffer_trips <- function(spatialDatabaseODOrigin,
                               spatialDatabaseODestination,
                               spatialSimulatedDatabaseOrigin,
                               spatialSimulatedDatabaseDestination,
                               bufferSize = 1000){
  source('503_buffering_functions.R')
  
  result <- c()
  for(i in 1:length(spatialDatabaseODOrigin)){
    aux <- origin_destination_buffer_trip(originSpatialPoint = spatialDatabaseODOrigin[i,],
                                          simulatedTripsOrigins = spatialSimulatedDatabaseOrigin,
                                          destinationSpatialPoint = spatialDatabaseODestination[i,],
                                          simulatedTripsDestinations = spatialSimulatedDatabaseDestination,
                                          bufferSize = bufferSize)
    
    result <- c(result,length(aux))
    print(paste0(round(100*i/length(spatialDatabaseODOrigin), digits = 2), "%"))
  }  
  return(result)
}



