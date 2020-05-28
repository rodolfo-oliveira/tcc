#function to associate simulated trips to OD trips

associate_buffer_trips <- function(spatialDatabaseODOrigin,
                                   spatialDatabaseODestination,
                                   spatialSimulatedDatabaseOrigin,
                                   spatialSimulatedDatabaseDestination,
                                   bufferSize = 1000, 
                                   time = F){
  source('503_buffering_functions.R')
  
  result <- data.frame(IDs = spatialDatabaseODOrigin$DURACAO)
  result$IDs <- c(NA)
  for(i in 1:length(spatialDatabaseODOrigin)){
    aux <- origin_destination_buffer_trip(originSpatialPoint = spatialDatabaseODOrigin[i,],
                                          simulatedTripsOrigins = spatialSimulatedDatabaseOrigin,
                                          destinationSpatialPoint = spatialDatabaseODestination[i,],
                                          simulatedTripsDestinations = spatialSimulatedDatabaseDestination,
                                          bufferSize = bufferSize,
                                          time = time)
    
    result$IDs[i]  <- list(aux@data$ID)
    print(paste0(round(100*i/length(spatialDatabaseODOrigin), digits = 2), "%"))
  }  
  return(result)
}



