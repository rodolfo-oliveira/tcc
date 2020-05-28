
origin_destination_buffer_trip <- function(originSpatialPoint,
                                           simulatedTripsOrigins,
                                           destinationSpatialPoint,
                                           simulatedTripsDestinations,
                                           bufferSize = 1000, time = F){
  require(raster)
  require(dplyr)
  require(sp)
  source('904_pluck_simulated_trip.R')
  
  origins <- pluck_simulated_trip(spatialPoint = originSpatialPoint,
                                  spatialDatabase = simulatedTripsOrigins,
                                  bufferSize = bufferSize,
                                  time = time)
  
  destinations <- pluck_simulated_trip(spatialPoint = destinationSpatialPoint,
                                       spatialDatabase = simulatedTripsDestinations,
                                       bufferSize =  bufferSize,
                                       time = time)
  
  
  
  return(origins[origins@data$ID %in% destinations@data$ID,])
  
}
