
origin_destination_buffer_trip <- function(originSpatialPoint,
                                           simulatedTripsOrigins,
                                           destinationSpatialPoint,
                                           simulatedTripsDestinations,
                                           bufferSize = 1000, time = F){
  require(raster)
  require(dplyr)
  require(sp)
  source('904_pluck_simulated_trip.R')
  
  origins <- pluck_simulated_trip(originSpatialPoint,simulatedTripsOrigins, bufferSize)
  destinations <- pluck_simulated_trip(destinationSpatialPoint, simulatedTripsDestinations, bufferSize)
  
  
  
  return(origins[origins@data$ID %in% destinations@data$ID,])
  
}
