#function that receives simulated database and OD database and returns comparative measures



MF_database_comparison <- function(ODDatabaseOrigin,
                                   ODDatabaseDestination,
                                   simulatedDatabaseOrigin,
                                   simulatedDatabaseDestination,
                                   publico = T,
                                   time = F){
  source('503_buffering_functions.R')
  source('504_analysis_functions.R')
  
  
  #assumindo velocidade de 6 km/h ou 100m/min e criterio de 95% das viagens 
  #bufferSize <- as.integer(100*quantile(c(ODDatabaseOrigin@data[,'ANDA_O'], ODDatabaseOrigin@data[,'ANDA_D']), 0.95))
  bufferSize <- 1500
  print(paste0('Buffer de ', bufferSize,' metros'))
  
  print('associando bases de dados')
  associatedTrips <- associate_buffer_trips(spatialDatabaseODOrigin = ODDatabaseOrigin,
                                            spatialDatabaseODestination = ODDatabaseDestination,
                                            spatialSimulatedDatabaseOrigin = simulatedDatabaseOrigin,
                                            spatialSimulatedDatabaseDestination = simulatedDatabaseDestination,
                                            bufferSize = bufferSize,
                                            time = time)
  
  means <-  means_buffered_trips(bufferId = associatedTrips,
                                 simulatedDatabase = simulatedDatabaseOrigin,
                                 timeColumn = ifelse(publico == T,'temppublico','temppriv'))
  
  aux <- difference_travel_time_buffer_simulated(ODDatabaseOrigin, means)
  
  aux2 <- ratio_travel_time_buffer_simulated(ODDatabaseOrigin, means)
  
  aux$ratio <- aux2$ratio
  
  means <- means_buffered_trips(bufferId = associatedTrips,
                                simulatedDatabase =  simulatedDatabaseOrigin,
                                timeColumn = ifelse(publico == T,'distpublico','distpriv'))
 
  aux$distSimulated <- means
  return(aux)
}
