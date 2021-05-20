#diagnostic function - how many simulated trips fitted for each OD travel

#conta quantas viagem simuladas foram encontradas para cada ponto 
#spatialDatabaseODOrigin = ponto espacial da origem da viagem OD
#spatialDatabaseODestination = ponto espacial do destino da viagem OD
#spatialSimulatedDatabaseOrigin = banco de pontos de origem das viagens simuladas
#spatialSimulatedDatabaseDestination = banco de pontos de destino das viagens simuladas
#buffersize = tamanho do buffer em torno do ponto em metros

count_buffer_trips <- function(spatialDatabaseODOrigin,
                               spatialDatabaseODestination,
                               spatialSimulatedDatabaseOrigin,
                               spatialSimulatedDatabaseDestination,
                               bufferSize = 1000){
  source('503_buffering_functions.R')
  
  result <- c()
  for(i in 1:length(spatialDatabaseODOrigin)){
    #selecao de viagens simuladas dentro dos buffers de origem e destino
    aux <- origin_destination_buffer_trip(originSpatialPoint = spatialDatabaseODOrigin[i,],
                                          simulatedTripsOrigins = spatialSimulatedDatabaseOrigin,
                                          destinationSpatialPoint = spatialDatabaseODestination[i,],
                                          simulatedTripsDestinations = spatialSimulatedDatabaseDestination,
                                          bufferSize = bufferSize)
    
    #contagem do número de viagem selecionadas
    result <- c(result,length(aux))
    print(paste0(round(100*i/length(spatialDatabaseODOrigin), digits = 2), "%"))
  }  
  return(result)
}



