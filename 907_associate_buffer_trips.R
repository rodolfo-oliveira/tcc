#function to associate simulated trips to OD trips

#associa as viagens simuladas dentro do buffer à viagem da Pesquisa OD 
#spatialDatabaseODOrigin = ponto espacial da origem da viagem OD
#spatialDatabaseODestination = ponto espacial do destino da viagem OD
#spatialSimulatedDatabaseOrigin = banco de pontos de origem das viagens simuladas
#spatialSimulatedDatabaseDestination = banco de pontos de destino das viagens simuladas
#buffersize = tamanho do buffer em torno do ponto em metros
#time = se TRUE realiza o controle de viagens que ocorreram em um intervalo de duas horas para cima e para baixo


associate_buffer_trips <- function(spatialDatabaseODOrigin,
                                   spatialDatabaseODestination,
                                   spatialSimulatedDatabaseOrigin,
                                   spatialSimulatedDatabaseDestination,
                                   bufferSize = 1000, 
                                   time = F){
  source('503_buffering_functions.R')
  
  #inicialização do dataframe de identificadores
  result <- data.frame(IDs = spatialDatabaseODOrigin$DURACAO)
  result$IDs <- c(NA)
  
  #identificação das viagens dentro do buffer
  for(i in 1:length(spatialDatabaseODOrigin)){
    aux <- origin_destination_buffer_trip(originSpatialPoint = spatialDatabaseODOrigin[i,],
                                          simulatedTripsOrigins = spatialSimulatedDatabaseOrigin,
                                          destinationSpatialPoint = spatialDatabaseODestination[i,],
                                          simulatedTripsDestinations = spatialSimulatedDatabaseDestination,
                                          bufferSize = bufferSize,
                                          time = time)
    
    result$IDs[i]  <- list(aux@data$ID)
    print(paste0(round(100*i/length(spatialDatabaseODOrigin), digits = 2), "% - associando base de dados"))
  }  
  return(result)
}



