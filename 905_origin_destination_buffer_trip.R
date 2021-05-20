#Função que seleciona viagens simuladas com origem e destino dentro de buffers espaciais centrados nos pontos de origem e destino de uma viagem da pesquisa OD
#originSpatialDataPoint = ponto espacial da origem da viagem OD
#simulatedTripsOrigins = banco de pontos de origem das viagens simuladas
#destinationSpatialPoint = ponto espacial do destino da viagem OD
#simulatedTripsDestinations = banco de pontos de destino das viagens simuladas
#buffersize = tamanho do buffer em torno do ponto em metros
#time = se TRUE realiza o controle de viagens que ocorreram em um intervalo de duas horas para cima e para baixo


origin_destination_buffer_trip <- function(originSpatialPoint,
                                           simulatedTripsOrigins,
                                           destinationSpatialPoint,
                                           simulatedTripsDestinations,
                                           bufferSize = 1000, time = F){
  require(raster)
  require(dplyr)
  require(sp)
  source('904_pluck_simulated_trip.R')
  
  
  #seleção de viagem em torno do buffer no ponto da origem
  origins <- pluck_simulated_trip(spatialPoint = originSpatialPoint,
                                  spatialDatabase = simulatedTripsOrigins,
                                  bufferSize = bufferSize,
                                  time = time)
  #seleção de viagem em torno do buffer no ponto de destino
  destinations <- pluck_simulated_trip(spatialPoint = destinationSpatialPoint,
                                       spatialDatabase = simulatedTripsDestinations,
                                       bufferSize =  bufferSize,
                                       time = time)
  
  
  
  return(origins[origins@data$ID %in% destinations@data$ID,])
  
}
