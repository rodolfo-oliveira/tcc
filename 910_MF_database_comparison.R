#function that receives simulated database and OD database and returns comparative measures

#spatialDatabaseODOrigin = ponto espacial da origem da viagem OD
#spatialDatabaseODestination = ponto espacial do destino da viagem OD
#spatialSimulatedDatabaseOrigin = banco de pontos de origem das viagens simuladas
#spatialSimulatedDatabaseDestination = banco de pontos de destino das viagens simuladas
#publico = se TRUE realiza os processos para o conjunto de viagens de modo público
#time = se TRUE realiza o controle de viagens que ocorreram em um intervalo de duas horas para cima e para baixo

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
  
  #calculo das médias de tempo viagem simuladas
  means <-  means_buffered_trips(bufferId = associatedTrips,
                                 simulatedDatabase = simulatedDatabaseOrigin,
                                 timeColumn = ifelse(publico == T,'temppublico','temppriv'))
  
  #calculo da diferença entre médias de tempo de viagens e tempos de viagem da pesquisa OD
  aux <- difference_travel_time_buffer_simulated(ODDatabaseOrigin, means)
  #calculo da razão entre médias de tempo de viagens e tempos de viagem da pesquisa OD
  aux2 <- ratio_travel_time_buffer_simulated(ODDatabaseOrigin, means)
  
  #junçao das duas medidas  
  aux$ratio <- aux2$ratio
  
  
  #cálculo de média de distância das viagens simuladas associadas as viagens OD
  means <- means_buffered_trips(bufferId = associatedTrips,
                                simulatedDatabase =  simulatedDatabaseOrigin,
                                timeColumn = ifelse(publico == T,'distpublico','distpriv'))
 
  #atribuição do dado ao banco
  aux$distSimulated <- means
  return(aux)
}
