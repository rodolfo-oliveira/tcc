#functions to return diferences and ratios of tavel times in OD database and mean simulated travel times

#função que calcula a diferença entre tempos de viagens da pesquisa OD e tempo médio de viagens simuladas dentro do buffer em torno das viagem OD
difference_travel_time_buffer_simulated <- function(ODDatabase, meanSimulatedTravelTimes){
  
  return(data.frame(ODmeasure = 60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")])),
                    simulatedMeasure = meanSimulatedTravelTimes,
                    difference = meanSimulatedTravelTimes - 60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")]))))
  
}

#função que calcula a razão entre tempos de viagens da pesquisa OD e tempo médio de viagens simuladas dentro do buffer em torno das viagem OD
ratio_travel_time_buffer_simulated <- function(ODDatabase, meanSimulatedTravelTimes){
  
  return(data.frame(ODmeasure = 60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")])),
                    simulatedMeasure = meanSimulatedTravelTimes,
                    ratio = meanSimulatedTravelTimes/(60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")])))))
  
}