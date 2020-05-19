#functions to return diferences and ratios of tavel times in OD database and mean simulated travel times

difference_travel_time_buffer_simulated <- function(ODDatabase, meanSimulatedTravelTimes){
  
  return(data.frame(ODmeasure = 60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")])),
                    simulatedMeasure = meanSimulatedTravelTimes,
                    difference = 60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")]))- meanSimulatedTravelTimes))
  
}

ratio_travel_time_buffer_simulated <- function(ODDatabase, meanSimulatedTravelTimes){
  
  return(data.frame(ODmeasure = 60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")])),
                    simulatedMeasure = meanSimulatedTravelTimes,
                    ratio = 60*(rowSums(ODDatabase@data[,c("DURACAO","ANDA_O","ANDA_D")]))/meanSimulatedTravelTimes - 1))
  
}