#funtion to calculate means of buffered travel times of simulated data

means_buffered_trips <- function(bufferId, simulatedDatabase, timeColumn){
  
  means <- c()
  
  for(i in 1:length(bufferId[[1]])){
    means <- c(means, mean(simulatedDatabase[simulatedDatabase$ID %in% bufferId[[1]][[i]],]@data[,timeColumn]))  
    print(paste0(round(100*i/length(bufferId[[1]]),digits = 2),"% - calculando média"))
  }
  return(means)
}
