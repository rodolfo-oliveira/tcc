#function that selects travels near a bufferzone from a point
#spatialPoint = ponto central do buffer
#spatialDatabase = banco de pontos para serem slecionados dentro do buffer
#buffersize = tamanho do buffer em torno do ponto em metros
#time = se TRUE realiza o controle de viagens que ocorreram em um intervalo de duas horas para cima e para baixo

pluck_simulated_trip <- function(spatialPoint, 
                                 spatialDatabase, 
                                 bufferSize = 1000, 
                                 time = F){
  require(raster)
  require(dplyr)
  require(sp)
  
  if(time == T){
    return(spatialDatabase[which(is.na(over(x = spatialDatabase,y = buffer(spatialPoint, width = bufferSize))) == FALSE & 
                            ((spatialDatabase@data$Hour_cut >= spatialPoint@data$H_SAIDA-2) & (spatialDatabase@data$Hour_cut <= spatialPoint@data$H_SAIDA+2))),])
  }
  return(spatialDatabase[which(is.na(over(x = spatialDatabase,y = buffer(spatialPoint, width = bufferSize))) == FALSE),])
}
