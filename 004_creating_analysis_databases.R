#diagnostics on buffering

#pacote de funções para montar os buffers das viagens
source('503_buffering_functions.R')

#pacote de funções de analises
source('504_analysis_functions.R')


#comparing databases and mounting analyisis database
#a comparação das feitas a partir de diferentes premissas - com e sem restrição de tempo, na origem e no destino

#no time restriction----
#origem

#inicializando indicador de nome e do banco de dados
databaseNames <- c('privado','publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])

  #composição do nome da variável a ser passada para a função
  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_origem')))
  auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  #cálculo das medidas de diferença e razão de tempos de viagem entre viagens simuladas e viagens OD
  aux <- MF_database_comparison(ODDatabaseOrigin = eval(as.name(paste0('database_',databaseNames[i],'_origem'))),
                                ODDatabaseDestination = eval(as.name(paste0('database_',databaseNames[i],'_destino'))),
                                simulatedDatabaseOrigin = simulated_database_origem,
                                simulatedDatabaseDestination = simulated_database_destino,
                                publico = ifelse(databaseNames[i] == 'publico', T, F),
                                time = F)
  
  auxDatabase@data$difference <- aux$difference
  auxDatabase@data$ratio <- aux$ratio
  auxDatabase@data$simulatedDist <- aux$distSimulated
  auxDatabase@data$simulatedTime <- aux$simulatedMeasure
  auxDatabase@data$tipo <- databaseNames[i]
  
  if(length(masterDatabase)==0){
    masterDatabase <- auxDatabase
  }else{
    masterDatabase <- raster::bind(masterDatabase, auxDatabase)
  }
}
#gravando o resultado em shapefile
rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseOrigem.shp',
                layer = 'masterDatabaseOrigem', 
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)


#destino

#inicializando indicador de nome e do banco de dados
#databaseNames <- c('escolar','fretados','privado', 'privado_moto', 'publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])
  
  #composição do nome da variável a ser passada para a função
  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_destino')))
  auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  #cálculo das medidas de diferença e razão de tempos de viagem entre viagens simuladas e viagens OD
  aux <- MF_database_comparison(eval(as.name(paste0('database_',databaseNames[i],'_origem'))),
                                eval(as.name(paste0('database_',databaseNames[i],'_destino'))),
                                simulated_database_origem,
                                simulated_database_destino,
                                publico = ifelse(databaseNames[i] == 'publico', T, F),
                                time = F)
  
  auxDatabase@data$difference <- aux$difference
  auxDatabase@data$ratio <- aux$ratio
  auxDatabase@data$simulatedDist <- aux$distSimulated
  auxDatabase@data$simulatedTime <- aux$simulatedMeasure
  auxDatabase@data$tipo <- databaseNames[i]
  
  if(length(masterDatabase)==0){
    masterDatabase <- auxDatabase
  }else{
    masterDatabase <- raster::bind(masterDatabase, auxDatabase)
  }
}
#gravando o resultado em shapefile
rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseDestino.shp',
                layer = 'masterDatabaseDestino',
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)


#with time restriction----
#origem

#inicializando indicador de nome e do banco de dados
#databaseNames <- c('escolar','fretados','privado', 'privado_moto', 'publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])
  
  #composição do nome da variável a ser passada para a função
  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_origem')))
  auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  #cálculo das medidas de diferença e razão de tempos de viagem entre viagens simuladas e viagens OD
  aux <- MF_database_comparison(ODDatabaseOrigin = eval(as.name(paste0('database_',databaseNames[i],'_origem'))),
                                ODDatabaseDestination = eval(as.name(paste0('database_',databaseNames[i],'_destino'))),
                                simulatedDatabaseOrigin = simulated_database_origem,
                                simulatedDatabaseDestination = simulated_database_destino,
                                publico = ifelse(databaseNames[i] == 'publico', T, F),
                                time = T)
  
  auxDatabase@data$difference <- aux$difference
  auxDatabase@data$ratio <- aux$ratio
  auxDatabase@data$simulatedDist <- aux$distSimulated
  auxDatabase@data$simulatedTime <- aux$simulatedMeasure
  auxDatabase@data$tipo <- databaseNames[i]
  
  if(length(masterDatabase)==0){
    masterDatabase <- auxDatabase
  }else{
    masterDatabase <- raster::bind(masterDatabase, auxDatabase)
  }
}

#gravando o resultado em shapefile
rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseOrigemTimeRes.shp', 
                layer = 'masterDatabaseOrigemTimeRes',
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)


#destino

#inicializando indicador de nome e do banco de dados
#databaseNames <- c('escolar','fretados','privado', 'privado_moto', 'publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])
  
  #composição do nome da variável a ser passada para a função
  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_destino')))
  auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  #cálculo das medidas de diferença e razão de tempos de viagem entre viagens simuladas e viagens OD
  aux <- MF_database_comparison(eval(as.name(paste0('database_',databaseNames[i],'_origem'))),
                                eval(as.name(paste0('database_',databaseNames[i],'_destino'))),
                                simulated_database_origem,
                                simulated_database_destino,
                                publico = ifelse(databaseNames[i] == 'publico', T, F),
                                time = T)
  
  auxDatabase@data$difference <- aux$difference
  auxDatabase@data$ratio <- aux$ratio
  auxDatabase@data$simulatedDist <- aux$distSimulated
  auxDatabase@data$simulatedTime <- aux$simulatedMeasure
  auxDatabase@data$tipo <- databaseNames[i]
  
  if(length(masterDatabase)==0){
    masterDatabase <- auxDatabase
  }else{
    masterDatabase <- raster::bind(masterDatabase, auxDatabase)
  }
}

#gravando o resultado em shapefile
rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseDestinoTimeRes.shp', 
                layer = 'masterDatabaseDestinoTimeRes', 
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)
