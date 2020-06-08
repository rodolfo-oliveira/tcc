#diagnostics on buffering

source('503_buffering_functions.R')

source('504_analysis_functions.R')


#comparing databases and mounting analyisis database

#no time restriction----
#origem
databaseNames <- c('privado','publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])

  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_origem')))
  #auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  
  aux <- MF_database_comparison(ODDatabaseOrigin = eval(as.name(paste0('database_',databaseNames[i],'_origem'))),
                                ODDatabaseDestination = eval(as.name(paste0('database_',databaseNames[i],'_destino'))),
                                simulatedDatabaseOrigin = simulated_database_origem,
                                simulatedDatabaseDestination = simulated_database_destino,
                                publico = ifelse(databaseNames[i] == 'publico', T, F))
  
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

rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseOrigem.shp',
                layer = 'masterDatabaseOrigem', 
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)


#destino
#databaseNames <- c('escolar','fretados','privado', 'privado_moto', 'publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])
  
  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_destino')))
  auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  
  aux <- MF_database_comparison(eval(as.name(paste0('database_',databaseNames[i],'_origem'))),
                                eval(as.name(paste0('database_',databaseNames[i],'_destino'))),
                                simulated_database_origem,
                                simulated_database_destino,
                                publico = ifelse(databaseNames[i] == 'publico', T, F))
  
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

rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseDestino.shp',
                layer = 'masterDatabaseDestino',
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)


#with time restriction----
#origem
#databaseNames <- c('escolar','fretados','privado', 'privado_moto', 'publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])
  
  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_origem')))
  auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  
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


rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseOrigemTimeRes.shp', 
                layer = 'masterDatabaseOrigemTimeRes',
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)


#destino
#databaseNames <- c('escolar','fretados','privado', 'privado_moto', 'publico')
masterDatabase <- numeric()

for(i in 1:length(databaseNames)){
  print(databaseNames[i])
  
  auxDatabase <- eval(as.name(paste0('database_',databaseNames[i],'_destino')))
  auxDatabase@data<- auxDatabase@data[,c("CRITERIOBR","MODOPRIN","DURACAO","ANDA_O","ANDA_D","H_SAIDA")]
  
  
  
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

rgdal::writeOGR(obj = masterDatabase,
                dsn = 'masterDatabaseDestinoTimeRes.shp', 
                layer = 'masterDatabaseDestinoTimeRes', 
                driver = 'ESRI Shapefile',
                delete_dsn = T,
                overwrite_layer = T)
