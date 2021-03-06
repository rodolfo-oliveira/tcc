#regionalizing results

library(rgdal)

#mapa das zonas da Pesquisa OD
mapa <- readOGR('OD 2017/Mapas/Shape/Zonas_2017_region.shp',
                encoding = "UTF-8",
                p4s = "+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs")

#filtrando zonas do município de São Paulo
mapa <- mapa[mapa@data$NumeroMuni==36,]

#nomes dos bancos de dados com resultados
DBNames <- list.files(pattern = 'master.+shp')


#loop para passar por todos bancos
for(j in DBNames){
  analysisDatabaseName <- j
  
  
  #leitura do banco de dados
  analysisDatabase <- readOGR(analysisDatabaseName)
  
  #Inicialização de flags
  destino <- F
  tRestraint <- F
  
  #flags ativadas a partir do nome dos arquivos de banco de dados
  if(stringr::str_detect(analysisDatabaseName, 'Destino') ==T ){
    destino <- T
    if(stringr::str_detect(analysisDatabaseName, "TimeRes") == T){
      tRestraint <- T
    }
  }else{
    destino <- F
    if(stringr::str_detect(analysisDatabaseName, "TimeRes") == T){
      tRestraint <- T
    }
  }
  
  varName <- 'dif'
  
  if(destino == T){
    varName <- paste0(varName, 'Dest')
  }else{
    varName <- paste0(varName, 'Ori')
  }
  
  if(tRestraint == T){
    varName <- paste0(varName,'TRes')
  }
  
  #criaçao das variveis de dados no shapefile
  mapa@data[,paste0(varName, 'Pub')] <- numeric()
  
  mapa@data[,paste0(varName, 'Priv')] <- numeric()
  
  #calculo das médias das medidas para as zonas da pesquisa OD
  for (i in 1:length(mapa)){
    
    sample <- over(x = analysisDatabase, y = mapa[i,])
    sample <- analysisDatabase[which(is.na(sample$NumeroZona)==F),]
    #TODO responsive to different types
    mapa@data[i,paste0(varName, 'Pub')] <- mean(sample@data$diffrnc[sample$tipo=='publico'], na.rm = T)
    mapa@data[i,paste0(varName, 'Priv')] <- mean(sample@data$diffrnc[sample$tipo=='privado'], na.rm = T)
    print(paste0(round(100*i/length(mapa),digits = 2),'%')) 
  }
}

#resultados salvos em shapefile
writeOGR(obj = mapa,
         dsn = 'ZonasODDados.shp',
         layer = 'ZonasODDados',
         driver = 'ESRI Shapefile',
         delete_dsn = T,
         overwrite_layer = T, encoding = 'UTF-8')
