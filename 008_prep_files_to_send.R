#preparando arquivos para exportação
library(rgdal)

listaGeral <- list.files( pattern = 'masterDatabase.+')


filtros <- c('Origem\\.','Destino\\.','OrigemTimeRes','DestinoTimeRes')


for(i in 1:length(filtros)){
  lista <- listaGeral[stringr::str_detect(listaGeral, filtros[i])]
  
  names <- stringr::str_replace(lista,pattern = 'masterDatabase', replacement = 'pontos_') 
  names <- stringr::str_replace(names,pattern = 'TimeRes', replacement = '_restricao_tempo') 
  
  
  file.copy( from = lista, to = paste0("toExport/",names),overwrite = T)
  
  mapa <- readOGR(paste0("toExport/",names[stringr::str_detect(names, '\\.shp')]))
  
  mapa$DURACAOOD <- (mapa$DURACAO + mapa$ANDA_D + mapa$ANDA_O)*60
  
  mapa@data <- mapa@data[,c("diffrnc","ZONA","PONTO_BR","RENDA_FA","GRAU_INS","CD_ATIVI","MOTIVO_D","DURACAOOD","tipo",'smltdTm')]
  
  mapa <- mapa[is.na(mapa@data$diffrnc) != T,]
  writeOGR(mapa,
           dsn = paste0("toExport/",names[stringr::str_detect(names, '\\.shp')]),
           layer = names[stringr::str_detect(names, '\\.shp')],
           driver = 'ESRI Shapefile',
           delete_dsn = T,
           overwrite_layer = T,
           encoding = 'UTF-8')
}
