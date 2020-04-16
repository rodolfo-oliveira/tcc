setwd(dir ="~/Documentos/Programação/PIBIC 2018-2019/Dados/")

#pacotes ----------------------------------------------

library(tidyverse)
library(rgdal)
library(sp)
library(car)
library (raster)
library (rgeos)

#Tratamento dos dados --------------------------------------
dados = read.csv(file = "Bancos/Cloud_SQL_Export_2019-06-10 (00_06_00)alt", header = FALSE, col.names = c("ID","Data","Hour","Day", "OrCoords","OrAdress","DestCoords", "DestAdress", "duration","distance", "fare", "mode"))
dados = dados[dados$ID>400,]

#separacao dos pares latitudelongitude
dados %>%
  separate(col=OrCoords,into = c("OrLat", "OrLong"), sep = ",", remove=TRUE,fill = "right", convert = TRUE) %>%
  separate(col=DestCoords,into = c("DestLat", "DestLong"), sep = ",", remove=TRUE,fill = "right", convert = TRUE) ->dados

#Acertando o fuso, o horario de verão e horarios que passam do registro da meia noite
dados %>%
  mutate(Hour_cut=case_when(as.Date(dados$Data, format = "%d-%m-%Y") < as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 2,
                                                                       as.Date(dados$Data, format = "%d-%m-%Y") > as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 3)) %>%
  mutate(minutecut=as.numeric(substring(as.character(Hour),4,5))) %>%
  mutate(Hour_cut = ifelse(Hour_cut<0, Hour_cut+24,Hour_cut)) %>%
  mutate(Day = ifelse(substring(as.character(Hour),1,2) == "00", 
                      case_when(Day == "Tuesday" ~ "Monday",
                                Day == "Wednesday" ~ "Tuesday",
                                Day == "Thursday" ~ "Wednesday",
                                Day == "Friday" ~ "Thursday",
                                Day == "Saturday" ~ "Friday"), 
                      as.character(Day)))->dados


#Espacializacao dos dados do banco - criacao de um spatialdataframe para os enderecos-------------------


#de factor para numeric 
dados$OrLat = as.numeric(dados$OrLat) 
dados$OrLong = as.numeric(dados$OrLong)
dados$DestLong = as.numeric(dados$DestLong)
dados$DestLat = as.numeric(dados$DestLat)

#importando mapa de são paulo
mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/distritos_SP_SIRGAS_e_SAD69/",layer = "SAD69-96_SHP_distrito_polygon")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))

#criando os vetores espaciais - pontos - para a localizacao dos enderecos
coordsor = dados[c("ID","OrLong","OrLat")]
coordsor = coordsor[is.na(coordsor$OrLong) == 0,]
spatialorigins = SpatialPointsDataFrame(coordsor[2:3],coordsor[1], proj4string = CRS("+init=epsg:4326"))

coordsdest = dados[c("ID","DestLong","DestLat")]
coordsdest = coordsdest[is.na(coordsdest$DestLong) == 0,]
spatialdestinations = SpatialPointsDataFrame(coordsdest[2:3], coordsdest[1], proj4string = CRS("+init=epsg:4326"))

# carregando banco para comparacao de transporte publico e privado
viagenspares = read.csv2("Bancos/banco_viagens_pareadas.csv")

#Shapefiles para analise - origens e destinos ---------------
#versão regioes da SPTrans-----------

#importando mapa de são paulo - regioes SPTrans

mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/regioesSPTrans/",layer = "regioesSPtrans")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp <- spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map <- mapsp
mapsp[[2]] <- c(0)
new_map[[2]] <- c(0)
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V2[i]=i
  new_map$V2[i]=i
}

for (i in 1:length(mapsp@data$ds_codigo)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ds_codigo))), digits = 3), "%",sep = ""))
  new_map[[length(mapsp@data)-1+2*i]] = c(0)
  new_map[[length(mapsp@data)+2*i]] = c(0)
  names(new_map)[[length(mapsp@data)-1+2*i]] <- as.character(mapsp[mapsp$V2==i,]$ds_codigo)
  names(new_map)[[length(mapsp@data) + 2*i]] <- as.character(paste(mapsp[mapsp$V2==i,]$ds_codigo, ".cont", sep = ""))
  oriaux=over(spatialorigins,mapsp[mapsp$V2==i,])
  oriaux=cbind(coordsor$ID, oriaux)
  oriaux=oriaux[is.na(oriaux$V2)==FALSE,]
  for (j in 1:length(mapsp@data$ds_codigo)){
    #selecionando as viagens que saem da origem i e chegam no destino j
    print(paste(round(100*(j/(length(mapsp@data$ds_codigo))), digits = 1), "%",sep = ""))
    destaux=over(spatialdestinations,mapsp[mapsp$V2==j,])
    destaux=cbind(coordsor$ID, destaux)
    destaux=destaux[is.na(destaux$V2)==FALSE,]
    ides = c(intersect(oriaux$`coordsor$ID`, destaux$`coordsor$ID`))
    ides = intersect(ides, viagenspares$ID)
    aux = viagenspares[viagenspares$ID %in% c(ides),]
    aux=aux[aux$temprelativo < 10000,]
    aux=aux[is.na(aux$ID)==FALSE,]
    soma = sum(aux$temprelativo)
    #associando a media das viagens ao poligono do shapefile
    new_map[[length(mapsp@data)-1+2*i]][j] = as.numeric((soma/length(ides)))
    new_map[[length(mapsp@data)+2*i]][j] = NROW(aux$ID)    
  }
}

writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_OD_tempo_relativo_regioes_SPTrans", driver = "ESRI Shapefile")


#versão distritos -------------

#importando mapa de são paulo - distritos
mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/distritos_SP_SIRGAS_e_SAD69/",layer = "SAD69-96_SHP_distrito_polygon")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp



#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V2[i]=i
  new_map$V2[i]=i
}


for (i in 1:length(mapsp@data$ds_codigo)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ds_codigo))), digits = 3), "%",sep = ""))
  new_map[[1+2*i]] = c(0)
  new_map[[2+2*i]] =c(0)
  names(new_map)[[1+2*i]] <- as.character(mapsp[mapsp$V2==i,]$ds_nome)
  names(new_map)[[2+2*i]] <- as.character(paste(mapsp[mapsp$V2==i,]$ds_nome), "count", sep = "_")
  oriaux=over(spatialorigins,mapsp[mapsp$V2==i,])
  oriaux=cbind(coordsor$ID, oriaux)
  oriaux=oriaux[is.na(oriaux$V2)==FALSE,]
  for (j in 1:length(mapsp@data$ds_codigo)){
    #selecionando as viagens que saem da origem i e chegam no destino j
    print(paste(round(100*(j/(length(mapsp@data$ds_codigo))), digits = 1), "%",sep = ""))
    destaux=over(spatialdestinations,mapsp[mapsp$V2==j,])
    destaux=cbind(coordsor$ID, destaux)
    destaux=destaux[is.na(destaux$V2)==FALSE,]
    ides = c(intersect(oriaux$`coordsor$ID`, destaux$`coordsor$ID`))
    ides = intersect(ides, viagenspares$ID)
    aux = viagenspares[viagenspares$ID %in% c(ides),]
    aux=aux[aux$temprelativo < 10000,]
    aux=aux[is.na(aux$ID)==FALSE,]
    soma = sum(aux$temprelativo)
    #associando a media das viagens ao poligono do shapefile
    new_map[[1+2*i]][j] =  as.numeric((soma/length(ides)))
    new_map[[2+2*i]][j] = NROW(aux$ID)
  }
}

writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_OD_tempo_relativo_distritos", driver = "ESRI Shapefile")


#versão areas de ponderacao -------------

#importando mapa de são paulo - distritos
mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/LAYER_AREA_PONDERACAO_2010/",layer = "DEINFO_AREA_PONDERACAO_2010")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp

#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V2[i]=i
  new_map$V2[i]=i
}

for (i in 1:length(mapsp@data$ID)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ID))), digits = 3), "%",sep = ""))
  new_map[[3+2*i]] = c(0)
  new_map[[4+2*i]] = c(0)
  names(new_map)[[3+2*i]] <- as.character(mapsp[mapsp$V2==i,]$ID)
  names(new_map)[[4+2*i]] <- as.character(paste(mapsp[mapsp$V2==i,]$ds_nome), "count", sep = "_")
  oriaux=over(spatialorigins,mapsp[mapsp$V2==i,])
  oriaux=cbind(coordsor$V2, oriaux)
  oriaux=oriaux[is.na(oriaux$V2)==FALSE,]
  for (j in 1:length(mapsp@data$ID)){
    #selecionando as viagens que saem da origem i e chegam no destino j
    print(paste(round(100*(j/(length(mapsp@data$ID))), digits = 1), "%",sep = ""))
    destaux=over(spatialdestinations,mapsp[mapsp$V2==j,])
    destaux=cbind(coordsor$ID, destaux)
    destaux=destaux[is.na(destaux$V2)==FALSE,]
    ides = c(intersect(oriaux$`coordsor$ID`, destaux$`coordsor$ID`))
    ides = intersect(ides, viagenspares$ID)
    aux = viagenspares[viagenspares$ID %in% c(ides),]
    aux=aux[aux$temprelativo < 10000,]
    aux=aux[is.na(aux$ID)==FALSE,]
    soma = sum(aux$temprelativo)
    #associando a media das viagens ao poligono do shapefile
    new_map[[3+2*i]][j] =  as.numeric((soma/length(ides)))
    new_map[[4+2*i]][j] = NROW(aux$ID)
  }
}

writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_OD_tempo_relativo_areas_de_ponderacao", driver = "ESRI Shapefile")



#Shapefiles para analise - somente origem  ---------------
#versão regioes da SPTrans-----------

#importando mapa de são paulo - regioes SPTrans

mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/regioesSPTrans/",layer = "regioesSPtrans")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp
mapsp[[2]] = c(0)
new_map[[2]] = c(0)
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V2[i]=i
  new_map$V2[i]=i
}
new_map[[3]] = c(0)
new_map[[4]] = c(0)



for (i in 1:length(mapsp@data$ds_codigo)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ds_codigo))), digits = 3), "%",sep = ""))
  oriaux=over(spatialorigins,mapsp[mapsp$V2==i,])
  oriaux=cbind(coordsor$ID, oriaux)
  oriaux=oriaux[is.na(oriaux$V2)==FALSE,]
  ides = c(intersect(oriaux$`coordsor$ID`, viagenspares$ID))
  aux = viagenspares[viagenspares$ID %in% c(ides),]
  aux=aux[aux$temprelativo < 10000,]
  aux=aux[is.na(aux$ID)==FALSE,]
  soma = sum(aux$temprelativo)
  new_map[[3]][k] = ifelse (new_map@data$V2[k]==i,  ifelse(length(ides)>0, as.numeric((soma/length(ides))), ""),as.numeric(new_map[[3]][k]))
  }
}

writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_Origem_tempo_relativo_regioes_SPTrans", driver = "ESRI Shapefile")


#versão distritos -------------

#importando mapa de são paulo - distritos
mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/distritos_SP_SIRGAS_e_SAD69/",layer = "SAD69-96_SHP_distrito_polygon")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp
mapsp[[3]] = c(0)
new_map[[3]] = c(0)
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V3[i]=i
  new_map$V3[i]=i
}
new_map[[4]] = c(0)

for (i in 1:length(mapsp@data$ds_codigo)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ds_codigo))), digits = 3), "%",sep = ""))
  oriaux=over(spatialorigins,mapsp[mapsp$V3==i,])
  oriaux=cbind(coordsor$ID, oriaux)
  oriaux=oriaux[is.na(oriaux$ds_codigo)==FALSE,]
  ides = c(intersect(oriaux$`coordsor$ID`, viagenspares$ID))
  aux = viagenspares[viagenspares$ID %in% c(ides),]
  aux=aux[aux$temprelativo < 10000,]
  aux=aux[is.na(aux$ID)==FALSE,]
  soma = sum(aux$temprelativo)
  
  for (k in 1:length(mapsp@data$ds_codigo)){
    new_map[[4]][k] = ifelse (new_map@data$V3[k]==i,  ifelse(length(ides)>0, as.numeric((soma/length(ides))), ""),as.numeric(new_map[[4]][k]))
  }
}


writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_Origem_tempo_relativo_distritos", driver = "ESRI Shapefile")

#versão areas de ponderacao-------------

#importando mapa de são paulo - area de ponderacao
mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/LAYER_AREA_PONDERACAO_2010/",layer = "DEINFO_AREA_PONDERACAO_2010")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp
mapsp[[5]] = c(0)
new_map[[5]] = c(0)
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V5[i]=i
  new_map$V5[i]=i
}
new_map[[6]] = c(0)

for (i in 1:length(mapsp@data$ID)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ID))), digits = 3), "%",sep = ""))
  oriaux=over(spatialorigins,mapsp[mapsp$V5==i,])
  oriaux=cbind(coordsor$ID, oriaux)
  oriaux=oriaux[is.na(oriaux$ID)==FALSE,]
  ides = c(intersect(oriaux$`coordsor$ID`, viagenspares$ID))
  aux = viagenspares[viagenspares$ID %in% c(ides),]
  aux=aux[aux$temprelativo < 10000,]
  aux=aux[is.na(aux$ID)==FALSE,]
  soma = sum(aux$temprelativo)
  
  for (k in 1:length(mapsp@data$ID)){
    new_map[[6]][k] = ifelse (new_map@data$V5[k]==i,  ifelse(length(ides)>0, as.numeric((soma/length(ides))), ""),as.numeric(new_map[[6]][k]))
  }
}


writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_Origem_tempo_relativo_areas_de_ponderacao", driver = "ESRI Shapefile")

#Shapefiles para analise - somente destino  ---------------
#versão regioes da SPTrans-----------

#importando mapa de são paulo - regioes SPTrans

mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/regioesSPTrans/",layer = "regioesSPtrans")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp
mapsp[[2]] = c(0)
new_map[[2]] = c(0)
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V2[i]=i
  new_map$V2[i]=i
}
new_map[[3]] = c(0)


for (i in 1:length(mapsp@data$ds_codigo)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ds_codigo))), digits = 3), "%",sep = ""))
  destaux=over(spatialdestinations,mapsp[mapsp$V2==i,])
  destaux=cbind(coordsdest$ID, destaux)
  destaux=destaux[is.na(destaux$ds_codigo)==FALSE,]
  ides = c(intersect(destaux$`coordsdest$ID`, viagenspares$ID))
  aux = viagenspares[viagenspares$ID %in% c(ides),]
  aux=aux[aux$temprelativo < 10000,]
  aux=aux[is.na(aux$ID)==FALSE,]
  soma = sum(aux$temprelativo)
  
  for (k in 1:length(mapsp@data$ds_codigo)){
    new_map[[3]][k] = ifelse (new_map@data$V2[k]==i,  ifelse(length(ides)>0, as.numeric((soma/length(ides))), ""),as.numeric(new_map[[3]][k]))
  }
}

writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_Destino_tempo_relativo_regioes_SPTrans", driver = "ESRI Shapefile")


#versão distritos -------------

#importando mapa de são paulo - distritos
mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/distritos_SP_SIRGAS_e_SAD69/",layer = "SAD69-96_SHP_distrito_polygon")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp
mapsp[[3]] = c(0)
new_map[[3]] = c(0)
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V3[i]=i
  new_map$V3[i]=i
}
new_map[[4]] = c(0)

for (i in 1:length(mapsp@data$ds_codigo)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ds_codigo))), digits = 3), "%",sep = ""))
  destaux=over(spatialdestinations,mapsp[mapsp$V3==i,])
  destaux=cbind(coordsdest$ID, destaux)
  destaux=destaux[is.na(destaux$ds_codigo)==FALSE,]
  ides = c(intersect(destaux$`coordsdest$ID`, viagenspares$ID))
  aux = viagenspares[viagenspares$ID %in% c(ides),]
  aux=aux[aux$temprelativo < 10000,]
  aux=aux[is.na(aux$ID)==FALSE,]
  soma = sum(aux$temprelativo)
  
  for (k in 1:length(mapsp@data$ds_codigo)){
    new_map[[4]][k] = ifelse (new_map@data$V3[k]==i,  ifelse(length(ides)>0, as.numeric((soma/length(ides))), ""),as.numeric(new_map[[4]][k]))
  }
}


writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_Destino_tempo_relativo_distritos", driver = "ESRI Shapefile")

#versão areas de ponderacao-------------

#importando mapa de são paulo - area de ponderacao
mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/LAYER_AREA_PONDERACAO_2010/",layer = "DEINFO_AREA_PONDERACAO_2010")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp = spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel para o novo shapefile
new_map = mapsp
mapsp[[5]] = c(0)
new_map[[5]] = c(0)
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V5[i]=i
  new_map$V5[i]=i
}
new_map[[6]] = c(0)

for (i in 1:length(mapsp@data$ID)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ID))), digits = 3), "%",sep = ""))
  destaux=over(spatialdestinations,mapsp[mapsp$V5==i,])
  destaux=cbind(coordsdest$ID, destaux)
  destaux=destaux[is.na(destaux$ID)==FALSE,]
  ides = c(intersect(destaux$`coordsdest$ID`, viagenspares$ID))
  aux = viagenspares[viagenspares$ID %in% c(ides),]
  aux=aux[aux$temprelativo < 10000,]
  aux=aux[is.na(aux$ID)==FALSE,]
  soma = sum(aux$temprelativo)
  
  for (k in 1:length(mapsp@data$ID)){
    new_map[[6]][k] = ifelse (new_map@data$V5[k]==i,  ifelse(length(ides)>0, as.numeric((soma/length(ides))), ""),as.numeric(new_map[[6]][k]))
  }
}


writeOGR(new_map,dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/", layer="mapa_Destino_tempo_relativo_areas_de_ponderacao", driver = "ESRI Shapefile")

