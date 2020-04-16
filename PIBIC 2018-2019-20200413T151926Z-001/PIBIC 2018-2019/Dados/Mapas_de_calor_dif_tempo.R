setwd(dir ="~/Documentos/Programação/PIBIC 2018-2019/Dados/")

#pacotes ----------------------------------------------

library(tidyverse)
library(rgdal)
library(sp)
library(car)
library (raster)
library (rgeos)
library(GWmodel)
library(spatstat)
library(boot)
library(stringr)

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
viagensdif = read.csv2("Bancos/banco_comparacao_medias.csv")
viagenspublicas = dados[dados$mode == "traffic" | dados$mode == "traffic2",]
viagensprivadas = dados[dados$mode == "private" | dados$mode == "private2",]

#variaveis do GWR
tipo = "gaussian"
bandwidth.gwr = 20
dado = "dif_tempo"


#GWR diferenca de tempo para distritos ---------------

#importando mapa de são paulo - distritos

mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/distritos_SP_SIRGAS_e_SAD69/",layer = "SAD69-96_SHP_distrito_polygon")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp <- spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel auxiliar
new_map <- mapsp
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V2[i]=i
  new_map$V2[i]=i
}

setwd(dir ="~/Documentos/Programação/PIBIC 2018-2019/Dados/GWR/")

for (i in 1:length(mapsp@data$ds_codigo)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ds_codigo))), digits = 3), "%",sep = ""))
  oriaux=over(spatialorigins,mapsp[mapsp$V2==i,])
  oriaux=cbind(coordsor$ID, oriaux)
  oriaux=oriaux[is.na(oriaux$V2)==FALSE,]
  aux = viagensdif[viagensdif$ID %in% oriaux$`coordsor$ID`,]
  spatialaux <- spatialdestinations[spatialdestinations$ID %in% aux$ID,]
  spatialaux@data <- merge(spatialaux@data, aux, by = "ID")
  
  #grade para o calculo do GWR
  grd <- SpatialGrid(GridTopology(c(-46.82625,-24.00813),c(0.004,0.004),c(120,165)))
  #matriz de vizinhanca entre a grade e os pontos de destino
  DM <- gw.dist(dp.locat=coordinates(spatialaux),rp.locat=coordinates(grd))
  #GWR "Univariado"  
  gwr.res <- gwr.basic(tempodif~tempodif,
                       data=spatialaux,
                       regression.points=grd,
                       adaptive = TRUE,
                       bw=bandwidth.gwr,
                       #bw=10000, # Fixed bandwidth
                       dMat=DM,
                       kernel=tipo)
  
 
  #montagem do titulo 
  distrito <-  str_to_title(mapsp$ds_nome[i])
  subtitulo <- paste("Viagens partindo de", distrito, sep = " ")
  tema <- "Diferença de Tempos"
  titulo <- paste("Regressão GWR Univariada", tema, subtitulo, sep = " - ")
  
  #gravacao do mapa
  #jpeg(paste("GWR_Origem_Diferença_Tempo_escala_uniforme_", tipo,"_",distrito,".jpg", sep = ""), width = 700, height = 500)
  jpeg(paste("GWR_Origin_Time_Diference_scale_uniform_", tipo,"_",distrito,".jpg", sep = ""), width = 700, height = 500)
  layout(mat = 1,widths = 1.4, heights = 2)
  plot(gwr.res$SDF[mapsp,], col = rev(heat.colors(50)), zlim = c(0,12000))
  #title(main = titulo)
  title(main = "Diference in travel times - District of Jabaquara")
  #legend(-46.25,-23.85, title = "Legenda", inset = 0.05, legend=c("Endereços de Origem", "Endereços de Destino"),
  #       fill = c("blueviolet", "darkgreen"), text.width = 0.15, text.font = 2, cex = 0.6,
  #       xjust = 0.5, yjust = 0.5)
  legend(-46.25,-23.85, title = "Legend", inset = 0.05, legend=c("Origins coordinates", "Destinations coordinates"),
         fill = c("blueviolet", "darkgreen"), text.width = 0.15, text.font = 2, cex = 0.6,
         xjust = 0.5, yjust = 0.5)
  
  plot(mapsp,add=TRUE)
  plot(spatialorigins[spatialorigins$ID %in% oriaux$`coordsor$ID`,],add=TRUE,pch=16,col='blueviolet',cex=0.5)
  plot(spatialdestinations[spatialdestinations$ID %in% oriaux$`coordsor$ID`,],add=TRUE,pch=16,col='darkgreen',cex=0.4)
  dev.off()
}



#GWR diferenca de tempo para areas de ponderacao ---------------

#importando mapa de são paulo - areas de ponderacao

mapsp <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/mapas base/LAYER_AREA_PONDERACAO_2010/",layer = "DEINFO_AREA_PONDERACAO_2010")
mapsp@proj4string <- CRS("+init=epsg:5533")
mapsp <- spTransform(mapsp, CRS("+init=epsg:4326"))
#criando variavel auxiliar
new_map <- mapsp
#inicializando codigo para o acompanhamento dos poligonos do shapefile
for(i in 1:length(mapsp[[2]])){
  mapsp$V2[i]=i
  new_map$V2[i]=i
}

setwd(dir ="~/Documentos/Programação/PIBIC 2018-2019/Dados/GWR/")

for (i in 1:length(mapsp@data$ID)){
  #selecionando as viagens que saem da origem i
  print(paste(round(100*(i/(length(mapsp@data$ID))), digits = 3), "%",sep = ""))
  oriaux=over(spatialorigins,mapsp[mapsp$V2==i,])
  oriaux=cbind(coordsor$ID, oriaux)
  oriaux=oriaux[is.na(oriaux$V2)==FALSE,]
  aux = viagensdif[viagensdif$ID %in% oriaux$`coordsor$ID`,]
  spatialaux <- spatialdestinations[spatialdestinations$ID %in% aux$ID,]
  spatialaux@data <- merge(spatialaux@data, aux, by = "ID")
  
  #grade para o calculo do GWR
  grd <- SpatialGrid(GridTopology(c(-46.82625,-24.00813),c(0.004,0.004),c(120,165)))
  #matriz de vizinhanca entre a grade e os pontos de destino
  DM <- gw.dist(dp.locat=coordinates(spatialaux),rp.locat=coordinates(grd))
  #GWR "Univariado"  
  gwr.res <- gwr.basic(tempodif~tempodif,
                       data=spatialaux,
                       regression.points=grd,
                       adaptive = TRUE,
                       bw=bandwidth.gwr,
                       #bw=10000, # Fixed bandwidth
                       dMat=DM,
                       kernel=tipo)
  
  
  #montagem do titulo 
  distrito <-  str_to_title(mapsp$ID[i])
  subtitulo <- paste("Viagens partindo de", distrito, sep = " ")
  tema <- "Diferença de Tempos"
  titulo <- paste("Regressão GWR Univariada", tema, subtitulo, sep = " - ")
  
  #gravacao do mapa
  jpeg(paste("areas de ponderação/GWR_Origem_Diferença_Tempo_escala_uniforme_", tipo,"_",distrito,".jpg", sep = ""), width = 700, height = 500)
  #jpeg(paste("areas de ponderação/GWR_Origin_Time_Diference_scale_uniform_", tipo,"_",distrito,".jpg", sep = ""), width = 700, height = 500)
  layout(mat = 1,widths = 1.4, heights = 2)
  plot(gwr.res$SDF[mapsp,], col = rev(heat.colors(50)), zlim = c(0,12000))
  title(main = titulo)
  #title(main = "Diference in travel times - District of Jabaquara")
  legend(-46.25,-23.85, title = "Legenda", inset = 0.05, legend=c("Endereços de Origem", "Endereços de Destino"),
         fill = c("blueviolet", "darkgreen"), text.width = 0.15, text.font = 2, cex = 0.6,
         xjust = 0.5, yjust = 0.5)
  #legend(-46.25,-23.85, title = "Legend", inset = 0.05, legend=c("Origins coordinates", "Destinations coordinates"),
  #       fill = c("blueviolet", "darkgreen"), text.width = 0.15, text.font = 2, cex = 0.6,
  #       xjust = 0.5, yjust = 0.5)
  
  plot(mapsp,add=TRUE)
  plot(spatialorigins[spatialorigins$ID %in% oriaux$`coordsor$ID`,],add=TRUE,pch=16,col='blueviolet',cex=0.5)
  plot(spatialdestinations[spatialdestinations$ID %in% oriaux$`coordsor$ID`,],add=TRUE,pch=16,col='darkgreen',cex=0.4)
  dev.off()
}




