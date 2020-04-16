#inicialização -------------------------------

library(rgdal)
library(sp)
library(ggplot2)
library(ggmap)
library(maps)
library(RgoogleMaps)
library(mapdata)
library(googleway)
library(dplyr)

setwd(dir = "Documentos/Programação/Programa de tempos de viagem/Randomização de endereços/SIRGAS_SHP_distrito/")

#importanção -------------------------------------------------

Mapasaopaulo<-readOGR(dsn = "SIRGAS_SHP_distrito_polygon.shp",layer = "SIRGAS_SHP_distrito_polygon")

#Aleatorização de coordenadas ------------------------------------

#numero de coordenadas por polígono
ncoord <- 9

#inicialização do vetor de amostragem
sample1 <- spsample(Mapasaopaulo[Mapasaopaulo$ds_codigo==1,],ncoord,type="random")
sample1$zona = seq(from=1, to =1, length.out = ncoord)

#amostragem para todos os distritos encaixadas no mesmo vetor
for (i in c(2:96)){
  sample_aux = spsample(Mapasaopaulo[Mapasaopaulo$ds_codigo==i,],ncoord, type="random")
  sample_aux$zona = seq(from=i, to =i, length.out = ncoord)
  sample1 = rbind(sample1, sample_aux)
}


#coerção do vetor de endereços em dataframe e do shapefile em formato para o ggplot2

sample1 = as.data.frame(sample1)

mapdat <- map_data(Mapasaopaulo)

#plotagem para confirmação
ggplot() +  geom_polygon(data = mapdat, aes(x=long,y=lat, group = group)) + 
            geom_point(data=sample1, aes(x=x, y=y)) +
            coord_fixed(1.1) 
            
#transformação das coordendas de UTM para lLatitude e Longitude
refdata = SpatialPoints(coords = sample1[2:3],proj4string = CRS("+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"))            

refdata = spTransform(refdata, CRSobj = "+proj=longlat")

#Manter uma identificação da zona de referência de cada endereço no objeto para manipulação futura
refdata$zona = sample1$zona

#manipulação das latitudes e longitudes para um formato aceito pela função google_distance
for (i in 1:length(refdata)){
  aux <- paste(refdata@coords[i,2], refdata@coords[i,1],sep = ",")
  refdata$coordenadas[i] = aux
}

#Cálculo do tempo e distância de deslocamento entre os endereços (laço a implementar)
auxiliar=NA
auxiliar=google_distance(refdata[refdata$zona==80,]$coordenadas, refdata[refdata$zona==40,]$coordenadas,mode = c("transit"),transit_mode = c("bus"), key = "AIzaSyABtm_R-SAFz-U5yoy5efo2dG2eWF89l3I")


#montagem do banco de dados das viagens simuladas
viagens <-data.frame()
viag_aux <- c()

for(i in 1:length(refdata[refdata$zona==80,])){
    for(k in 1:length(refdata[refdata$zona==40,])){
        
        viag_aux$zona_orig <- 80
        viag_aux$zona_dest <- 40
        viag_aux$coord_orig <- refdata[refdata$zona==80,]$coordenadas[i]
        viag_aux$coord_dest <- refdata[refdata$zona==40,]$coordenadas[k]
        viag_aux$dist <- auxiliar$rows$elements[[i]]$distance$value[k]
        viag_aux$tempo <- auxiliar$rows$elements[[i]]$duration$value[k]
        viag_aux <- data.frame(viag_aux)
        print(viag_aux)
        viagens = rbind( viag_aux, viagens)
    }
}

#http://www.dpi.inpe.br/calcula/
