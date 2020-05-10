#script para mapas LISA

library(rgdal)
library(spdep)
library(dplyr)
library(sp)
library(ggplot2)

plot_LISA <- function(
  OGRdsn,
  column,
  legenda = F,
  agregLab,
  linhasTransp = F,
  residual_map = F)
{
  #reference: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
  mapa <- readOGR(dsn = OGRdsn)
  
  if(residual_map == F){
    morantest <- moran.test(x = as.numeric(mapa@data[,column]),
                           listw = nb2listw(neighbours = poly2nb(mapa)),alternative = 'two.sided')
    
    morant <- localmoran(x = as.numeric(mapa@data[,column]),
                         listw = nb2listw(neighbours = poly2nb(mapa)), alternative = 'two.sided', p.adjust.method = 'holm')
    
    
    mapa$varI <- scale(mapa@data[,column])
    mapa$lag_varI <- lag.listw(nb2listw(neighbours = poly2nb(mapa)), mapa$varI)
    
  
    
    mapa$quad_sig <- NA
    mapa@data[(mapa$varI >= 0 & mapa$lag_varI >= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 1
    mapa@data[(mapa$varI <= 0 & mapa$lag_varI <= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 2
    mapa@data[(mapa$varI >= 0 & mapa$lag_varI <= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 3
    mapa@data[(mapa$varI >= 0 & mapa$lag_varI <= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 4
    mapa@data[(mapa$varI <= 0 & mapa$lag_varI >= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
    
    # Set the breaks for the thematic map classes
    breaks <- seq(1, 5, 1)
    
    # Set the corresponding labels for the thematic map classes
    labels <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significante")
    
    # see ?findInterval - This is necessary for making a map
    np <- findInterval(mapa$quad_sig, breaks)
    
    # Assign colors to each map class
    colors <- c("red", "blue", "lightpink", "skyblue2", "white")
    par(mar=c(0,0,2,0))
    plot(mapa, col = colors[np])  #colors[np] manually sets the color for each county
    title(agregLab, cex.main = 2)
    #corredor de onibus
    if(linhasTransp == T){
      
      mapaAux <- readOGR('PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/mapas base/SAD69-96_SHP_linhametro/SAD69-96_SHP_linhametro_line.shp')
      proj4string(mapaAux) <- CRS(as.character(rgdal::make_EPSG()[rgdal::make_EPSG()$code %in% 5533,]$prj4))
      mapaAux <- spTransform(mapaAux, proj4string(mapa))
      plot(mapaAux,col = 'purple', lwd = 3, add = TRUE)
      
      mapaAux <- readOGR('PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/mapas base/SIRGAS_SHP_linhatrem/SIRGAS_SHP_linhatrem_line.shp')
      proj4string(mapaAux) <- CRS(as.character(rgdal::make_EPSG()[rgdal::make_EPSG()$code %in% 31983,]$prj4))
      mapaAux <- spTransform(mapaAux, proj4string(mapa))
      plot(mapaAux,col = 'yellow', lwd = 3, add = TRUE)
      
      mapaAux <- readOGR('PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/mapas base/SAD69-96_SHP_corredoronibus/SAD69-96_SHP_corredoronibus.shp')
      proj4string(mapaAux) <- CRS(as.character(rgdal::make_EPSG()[rgdal::make_EPSG()$code %in% 5533,]$prj4))
      mapaAux <- spTransform(mapaAux, proj4string(mapa))
      plot(mapaAux,col = 'green', lwd = 2, add = TRUE)
    }
    
    if (legenda == T){
      if(linhasTransp == T){
        par(mar=c(0,0,0,0))
        plot.new()
        labels <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significativo", "Corredores Ônibus", "Metrô", "Trem")
        colors <- c("red", "blue", "lightpink", "skyblue2", "white", "green", "purple", "yellow")
        legend("center", 
               legend = labels, 
               col = colors,
               fill = colors,
               lty = c(NA, NA, NA, NA, NA, 1,1,1),
               lwd = c(NA, NA, NA, NA, NA, 2,4,4),
               border = c('black', 'black','black','black','black',NA,NA,NA),
               cex = 2,
               density = c(NA,NA,NA,NA,NA,0,0,0),
               x.intersp = c(0.5,0.5,0.5,0.5,0.5,2,2,2))
      }else{
        par(mar=c(0,0,0,0))
        plot.new()
        legend("center", legend = labels, fill = colors, bty = "n", cex = 2)
        }
    }
  } else{
    breaks <- seq(1, 5, 1)
    
    # Set the corresponding labels for the thematic map classes
    labels <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significante")
    
    # see ?findInterval - This is necessary for making a map
    np <- findInterval(mapa$LISA__trel, breaks)
    
    # Assign colors to each map class
    par(mar=c(0,0,2,0))
    colors <- as.integer(as.character(mapa@data[,column]))
    colors[colors == 0] <- 'white'
    colors[colors == 1] <- 'red'
    colors[colors == 2] <- 'blue'
    colors[colors == 3] <- 'skyblue2'
    colors[colors == 4] <- 'lightpink'
    plot(mapa, col = colors)  #colors[np] manually sets the color for each county
    title(agregLab, cex.main = 2)
    colors <- c("red", "blue",  "lightpink" ,"skyblue2", "white" )
    if (legenda == T){
      par(mar=c(0,0,0,0))
      plot.new()
      legend("center", legend = labels, fill = colors, bty = "n", cex = 2)
    }
  }
  
}


#distritos

jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/LISAMapDistritos.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 6, nrow = 2),widths = 1,heights = 0.5)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_distritos.shp',
          column = 4,
          agregLab = 'A) Diferença de Tempos')

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
           column = 11,
           legenda = T,
          agregLab = 'B) Tempo Relativo')
dev.off()

#areas de ponderacao
jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/LISAMapsAreasPond.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 6, nrow = 2),widths = 1,heights = 0.5)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_areas_de_ponderacao.shp',
           agregLab = 'A) Diferença de Tempos',
           column = 6)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_areas_de_ponderacao.shp',
           legenda = T,
           agregLab = 'B) Tempo Relativo',
           column = 6)
dev.off()

#distritos

jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/LISAMapDistritosLinhas.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 6, nrow = 2),widths = 1,heights = 0.5)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_distritos.shp',
          column = 4,
          agregLab = 'A) Diferença de Tempos',linhasTransp = T)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
          column = 11,
          legenda = T,
          agregLab = 'B) Tempo Relativo',linhasTransp = T)
dev.off()

#areas de ponderacao
jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/LISAMapsAreasPondLinhas.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 6, nrow = 2),widths = 1,heights = 0.5)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_areas_de_ponderacao.shp',
          agregLab = 'A) Diferença de Tempos',
          column = 6,linhasTransp = T)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_areas_de_ponderacao.shp',
          legenda = T,
          agregLab = 'B) Tempo Relativo',
          column = 6,linhasTransp = T)
dev.off()

  #residuos OLS e SAR

#distritos

jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/LISAMapDistritosOLS.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 6, nrow = 2),widths = 1,heights = 0.5)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
          column = 55,
          agregLab = 'A) Diferença de Tempos',
          residual_map = T)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
          column = 56,
          legenda = T,
          agregLab = 'B) Tempo Relativo',
          residual_map = T)
dev.off()

#SAR
jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/LISAMapDistritosSAR.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 6, nrow = 2),widths = 1,heights = 0.5)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
          column = 48,
          agregLab = 'A) Diferença de Tempos',
          residual_map = T)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
          column = 51,
          legenda = T,
          agregLab = 'B) Tempo Relativo',
          residual_map = T)
dev.off()

#Distribuição espacial das variáveis
jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/DistribuicaoVariaveis.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,2,2), ncol = 2, nrow = 2),widths = 1,heights = 0.5)
ndiv <- 6

mapa <- readOGR('PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp')
#diferenca de tempo
var <- mapa@data$Tempo_dif
# Set the breaks for the thematic map classes
breaks <- seq(min(var), max(var), (max(var)-min(var))/ndiv)
np <- findInterval(var, breaks,all.inside = T)
colors <- c(RColorBrewer::brewer.pal(ndiv,'Greens'))

legenda <- c()
for(i in 2:length(breaks)){
  aux <- paste0(round(breaks[i-1],digits = 0) , "s - ", round(breaks[i], digits =0),'s')
  legenda <- c(legenda, aux)
}

plot(mapa, col = colors[np])
agregLab = 'A) Diferença de Tempos'
title(agregLab, cex.main = 2)
legend(x=-46.55,
       y = -23.7,
       legend = legenda,
       fill = colors)

#tempo relativo
var <- mapa@data$Temp_rel
# Set the breaks for the thematic map classes
breaks <- seq(min(var), max(var), (max(var)-min(var))/ndiv)
np <- findInterval(var, breaks,all.inside = T)
colors <- c(RColorBrewer::brewer.pal(ndiv,'Oranges'))

plot(mapa, col = colors[np])
agregLab = 'B) Tempo Relativo'
title(agregLab, cex.main = 2)

legenda <- c()
for(i in 2:length(breaks)){
  aux <- paste0(round(breaks[i-1],digits = 2) , " - ", round(breaks[i], digits =2))
  legenda <- c(legenda, aux)
}

legend(x=-46.55,
       y = -23.7,
       legend = legenda,
       fill = colors)
dev.off()
