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
  linhasTransp = F
)
{
  #reference: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
  mapa <- readOGR(dsn = OGRdsn)
  
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
  labels <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Signif.")
  
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
      labels <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Signif.", "Corredores Ônibus", "Metrô", "Trem")
      colors <- c("red", "blue", "lightpink", "skyblue2", "white", "green", "purple", "yellow")
      legend("center", legend = labels, fill = colors, bty = "n", cex = 2)
    }else{
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

jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/LISAMapDistritos.jpeg', width = 960, height = 480)
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3), ncol = 6, nrow = 2),widths = 1,heights = 0.5)

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
          column = 50,
          agregLab = 'A) Diferença de Tempos')

plot_LISA(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
          column = 11,
          legenda = T,
          agregLab = 'B) Tempo Relativo')
dev.off()

