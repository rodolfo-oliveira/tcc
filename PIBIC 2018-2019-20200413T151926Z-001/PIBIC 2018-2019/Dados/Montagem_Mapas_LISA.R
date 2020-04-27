#script para mapas LISA

library(rgdal)
library(spdep)
library(dplyr)
library(sp)
library(ggplot2)

plot_LISA <- function(
  OGRdsn,
  xlab,
  ylab,
  xlim = c(-6, 6),
  ylim = c(-6, 6),
  pch = 10,
  agregLab = F,
  column
)
{
  #reference: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
  mapa <- readOGR(dsn = OGRdsn)
  
  morantest <- moran.test(x = as.numeric(mapa@data[,column]),
                         listw = nb2listw(neighbours = poly2nb(mapa)))
  morant <- localmoran(x = as.numeric(mapa@data[,column]),
                       listw = nb2listw(neighbours = poly2nb(mapa)))
  
  
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
  labels <- c("high-High", "low-Low", "High-Low", "Low-High", "Not Signif.")
  
  # see ?findInterval - This is necessary for making a map
  np <- findInterval(mapa$quad_sig, breaks)
  
  # Assign colors to each map class
  colors <- c("red", "blue", "lightpink", "skyblue2", "white")
  plot(mapa, col = colors[np])  #colors[np] manually sets the color for each county
  mtext("Local Moran's I", cex = 1.5, side = 3, line = 1)
  legend("topleft", legend = labels, fill = colors, bty = "n")
  

  }
}


#distritos

jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/MoranIDistritos', width = 960, height = 480)
layout(matrix(c(1,2), ncol = 2),widths = 1,heights = 2,)

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_distritos.shp',
           xlab = 'Diferença de Tempos',
           ylab = 'Lag de Diferença de Tempos',
           agregLab = 'Distritos',
           pch = 20,
           column = 4)

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
           xlab = 'Tempo Relativo',
           ylab = 'Lag de Tempo Relativo',
           agregLab = 'Distritos',
           pch = 20,
           column = 11)
dev.off()

#areas de ponderacao
jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/MoranIAreasPond', width = 960, height = 480)
layout(matrix(c(1,2), ncol = 2),widths = 1,heights = 2,)

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_areas_de_ponderacao.shp',
           xlab = 'Diferença de Tempos',
           ylab = 'Lag de Diferença de Tempos',
           agregLab = 'Áreas de Ponderação',
           pch = 20,
           column = 6)

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_areas_de_ponderacao.shp',
           xlab = 'Tempo Relativo',
           ylab = 'Lag de Tempo Relativo',
           agregLab = 'Áreas de Ponderação',
           pch = 20,
           column = 6)
dev.off()