#script para mapas LISA

library(rgdal)
library(spdep)
library(dplyr)
library(sp)
library(ggplot2)

plot_moran <- function(
  OGRdsn,
  xlab,
  ylab,
  xlim = c(-6, 6),
  ylim = c(-6, 6),
  pch = 10,
  agregLab = F,
  column,
  col
)
  {
  mapa <- readOGR(dsn = OGRdsn)
  
  morantest <- moran.test(x = as.numeric(mapa@data[,column]),
                          listw = nb2listw(neighbours = poly2nb(mapa)))
  morant <- localmoran(x = as.numeric(mapa@data[,column]),
                       listw = nb2listw(neighbours = poly2nb(mapa)))
  
  
  mapa$varI <- scale(mapa@data[,column])
  mapa$lag_varI <- lag.listw(nb2listw(neighbours = poly2nb(mapa)), mapa$varI)

  plot(x = mapa$varI,
       y = mapa$lag_varI,
       pch = pch,
       col = col,
       xlim = xlim,
       ylim = ylim,
       xlab = xlab,
       ylab = ylab, cex.lab=1.4, cex.axis=1.2, cex.main=1.2, cex.sub=1.2)
  abline(h = 0, v = 0)
  abline(lm(mapa$lag_varI ~ mapa$varI), lty = 3, lwd = 2, col = "red")
  
  text(x = (mean(xlim) + (xlim[2]-xlim[1])/4),
       y = (ylim[1] + (ylim[2]-ylim[1])/12),
       paste0('Valor do I de Moran: ', round(as.numeric(morantest$estimate[1]), digits = 4)), cex = 1.2)
  if(agregLab!=F){
    text(x = (xlim[1] + (xlim[2]-xlim[1])/4),
         y = (ylim[2] - 0*(ylim[2]-ylim[1])/60),
         agregLab)
    text(x = (xlim[1] + (xlim[2]-xlim[1])/4),
         y = (ylim[2] - 0*(ylim[2]-ylim[1])/60 - 1),
         paste0('N = ', length(mapa$varI)))
  }
}


#distritos

jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/MoranIDistritos.jpeg', width = 960, height = 480)
layout(matrix(c(1,2), ncol = 2),widths = 1,heights = 2,)

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_distritos.shp',
           xlab = 'Diferença de Tempos',
           ylab = 'Lag de Diferença de Tempos',
           agregLab = 'Distritos',
           pch = 20,
           column = 4,col = 'orange')

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp',
           xlab = 'Tempo Relativo',
           ylab = 'Lag de Tempo Relativo',
           agregLab = 'Distritos',
           pch = 20,
           column = 11, col = 'darkgreen')
dev.off()

#areas de ponderacao
jpeg(filename = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/MoranIAreasPond.jpeg', width = 960, height = 480)
layout(matrix(c(1,2), ncol = 2),widths = 1,heights = 2,)

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_areas_de_ponderacao.shp',
           xlab = 'Diferença de Tempos',
           ylab = 'Lag de Diferença de Tempos',
           agregLab = 'Áreas de Ponderação',
           pch = 20,
           column = 6, col = 'orange')

plot_moran(OGRdsn = 'PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_areas_de_ponderacao.shp',
           xlab = 'Tempo Relativo',
           ylab = 'Lag de Tempo Relativo',
           agregLab = 'Áreas de Ponderação',
           pch = 20,
           column = 6, col = 'darkgreen')
dev.off()
