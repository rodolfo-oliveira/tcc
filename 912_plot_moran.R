#função para gráficos I de Moran

library(rgdal)
library(spdep)
library(dplyr)
library(sp)
library(ggplot2)

#mapa = shapefile do conjunto de dados
#xlab = nome do eixo x
#ylab = nome do eixo y
#pch = estilo de pontos no gráfico
#column = nome da variável de análise
#agregLab = nome do mapa
#abs = se TRUE, considera os valores absolutos do dados para analise

plot_moran <- function(
  mapa,
  xlab,
  ylab,
  pch = 10,
  agregLab = F,
  column,
  col,
  abs = F)
{

  #filtragem de observações nulas
  mapa <- mapa[is.na(mapa@data[,column])==F,]
  #separação da variável de análise
  var <- mapa@data[,column]
  
  #caso flag seja TRUE, cálculo do valor absoluto da variável
  if(abs == T){var <- abs(mapa@data[,column])}
  
  #cálculo do teste de moran 
  morantest <- moran.test(x = as.numeric(var),
                          listw = nb2listw(neighbours = poly2nb(mapa)),alternative = 'two.sided',randomisation = T)
  #cálculo do moran local para as zonas de análise
  morant <- localmoran(x = as.numeric(var),
                       listw = nb2listw(neighbours = poly2nb(mapa)), alternative = 'two.sided', p.adjust.method = 'holm')
  
  #padronização da variável de análise
  mapa$varI <- scale(var)
  #cálculo do lag da variável
  mapa$lag_varI <- lag.listw(nb2listw(neighbours = poly2nb(mapa)), mapa$varI)
  
  #calculo dos limites do gráfico
  xlim <- ifelse(abs(min(mapa$varI)) < abs(max(mapa$varI)), abs(max(mapa$varI)), abs(min(mapa$varI)))
  ylim <- ifelse(abs(min(mapa$lag_varI)) < abs(max(mapa$lag_varI)), abs(max(mapa$lag_varI)), abs(min(mapa$lag_varI)))
  
  
  xlim <- c(-xlim,xlim)
  ylim <- c(-ylim,ylim)
  
  #impressão do gráfico de autocorrelação espacial
  plot(x = mapa$varI,
       y = mapa$lag_varI,
       pch = pch,
       col = col,
       xlim = xlim,
       ylim = ylim,
       xlab = xlab,
       ylab = ylab) #, cex.lab=1.4, cex.axis=1.2, cex.main=1.2, cex.sub=1.2)
  abline(h = 0, v = 0)
  abline(lm(mapa$lag_varI ~ mapa$varI), lty = 3, lwd = 2, col = "red")
  
  text(x = (mean(xlim) + 12*(xlim[2]-xlim[1])/40),
       y = (ylim[1] + (ylim[2]-ylim[1])/24),
       paste0('Valor do I de Moran: ', round(as.numeric(morantest$estimate[1]), digits = 4)), cex = 1.1)
  if(agregLab!=F){
    text(x = (xlim[1] + (xlim[2]-xlim[1])/4),
         y = (ylim[2] - (ylim[2]-ylim[1])/40),
         agregLab)
    text(x = (xlim[1] + (xlim[2]-xlim[1])/4),
         y = (ylim[1] + (ylim[2]-ylim[1])/40),
         paste0('N = ', length(mapa$varI)))
  }
}