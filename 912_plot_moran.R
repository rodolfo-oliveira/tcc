#função para gráficos I de Moran

library(rgdal)
library(spdep)
library(dplyr)
library(sp)
library(ggplot2)

plot_moran <- function(
  OGRdsn,
  xlab,
  ylab,
  pch = 10,
  agregLab = F,
  column,
  col
)
{
  mapa <- readOGR(dsn = OGRdsn)
  mapa <- mapa[is.na(mapa@data[,column])==F,]
  
  morantest <- moran.test(x = as.numeric(mapa@data[,column]),
                          listw = nb2listw(neighbours = poly2nb(mapa)))
  morant <- localmoran(x = as.numeric(mapa@data[,column]),
                       listw = nb2listw(neighbours = poly2nb(mapa)))
  
  
  mapa$varI <- scale(mapa@data[,column])
  mapa$lag_varI <- lag.listw(nb2listw(neighbours = poly2nb(mapa)), mapa$varI)
  
  xlim <- ifelse(abs(min(mapa$varI)) < abs(max(mapa$varI)), abs(max(mapa$varI)), abs(min(mapa$varI)))
  ylim <- ifelse(abs(min(mapa$lag_varI)) < abs(max(mapa$lag_varI)), abs(max(mapa$lag_varI)), abs(min(mapa$lag_varI)))
  
  
  xlim <- c(-xlim,xlim)
  ylim <- c(-ylim,ylim)
  
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