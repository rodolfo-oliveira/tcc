#script para mapas LISA



plot_LISA <- function(
  OGRdsn,
  column,
  legenda = F,
  agregLab,
  linhasTransp = F,
  residual_map = F,
  abs = F)
{
  require(rgdal)
  require(spdep)
  require(dplyr)
  require(sp)
  require(ggplot2)
  #reference: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
  mapa <- readOGR(dsn = OGRdsn)
  
  mapa <- mapa[is.na(mapa@data[,column])==F,]
  
  var <- mapa@data[,column]
  if(abs == T){var <- abs(mapa@data[,column])}
  
  if(residual_map == F){
    morantest <- moran.test(x = as.numeric(var),
                            listw = nb2listw(neighbours = poly2nb(mapa)),alternative = 'two.sided', randomisation = T)
    
    morant <- localmoran(x = as.numeric(var),
                         listw = nb2listw(neighbours = poly2nb(mapa)), alternative = 'two.sided', p.adjust.method = 'holm')
    
    
    mapa$varI <- scale(var)
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
    title(agregLab, cex.main = 2, line = -2)
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
        labels <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significativo", "Corredores Ônibus", "Metrô", "Trem")
        colors <- c("red", "blue", "lightpink", "skyblue2", "white", "green", "purple", "yellow")
        legend(x = mapa@bbox[3]-(mapa@bbox[3]-mapa@bbox[1])*4/9,
               y = mapa@bbox[4]-(mapa@bbox[4]-mapa@bbox[2])/2, 
               legend = labels, 
               col = colors,
               fill = colors,
               lty = c(NA, NA, NA, NA, NA, 1,1,1),
               lwd = c(NA, NA, NA, NA, NA, 2,4,4),
               border = c('black', 'black','black','black','black',NA,NA,NA),
               density = c(NA,NA,NA,NA,NA,0,0,0),
               x.intersp = c(0.5,0.5,0.5,0.5,0.5,2,2,2), cex = 1.5)
      }else{
        par(mar=c(0,0,0,0))
        legend(x = mapa@bbox[3]-(mapa@bbox[3]-mapa@bbox[1])*4/9,
               y = mapa@bbox[4]-(mapa@bbox[4]-mapa@bbox[2])/2,
               legend = labels,
               col = colors,
               fill = colors,
               #bty = "n",
               border = c('black', 'black','black','black','black'), cex = 1.5)
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
    colors <- as.integer(as.character(var))
    colors[colors == 0] <- 'white'
    colors[colors == 1] <- 'red'
    colors[colors == 2] <- 'blue'
    colors[colors == 3] <- 'skyblue2'
    colors[colors == 4] <- 'lightpink'
    plot(mapa, col = colors)  #colors[np] manually sets the color for each county
    title(agregLab)
    colors <- c("red", "blue",  "lightpink" ,"skyblue2", "white" )
    if (legenda == T){
      par(mar=c(0,0,0,0))
      legend(x = mapa@bbox[3]-(mapa@bbox[3]-mapa@bbox[1])*4/9,
             y = mapa@bbox[4]-(mapa@bbox[4]-mapa@bbox[2])/2,
             legend = labels,
             fill = colors,
             col = colors,
             #bty = "n",
             border = c('black', 'black','black','black','black'), cex = 1.5)
    }
  }
  
}
