#script para mapas LISA
#função que imprime diagramas de autocorrelação espacial para o dado indicado

#mapa = shapefile do conjunto de dados 
#column = nome da variável de análise
#legenda = se TRUE, imprime legenda
#agregLab = nome do mapa
#linhasTransp = se TRUE, imprime linhas de transporte,
#residual_map = controle não utilizado
#abs = se TRUE, considera os valores absolutos do dados para analise

plot_LISA <- function(
  mapa,
  column,
  legenda = F,
  agregLab,
  linhasTransp = F,
  #controle não utilizado
  residual_map = F,
  abs = F)
{
  require(rgdal)
  require(spdep)
  require(dplyr)
  require(sp)
  require(ggplot2)
  #reference: http://rstudio-pubs-static.s3.amazonaws.com/4938_b5fc230d586c48b291627ff6ea484d2e.html
  mapaMor <- mapa
  
  #retiradas de valores nulos
  mapa <- mapaMor[is.na(mapaMor@data[,column])==F,]
  
  #separação do dado do banco
  var <- mapa@data[,column]
  
  #transformaçao do dados em valor absoluto
  if(abs == T){var <- abs(mapa@data[,column])}
  
  if(residual_map == F){
    #calculo do teste de moran
    morantest <- moran.test(x = as.numeric(var),
                            listw = nb2listw(neighbours = poly2nb(mapa)),
                            alternative = 'two.sided',adjust.n = T,
                            randomisation = T)
    
    #calculo do maran local
    morant <- localmoran(x = as.numeric(var),
                         listw = nb2listw(neighbours = poly2nb(mapa)), alternative = 'two.sided', p.adjust.method = 'holm')
    
    
    #padronização da variável de análise
    mapa$varI <- scale(var)
    #cálculo do lag da variável
    mapa$lag_varI <- lag.listw(nb2listw(neighbours = poly2nb(mapa)), mapa$varI)
    

  

    #associação dos valores relacionados aos quadrantes de autocorrelação às zonas de analise
    mapa$quad_sig <- NA
    mapa@data[(mapa$varI >= 0 & mapa$lag_varI >= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 1
    mapa@data[(mapa$varI <= 0 & mapa$lag_varI <= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 2
    mapa@data[(mapa$varI >= 0 & mapa$lag_varI <= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 3
    mapa@data[(mapa$varI >= 0 & mapa$lag_varI <= 0) & (morant[, 5] <= 0.05), "quad_sig"] <- 4
    mapa@data[(morant[, 5] > 0.05), "quad_sig"] <- 5  #WE ASSIGN A 5 TO ALL NON-SIGNIFICANT OBSERVATIONS
    
    #salvando os dados no mapa
    mapa <- merge(mapaMor, mapa[,c("NumerZn","varI","lag_varI", "quad_sig")], by = 'NumerZn', all.x = T)
    
    #preenchendo as zonas com valores nulos de uma identificação para vazio
    mapa@data[is.na(mapa$varI),'quad_sig'] <- 6
    
    
    # Set the breaks for the thematic map classes
    breaks <- seq(1, 6, 1)
    
    # Set the corresponding labels for the thematic map classes
    labels <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significante", "Sem dados")
    
    # see ?findInterval - This is necessary for making a map
    np <- findInterval(mapa$quad_sig, breaks)
    
    # Assign colors to each map class
    colors <- c("red", "blue", "lightpink", "skyblue2", "white", "darkgrey")
    par(mar=c(0,0,2,0))
    plot(mapa, col = colors[np], border = 'gray40')  #colors[np] manually sets the color for each county
    title(agregLab, cex.main = 1.6, line = -2)
    #corredor de onibus
    if(linhasTransp == T){
      #impressão das linha de trasnporte caso flag seja T
      
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
      #impressão da legenda caso flag seja TRUE
      if(linhasTransp == T){
       #legenda específica para linhas de transporte
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
        #legenda sem linhas de transporte
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
    plot(mapa, col = colors, border = 'gray40')  #colors[np] manually sets the color for each county
    title(agregLab, cex.main = 1.6)
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
