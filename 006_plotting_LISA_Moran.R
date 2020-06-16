
#plotting LISA maps
source('505_plotting_functions.R', encoding = "UTF-8")
library('rgdal')

siglas <- c("dfDstPb", 
            "dfDstPr",
            "dfDstTRsPb",
            "dfDstTRsPr", 
            "difOrPb", 
            "dfOrPrv",
            "dfOrTRsPb",
            "dfOrTRsPr")

nomes <-  c(paste0(strwrap('Tr. Público - Destino', width = 25), collapse = '\n'),
            paste0(strwrap('Tr. Privado - Destino', width = 25), collapse = '\n'),
            paste0(strwrap('Tr. Público - Restrição Temporal - Destino', width = 25), collapse = '\n'),
            paste0(strwrap('Tr. Privado - Restrição Temporal - Destino', width = 25), collapse = '\n'),
            paste0(strwrap('Tr. Público - Origem', width = 25), collapse = '\n'),
            paste0(strwrap('Tr. Privado - Origem', width = 25), collapse = '\n'),
            paste0(strwrap('Tr. Público - Restrição Temporal - Origem', width = 25), collapse = '\n'),
            paste0(strwrap('Tr. Privado - Restrição Temporal - Origem', width = 25), collapse = '\n'))

mapa <- readOGR(dsn = 'ZonasODDados.shp', encoding = 'UTF-8')



for(i in 1:length(nomes)){

  
  jpeg(filename = paste0('70',i,'_LISA_',siglas[i],".jpeg"),width = 600, height = 960)
  plot_LISA(mapa = mapa,
            column = siglas[i],
            legenda = T,
            agregLab = nomes[i],
            linhasTransp = F,
            abs = F)
  dev.off()
  
  
  if(stringr::str_detect(nomes[i],'Público')){
    if(stringr::str_detect(nomes[i],'Restrição')){
      if(stringr::str_detect(nomes[i],'Origem')){
        col <- 'darkgoldenrod4'
      } else{
        col <- 'darkgoldenrod4'
      }
    }else{
      if(stringr::str_detect(nomes[i],'Origem')){
        col <- 'darkgoldenrod4'
      }else{
        col <- 'darkgoldenrod4'
        }
      }
    }else{
      if(stringr::str_detect(nomes[i],'Restrição')){
        if(stringr::str_detect(nomes[i],'Origem')){
          col <- 'forestgreen'
        }else{
          col <- 'forestgreen'
        }
      }else{
        if(stringr::str_detect(nomes[i],'Origem')){
          col <- 'forestgreen'
        }else{
          col <- 'forestgreen'
        }
      }
    }
  
  
  jpeg(filename = paste0('70',i,'_Moran_',siglas[i],".jpeg"),width = 480, height = 480)
  plot_moran(mapa = mapa,
             xlab = 'Diferença de Tempos',
             ylab = 'Lag da Diferença de Tempos',
             column = siglas[i],
             agregLab = nomes[i],col = col,abs = F,pch = 19  )
  dev.off()
  
  jpeg(filename = paste0('70',i,'_mapa_',siglas[i],".jpeg"),width = 600, height = 900)
  plot_MAP(mapa = mapa,
            column = siglas[i],
            agregLab = nomes[i],
           col = col)
  dev.off()
}

