
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

for(i in 1:length(nomes)){

  
  jpeg(filename = paste0('70',i,'_LISA_',siglas[i],".jpeg"),width = 480, height = 960)
  plot_LISA(OGRdsn = 'ZonasODDados.shp',
            column = siglas[i],
            legenda = T,
            agregLab = nomes[i],
            linhasTransp = F,abs = T)
  dev.off()
  
  
  
  jpeg(filename = paste0('70',i,'_Moran_',siglas[i],".jpeg"),width = 480, height = 480)
  plot_moran(OGRdsn = 'ZonasODDados.shp',
             xlab = 'Diferença de Tempos',
             ylab = 'Lag da Diferença de Tempos',
             column = siglas[i],
             agregLab = nomes[i],col = 'black',abs = T)
  dev.off()
}

