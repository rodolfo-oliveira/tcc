library(dplyr)
library(sp)
source('502_simulation_processing_functions.R')

#espacialização do banco de dados simulados
simulated_database_origem <- simulation_data_to_sp(origem = T)
simulated_database_destino <- simulation_data_to_sp(origem = F)

#reprojeção dos dados simulados para a projeção dos dados da pesquisa OD
simulated_database_origem <- spTransform(simulated_database_origem, CRSobj = CRS(as.character(rgdal::make_EPSG()[rgdal::make_EPSG()$code %in% 31983,]$prj4)))
simulated_database_destino <- spTransform(simulated_database_destino, CRSobj  = CRS(as.character(rgdal::make_EPSG()[rgdal::make_EPSG()$code %in% 31983,]$prj4)))

