library(dplyr)
library(sf)
source("501_OD_processing_functions.R")

#extracting projection
proj <- read_sf('OD 2017/Mapas/Shape/Municipios_2017_region.shp')
proj <- sf::st_crs(proj)

#convertendo para sf
#viagens de transporte motorizado (carro, proprio ou 'alugado')
database_privado_origem <- convert_spatial(database_privado, proj, origem = T)
database_privado_destino <- convert_spatial(database_privado, proj, origem = F)
rm(database_privado)
#viagens de transporte motorizado (moto, dirigindo ou passageiro)
database_privado_moto_origem <- convert_spatial(database_privado_moto, proj, origem = T)
database_privado_moto_destino <- convert_spatial(database_privado_moto, proj, origem = F)
rm(database_privado_moto)
#viagens de transporte público - todos os modais (sem fretados privados e sem trasnporte escolar)
database_publico_origem <- convert_spatial(database_publico, proj, origem = T)
database_publico_destino <- convert_spatial(database_publico, proj, origem = F)
rm(database_publico)
#viagens de transporte fretados privados
database_fretados_origem <- convert_spatial(database_fretados, proj, origem = T)
database_fretados_destino <- convert_spatial(database_fretados, proj, origem = F)
rm(database_fretados)
#viagens de transporte escolar
database_escolar_origem <- convert_spatial(database_escolar, proj, origem = T)
database_escolar_destino <- convert_spatial(database_escolar, proj, origem = F)
rm(database_escolar)