library(dplyr)
source('502_simulation_processing_functions.R')

simulated_database_origem <- simulation_data_to_sf(origem = T)
simulated_database_destino <- simulation_data_to_sf(origem = F)
