#sourcing functions to process simulated data

#função para converter a base de dados simulada para formato sp
source("902_convert_spatial_sim.R")

#função que encapsula a função convert_spatial_sim, e adapta o banco para a transformação
source("903_simulation_data_to_sp.R")
