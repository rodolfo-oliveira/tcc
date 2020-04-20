#diagnostics on buffering

source('503_buffering_functions.R')

#assumindo velocidade de 6 km/h ou 100m/min e criterio de 95% das viagens 
bufferSize <- as.integer(100*quantile(c(database_publico_origem@data$ANDA_O, database_publico_origem@data$ANDA_D), 0.95))


count <- count_buffer_trips(spatialDatabaseODOrigin = database_publico_origem,
                            spatialDatabaseODestination = database_publico_destino,
                            spatialSimulatedDatabaseOrigin = simulated_database_origem,
                            spatialSimulatedDatabaseDestination = simulated_database_destino,
                            bufferSize = bufferSize)

summary(count[which(database_publico_origem$MUNI_O==36 & database_publico_origem$MUNI_D==36)])
hist(count[which(database_publico_origem$MUNI_O==36 & database_publico_origem$MUNI_D==36)])



ssociatedTrips <- associate_buffer_trips(spatialDatabaseODOrigin = database_publico_origem,
                                          spatialDatabaseODestination = database_publico_destino,
                                          spatialSimulatedDatabaseOrigin = simulated_database_origem,
                                          spatialSimulatedDatabaseDestination = simulated_database_destino,
                                          bufferSize = bufferSize)

summary(associatedTrips$IDs[which(database_publico_origem$MUNI_O==36 & database_publico_origem$MUNI_D==36)])
hist(associatedTrips$IDs[which(database_publico_origem$MUNI_O==36 & database_publico_origem$MUNI_D==36)])
