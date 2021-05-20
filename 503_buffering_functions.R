#fuctions related to buffering trips

#função para selecionar pontos espaciais dentro de um buffer centrado em um ponto espacial
source('904_pluck_simulated_trip.R')

#funçao para selecionar o conjunto de viagens simuladas que se iniciam e terminam próximas da origem e destino de uma viagem da pesquisa OD
source('905_origin_destination_buffer_trip.R')

#função para contar o número de viagens selecionadas para uma origem e destino de viagem da OD
source('906_count_buffer_trips.R')

#função que associa as viagens simuladas dentro do buffer à viagem da Pesquisa OD 
source('907_associate_buffer_trips.R')