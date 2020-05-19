#sourcing for analysis functions

#calculating means of simulated trips
source('908_means_buffered_trips.R')

#functions for measuring differences and ratios
source('909_measures_for_buffered_trips.R')


#sqrt(var(diferencas$ODmeasure, na.rm = T)**2/length(diferencas$ODmeasure) +var(diferencas$simulatedMeasure, na.rm = T)**2/length(diferencas$simulatedMeasure))

#t.test(diferencas$ODmeasure,diferencas$simulatedMeasure,paired = T)
