import csv
import random
import numpy


def sorting_adresses(nameoffile, sizeofarrays):
	#inicializando a lista de enderecos, de origens e de destinos
	Adresses = list()
	origins = list()
	destinations = list()

	#lendo o arquivo de enderecos
	with open(nameoffile, 'r') as csvFile:
	    reader = csv.reader(csvFile)
	    for row in reader:
	        Adresses.append(row)
	csvFile.close()

	#adicionando uma coluna nos enderecos e formatando no forma Lat,Lon
	Adresses = [x + [0] for x in Adresses]

	for i in range(len(Adresses[0])):
		if isinstance(Adresses[0][i], str) and ('Lon' in Adresses[0][i] or 'Lon' in Adresses[0][i]):
			lon = i
	
		if isinstance(Adresses[0][i], str) and ('Lat' in Adresses[0][i] or 'Lat' in Adresses[0][i]):
			lat = i

	for i in range(len(Adresses)):
		Adresses[i][len(Adresses[0])-1] = str(Adresses[i][lat]+','+ Adresses[i][lon])

	#sorteando as origens das viagens
	for i in range (sizeofarrays):
		origins.append(random.choice(Adresses))

	#sorteando os destinos das viagens
	for i in range (sizeofarrays):
		destinations.append(random.choice(Adresses))

	origins = numpy.transpose(origins)[len(numpy.transpose(origins))-1]
	destinations = numpy.transpose(destinations)[len(numpy.transpose(destinations))-1]


	return origins,destinations









