import csv
import random
import localfunctions
import numpy
import googlemaps
import json
import datetime
import pymysql
from mysql.connector import MySQLConnection, Error

#Conexao com o cloudsql
connection = pymysql.connect(unix_socket='/cloudsql/' + 'indicador-de-mobilidade:us-east1:indicador-de-mobilidade2',
                             user='root',
                             password='654753rol',
                             db='viagens_publico')



#funcao para inserir as entradas na base de dados do Gcloud
def insert_viagens(viagens, connection,):

	query = """INSERT INTO viagens (year, hour, weekday, origincoord, originadress, destincoord, destinadress, duration, distance, fare, type) 
			VALUES(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)"""
	
	try:
		
		conn = connection
	 
		cursor = conn.cursor()
		cursor.executemany(query, viagens)
	 
		conn.commit()
	except Error as e:
		print('Error:', e)
	 
	finally:
		cursor.close()
		conn.close()


name = 'Adresses.csv'
now = datetime.datetime.now()

b = localfunctions.sorting_adresses(name,10)


origins = b[0]
destinations = b[1]

#Realizando a chamada da API para transporte publico

APIclient = googlemaps.Client(key='AIzaSyAKFxJ5VH5mDKE-uNxDsMnahff-U84bNZ0')
APIdata = APIclient.distance_matrix(origins, destinations, mode='transit', units='metric')
print APIdata
datafram = []

#interpretando os resultado Json para compor uma matriz
for i in range (len(APIdata['rows'])):

	for j in range (len(APIdata['rows'][i]['elements'])):
		
		#para os casos em que a coordenada nap acha um endereco
		if APIdata['rows'][i]['elements'][j]['status'] == "OK":

			row_aux = (now.strftime("%d-%m-%Y"), now.strftime("%H:%M"), now.strftime("%A"), origins[i], APIdata['origin_addresses'][i], destinations[j], APIdata['destination_addresses'][j], int(APIdata['rows'][i]['elements'][j]['duration']['value']), int(APIdata['rows'][i]['elements'][j]['distance']['value']), 'NORESULT', 'traffic')
			
			#para os casos em que nao ha o dado tarifa 
			if 'fare' in APIdata['rows'][i]['elements']:
				row_aux[9] = APIdata['rows'][i]['elements'][j]['fare']['value']

		else:
			row_aux = ('NORESULTS', 'NORESULTS', 'NORESULTS','NORESULTS', 'NORESULTS','NORESULTS','NORESULTS', 0, 0, 'NORESULTS', 'NORESULTS')
		
		datafram.append(row_aux)


#Realizando a chamada da API para transporte privado

#APIclient = googlemaps.Client(key='AIzaSyAKFxJ5VH5mDKE-uNxDsMnahff-U84bNZ0')
#APIdata = APIclient.distance_matrix(origins, mode = "driving",destinations, units='metric')



#interpretando os resultado Json para compor uma matriz
for i in range (len(APIdata['rows'])):

	for j in range (len(APIdata['rows'][i]['elements'])):
		
		#para os casos em que a coordenada nap acha um endereco
		if APIdata['rows'][i]['elements'][j]['status'] == "OK":

			row_aux = (now.strftime("%d-%m-%Y"), now.strftime("%H:%M"), now.strftime("%A"), origins[i], APIdata['origin_addresses'][i], destinations[j], APIdata['destination_addresses'][j], int(APIdata['rows'][i]['elements'][j]['duration']['value']), int(APIdata['rows'][i]['elements'][j]['distance']['value']), 'NORESULT', 'private')
			
			#para os casos em que nao ha o dado tarifa 
			if 'fare' in APIdata['rows'][i]['elements']:
				row_aux[9] = APIdata['rows'][i]['elements'][j]['fare']['value']

		else:
			row_aux = ('NORESULTS', 'NORESULTS', 'NORESULTS','NORESULTS', 'NORESULTS','NORESULTS','NORESULTS', 0, 0, 'NORESULTS', 'NORESULTS')
		
		datafram.append(row_aux)

print datafram


Can't connect to MySQL server on 'localhost' ([Errno 111] Connection refused)